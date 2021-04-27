library(Matrix)
library(rosqp)


# Quadprog CASANOVA maker
CASANOVA_objectmaker = function(y, Xl, Xf, adaptive = FALSE, prev.coefficients = NULL, observation.weights = NULL) {
  # We need to deal differently with Xl depending on whether or not it contains an intercept term
  Xl = as.matrix(Xl)
  Xf = as.data.frame(Xf)
  was.prev.coefficients = prev.coefficients
  if ( is.null(prev.coefficients) == FALSE ) adaptive = TRUE
  n = dim(Xf)[ 1 ]
  pf = dim(Xf)[ 2 ]
  pl = dim(Xl)[ 2 ]
  p = rep(0, pf)
  d = rep(0, pf)
  w = list()
  D = list()
  X = list()
  Xm = list()
  L = list()
  Amat = list()
  bvec = list()
  lvec = list()
  uvec = list()
  catnames = list()
  for ( j in 1:pf ) {
    catnames[[ j ]] = levels(Xf[ , j ])
  }
  # Dmat = list()
  dvec = list()
  if ( is.null(names(Xf)) ) {
    namevec = rep(0, pf)
    for ( j in 1:pf ) {
      namevec[ j ] =  paste0("V", j)
    }
    names(Xf) = namevec
  }
  varnames = names(Xf)
  adaptive.weights = list() # Only use this for adaptive weights, use this to store the initial coefficients

  if ( adaptive == TRUE ) {

    # Routine to find the zero-sum-constrained least squares solution
    df1 = data.frame(y, Xl[ , -1 ], Xf)
    for ( j in 1:pf ) {
      df1[[ j + pl ]] = as.factor(df1[[ j + pl ]])
      contrasts(df1[[ j + pl ]]) = contr.sum(length(levels(Xf[ , j ])), contrasts = TRUE)
    }
    if ( is.null(prev.coefficients) ) prev.coefficients = lm(y ~ ., data = df1)$coefficients
    init.linear = prev.coefficients[ 1:pl ]
    coeffcounter = pl + 1
    for ( j in 1:pf ) {
      adaptive.weights[[ j ]] = rep(0, length(levels(Xf[ , j ])))
      if ( is.null(was.prev.coefficients) ) {
        adaptive.weights[[ j ]][ 1:(length(levels(Xf[ , j ])) - 1) ] = prev.coefficients[ coeffcounter:(coeffcounter + length(levels(Xf[ , j ])) - 2) ]
        adaptive.weights[[ j ]][ length(levels(Xf[ , j ])) ] = -sum(head(adaptive.weights[[ j ]], -1))
        coeffcounter = coeffcounter + length(levels(Xf[ , j ])) - 1
      } else {
        adaptive.weights[[ j ]][ 1:(length(levels(Xf[ , j ]))) ] = prev.coefficients[ coeffcounter:(coeffcounter + length(levels(Xf[ , j ])) - 1) ]
        coeffcounter = coeffcounter + length(levels(Xf[ , j ]))
      }
      adaptive.weights[[ j ]] = abs(as.vector(dist(adaptive.weights[[ j ]], method = "manhattan")))


    }
    # for ( j in 1:pf ) total_adaptive = total_adaptive + sum(adaptive.weights[[ j ]])
    # for ( j in 1:pf ) adaptive.weights[[ j ]] = adaptive.weights[[ j ]] / total_adaptive

  }
  if ( is.null(observation.weights) == FALSE ) {
    weight.matrix = diag(sqrt(observation.weights))
    Xl = weight.matrix %*% Xl
    y = as.vector(weight.matrix %*% y)

  }

  # Idea: rescale the vector of overall; weights so that the sum of them is the same as before
  # this will have the effect of meaning that our existing lambda path (from forst attempt) will
  # have the correct scaling

  old_w = list()

  for ( j in 1:pf ) {

    p[ j ] = length(levels(Xf[ , j ]))
    d[ j ] = p[ j ] * ( p[ j ] - 1 ) / 2
    D[[ j ]] = matrix(0, d[ j ], p[ j ])
    rowcounter = 1
    for ( a in 1:(p[ j ] - 1) ) {
      for ( b in (a + 1):p[ j ] ) {
        D[[ j ]][ rowcounter, a ] = 1
        D[[ j ]][ rowcounter, b ] = -1
        rowcounter = rowcounter + 1
      }
    }
    w[[ j ]] = rep(0, d[ j ])
    # varstring = paste0("V", j) Given it names already
    # X[[ j ]] = model.matrix(~ 0 + eval(parse(text = varstring)), data = Xf)
    X[[ j ]] = model.matrix(~ 0 + get(names(Xf)[ j ]), data = Xf)
    if ( is.null(observation.weights) == FALSE ) {
      X[[ j ]] = weight.matrix %*% X[[ j ]]
    }

    for ( a in 1:d[ j ] ) {
      w[[ j ]][ a ] = sqrt(sum(X[[ j ]][ , as.logical(abs(D[[ j ]][ a, ]))])) / ( 1 + p[ j ] )
    }

    old_w[[ j ]] = w[[ j ]]

    if ( adaptive == TRUE ) {
      w[[ j ]] = w[[ j ]] / adaptive.weights[[ j ]]
    }



    w[[ j ]] = c(rep(0, p[ j ]), rep(w[[ j ]], 2))
    Xm[[ j ]] = cbind(X[[ j ]], matrix(0, n, 2 * d[ j ]))
    L1 = rbind(D[[ j ]], matrix(1, p[ j ], p[ j ]))
    L2 = rbind(diag(d[ j ]), matrix(0, p[ j ], d[ j ]))
    L3 = rbind(-diag(d[ j ]), matrix(0, p[ j ], d[ j ]))
    L[[ j ]] = cbind(L1, L2, L3)

    Amat[[ j ]] = rbind(L[[ j ]], -L[[ j ]], cbind(matrix(0, d[[ j ]], p[[ j ]]), diag(d[[ j ]]), matrix(0, d[[ j ]], d[[ j ]])),
                        cbind(matrix(0, d[[ j ]], p[[ j ]]), matrix(0, d[[ j ]], d[[ j ]]), diag(d[[ j ]])))
    bvec[[ j ]] = c(rep(0, 4 * d[ j ] + 2 * p[ j ]))
    # Dmat[[ j ]] = t(Xm[[ j ]]) %*% Xm[[ j ]] / n
    dvec[[ j ]] = as.vector(- t(y) %*% Xm[[ j ]] / n)
    lvec[[ j ]] = bvec[[ j ]]
    uvec[[ j ]] = rep(Inf, length(bvec[[ j ]]))
    # uvec[[ j ]] = rep(0, length(bvec[[ j ]]))
    Xm[[ j ]] = Matrix(Xm[[ j ]], sparse = T)
    # Dmat[[ j ]] = Matrix(Dmat[[ j ]], sparse = T)
  }

  # If weights are too large then small errors around >= 0 constraints get multiplied, giving 'credit' to varialbes with smaller weights
  # to take larger values than they would usually do and still satisfy constraint.


  for ( j in 1:pf ) w[[ j ]] = pmin(w[[ j ]], 1e2)






  # guess to fix?? 4june
  for ( j in 1:pf ) w[[ j ]] =  w[[ j ]] * 1e4

  for ( j in 1:pf ) w[[ j ]] = - w[[ j ]]

  dvec = c(as.vector(- t(y) %*% as.matrix(Xl)) / n, unlist(dvec))


  # dvec = c(-mean(y), unlist(dvec)) BEFORE LINEAR ADDITION

  lvec = c(unlist(lvec), -1)
  uvec = c(unlist(uvec), 0)
  # Amat = cbind(0, rbind(bdiag(Amat), unlist(w))) BEFORE LINEAR ADDITION
  Amat = cbind(0, rbind(bdiag(Amat), unlist(w)))
  if ( pl > 1 ) {
    for ( j in 1:(pl - 1) ) {
      Amat = cbind(0, Amat)
    }
  }
  # Amat = rbind(Amat, c(0, unlist(w)))
  Xm = do.call(cbind, Xm)
  # Xm = cbind(1, Xm) BEFORE LINEAR ADDITION

  # Xl = as.matrix(Xl)
  # Xl = data.frame(Xl)
  # print(typeof(Xm))
  Xm = cbind(Xl, Xm)
  Dmat = t(Xm) %*% Xm / n
  coeffsindex = rep(0, sum(p))
  indexcounter = 1
  varcounter = 1
  # coeffsindex[ 1 ] = 1 BEFORE LINEAR ADDITION
  coeffsindex[ 1:pl ] = 1:pl
  # indexcounter = indexcounter + 1 BEFORE LINEAR ADDITION
  # varcounter = varcounter + 1 BEFORE LINEAR ADDITION
  indexcounter = 1 + pl
  varcounter = 1 + pl
  for (j in 1:pf) {
    coeffsindex[ indexcounter:(indexcounter + p[ j ] - 1) ] = varcounter:(varcounter + p[ j ] - 1)
    indexcounter = indexcounter + p[ j ]
    varcounter = varcounter + p[ j ] + 2 * d[ j ]
  }

  L = list()
  L$Dmat = Dmat
  L$dvec = dvec
  L$Amat = Amat
  L$lvec = lvec
  L$uvec = uvec
  L$p = p
  L$pl = pl
  L$pf = pf
  L$n = n
  L$coeffsindex = coeffsindex
  L$varnames = varnames
  L$catnames = catnames
  return(L)
}


#Command to solve the entire program using quadratic programming
#solve_osqp(Dmat, dvec, Amat, 10*lvec, uvec, pars = osqpSettings(eps_abs = 1e-7))$x[coeffsindex]
ComputeCASANOVA = function(L, constraint = 1, full = F, casmodel = NULL) {
  Dmat = L$Dmat
  dvec = L$dvec
  Amat = L$Amat
  lvec = L$lvec
  uvec = L$uvec
  n = L$n
  p = L$p
  pl = L$pl
  pf = L$pf
  varnames = L$varnames
  catnames = L$catnames
  coeffsindex = L$coeffsindex
  if ( constraint < 1e-16 ) constraint = 1e-16

  # 4june
  constraint = 1e4 * constraint


  if ( full == F ) {

    if ( is.null(casmodel) == FALSE ) {
      casmodel$Update(u = constraint * L$lvec) # package ROSQP has this the wrong way round!!!
      fullsols = casmodel$Solve()
      print(fullsols$info$iter)
      fullsols = fullsols$x
    } else {
      fullsols = solve_osqp(Dmat, dvec, Amat, constraint * lvec, uvec, pars = osqpSettings(eps_abs = 1e-8, eps_rel = 1e-9, max_iter = 2e5, verbose = FALSE))$x
    }

    # fullsols = solve_osqp(Dmat, dvec, Amat, constraint * lvec, uvec, pars = osqpSettings(eps_abs = 1e-8, eps_rel = 1e-9, max_iter = 1e5, verbose = FALSE))$x
    # print("29maypm")
    # print(Amat[ dim(Amat)[ 1 ], ] )
    # print(solve_osqp(Dmat, dvec, Amat, constraint * lvec, uvec, pars = osqpSettings(eps_abs = 1e-8, eps_rel = 1e-9, verbose = FALSE))$x)

    # print(constraint)

    # Prints actual realisation of the sum constraint
    print(-sum(Amat[ dim(Amat)[ 1 ], ] * fullsols) * 1e-4)
    sols = fullsols[ coeffsindex ]
    solsList = list()
    solsList$Linear = sols[1:pl]
    varcounter = 1 + pl
    for (j in 1:length(p)) {
      solsList[[ j + 1 ]] = sols[ varcounter:(varcounter + p[ j ] - 1) ]
      names(solsList[[ j + 1 ]]) = catnames[[ j ]]
      varcounter = varcounter + p[ j ]
    }
    names(solsList) = c( "Linear", varnames)
    return(solsList)
  } else {
    return(solve_osqp(Dmat, dvec, Amat, constraint * lvec, uvec, pars = osqpSettings(eps_abs = 1e-8, eps_rel = 1e-9, max_iter = 1e5)))
  }
}



PredictCASANOVA = function( sol, Zl, Zf ) {

  Zl = as.matrix(Zl)
  Zf = as.matrix(Zf)
  pred_vec = as.vector(Zl %*% sol[[ 1 ]])
  for ( j in 1:dim(Zf)[ 2 ] ) {
    pred_vec = pred_vec + sol[[ j + 1 ]][ Zf[ , j ] ]
  }
  return(as.vector(pred_vec))
}



