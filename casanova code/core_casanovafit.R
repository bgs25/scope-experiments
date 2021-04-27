core_casanovafit = function ( casmodel, y, xlinear, xshrink, L, fold, AIC, mBICconstant, lambdaseq, BICearlystop, BICterminate ) {
  # y is response vector, xlinear is numeric matrix of covariates that will just be used for the unpenalized linear part of the regression
  # xshrink is an integer matrix where each column corresponds to a categorical variable, categories are numbered 1, 2, 3, ...
  # rest is self-explanatory

  # Important note is that we need to have no empty categories in 1, ..., c_j; if there are any empty ones in advance then ensure that
  # relabelling is performed before calling mbfit otherwise we'll get errors (I'll code this exception up myself in due course 7AUG NOW DONE)
  n = L$n
  plinear = L$pl
  pshrink = L$pf
  pathlength = length(lambdaseq)
  OldObjectiveValue = 0
  lambdaseq.augment = FALSE
  if ( AIC == TRUE ) {
    BICincrement = 2
  } else {
    BICincrement = 2 * mBICconstant
  }

  beginagain = FALSE
  # Ensure intercept term is taken care of in the unpenalized part
  P = solve(t(xlinear) %*% xlinear) %*% t(xlinear)
  catnumbers = rep(0, pshrink)
  catnames = list()
  for ( j in 1:pshrink ) {
    catnames[[ j  ]] = levels(xshrink[ , j ])
    catnumbers[ j ] = length(catnames[[ j ]])
  }


  if ( BICearlystop == FALSE ) {
    BICterminate = pathlength
  }
  coefficientshrink = list()
  for ( j in 1:pshrink ) {
    coefficientshrink[[ j ]] = matrix(0, catnumbers[ j ], pathlength)
    rownames(coefficientshrink[[ j ]]) = catnames[[ j ]]
  }
  coefficientlinear = matrix(0, plinear, pathlength)

  BIC = rep(0, pathlength)


  BICincreasecounter = 0
  for ( l in 1:pathlength ) {
    if ( BICincreasecounter > BICterminate ) {
      l = l - 1
      break
    } else {
      print(lambdaseq[ l ])

      sol = ComputeCASANOVA(casmodel, L, constraint = lambdaseq[ l ])
      coefficientlinear[ , l ] = sol[[ 1 ]]
      for ( j in 1:pshrink ) {
        coefficientshrink[[ j ]][ , l ] = sol[[ j + 1 ]]
        if ( mean(abs(coefficientshrink[[ j ]][ , l ])) < 1e-6 ) coefficientshrink[[ j ]][ , l ] = rep(0, catnumbers[ j ])
      }

      if ( AIC == TRUE ) {
        BIC[ l ] =  2 * ( plinear )
      } else {
        BIC[ l ] =  mBICconstant * log(n + plinear + pshrink) * ( plinear )
      }

      fitvals = xlinear %*% coefficientlinear[ , l ]

      for ( j in 1:pshrink ) {
        if ( AIC == TRUE ) {
          BIC[ l ] = BIC[ l ] +  2 * ( length(almost.unique(coefficientshrink[[ j ]][ , l ], tolerance = 0.001)) - 1 )
        } else {
          BIC[ l ] = BIC[ l ] +  mBICconstant * log(n + plinear + pshrink) * ( length(almost.unique(coefficientshrink[[ j ]][, l ], tolerance = 0.001)) - 1 )
        }

        fitvals = fitvals + coefficientshrink[[ j ]][ xshrink[ , j ], l ]
      }
      if ( AIC == TRUE ) {
        BIC[ l ] = BIC[ l ] + 2 * (1 / n) * ( t(y - fitvals) %*% (y - fitvals) )
      } else {
        BIC[ l ] = BIC[ l ] + (1 / n) * ( t(y - fitvals) %*% (y - fitvals) )
      }

      if ( fold == 1 ) {
        if ( l >= 2 ) {
          if ( BICearlystop == TRUE ) {
            BICincreasecounter = l - which.min(BIC[ 1:l ])
          }

        }
      }


    }

    if ( l < pathlength ) {
      for ( j in 1:pshrink ) {
        coefficientshrink[[ j ]][ , l + 1 ] = coefficientshrink[[ j ]][ , l ]
      }
    }
    if ( fold == 1 ) {
      print(paste0("Pathpoint ", l, " done; AIC = ", BIC[ l ], ", stopping counter = ", BICincreasecounter, "."))
    } else {
      print(paste0("Pathpoint ", l, " done; AIC = ", BIC[ l ], "."))
    }

    pathpoint.finished = l

  }

  pathlength = pathpoint.finished

  coefficientlinear = coefficientlinear[ , 1:pathlength ]
  BIC = BIC[ 1:pathlength ]
  for ( j in 1:pshrink ) {
    coefficientshrink[[ j ]] = coefficientshrink[[ j ]][ , 1:pathlength ]
  }
  lambdaseq = lambdaseq[ 1:pathlength ]


  if ( AIC == TRUE ) {
    print(paste0("Smallest AIC model is pathpoint = ", which.min(BIC), ", AIC = ", min(BIC), ", terminated at pathpoint ", pathlength, "."))
  } else {
    print(paste0("Smallest BIC model is pathpoint = ", which.min(BIC), ", BIC = ", min(BIC), ", terminated at pathpoint ", pathlength, "."))
  }
  return(list(coefficientlinear, coefficientshrink, BIC, lambdaseq))
}
