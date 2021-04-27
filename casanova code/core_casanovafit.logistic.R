
core_casanovafit.logistic = function ( y, xlinear, xshrink, fold, AIC, mBICconstant,  lambdaseq, max.out.iter, BICearlystop, BICterminate, adaptive, prev.coefficients ) { # WHAT ADDITIONAL ARGUMENTS ARE NEEDED HERE!
  # y is response vector, xlinear is numeric matrix of covariates that will just be used for the unpenalized linear part of the regression

  # xshrink is an integer matrix where each column corresponds to a categorical variable, categories are numbered 1, 2, 3, ...
  # rest is self-explanatory

  # Important note is that we need to have no empty categories in 1, ..., c_j; if there are any empty ones in advance then ensure that
  # relabelling is performed before calling mbfit otherwise we'll get errors (I'll code this exception up myself in due course 7AUG NOW DONE)

  library(bazar)
  
  logit = function ( x ) {
    if ( abs(x) > 20 ) {
      return(0.5 * (sign(x) + 1))
    } else {
      return(exp(x) / ( 1 + exp(x) ))
    }
  }
  logit = Vectorize(logit)

  inv.logit = function( x ) return(log( x / ( 1 - x )))

  n = length(y)
  plinear = dim(xlinear)[ 2 ]
  pshrink = dim(xshrink)[ 2 ]
  # Next two variables are used for step-size halving
  alpha = 1
  step.halve = 25
  TerminateEpsilon = 1e-7
  n = length(y)
  null.deviance = - sum(y) * log(mean(y)) - sum(1 - y) * log( 1 - mean(y))
  deviance = null.deviance
  TerminateCriterion = null.deviance * TerminateEpsilon #rewrite this thing, this is not the correct null deviance!!!
  OuterQuadraticObjectiveValue = null.deviance
  OldOuterQuadraticObjectiveValue = 0
  pathlength = length(lambdaseq)
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
  coefficientshrink = list()
  oldcoefficientshrink = list()

  for ( j in 1:pshrink ) {
    coefficientshrink[[ j ]] = matrix(0, catnumbers[ j ], pathlength)
    oldcoefficientshrink[[ j ]] = rep(0, catnumbers[ j ])
    rownames(coefficientshrink[[ j ]]) = catnames[[ j ]]

  }
  coefficientlinear = matrix(0, plinear, pathlength)
  beta = rep(0, plinear)
  oldbeta = rep(0, plinear)

  partialresiduals = y
  BIC = rep(0, pathlength)
  fittedvalues = rep(0, n)
  adaptivefittedvalues = fittedvalues
  fittedprobs = rep(0.5, n)
  partialresiduals = ( y - fittedprobs ) / ( fittedprobs * ( 1 - fittedprobs ) )
  oldpartialresiduals = rep(0, n)
  beta = rep(0, plinear)
  OldOuterQuadraticObjectiveValue = 0
  OuterQuadraticObjectiveValue = 0
  BICincreasecounter = 0
  effd = 1
  model.saturated = FALSE
  for ( l in 1:pathlength ) {

    if ( BICincreasecounter > BICterminate ) {
      l = l - 1
      break
    } else if ( ( fold > 1 ) && ( model.saturated ) ) {
      if ( l < pathlength ) {
        coefficientlinear[ , l + 1 ] = coefficientlinear[ , l ]
        for ( j in 1:pshrink ) {
          coefficientshrink[[ j ]][ , l + 1 ] = coefficientshrink[[ j ]][ , l ]
        }
        out.iter = max.out.iter
      }

    } else {

      out.iter = 0

      # MIDDLE LOOP
      while ((( out.iter < max.out.iter ) && ( abs(OldOuterQuadraticObjectiveValue - OuterQuadraticObjectiveValue) > TerminateCriterion )) || ( out.iter == 0 )) {


        observationweights = as.vector(fittedprobs * ( 1 - fittedprobs )) / n # This was previously p/(1-p) instead of p*(1-p) but somehow that didn't matter?

        repvec = (pmin(fittedprobs, 1 - fittedprobs) < 1e-5)
        # if (sum(is.na(repvec)) > 0 ) {
        #   print(repvec)
        #   print(adaptivefittedvalues)
        #   print(fittedprobs)
        # }

        fittedprobs[ repvec ] = round(fittedprobs[ repvec ])
        observationweights[ repvec ] = 1e-5

        partialresiduals = as.vector(( y - fittedprobs ) / ( fittedprobs * ( 1 - fittedprobs ) ))
        partialresiduals[ repvec ] = oldpartialresiduals[ repvec ] # bit of a fudge, but maybe will work?
        middleresponse = partialresiduals + adaptivefittedvalues

        OldOuterQuadraticObjectiveValue = OuterQuadraticObjectiveValue

        oldbeta = beta

        for ( j in 1:pshrink ) {
          oldcoefficientshrink[[ j ]] = coefficientshrink[[ j ]][ , l ]
        }



        L = CASANOVA_objectmaker(middleresponse, xlinear, xshrink, adaptive, prev.coefficients, observationweights)


        sol = ComputeCASANOVA(L, constraint = lambdaseq[ l ])

        coefficientlinear[ , l ] = sol[[ 1 ]]
        for ( j in 1:pshrink ) {
          coefficientshrink[[ j ]][ , l ] = sol[[ j + 1 ]]
        }

        beta = coefficientlinear[ , l ]




        # END OF INNER LOOP
        beta = alpha * beta + ( 1 - alpha ) * oldbeta
        for ( j in 1:pshrink ) {
          coefficientshrink[[ j ]][ , l ] = alpha * coefficientshrink[[ j ]][ , l ] + ( 1 - alpha ) * oldcoefficientshrink[[ j ]]
        }
        adaptivefittedvalues = rep(0, n)
        adaptivefittedvalues = adaptivefittedvalues + as.vector(xlinear %*% beta)
        for ( j in 1:pshrink ) {
          adaptivefittedvalues = adaptivefittedvalues + as.vector(coefficientshrink[[ j ]][ xshrink[ , j ], l ])

        }

        fittedprobs = as.vector(logit(adaptivefittedvalues))


        out.iter = out.iter + 1
        if ( out.iter %% 100 == 0 ) {
          print(paste0("Outer iteration ", out.iter, "."))
        }

        if ( out.iter == step.halve ) {
          step.halve = step.halve + min(round(12.5 / alpha), 50)
          alpha = 0.5 * alpha
        }

        ## REWRITE THIS NEXT BEN

        OuterQuadraticObjectiveValue = ( sum(log(1 + exp(- ( 2 * y - 1 ) * adaptivefittedvalues))) / n )

        deviance = - sum(log(1 - y + fittedprobs * (2 * y - 1)))

        # Next forces program to terminate early in case of saturation. Criteria are: more than half of fitted probabilities being zero or one, or
        # deviance being less than 1% of null deviance
        if ( ( sum(pmin(fittedprobs, 1 - fittedprobs) < 1e-5) >= 0.5 * n ) || ( deviance / null.deviance - 0.01 < 0 ) ) {
          print("Model saturated")
          if ( fold == 1 ) {
            BICincreasecounter = BICterminate + 1
            out.iter = max.out.iter
          } else {
            model.saturated = TRUE
            out.iter = max.out.iter
          }
        }

        # END OF MIDDLE LOOP
      }

      coefficientlinear[ , l ] = beta
      alpha = 1
      step.halve = 25

      if ( AIC == TRUE ) {
        BIC[ l ] =  2 * ( plinear )
      } else {
        BIC[ l ] =  mBICconstant * log(n + plinear + pshrink) * ( plinear )
      }

      fitvals = xlinear %*% coefficientlinear[ , l ]

      for ( j in 1:pshrink ) {
        if ( AIC == TRUE ) {
          BIC[ l ] = BIC[ l ] +  2 * ( length(almost.unique(coefficientshrink[[ j ]][, l ], tolerance = 0.01)) - 1 )
        } else {
          BIC[ l ] = BIC[ l ] +  mBICconstant * log(n + plinear + pshrink) * ( length(almost.unique(coefficientshrink[[ j ]][ , l ], tolerance = 0.01)) - 1 )
        }

        fitvals = fitvals + coefficientshrink[[ j ]][ xshrink[ , j ], l ]
      }
      if ( AIC == TRUE ) {
        # BIC[ l ] = BIC[ l ] + 2 * (1 / n) * ( t(y - fitvals) %*% (y - fitvals) )
        BIC[ l ] = BIC[ l ] + 2 * ( sum(log(1 + exp(- ( 2 * y - 1 ) * fitvals))) )
      } else {
        BIC[ l ] = BIC[ l ] + (1 / n) * ( t(y - fitvals) %*% (y - fitvals) )
        BIC[ l ] = BIC[ l ] + ( sum(log(1 + exp(- ( 2 * y - 1 ) * fitvals))) )
      }

      if ( ( fold == 1 ) && ( BICincreasecounter <= BICterminate ) ) {
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
      print(paste0("Pathpoint ", l, " done; outer iterations = ", out.iter, ", AIC = ", BIC[ l ], ", stopping counter = ", BICincreasecounter, "."))
    } else {
      print(paste0("Pathpoint ", l, " done; outer iterations = ", out.iter, ", AIC = ", BIC[ l ], "."))
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
