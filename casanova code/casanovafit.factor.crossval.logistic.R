# DRAFT OF REWRITTEN MBFIT TO ALLOW FOR FACTOR VARIABLES INSTEAD OF INTEGERS
# Why on earth I didn't do this from the start is beyond my comprehension

# Proper multivariate wrapper
source("core_casanovafit.logistic.R")
source("CASANOVA_quadratic_programming.R")

casanovafit.logistic = function ( y, xlinear, xshrink, AIC = TRUE, mBICconstant = 25, default.lambdaseq = TRUE, default.length = 200,
lambdaseq = NULL, interceptxlinear = FALSE, max.out.iter = 1000, BICearlystop = TRUE, BICterminate = 50,
K = 5, simplythebest = TRUE, simply.cross.validated = FALSE, remove.unnecessary.labels = TRUE, adaptive = FALSE, prev.coefficients = NULL ) {
    # y is response vector, xlinear is numeric matrix of covariates that will just be used for the unpenalized linear part of the regression
    # xshrink is an integer matrix where each column corresponds to a categorical variable, categories are numbered 1, 2, 3, ...
    # rest is self-explanatory
    
    # Important note is that we need to have no empty categories in 1, ..., c_j; if there are any empty ones in advance then ensure that
    # relabelling is performed before calling mbfit otherwise we'll get errors (I'll code this exception up myself in due course 7AUG NOW DONE)
    bugindicator = F
    logit = function( x ) {
        return(exp(x) / (1 + exp(x)))
    }
    n = length(y)
    null.deviance = - sum(y) * log(mean(y)) - sum(1 - y) * log( 1 - mean(y))
    deviance = null.deviance
    ObjectiveValue = null.deviance
    OldObjectiveValue = 0
    
    plinear = dim(xlinear)[ 2 ]
    pshrink = dim(xshrink)[ 2 ]
    
    for ( j in 1:pshrink ) {
        if ( remove.unnecessary.labels == TRUE ) {
            xshrink[ , j ] = as.character(xshrink[ , j ])
            xshrink[ , j ] = as.factor(xshrink[ , j ])
        }
    }
    if ( interceptxlinear == FALSE ) {
        xlinear = cbind(1, xlinear)
        plinear = plinear + 1
    }
    interceptxlinear = TRUE
    xlinear = as.matrix(xlinear)
    P = solve(t(xlinear) %*% xlinear) %*% t(xlinear)
    catnumbers = rep(0, pshrink)
    catnames = list()
    for ( j in 1:pshrink ) {
        catnames[[ j  ]] = levels(xshrink[ , j ])
        catnumbers[ j ] = length(catnames[[ j ]])
    }
    if ( default.lambdaseq == TRUE ) {
        pathlength = default.length
    }
    
    
    # Now treat minstdev, the minimal standard deviation, as a starting point for what we consider the variance to be.
    # Then we begin at a certain scaling of the tuning parameters, starting again from a rescaled version if this is too small
    
    # end of longer routine, though first we do need to compute the subaverages certainly
    
    # Generates the matrix of lambda values in the case that we use default arguments - initialise the lambda values here in a data-driven way
    # Start-point that guarantees null-consistency is 8 * sd in the first term but this is conservative.
    
    max.lambda =  0.5 * null.deviance * pshrink # This is a trial value at the start, don't read too much into this
    lambdaseq = c(0)
    
    if ( default.lambdaseq == TRUE ) {
        # lambdaseq = c(0, exp(seq(log(0.001), log(max.lambda), length = default.length - 1)))
        lambdaseq = seq(0, sqrt(max.lambda), length = pathlength)^2
    }
    BICincreasecounter = 0
    
    if ( K > 1 ) {
        FoldAssignment = as.integer(sample(ceiling((1:n)*K/n)))
        if ( default.lambdaseq == TRUE ){
            cverrors = matrix(0, n, default.length)
        } else {
            cverrors = matrix(0, n, length(lambdaseq))
        }
        removecounter = 0
        counter = 0
        
        for ( k in 1:K ) {
            print(paste0("Fold ", k))
            yfold = y[ (FoldAssignment != k), drop = FALSE ]
            xlinearfold = xlinear[ (FoldAssignment != k), , drop = FALSE ]
            xshrinkfold = xshrink[ (FoldAssignment != k), , drop = FALSE ]
            yremove = y[ (FoldAssignment == k), drop = FALSE ]
            xlinearremove = xlinear[ (FoldAssignment == k), , drop = FALSE ]
            xshrinkremove = xshrink[ (FoldAssignment == k), , drop = FALSE ]
            nremove = length(yremove)
            keepidentifier = rep(TRUE, nremove)
            removelastcategory = c()
            for ( i in 1:nremove ) {
                
                for ( j in 1:pshrink ) {
                    
                    if ( xshrinkremove[ i, j ] %in% xshrinkfold[ , j ] == FALSE ) {
                        keepidentifier[ i ] = FALSE
                        removecounter = removecounter + 1
                        
                    }
                }
            }
            
            yremove = yremove[ keepidentifier, drop = FALSE ]
            xlinearremove = xlinearremove[ keepidentifier, , drop = FALSE ]
            xshrinkremove = xshrinkremove[ keepidentifier, , drop = FALSE ]
            for ( j in 1:pshrink ) {
                xshrinkfold[ , j ] = as.character(xshrinkfold[ , j ])
                xshrinkfold[ , j ] = as.factor(xshrinkfold[ , j ])
                xshrinkremove[ , j ] = as.character(xshrinkremove[ , j ])
                xshrinkremove[ , j ] = as.factor(xshrinkremove[ , j ])
            }
            
            
            
            # For the gaussian regression version we don't require a core wrapper, since we have all the results already surely?
            cvsolution = core_casanovafit.logistic(yfold, xlinearfold, xshrinkfold, k, AIC, mBICconstant, lambdaseq, max.out.iter, BICearlystop, BICterminate, adaptive, prev.coefficients )
            if ( k == 1 ) {
                lambdaseq = cvsolution[[ 4 ]]
            }
            cvtemp = xlinearremove %*% cvsolution[[ 1 ]]
            for ( j in 1:pshrink ) {
                cvtemp = cvtemp + cvsolution[[ 2 ]][[ j ]][ xshrinkremove[ , j], ]
            }
            cverrorstemp = abs(yremove - logit(cvtemp))
            
            
            cverrors[ (counter + 1):(counter + length(yremove)), 1:dim(cverrorstemp)[ 2 ] ] = as.numeric(cverrorstemp)
            
            counter = counter + length(yremove)
        }
        
        cverrors = as.matrix(cverrors[ 1:(n - removecounter), 1:dim(cverrorstemp)[ 2 ] ])
        if ( removecounter > 0 ) {
            warning(paste0(removecounter, " observations removed from test sets; number of evaluated predictions is ", n - removecounter, "."))
        }
        cverrors = colMeans(cverrors > 0.5)
        cverrors[ is.na(cverrors) ] = Inf # Fixes consequences of fitting saturated model
        pathlengthfinal = which.min(cverrors)
        if ( simplythebest == FALSE ) pathlengthfinal = length(lambdaseq)
        lambdaseqused = lambdaseq
        
        lambdaseq = lambdaseq[ 1:pathlengthfinal ]
        print(paste0("Minimal cross-validation error = ", min(cverrors), " at pathpoint ", pathlengthfinal))
        if ( simply.cross.validated == TRUE ) {
            return(min(cverrors))
            break
        }
        finalsolution = core_casanovafit.logistic(y, xlinear, xshrink, 2, AIC, mBICconstant, lambdaseq, max.out.iter, BICearlystop, BICterminate, adaptive, prev.coefficients )
        if ( simplythebest == TRUE ) {
            finalsolution[[ 1 ]] = matrix(finalsolution[[ 1 ]], plinear, pathlengthfinal)[ , pathlengthfinal ]
            for ( j in 1:pshrink ) {
                if ( pathlengthfinal > 1 ) {
                    finalsolution[[ 2 ]][[ j ]] = finalsolution[[ 2 ]][[ j ]][ , pathlengthfinal ]
                }
                
            }
            finalsolution = list(finalsolution[[ 1 ]], finalsolution[[ 2 ]])
            
            return(list(finalsolution, cverrors, lambdaseqused))
        } else {
            return(list(finalsolution, cverrors, lambdaseqused))
        }
        
        
        
    } else {
        solution = core_casanovafit.logistic(y, xlinear, xshrink, 1, AIC, mBICconstant, lambdaseq, max.out.iter, BICearlystop, BICterminate, adaptive, prev.coefficients )
        
        return(list(solution, lambdaseq))
    }
    
}









