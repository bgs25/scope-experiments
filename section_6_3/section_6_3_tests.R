# Routine to make training and test datasets


setwd("~/private/workspace3")
totallist = list()
library(CatReg)
load("full_adult_data.Rdata")

library(parallel)

library(bazar)
no_cores = detectCores()


logit = function ( x ) return(exp(x) / ( 1 + exp(x) ))

category.noise = function( discrete_df, nmult ) {
    #takes a dataframe, returns a dataframe with the categories divided
    for ( j in 1:dim(discrete_df)[ 2 ] ) {
        print(paste0("Variable = ", j))
        nlevels = length(levels(discrete_df[ , j ]))
        for ( i in 1:floor((nmult-1) * nlevels) ) {
            which.level = sample(levels(discrete_df[ , j ]), prob= table(discrete_df[ , j ]), 1)
            discrete_df[ , j ] = divide.level(discrete_df[ , j ], which.level)
        }
        
    }
    return(discrete_df)
}


divide.level = function( data.vector, which.level ) {
    #lower-level function that ranodmly divides a specified variable in two
    data.vector = as.character(data.vector)
    for (i in 1:length(data.vector)) {
        if ( data.vector[i] == which.level ) {
            if ( rbinom(1,1,0.5) == 1 ) {
                data.vector[i] = paste0(which.level, "-0")
            } else {
                data.vector[i] = paste0(which.level, "-1")
            }
        }
    }
    data.vector = as.factor(data.vector)
    return(data.vector)
}

totallist = list()

for (i in 1:250)
{
    n = length(fully)
    subn = 0.1 * n
    keepvec = sample(c(rep(TRUE, subn), rep(FALSE, n - subn)))
    removec = as.logical(1 - keepvec)
    traindf = list()
    testdf = list()
    dscdf = list()
    dscdf[[ 1 ]] = fulldsc
    dscdf[[ 2 ]] = category.noise(dscdf[[ 1 ]], 5)
    dscdf[[ 3 ]] = category.noise(dscdf[[ 2 ]], 2)
    dscdf[[ 4 ]] = category.noise(dscdf[[ 3 ]], 2)
    y = fully[ keepvec ]
    cts = fullcts[ keepvec, ]
    for (j in 1:4) {
        dsc = dscdf[[ j ]][ keepvec, ]
        traindf[[ j ]] = data.frame(y, cts, dsc)
    }
    

    for ( j in 1:4 ) {
        y = fully[ removec ]
        cts = fullcts[ removec, ]
        dsc = dscdf[[ j ]][ removec, ]
        keepind = rep(TRUE, length(y))
        for ( k in 1:8 ) {
            for ( l in 1:length(y) ) {
                if ( sum(traindf[[ j ]][ , 3 + k ] == dsc[ l, k ]) == 0 ) {
                    keepind[ l ] = FALSE
                }
            }
        }
        y = y[ keepind ]
        cts = cts[ keepind, ]
        dsc = dsc[ keepind, ]
        testdf[[ j ]] = data.frame(y, cts, dsc)
    }

    
    cl = makeCluster(min(no_cores, 4), type = "FORK", outfile = paste0("ParallelLog_.txt"))

    scope_models = parLapply(cl, 1:4, function(x) scope.logistic(traindf[[ x ]][ , 1 ], traindf[[ x ]][ , 2:11 ], BICterminate = 15))
    stopCluster(cl)
    glm_models = list()
    #  rf_models = list()
    for ( j in 1:4 ) {
        glm_models[[ j ]] = glm(y ~ ., data = traindf[[ j ]], family = "binomial")
        #   rf_models[[ j ]] = randomForest(as.factor(y) ~ ., data = traindf[[ j ]])
    }
    glm_error = rep(0, 4)
    # rf_error = rep(0, 4)
    scope_error = rep(0, 4)
    glm_dim = rep(0, 4)
    scope_dim = rep(0, 4)
    
    for ( j in 1:4 ) {
        y = testdf[[ j ]][ , 1 ]
        cts = as.matrix(cbind(1, testdf[[ j ]][ , 2:3]))
        dsc = testdf[[ j ]][ , 4:11 ]
        
        scope_predict = predict(scope_models[[ j ]][[ 1 ]], testdf[[ j ]][ , 2:11 ])
        #scope_predict = as.vector(cts %*% scope_models[[ j ]][[ 1 ]][[ 1 ]])
        #for ( k in 1:8 ) {
        #    scope_predict = scope_predict + scope_models[[ j ]][[ 1 ]][[ 2 ]][[ k ]][ dsc[ , k ] ]
        #}
        scope_predict = as.integer(logit(scope_predict) >= 0.5)
        scope_error[ j ] = mean(abs(scope_predict - y))
        
        glm_predict = as.integer(predict.glm(glm_models[[ j ]], newdata = testdf[[ j ]]) >= 0.5)
        glm_error[ j ] = mean(abs(glm_predict - y))
        
        #  rf_error[ j ] = mean(predict(rf_models[[ j ]], newdata = testdf) != as.factor(y))
        
        glm_dim[ j ] = length(glm_models[[ j ]]$coefficients)
        scope_dim[ j ] = 3
        for ( k in 1:8 ) {
            scope_dim[ j ] = scope_dim[ j ] + length(almost.unique(scope_models[[ j ]][[ 1 ]][[ 2 ]][[ k ]][ scope_models[[ j ]][[ 1 ]][[ 2 ]][[ k ]] != 0 ], tolerance = 1e-4))
        }
    }
    
    
    #locallist = list(scope_models, scope_error, scope_dim, glm_models, glm_error, glm_dim, rf_models, rf_error, traindf, testdf)
    locallist = list(scope_models, scope_error, scope_dim, glm_models, glm_error, glm_dim)
    

    totallist = c(totallist, locallist)
    
    #save(totallist, file=paste0("19May_test_1_", comp_name, ".Rdata"))
    save(totallist, file="section_6_3_raw.Rdata")
    file.remove(paste0("ParallelLog_.txt"))
}
