# Routine to make training and test datasets



totallist = list()
load("full_adult_data.Rdata")
library(CatReg)
source("casanovafit.factor.crossval.logistic.R")
library(randomForest)
library(bazar)
#library(DMRnet)
library(effectFusion)
library(e1071)
library(rpart)
library(tictoc)
library(glmnet)

logit = function ( x ) return(exp(x) / ( 1 + exp(x) ))



for (i in 1:250)
{
    n = length(fully)
    subn = floor(0.01 * n)
    keepvec = sample(c(rep(TRUE, subn), rep(FALSE, n - subn)))
    removec = as.logical(1 - keepvec)
    y = fully[ keepvec ]
    cts = fullcts[ keepvec, ]
    dsc = fulldsc[ keepvec, ]
    traindsc = dsc
    catnumbers = rep(0, 8)
    for ( j in 1:8 ) {
        dsc[ , j ] = as.character(dsc[ , j ])
        dsc[ , j ] = as.factor(dsc[ , j ])
        catnumbers[ j ] = length(levels(dsc[ , j ]))
    }
    
    
    traindf = data.frame(y, cts, dsc)

    
    print("SCOPE")
    starttime = tic()
    # scope_model = mbfit.logistic(y, cts, dsc, gamma = 100)
    scope_model = scope.logistic(y, data.frame(cts, dsc), gamma = 100)
    stoptime = tic()
    scope_time = stoptime - starttime
    
    print("SCOPE large gamma")
    starttime = tic()
    # scopel_model = mbfit.logistic(y, cts, dsc, gamma = 250)
    scopel_model = scope.logistic(y, data.frame((cts, dsc), gamma = 250)
    stoptime = tic()
    scopel_time = stoptime - starttime
    
    print("glm")
    starttime = tic()
    glm_model = glm(y ~ ., data = traindf, family="binomial")
    stoptime = tic()
    glm_time = stoptime - starttime
    
    print("glmnet")
    starttime = tic()
    glmnet_model = cv.glmnet(cbind(cts, model.matrix(~ .-1, data=dsc, contrasts.arg = lapply(dsc, contrasts, contrasts=FALSE))), y, family = "binomial")
    stoptime = tic()
    glmnet_time = stoptime - starttime
    
    
    print("CASANOVA")
    starttime = tic()
    cas_model = casanovafit.logistic(y, cts, dsc)
    stoptime = tic()
    cas_time = stoptime - starttime
    
    #ac_model = casanovafit.logistic(y, cts, dsc, prev.coefficients = unlist(dummy.coef(glm_model)))

    print("Adaptive CASANOVA")
    starttime = tic()
    acc_model = casanovafit.logistic(y, cts, dsc, prev.coefficients = unlist(cas_model[[ 1 ]]))
    stoptime = tic()
    acc_time = stoptime - starttime
    
    print("Random Forests")
    starttime = tic()
    rf_model = randomForest(as.factor(y) ~ ., data = data.frame(y, cts, dsc))
    stoptime = tic()
    rf_time = stoptime - starttime

    print("CART")
    starttime = tic()
    cpsolution = rpart(as.factor(y) ~ ., data = data.frame(y, cts, dsc))
    cptable = printcp(cpsolution)
    minerror = which.min(cptable[ , 4 ])
    minthresh = cptable[ minerror, 4 ] + cptable[ minerror, 5 ] # This is using the 1-SE rule for pruning these trees
    bestcp = min(which(cptable[ , 4 ] < minthresh))
    if ( bestcp > 1 ) {
        cpthresh = 0.5 * (cptable[ bestcp, 1 ] + cptable[ bestcp - 1, 1 ])
    } else {
        cpthresh = 1
    }
    cart_model = prune(cpsolution, cp = cpthresh)
    stoptime = tic()
    cart_time = stoptime - starttime
    
    #print("DMR")
    #starttime = tic()
    #print("fitting cv solution")
    #cvsolution = cv.DMRnet(data.frame(cts, dsc), as.factor(y), nfolds = 5, family="binomial")
    #cvmaxp = cvsolution$df.min - 1
    #print(cvmaxp)
    #print("fitting full solution")
    #solution = DMRnet(data.frame(cts, dsc), as.factor(y), maxp = cvmaxp, family="binomial")
    #solution = solution$beta[ , 1 ]
    #dmr_model = list()
    #dmr_model[[ 1 ]] = solution[ 1:3 ]
    #startercounter = 4
    #for ( j in 1:8 ) {
    #    dmr_model[[ 1 + j ]] = c(0, solution[ startercounter:(startercounter + catnumbers[ j ] - 2) ])
    #    startercounter = startercounter + catnumbers[ j ] - 1
    #}
    #stoptime = tic()
    #dmr_time = stoptime - starttime

    print("effectFusion")
    starttime = tic()
    solution = effectFusion(y, data.frame(cts, dsc), types = c(rep("c", 2) ,rep("n", 8)), method = "FinMix", family = "binomial")
    solution = solution$refit$beta
    solution = colMeans(solution[ sample(1:3000, 1000, replace = F), ])
    bayes_model = list()
    bayes_model[[ 1 ]] = solution[ 1:3 ]
    startercounter = 4
    for ( j in 1:8 ) {
        bayes_model[[ 1 + j ]] = c(0, solution[ startercounter:(startercounter + catnumbers[ j ] - 2) ])
        startercounter = startercounter + catnumbers[ j ] - 1
    }
    stoptime = tic()
    bayes_time = stoptime - starttime
    
    print("SVM")
    starttime = tic()
    svm_model = svm(as.factor(y) ~ ., data = data.frame(y, cts, dsc))
    stoptime = tic()
    svm_time = stoptime - starttime

    save(keepvec, glm_model, scope_model, scopel_model, cas_model, acc_model, cart_model, rf_model, bayes_model, svm_model, glmnet_model, file = paste0("section_6_2_temp_models_.Rdata"))
    

    
    
    y = fully[ removec ]
    cts = fullcts[ removec, ]
    dsc = fulldsc[ removec, ]
    keepind = rep(TRUE, length(y))
    

    
    pshrink = dim(dsc)[ 2 ]
    for ( j in 1:pshrink ) {
        for ( i in 1:length(y) ) {
            if ( sum(traindsc[ , j ] == dsc[ i, j ]) == 0 ) {
                keepind[ i ] = FALSE
            }
        }
    }
    y = y[ keepind ]
    cts = cts[ keepind, ]
    dsc = dsc[ keepind, ]
    
    for ( j in 1:8 ) {
        dsc[ , j ] = as.character(dsc[ , j ])
        dsc[ , j ] = as.factor(dsc[ , j ])
    }
    
    testdf = data.frame(cts, dsc)
    

    
    
    scope_predict = predict(scope_model[[1]], data.frame(cts, dsc))
    
    scopel_predict = predict(scopel_model[[1]], data.frame(cts, dsc))

    
    cas_predict = as.vector(cts %*% cas_model[[ 1 ]][[ 1 ]])
    for ( j in 1:pshrink ) {
        cas_predict = cas_predict + cas_model[[ 1 ]][[ 2 ]][[ j ]][ dsc[ , j ] ]
    }
    

    acc_predict = as.vector(cts %*% acc_model[[ 1 ]][[ 1 ]])
    for ( j in 1:pshrink ) {
        acc_predict = acc_predict + acc_model[[ 1 ]][[ 2 ]][[ j ]][ dsc[ , j ] ]
    }
    
    rf_thresh = as.integer(predict(rf_model, newdata = testdf)) - 1
    
    cart_thresh = as.integer(predict(cart_model, newdata = testdf, type="class")) - 1
    
    svm_thresh = as.integer(predict(svm_model, newdata = testdf, type="class")) - 1
    

    
    bayes_predict = as.vector(cts %*% bayes_model[[ 1 ]])
    for ( j in 1:pshrink ) {
        bayes_predict = bayes_predict + bayes_model[[ 1 + j ]][ dsc[ , j ] ]
    }
    
    glm_predict = predict.glm(glm_model, newdata = testdf)
    
    glmnet_predict = as.numeric(predict(glmnet_model, cbind(cts, model.matrix(~ .-1, data=dsc, contrasts.arg = lapply(dsc, contrasts, contrasts=FALSE))), type = "class"))
    
    scope_logit = logit(scope_predict)
    scopel_logit = logit(scopel_predict)
    glm_logit = logit(glm_predict)
    cas_logit = logit(cas_predict)
    acc_logit = logit(acc_predict)
    #dmr_logit = logit(dmr_predict)
    bayes_logit = logit(bayes_predict)
    
    
    scope_thresh = as.integer(scope_logit >= 0.5)
    scopel_thresh = as.integer(scopel_logit >= 0.5)
    glm_thresh = as.integer(glm_logit >= 0.5)
    cas_thresh = as.integer(cas_logit >= 0.5)
    acc_thresh = as.integer(acc_logit >= 0.5)
    #dmr_thresh = as.integer(dmr_logit >= 0.5)
    bayes_thresh = as.integer(bayes_logit >= 0.5)
    
    glmnet_thresh = glmnet_predict
    
    
    scope_err = mean(abs(y - scope_thresh))
    scopel_err = mean(abs(y - scopel_thresh))
    glm_err = mean(abs(y - glm_thresh))
    cas_err = mean(abs(y - cas_thresh))
    acc_err = mean(abs(y - acc_thresh))
    rf_err = mean(abs(y - rf_thresh))
    cart_err = mean(abs(y - cart_thresh))
    #dmr_err = mean(abs(y - dmr_thresh))
    bayes_err = mean(abs(y - bayes_thresh))
    svm_err = mean(abs(y - svm_thresh))
    
    glmnet_err = mean(abs(y - glmnet_thresh))
    
    glm_dim = length(glm_model$coefficients)
    cas_dim = length(cas_model[[ 1 ]][[ 1 ]])
    for ( j in 1:pshrink ) cas_dim = cas_dim + length(almost.unique(cas_model[[ 1 ]][[ 2 ]][[ j ]], tolerance = 1e-3)) - 1

    acc_dim = length(acc_model[[ 1 ]][[ 1 ]])
    for ( j in 1:pshrink ) acc_dim = acc_dim + length(almost.unique(acc_model[[ 1 ]][[ 2 ]][[ j ]], tolerance = 1e-3)) - 1
    scope_dim = length(scope_model[[ 1 ]][[ 1 ]])
    for ( j in 1:pshrink ) scope_dim = scope_dim + length(almost.unique(scope_model[[ 1 ]][[ 2 ]][[ j ]], tolerance = 1e-3)) - 1
    
    scopel_dim = length(scopel_model[[ 1 ]][[ 1 ]])
    for ( j in 1:pshrink ) scopel_dim = scopel_dim + length(almost.unique(scopel_model[[ 1 ]][[ 2 ]][[ j ]], tolerance = 1e-3)) - 1
    
    #dmr_dim = length(dmr_model[[ 1 ]])
    #for ( j in 1:pshrink ) dmr_dim = dmr_dim + length(almost.unique(dmr_model[[ 1 + j ]], tolerance = 1e-3)) - 1
    bayes_dim = length(bayes_model[[ 1 ]])
    for ( j in 1:pshrink ) bayes_dim = bayes_dim + length(almost.unique(bayes_model[[ 1 + j ]], tolerance = 1e-3)) - 1
    
    glmnet_dim = 1 + sum(glmnet_model$glmnet.fit$beta[ , which(glmnet_model$lambda == glmnet_model$lambda.min) ] != 0)
    
    listblock = list(keepvec, removec, glm_err, cas_err, acc_err, rf_err, scope_err, scopel_err, bayes_err, rf_err, cart_err, svm_err, glmnet_err, glm_dim, cas_dim, acc_dim, scope_dim, scopel_dim, bayes_dim, glmnet_dim, glm_time, cas_time, acc_time, rf_time, scope_time, scopel_time, bayes_time, rf_time, cart_time, svm_time, glmnet_time)
    
    totallist = c(totallist, listblock)
    
    save(totallist, file="section_6_2_raw.Rdata")
}
