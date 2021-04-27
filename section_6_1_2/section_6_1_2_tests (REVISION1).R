# SCRIPT to perform set of high-dimensional tests

# Want to report prediction error, time to compute, dimension of fitted model




comp_name = "modified_code_to_run_on_single_machine"

library(bazar)
library(DMRnet)
library(randomForest)
library(rpart)
library(glmnet)
library(tictoc)
library(CatReg)

gamsol = list()
n_jobs = 500

n_settings = 8

gammaseq = c(4, 8, 16, 32, 64)

scopes_prederr = matrix(0, n_jobs, n_settings)
scopel_prederr = matrix(0, n_jobs, n_settings)
scopecv_prederr = matrix(0, n_jobs, n_settings)
dmr_prederr = matrix(0, n_jobs, n_settings)
cart_prederr = matrix(0, n_jobs, n_settings)
rf_prederr = matrix(0, n_jobs, n_settings)
glmnet_prederr = matrix(0, n_jobs, n_settings)

ols_prederr = matrix(0, n_jobs, n_settings)


scopes_time = matrix(0, n_jobs, n_settings)
scopel_time = matrix(0, n_jobs, n_settings)
scopecv_time = matrix(0, n_jobs, n_settings)
dmr_time = matrix(0, n_jobs, n_settings)
cart_time = matrix(0, n_jobs, n_settings)
rf_time = matrix(0, n_jobs, n_settings)
glmnet_time = matrix(0, n_jobs, n_settings)

ols_time = matrix(0, n_jobs, n_settings)

scopes_dim = matrix(0, n_jobs, n_settings)
scopel_dim = matrix(0, n_jobs, n_settings)
scopecv_dim = matrix(0, n_jobs, n_settings)
dmr_dim = matrix(0, n_jobs, n_settings)
glmnet_dim = matrix(0, n_jobs, n_settings)

scopecv_gam = matrix(0, n_jobs, n_settings)

# now save the models as well
scopes_models = list()
scopel_models = list()
scopecv_models = list()
dmr_models = list()
glmnet_models = list()
for (setting in 1:n_settings) {
    scopes_models[[ setting ]] = list()
    scopel_models[[ setting ]] = list()
    scopecv_models[[ setting ]] = list()
    dmr_models[[ setting ]] = list()
    glmnet_models[[ setting ]] = list()
}

# setting up example
signal = list()

# set parameters for the 8 examples
n_train = c(500, 500, 500, 500, 500, 500, 500, 500)
n_test = 10000

p_vars = c(100, 100, 100, 100, 100, 100, 100, 100)

k_cats = c(24, 24, 24, 24, 24, 24, 24, 24)
corvec = c(0, 0.5, 0.5, 0, 0, 0.5, 0, 0)
noise_level = c(sqrt(50), sqrt(50), 10, 5, 1, 1, 5, 5)


do_glmnet = rep(TRUE, 8)
cv_dmrnet = rep(TRUE, 8)

#make matrices whose columns are signal coefficient vectors


signal[[ 1 ]] = matrix(0, 24, 25)
for ( j in 1:3 ) signal[[ 1 ]][ , j ] = c(rep(-2, 10), rep(0, 4), rep(2, 10))
for ( j in 4:6 ) signal[[ 1 ]][ , j ] = c(rep(-2, 8), rep(0, 8), rep(2, 8))

signal[[ 2 ]] = signal[[ 1 ]]

signal[[ 3 ]] = matrix(0, 24, 25)
for ( j in c(1, 3, 5) ) signal[[ 3 ]][ , j ] = c(rep(-2, 8), rep(0, 8), rep(4, 8))
for ( j in c(2, 4, 6) ) signal[[ 3 ]][ , j ] = c(rep(-3, 16), rep(2, 8))

signal[[ 4 ]] = matrix(0, 24, 25)
for ( j in 1:5 ) signal[[ 4 ]][ , j ] = c(rep(-2, 5), rep(-1, 5), rep(0, 4), rep(1, 5), rep(2, 5))


signal[[ 5 ]] = matrix(0, 24, 25)
for ( j in 1:25 ) signal[[ 5 ]][ , j ] = c(rep(-2, 16), rep(3, 8))


signal[[ 6 ]] = matrix(0, 24, 25)
for ( j in 1:25 ) signal[[ 6 ]][ , j ] = c(rep(-2, 16), rep(3, 8))


signal[[ 7 ]] = matrix(0, 24, 25)
for ( j in 1:10 ) signal[[ 7 ]][ , j ] = c(rep(-2, 4), rep(0, 12), rep(2, 8))

signal[[ 8 ]] = matrix(0, 24, 25)
for ( j in 1:5 ) signal[[ 8 ]][ , j ] = c(rep(-3, 6), rep(-1, 6), rep(1, 6), rep(3, 6))





number_finished = 1
for (job_id in 1:n_jobs) {
    print(paste0("Job number: ", job_id))
    
    for ( setting in 1:8 ) {
        # make the design matrices
        if ( corvec[ setting ] == 0 ) {
            train.design = UniformDesignMatrix(n_train[ setting ], p_vars[ setting ], k_cats[ setting ])#, alphabetical = TRUE)
            test.design = UniformDesignMatrix(n_test, p_vars[ setting ], k_cats[ setting ])#, alphabetical = TRUE)
        } else {
            cormat = matrix(corvec[ setting ], p_vars[ setting ], p_vars[ setting ]) + (1 - corvec[ setting ]) * diag(p_vars[ setting ])
            train.design = CorrelatedDesignMatrix(n_train[ setting ], cormat, k_cats[ setting ])#, alphabetical = TRUE)
            test.design = CorrelatedDesignMatrix(n_test, cormat, k_cats[ setting ])# , alphabetical = TRUE)
        }
        
        # this is to remove observations from the test set if a level is present
        # in the test data that is not in the training data. SCOPE can cope with this but it is essentially a missing data problem and not relevant here.
        keepind = rep(TRUE, n_test)
        for ( j in 1:p_vars[ setting ] ) {
            for ( i in 1:length(levels(test.design[ , j ])) ) {
                if ( sum(train.design[ , j ] == levels(test.design[ , j ])[ i ]) == 0 ) {
                    keepind[ test.design[ , j ] == levels(test.design[ , j ])[ i ] ] = FALSE
                }
            }
        }
        
        for ( j in 1:p_vars[ setting ] ) {
            train.design[ , j ] = as.character(train.design[ , j ])
            train.design[ , j ] = as.factor(train.design[ , j ])
        }
        
        test.design = test.design[ keepind, ]
        for ( j in 1:p_vars[ setting ] ) {
            test.design[ , j ] = as.character(test.design[ , j ])
            test.design[ , j ] = as.factor(test.design[ , j ])
        }
        
        print(paste0("Removing ", n_test - sum(keepind), " observations from test set due to untrained categories."))
        
        # make the signal variables
        train.signal = 0
        test.signal = 0
        for ( j in 1:25 ) train.signal = train.signal + signal[[ setting ]][ train.design[ , j ], j ]
        for ( j in 1:25 ) test.signal = test.signal + signal[[ setting ]][ test.design[ , j ], j ]
        
        y = train.signal + noise_level[ setting ] * rnorm(n_train[ setting ])
        
        # SCOPE gamma = 8
        print("SCOPE gamma = 8")
        starttime = tic()
        scopes_mod = scope(data.frame(matrix(1, n_train[ setting ], 1), train.design), y, include_intercept = FALSE, gamma = 8, early_stopping_rounds = 35)
        stoptime = tic()
        duration = stoptime - starttime
        scopes_time[ job_id, setting ] = duration
        scopes_models[[ setting ]][[ job_id ]] = scopes_mod
        
        # SCOPE gamma = 32
        print("SCOPE gamma = 32")
        starttime = tic()
        scopel_mod = scope(data.frame(matrix(1, n_train[ setting ], 1), train.design), y, include_intercept = FALSE, gamma = 32, early_stopping_rounds = 35)
        stoptime = tic()
        duration = stoptime - starttime
        scopel_time[ job_id, setting ] = duration
        scopel_models[[ setting ]][[ job_id ]] = scopel_mod
        
        # SCOPE cross-validated
        print("scope CV")
        cycleorder = sample(1:p_vars[ setting ])
        cvfold = as.integer(sample(ceiling((1:n_train[ setting ])*5/n_train[ setting ])))
        starttime = tic()
        gamsol = list()
        bestcv = rep(0, 5)
        for ( gam in 1:length(gammaseq) ) {
            print(paste0("gamma = ", gammaseq[gam]))
            gamsol[[ gam ]] = scope(data.frame(matrix(1, n_train[ setting ], 1), train.design), y, include_intercept = FALSE,  gamma = gammaseq[ gam ], early_stopping_rounds = 35, only_cross_validate = TRUE, silent = TRUE, block_order = cycleorder, fold_assignment = cvfold)
            #bestcv[ gam ] = min(gamsol[[ gam ]][ , 1 ])
            bestcv[ gam ] = min(gamsol[[ gam ]]$cverrors)
        }
        print(paste0("Cross-validation error vector: ", bestcv))
        bestgamind = which.min(bestcv)
        bestgam = gammaseq[ bestgamind ]
        print("Cross-validated gamma and lambda, now fitting final version")
        scopecv_mod = scope(data.frame(matrix(1, n_train[ setting ], 1), train.design), y, include_intercept = FALSE,  gamma = bestgam, block_order = cycleorder, fold_assignment = cvfold, early_stopping_rounds = 30)
        stoptime = tic()
        duration = stoptime - starttime
        scopecv_time[ job_id, setting ] = duration
        scopecv_gam[ job_id, setting ] = bestgam
        scopecv_models[[ setting ]][[ job_id ]] = scopecv_mod
        
        
        # DMRnet
        print("DMRnet")
        starttime = tic()
        if ( cv_dmrnet[ setting ] ) {
            cvdmrsol = cv.DMRnet(train.design, y, nfold = 5)
            cvmaxp = length(cvdmrsol$cvm) - which.min(cvdmrsol$cvm)
            dmr_mod = DMRnet(train.design, y, maxp = cvmaxp)
        } else {
            dmr_mod = DMRnet(train.design, y)
            gicdf = length(gic.DMR(dmr_mod)$gic) - which.min(gic.DMR(dmr_mod)$gic) + 1
        }
        
        stoptime = tic()
        duration = stoptime - starttime
        dmr_time[ job_id, setting ] = duration
        dmr_models[[ setting ]][[ job_id ]] = dmr_mod
        
        # CART
        print("CART")
        starttime = tic()
        cpsolution = rpart(y ~ ., data = data.frame(y, train.design))
        cptable = printcp(cpsolution)
        minerror = which.min(cptable[ , 4 ])
        minthresh = cptable[ minerror, 4 ] + cptable[ minerror, 5 ] # This is using the 1-SE rule for pruning these trees
        bestcp = min(which(cptable[ , 4 ] < minthresh))
        if ( bestcp > 1 ) {
            cpthresh = 0.5*(cptable[ bestcp, 1 ] + cptable[ bestcp - 1, 1 ])
        } else {
            cpthresh = 1
        }
        cart_mod = prune(cpsolution, cp = cpthresh)
        stoptime = tic()
        duration = starttime - stoptime
        cart_time[ job_id, setting ] = duration
        
        # random forests
        print("Random Forests")
        starttime = tic()
        rf_mod = randomForest(y ~ ., data = data.frame(y, train.design))
        stoptime = tic()
        duration = stoptime - starttime
        rf_time[ job_id, setting ] = duration
        
        
        # glmnet
        if ( do_glmnet[ setting ] ) {
            print("glmnet")
            
            train.design.dummy = model.matrix(~ .-1, data=train.design, contrasts.arg = lapply(train.design, contrasts, contrasts=FALSE))
            test.design.dummy = model.matrix(~ .-1, data=test.design, contrasts.arg = lapply(test.design, contrasts, contrasts=FALSE))
            starttime = tic()
            cv.glmnet_mod = cv.glmnet(train.design.dummy, y, nfolds = 5)
            bestlam = cv.glmnet_mod$lambda.min
            bestlam_ind = which.min(cv.glmnet_mod$cvm)
            glmnet_mod = glmnet(train.design.dummy, y)
            # NEW LINE
            glmnet_mod$beta.best = glmnet_mod$beta[ , bestlam_ind ]
            glmnet_mod$a0.best = glmnet_mod$a0[ bestlam_ind ]
            stoptime = tic()
            duration = stoptime - starttime
            glmnet_time[ job_id, setting ] = duration
            glmnet_models[[ setting ]][[ job_id ]] = glmnet_mod
        }
        
        
        
        
        
        print("Computing predictions")
        # NOW do the predictions and save the results
        scopes_pred = predict(scopes_mod, data.frame(matrix(1, n_test, 1), test.design), include_intercept = FALSE)
        scopel_pred = predict(scopel_mod, data.frame(matrix(1, n_test, 1), test.design), include_intercept = FALSE)
        scopecv_pred = predict(scopecv_mod, data.frame(matrix(1, n_test, 1), test.design), include_intercept = FALSE)
        if ( cv_dmrnet[ setting ] ) {
            dmr_pred = predict(dmr_mod, newx = test.design)[ , 1 ]
        } else {
            dmr_pred = predict(dmr_mod, newx = test.design, df = gicdf)
        }
        
        cart_pred = predict(cart_mod, newdata = test.design)
        rf_pred = predict(rf_mod, newdata = test.design)
        if ( do_glmnet[ setting ] )  glmnet_pred = predict(glmnet_mod, newx = test.design.dummy, s = bestlam)
        
        scopes_prederr[ job_id, setting ] = mean((test.signal - scopes_pred)^2)
        scopel_prederr[ job_id, setting ] = mean((test.signal - scopel_pred)^2)
        scopecv_prederr[ job_id, setting ] = mean((test.signal - scopecv_pred)^2)
        dmr_prederr[ job_id, setting ] = mean((test.signal - dmr_pred)^2)
        cart_prederr[ job_id, setting ] = mean((test.signal - cart_pred)^2)
        rf_prederr[ job_id, setting ] = mean((test.signal - rf_pred)^2)
        if ( do_glmnet[ setting ] ) glmnet_prederr[ job_id, setting ] = mean((test.signal - glmnet_pred)^2)
        
        print("Computing dimension")
        # Now to handle the dimension of models
        # scopes
        scopes_dim[ job_id, setting ] = 1
        for ( j in 1:p_vars[ setting ] ) scopes_dim[ job_id, setting ] = scopes_dim[ job_id, setting ] + length(almost.unique(scopes_mod$beta.best[[ 2 ]][[ j ]], tolerance = 1e-4)) - 1
        
        scopel_dim[ job_id, setting ] = 1
        for ( j in 1:p_vars[ setting ] ) scopel_dim[ job_id, setting ] = scopel_dim[ job_id, setting ] + length(almost.unique(scopel_mod$beta.best[[ 2 ]][[ j ]], tolerance = 1e-4)) - 1
        
        scopecv_dim[ job_id, setting ] = 1
        for ( j in 1:p_vars[ setting ] ) scopecv_dim[ job_id, setting ] = scopecv_dim[ job_id, setting ] + length(almost.unique(scopecv_mod$beta.best[[ 2 ]][[ j ]], tolerance = 1e-4)) - 1
        
        if ( cv_dmrnet[ setting ] ) {
            dmr_dim[ job_id, setting ] = cvmaxp + 1
        } else {
            dmr_dim[ job_id, setting ] = gicdf
        }
        
        if ( do_glmnet[ setting ] ) glmnet_dim[ job_id, setting ] = glmnet_mod$df[ which.min(abs(glmnet_mod$lambda - cv.glmnet_mod$lambda.min)) ] + 1
        
        
        # NOW FOR THE ORACLE RESULTS
        # START
        print('Computing oracle design matrices')
        # First change train.design and test.design
        if (setting == 1) {
          train.design = train.design[ , 1:6 ]
          test.design = test.design[ , 1:6 ]
          for (j in 1:3) {
            levels(train.design[ , j ]) = c(rep(levels(train.design[,j])[1], 8), rep(levels(train.design[,j])[2], 8), rep(levels(train.design[,j])[3], 8))
            levels(test.design[ , j ]) = c(rep(levels(test.design[,j])[1], 8), rep(levels(test.design[,j])[2], 8), rep(levels(test.design[,j])[3], 8))
          }
          for (j in 4:6) {
            levels(train.design[ , j ]) = c(rep(levels(train.design[,j])[1], 10), rep(levels(train.design[,j])[2], 4), rep(levels(train.design[,j])[3], 10))
            levels(test.design[ , j ]) = c(rep(levels(test.design[,j])[1], 10), rep(levels(test.design[,j])[2], 4), rep(levels(test.design[,j])[3], 10))
          }
        } else if (setting == 2) {
          train.design = train.design[ , 1:6 ]
          test.design = test.design[ , 1:6 ]
          for (j in 1:3) {
            levels(train.design[ , j ]) = c(rep(levels(train.design[,j])[1], 8), rep(levels(train.design[,j])[2], 8), rep(levels(train.design[,j])[3], 8))
            levels(test.design[ , j ]) = c(rep(levels(test.design[,j])[1], 8), rep(levels(test.design[,j])[2], 8), rep(levels(test.design[,j])[3], 8))
          }
          for (j in 4:6) {
            levels(train.design[ , j ]) = c(rep(levels(train.design[,j])[1], 10), rep(levels(train.design[,j])[2], 4), rep(levels(train.design[,j])[3], 10))
            levels(test.design[ , j ]) = c(rep(levels(test.design[,j])[1], 10), rep(levels(test.design[,j])[2], 4), rep(levels(test.design[,j])[3], 10))
          }
        } else if (setting == 3) {
          train.design = train.design[ , 1:6 ]
          test.design = test.design[ , 1:6 ]
          for (j in 1:3) {
            levels(train.design[ , j ]) = c(rep(levels(train.design[,j])[1], 8), rep(levels(train.design[,j])[2], 8), rep(levels(train.design[,j])[3], 8))
            levels(test.design[ , j ]) = c(rep(levels(test.design[,j])[1], 8), rep(levels(test.design[,j])[2], 8), rep(levels(test.design[,j])[3], 8))
          }
          for (j in 4:6) {
            levels(train.design[ , j ]) = c(rep(levels(train.design[,j])[1], 16), rep(levels(train.design[,j])[2], 8))
            levels(test.design[ , j ]) = c(rep(levels(test.design[,j])[1], 16), rep(levels(test.design[,j])[2], 8))
          }
          
          
        } else if (setting == 4) {
          train.design = train.design[ , 1:5 ]
          test.design = test.design[ , 1:5 ]
          for (j in 1:5) {
            levels(train.design[ , j ]) = c(rep(levels(train.design[,j])[1], 5), rep(levels(train.design[,j])[2], 5), rep(levels(train.design[,j])[3], 4), rep(levels(train.design[,j])[4], 5), rep(levels(train.design[,j])[5], 5))
            levels(test.design[ , j ]) = c(rep(levels(test.design[,j])[1], 5), rep(levels(test.design[,j])[2], 5), rep(levels(test.design[,j])[3], 4), rep(levels(test.design[,j])[4], 5), rep(levels(test.design[,j])[5], 5))
          }
          
        } else if (setting == 5) {
          train.design = train.design[,1:25]
          test.design = test.design[,1:25]
          for (j in 1:25) {
            levels(train.design[ , j ]) = c(rep(levels(train.design[,j])[1], 16), rep(levels(train.design[,j])[2], 8))
            levels(test.design[ , j ]) = c(rep(levels(test.design[,j])[1], 16), rep(levels(test.design[,j])[2], 8))
          }
        } else if (setting == 6) {
          train.design = train.design[,1:25]
          test.design = test.design[,1:25]
          for (j in 1:25) {
            levels(train.design[ , j ]) = c(rep(levels(train.design[,j])[1], 16), rep(levels(train.design[,j])[2], 8))
            levels(test.design[ , j ]) = c(rep(levels(test.design[,j])[1], 16), rep(levels(test.design[,j])[2], 8))
          }
        } else if (setting == 7) {
          train.design = train.design[,1:10]
          test.design = test.design[,1:10]
          for (j in 1:10) {
            levels(train.design[ , j ]) = c(rep(levels(train.design[,j])[1], 4), rep(levels(train.design[,j])[2], 12), rep(levels(train.design[,j])[3], 8))
            levels(test.design[ , j ]) = c(rep(levels(test.design[,j])[1], 4), rep(levels(test.design[,j])[2], 12), rep(levels(test.design[,j])[3], 8))
          }
        } else if (setting == 8) {
          train.design = train.design[,1:5]
          test.design = test.design[,1:5]
          for (j in 1:5) {
            levels(train.design[ , j ]) = c(rep(levels(train.design[,j])[1], 6), rep(levels(train.design[,j])[2], 6), rep(levels(train.design[,j])[3], 6), rep(levels(train.design[,j])[4], 6))
            levels(test.design[ , j ]) = c(rep(levels(test.design[,j])[1], 6), rep(levels(test.design[,j])[2], 6), rep(levels(test.design[,j])[3], 6), rep(levels(test.design[,j])[4], 6))
          }
        }

        print('Now compute oracle least squares models')
        starttime = tic()
        ols_mod = lm(y ~ ., data = data.frame(y, train.design))
        stoptime = tic()
        duration = stoptime - starttime
        ols_time[ job_id, setting ] = duration

        print("Computing OLS predictions")
        ols_pred = predict(ols_mod, test.design)
        # NOW do the predictions and save the results
        ols_prederr[ job_id, setting ] = mean((test.signal - ols_pred)^2)
        # STOP
        
        rm(test.design.dummy)
        rm(train.design.dummy)
        
    }
    number_finished = job_id
    save(scopes_prederr, scopel_prederr, scopecv_prederr, dmr_prederr, cart_prederr, rf_prederr, glmnet_prederr, ols_prederr,
    scopes_time, scopel_time, scopecv_time, dmr_time, cart_time, rf_time, glmnet_time, ols_time,
    scopes_dim, scopel_dim, scopecv_dim, dmr_dim, glmnet_dim, scopecv_gam, number_finished,
    scopes_models, scopel_models, scopecv_models, dmr_models, glmnet_models,
    file=paste0("section_6_1_2_raw_", comp_name, ".Rdata"))
    
}

