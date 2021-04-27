

load("section_6_1_1_raw.Rdata") # replace with the file that the running tests generates



library(CatReg)

library(bazar)

library(fossil)

library(rpart)

library(effectFusion)

library(randomForest)

library(DMRnet)

n_obs = 150
#n_obs = length(lm_model_list)


# first make all the matrices that results will be stored in

lm_prederr = matrix(0, n_obs, 9)
ols_prederr = matrix(0, n_obs, 9)
cas_prederr = matrix(0, n_obs, 9) # ordinary CasANOVA
acc_prederr = matrix(0, n_obs, 9) # CasANOVA_adaptive CasANOVA
scopes_prederr = matrix(0, n_obs, 9)
scopem_prederr = matrix(0, n_obs, 9)
scopel_prederr = matrix(0, n_obs, 9)
scopecv_prederr = matrix(0, n_obs, 9)
dmr_prederr = matrix(0, n_obs, 9)
rf_prederr = matrix(0, n_obs, 9)
cart_prederr = matrix(0, n_obs, 9)
bayes_prederr = matrix(0, n_obs, 9)

scopecv_gam = matrix(0, n_obs, 9)

lm_time = matrix(0, n_obs, 9)
ols_time = matrix(0, n_obs, 9)
cas_time = matrix(0, n_obs, 9) # ordinary CasANOVA
acc_time = matrix(0, n_obs, 9) # CasANOVA_adaptive CasANOVA
scopes_time = matrix(0, n_obs, 9)
scopem_time = matrix(0, n_obs, 9)
scopel_time = matrix(0, n_obs, 9)
scopecv_time = matrix(0, n_obs, 9)
dmr_time = matrix(0, n_obs, 9)
rf_time = matrix(0, n_obs, 9)
cart_time = matrix(0, n_obs, 9)
bayes_time = matrix(0, n_obs, 9)


lm_est_l1 = matrix(0, n_obs, 9)
lm_est_l2 = matrix(0, n_obs, 9)
ols_est_l1 = matrix(0, n_obs, 9)
ols_est_l2 = matrix(0, n_obs, 9)
cas_est_l1 = matrix(0, n_obs, 9)
cas_est_l2 = matrix(0, n_obs, 9)
acc_est_l1 = matrix(0, n_obs, 9)
acc_est_l2 = matrix(0, n_obs, 9)
scopes_est_l1 = matrix(0, n_obs, 9)
scopes_est_l2 = matrix(0, n_obs, 9)
scopem_est_l1 = matrix(0, n_obs, 9)
scopem_est_l2 = matrix(0, n_obs, 9)
scopel_est_l1 = matrix(0, n_obs, 9)
scopel_est_l2 = matrix(0, n_obs, 9)
scopecv_est_l1 = matrix(0, n_obs, 9)
scopecv_est_l2 = matrix(0, n_obs, 9)
dmr_est_l1 = matrix(0, n_obs, 9)
dmr_est_l2 = matrix(0, n_obs, 9)
bayes_est_l1 = matrix(0, n_obs, 9)
bayes_est_l2 = matrix(0, n_obs, 9)


lm_dim = matrix(0, n_obs, 9)
ols_dim = matrix(0, n_obs, 9)
cas_dim = matrix(0, n_obs, 9)
acc_dim = matrix(0, n_obs, 9)
scopes_dim = matrix(0, n_obs, 9)
scopem_dim = matrix(0, n_obs, 9)
scopel_dim = matrix(0, n_obs, 9)
scopecv_dim = matrix(0, n_obs, 9)
dmr_dim = matrix(0, n_obs, 9)
bayes_dim = matrix(0, n_obs, 9)





lm_sigrat = matrix(0, n_obs, 9)
ols_sigrat = matrix(0, n_obs, 9)
cas_sigrat = matrix(0, n_obs, 9)
acc_sigrat = matrix(0, n_obs, 9)
scopes_sigrat = matrix(0, n_obs, 9)
scopem_sigrat = matrix(0, n_obs, 9)
scopel_sigrat = matrix(0, n_obs, 9)
scopecv_sigrat = matrix(0, n_obs, 9)
dmr_sigrat = matrix(0, n_obs, 9)
bayes_sigrat = matrix(0, n_obs, 9)

# generating true signal vectors

signal = matrix(0, 24, 3)
signal[ , 1 ] = c(rep(-3, 10), rep(0, 4), rep(3, 10))
signal[ , 2 ] = c(rep(-3, 8), rep(0, 8), rep(3, 8))
signal[ , 3 ] = c(rep(-3, 10), rep(0, 4), rep(3, 10))

# true signal vectors with all the dummy labels (as oracle LSE would estimate)

truth = matrix(0, 241, 3)
truth[ , 1 ] = c(0, rep(signal[ , 1 ], 3), rep(0, 168))
truth[ , 2 ] = c(0, rep(signal[ , 2 ], 3), rep(0, 168))
truth[ , 3 ] = c(0, rep(signal[ , 3 ], 3), rep(0, 168))
cormat = 0.8 * matrix(1, 10, 10) + 0.2 * diag(10)
sigmas = c(0, 1, 2.5, 5, 10)
model.index = 1
result.index = 1
for ( i in 1:n_obs ) {
    
    print(paste0("set ", i))
    #test.design = UniformDesignMatrix(100000, 10, 24)
    for ( setting in 1:3 ) {
        # generate design matrices
        test.design = UniformDesignMatrix(100000, 10, 24)
        if ( setting == 3 ) {
            test.design = CorrelatedDesignMatrix(100000, cormat, 24)
        }
        
        y = signal[ test.design[ , 1 ], setting ] + signal[ test.design[ , 2 ], setting ] + signal[ test.design[ , 3 ], setting ]

        for ( noise.level in 1:5 ) {
            # Now actually fit the models
            if ( ( noise.level == 1 ) || ( noise.level == 5 ) ) {
                model.index = model.index + 1
            } else {
                # y = test.signal + sigmas[ noise.level ] * rnorm(10000)
                print("Model number:")
                print(paste0("Computing results: model.index = ", model.index, ", result.index = ", result.index))
                print("linear model")
                lm_pred = lm_models[[ model.index ]][[ 1 ]][[ 1 ]]
                for ( j in 1:10 ) lm_pred = lm_pred + lm_models[[ model.index ]][[ 1 ]][[ 1 + j ]][ test.design[ , j ] ]
                
                for ( j in 1:10 ) {
                    temp_mean = mean(lm_models[[ model.index ]][[ 1 ]][[ 1 + j ]])
                    lm_models[[ model.index ]][[ 1 ]][[ 1 ]] = lm_models[[ model.index ]][[ 1 ]][[ 1 ]] + temp_mean
                    lm_models[[ model.index ]][[ 1 ]][[ 1 + j]] = lm_models[[ model.index ]][[ 1 ]][[ 1 + j ]] - temp_mean
                }
                
                lm_est = unlist(lm_models[[ model.index ]][[ 1 ]])
                
                lm_dim[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = length(lm_est[ lm_est != 0 ])
                lm_esterr = truth[ , setting ] - unlist(lm_models[[ model.index ]][[ 1 ]])
                lm_est_l1[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = sum(abs(lm_esterr))
                lm_est_l2[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = sqrt(sum(lm_esterr^2))
                
                print("oracle least squares model")
                ols_pred = ols_models[[ model.index ]][[ 1 ]][[1]]
                for ( j in 1:10 ) ols_pred = ols_pred + ols_models[[ model.index ]][[ 1 ]][[ 1 + j ]][ test.design[ , j ] ]
                
                
                for ( j in 1:10 ) {
                    temp_mean = mean(ols_models[[ model.index ]][[ 1 ]][[ 1 + j ]])
                    ols_models[[ model.index ]][[ 1 ]][[ 1 ]] = ols_models[[ model.index ]][[ 1 ]][[ 1 ]] + temp_mean
                    ols_models[[ model.index ]][[ 1 ]][[ 1 + j]] = ols_models[[ model.index ]][[ 1 ]][[ 1 + j ]] - temp_mean
                }
                ols_est = unlist(ols_models[[ model.index ]][[ 1 ]])
                
                ols_dim[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = 7
                ols_esterr = truth[ , setting ] - unlist(ols_models[[ model.index ]][[ 1 ]])
                ols_est_l1[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = sum(abs(ols_esterr))
                ols_est_l2[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = sqrt(sum(ols_esterr^2))
                
                # Process the vector from dmr
                print("DMRnet")
                coefs = dmr_models[[ model.index ]][[ 1 ]]
                # Script to get into the form of lm dummy coefs
                dmrcoeflist = list()
                dmrcoeflist[[ 1 ]] = coefs[ 1 ]
                for ( j in 1:10 ) {
                    dmrcoeflist[[ j + 1 ]] = c(0, coefs[ 23 * ( j - 1 ) + 2:24 ])
                }
                dmr_pred = dmrcoeflist[[ 1 ]]
                for ( j in 1:10 ) dmr_pred = dmr_pred + dmrcoeflist[[ 1 + j ]][ test.design[ , j ] ]
                
                for ( j in 1:10 ) {
                    temp_mean = mean(dmrcoeflist[[ 1 + j ]])
                    dmrcoeflist[[ 1 ]] = dmrcoeflist[[ 1 ]] + temp_mean
                    dmrcoeflist[[ 1 + j ]] = dmrcoeflist[[ 1 + j ]] - temp_mean
                }
                dmr_est = unlist(dmrcoeflist)
                
                dmr_dim[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = length(dmrcoeflist[[ 1 ]])
                for ( j in 1:10 ) dmr_dim[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = dmr_dim[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] + length(almost.unique(dmrcoeflist[[ 1 + j ]], tolerance = 1e-4)) - 1
                dmr_esterr = truth[ , setting ] - dmr_est
                dmr_est_l1[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = sum(abs(dmr_esterr))
                dmr_est_l2[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = sqrt(sum(dmr_esterr^2))
                
                
                # Process the vector from effectFusion
                print("effectFusion model")
                betamat = bayes_models[[ model.index ]][[ 1 ]]
                # Use 1000 random posterior samples to get an estimate of the posterior means
                coefs = colMeans(betamat[ sample(1:3000, 1000), ])
                # Script to get into the form of lm dummy coefs
                bayescoeflist = list()
                bayescoeflist[[ 1 ]] = coefs[ 1 ]
                for ( j in 1:10 ) {
                    bayescoeflist[[ j + 1 ]] = c(0, coefs[ 23 * ( j - 1 ) + 2:24 ])
                }
                bayes_pred = bayescoeflist[[ 1 ]]
                for ( j in 1:10 ) bayes_pred = bayes_pred + bayescoeflist[[ 1 + j ]][ test.design[ , j ] ]
                
                for ( j in 1:10 ) {
                    temp_mean = mean(bayescoeflist[[ 1 + j ]])
                    bayescoeflist[[ 1 ]] = bayescoeflist[[ 1 ]] + temp_mean
                    bayescoeflist[[ 1 + j ]] = bayescoeflist[[ 1 + j ]] - temp_mean
                }
                bayes_est = unlist(bayescoeflist)
                
                bayes_dim[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = length(bayescoeflist[[ 1 ]])
                for ( j in 1:10 ) bayes_dim[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = bayes_dim[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] + length(almost.unique(bayescoeflist[[ 1 + j ]], tolerance = 1e-4)) - 1
                bayes_esterr = truth[ , setting ] - bayes_est
                bayes_est_l1[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = sum(abs(bayes_esterr))
                bayes_est_l2[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = sqrt(sum(bayes_esterr^2))
                
                print("Random Forests")
                rf_pred = predict(rf_models[[ model.index ]][[ 1 ]], data = test.design)
                
                print("CART")
                cart_pred = predict(cart_models[[ model.index ]][[ 1 ]], data = test.design)
                
                print("ordinary casanova model")
                cas_pred = cas_models[[ model.index ]][[ 1 ]][[ 1 ]][[ 1 ]]
                for ( j in 1:10 ) cas_pred = cas_pred + cas_models[[ model.index ]][[ 1 ]][[ 1 ]][[ 2 ]][[ j ]][ test.design[ , j ] ]
                
                for ( j in 1:10 ) {
                    temp_mean = mean(cas_models[[ model.index ]][[ 1 ]][[ 1 ]][[ 2 ]][[ j ]])
                    cas_models[[ model.index ]][[ 1 ]][[ 1 ]][[ 1 ]] = cas_models[[ model.index ]][[ 1 ]][[ 1 ]][[ 1 ]] + temp_mean
                    cas_models[[ model.index ]][[ 1 ]][[ 1 ]][[ 2 ]][[ j ]] = cas_models[[ model.index ]][[ 1 ]][[ 1 ]][[ 2 ]][[ j ]] - temp_mean
                }
                cas_est = unlist(cas_models[[ model.index ]][[ 1 ]])
                
                cas_dim[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = length(cas_models[[ model.index ]][[ 1 ]][[ 1 ]][[ 1 ]])
                for ( j in 1:10 ) cas_dim[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = cas_dim[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] + length(almost.unique(cas_models[[ model.index ]][[ 1 ]][[ 1 ]][[ 2 ]][[ j ]], tolerance = 1e-4)) - 1
                cas_esterr = truth[ , setting ] - unlist(cas_models[[ model.index ]][[ 1 ]][[ 1 ]])
                cas_est_l1[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = sum(abs(cas_esterr))
                cas_est_l2[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = sqrt(sum(cas_esterr^2))
                
                
                
                print("adaptive casanova model")
                acc_pred = acc_models[[ model.index ]][[ 1 ]][[ 1 ]][[ 1 ]]
                for ( j in 1:10 ) acc_pred = acc_pred + acc_models[[ model.index ]][[ 1 ]][[ 1 ]][[ 2 ]][[ j ]][ test.design[ , j ] ]
                
                for ( j in 1:10 ) {
                    temp_mean = mean(acc_models[[ model.index ]][[ 1 ]][[ 1 ]][[ 2 ]][[ j ]])
                    acc_models[[ model.index ]][[ 1 ]][[ 1 ]][[ 1 ]] = acc_models[[ model.index ]][[ 1 ]][[ 1 ]][[ 1 ]] + temp_mean
                    acc_models[[ model.index ]][[ 1 ]][[ 1 ]][[ 2 ]][[ j ]] = acc_models[[ model.index ]][[ 1 ]][[ 1 ]][[ 2 ]][[ j ]] - temp_mean
                }
                acc_est = unlist(acc_models[[ model.index ]][[ 1 ]][[ 1 ]])
                
                acc_dim[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = length(acc_models[[ model.index ]][[ 1 ]][[ 1 ]][[ 1 ]])
                for ( j in 1:10 ) acc_dim[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = acc_dim[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] + length(almost.unique(acc_models[[ model.index ]][[ 1 ]][[ 1 ]][[ 2 ]][[ j ]], tolerance = 1e-4)) - 1
                acc_esterr = truth[ , setting ] - unlist(acc_models[[ model.index ]][[ 1 ]][[ 1 ]])
                acc_est_l1[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = sum(abs(acc_esterr))
                acc_est_l2[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = sqrt(sum(acc_esterr^2))
                
                print("scope cv")
                #scopecv_pred = scopecv_models[[ model.index ]][[ 1 ]][[ 1 ]][[ 1 ]]
                #for ( j in 1:10 ) scopecv_pred = scopecv_pred + scopecv_models[[ model.index ]][[ 1 ]][[ 1 ]][[ 2 ]][[ j ]][ test.design[ , j ] ]
                scopecv_pred = predict(scopecv_models[[ model.index ]][[ 1 ]][[ 1 ]], data.frame(1,test.design)) #Not tested but this is how the syntax should go
                
                #for estimation error
                for ( j in 1:10 ) {
                    temp_mean = mean(scopecv_models[[ model.index ]][[ 1 ]][[ 1 ]][[ 2 ]][[ j ]])
                    scopecv_models[[ model.index ]][[ 1 ]][[ 1 ]][[ 1 ]] = scopecv_models[[ model.index ]][[ 1 ]][[ 1 ]][[ 1 ]] + temp_mean
                    scopecv_models[[ model.index ]][[ 1 ]][[ 1 ]][[ 2 ]][[ j ]] = scopecv_models[[ model.index ]][[ 1 ]][[ 1 ]][[ 2 ]][[ j ]] - temp_mean
                }
                scopecv_est = unlist(scopecv_models[[ model.index ]][[ 1 ]][[ 1 ]])
                
                # for model dimension
                scopecv_dim[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = length(scopecv_models[[ model.index ]][[ 1 ]][[ 1 ]][[ 1 ]])
                for ( j in 1:10 ) scopecv_dim[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = scopecv_dim[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] + length(almost.unique(scopecv_models[[ model.index ]][[ 1 ]][[ 1 ]][[ 2 ]][[ j ]], tolerance = 1e-4)) - 1
                scopecv_esterr = truth[ , setting ] - unlist(scopecv_models[[ model.index ]][[ 1 ]][[ 1 ]])
                scopecv_est_l1[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = sum(abs(scopecv_esterr))
                scopecv_est_l2[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = sqrt(sum(scopecv_esterr^2))
                
                scopecv_gam[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = scopecv_models[[ model.index ]][[ 2 ]]
                
                print("scope s")
                #scopes_pred = scopes_models[[ model.index ]][[ 1 ]][[ 1 ]][[ 1 ]]
                #for ( j in 1:10 ) scopes_pred = scopes_pred + scopes_models[[ model.index ]][[ 1 ]][[ 1 ]][[ 2 ]][[ j ]][ test.design[ , j ] ]
                scopes_pred = predict(scopes_models[[ model.index ]][[ 1 ]][[ 1 ]], data.frame(1,test.design))
                
                for ( j in 1:10 ) {
                    temp_mean = mean(scopes_models[[ model.index ]][[ 1 ]][[ 1 ]][[ 2 ]][[ j ]])
                    scopes_models[[ model.index ]][[ 1 ]][[ 1 ]][[ 1 ]] = scopes_models[[ model.index ]][[ 1 ]][[ 1 ]][[ 1 ]] + temp_mean
                    scopes_models[[ model.index ]][[ 1 ]][[ 1 ]][[ 2 ]][[ j ]] = scopes_models[[ model.index ]][[ 1 ]][[ 1 ]][[ 2 ]][[ j ]] - temp_mean
                }
                scopes_est = unlist(scopes_models[[ model.index ]][[ 1 ]][[ 1 ]])
                
                scopes_dim[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = length(scopes_models[[ model.index ]][[ 1 ]][[ 1 ]][[ 1 ]])
                for ( j in 1:10 ) scopes_dim[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = scopes_dim[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] + length(almost.unique(scopes_models[[ model.index ]][[ 1 ]][[ 1 ]][[ 2 ]][[ j ]], tolerance = 1e-4)) - 1
                scopes_esterr = truth[ , setting ] - unlist(scopes_models[[ model.index ]][[ 1 ]][[ 1 ]])
                scopes_est_l1[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = sum(abs(scopes_esterr))
                scopes_est_l2[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = sqrt(sum(scopes_esterr^2))
                
                print("scope m")
                scopem_pred = scopem_models[[ model.index ]][[ 1 ]][[ 1 ]][[ 1 ]]
                for ( j in 1:10 ) scopem_pred = scopem_pred + scopem_models[[ model.index ]][[ 1 ]][[ 1 ]][[ 2 ]][[ j ]][ test.design[ , j ] ]
                
                for ( j in 1:10 ) {
                    temp_mean = mean(scopem_models[[ model.index ]][[ 1 ]][[ 1 ]][[ 2 ]][[ j ]])
                    scopem_models[[ model.index ]][[ 1 ]][[ 1 ]][[ 1 ]] = scopem_models[[ model.index ]][[ 1 ]][[ 1 ]][[ 1 ]] + temp_mean
                    scopem_models[[ model.index ]][[ 1 ]][[ 1 ]][[ 2 ]][[ j ]] = scopem_models[[ model.index ]][[ 1 ]][[ 1 ]][[ 2 ]][[ j ]] - temp_mean
                }
                scopem_est = unlist(scopem_models[[ model.index ]][[ 1 ]][[ 1 ]])
                
                scopem_dim[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = length(scopem_models[[ model.index ]][[ 1 ]][[ 1 ]][[ 1 ]])
                for ( j in 1:10 ) scopem_dim[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = scopem_dim[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] + length(almost.unique(scopem_models[[ model.index ]][[ 1 ]][[ 1 ]][[ 2 ]][[ j ]], tolerance = 1e-4)) - 1
                scopem_esterr = truth[ , setting ] - unlist(scopem_models[[ model.index ]][[ 1 ]][[ 1 ]])
                scopem_est_l1[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = sum(abs(scopem_esterr))
                scopem_est_l2[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = sqrt(sum(scopem_esterr^2))
                
                print("scope l")
                scopel_pred = scopel_models[[ model.index ]][[ 1 ]][[ 1 ]][[ 1 ]]
                for ( j in 1:10 ) scopel_pred = scopel_pred + scopel_models[[ model.index ]][[ 1 ]][[ 1 ]][[ 2 ]][[ j ]][ test.design[ , j ] ]
                
                for ( j in 1:10 ) {
                    temp_mean = mean(scopel_models[[ model.index ]][[ 1 ]][[ 1 ]][[ 2 ]][[ j ]])
                    scopel_models[[ model.index ]][[ 1 ]][[ 1 ]][[ 1 ]] = scopel_models[[ model.index ]][[ 1 ]][[ 1 ]][[ 1 ]] + temp_mean
                    scopel_models[[ model.index ]][[ 1 ]][[ 1 ]][[ 2 ]][[ j ]] = scopel_models[[ model.index ]][[ 1 ]][[ 1 ]][[ 2 ]][[ j ]] - temp_mean
                }
                scopel_est = unlist(scopel_models[[ model.index ]][[ 1 ]][[ 1 ]])
                
                scopel_dim[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = length(scopel_models[[ model.index ]][[ 1 ]][[ 1 ]][[ 1 ]])
                for ( j in 1:10 ) scopel_dim[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = scopel_dim[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] + length(almost.unique(scopel_models[[ model.index ]][[ 1 ]][[ 1 ]][[ 2 ]][[ j ]], tolerance = 1e-4)) - 1
                scopel_esterr = truth[ , setting ] - unlist(scopel_models[[ model.index ]][[ 1 ]][[ 1 ]])
                scopel_est_l1[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = sum(abs(scopel_esterr))
                scopel_est_l2[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = sqrt(sum(scopel_esterr^2))
                
                print(paste0("(", i, ", ", result.index %% 9 + 9*(result.index %% 9 == 0), ")"))
                print(mean((y - lm_pred)^2))
                lm_prederr[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = mean((y - lm_pred)^2)
                print(mean((y - ols_pred)^2))
                ols_prederr[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = mean((y - ols_pred)^2)
                print(mean((y - cas_pred)^2))
                cas_prederr[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = mean((y - cas_pred)^2)
                acc_prederr[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = mean((y - acc_pred)^2)
                scopes_prederr[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = mean((y - scopes_pred)^2)
                scopem_prederr[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = mean((y - scopem_pred)^2)
                scopel_prederr[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = mean((y - scopel_pred)^2)
                print(mean((y - scopecv_pred)^2))
                scopecv_prederr[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = mean((y - scopecv_pred)^2)
                dmr_prederr[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = mean((y - dmr_pred)^2)
                bayes_prederr[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = mean((y - bayes_pred)^2)
                rf_prederr[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = mean((y - rf_pred)^2)
                cart_prederr[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = mean((y - cart_pred)^2)
                
           
                lm_sigrat[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = sqrt(sum(lm_est[ 1:73 ]^2)) / sqrt(sum(lm_est^2))
                ols_sigrat[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = sqrt(sum(ols_est[ 1:73 ]^2)) / sqrt(sum(ols_est^2))
                cas_sigrat[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = sqrt(sum(cas_est[ 1:73 ]^2)) / sqrt(sum(cas_est^2))
                acc_sigrat[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = sqrt(sum(acc_est[ 1:73 ]^2)) / sqrt(sum(acc_est^2))
                scopes_sigrat[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = sqrt(sum(scopes_est[ 1:73 ]^2)) / sqrt(sum(scopes_est^2))
                scopem_sigrat[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = sqrt(sum(scopem_est[ 1:73 ]^2)) / sqrt(sum(scopem_est^2))
                scopel_sigrat[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = sqrt(sum(scopel_est[ 1:73 ]^2)) / sqrt(sum(scopel_est^2))
                scopecv_sigrat[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = sqrt(sum(scopecv_est[ 1:73 ]^2)) / sqrt(sum(scopecv_est^2))
                dmr_sigrat[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = sqrt(sum(dmr_est[ 1:73 ]^2)) / sqrt(sum(dmr_est^2))
                bayes_sigrat[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = sqrt(sum(bayes_est[ 1:73 ]^2)) / sqrt(sum(bayes_est^2))
                
                lm_time[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = lm_models[[ model.index ]][[ 2 ]]
                ols_time[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = ols_models[[ model.index ]][[ 2 ]]
                cas_time[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = cas_models[[ model.index ]][[ 2 ]]
                acc_time[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = acc_models[[ model.index ]][[ 2 ]]
                scopes_time[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = scopes_models[[ model.index ]][[ 2 ]]
                scopem_time[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = scopem_models[[ model.index ]][[ 2 ]]
                scopel_time[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = scopel_models[[ model.index ]][[ 2 ]]
                scopecv_time[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = scopecv_models[[ model.index ]][[ 4 ]]
                dmr_time[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = dmr_models[[ model.index ]][[ 2 ]]
                bayes_time[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = bayes_models[[ model.index ]][[ 2 ]]
                rf_time[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = rf_models[[ model.index ]][[ 2 ]]
                cart_time[ i, result.index %% 9 + 9*(result.index %% 9 == 0) ] = cart_models[[ model.index ]][[ 2 ]]
                
                
                result.index = result.index + 1
                model.index = model.index + 1
            }
        }
    }
  
    
    
    # list of objects that I want to save that will contain all the results
    save(lm_est_l1, lm_est_l2, ols_est_l1, ols_est_l2, cas_est_l1, cas_est_l2, acc_est_l1, acc_est_l2, scopes_est_l1, scopes_est_l2, scopem_est_l1, scopem_est_l2, scopel_est_l1, scopel_est_l2, scopecv_est_l1, scopecv_est_l2, dmr_est_l1, dmr_est_l2, lm_prederr, ols_prederr, cas_prederr, acc_prederr, scopecv_prederr, scopes_prederr, scopem_prederr, scopel_prederr, dmr_prederr, rf_prederr, cart_prederr, lm_dim, cas_dim, acc_dim, scopes_dim, scopem_dim, scopel_dim, scopecv_dim, dmr_dim, lm_sigrat, cas_sigrat, acc_sigrat, scopes_sigrat, scopem_sigrat, scopel_sigrat, scopecv_sigrat, dmr_sigrat, lm_time, ols_time, cas_time, acc_time, scopes_time, scopem_time, scopel_time, scopecv_time, dmr_time, rf_time, cart_time, scopecv_gam, bayes_est_l1, bayes_est_l2, bayes_prederr, bayes_dim,  bayes_sigrat, bayes_time, file="section_6_1_1_results.Rdata")

}















