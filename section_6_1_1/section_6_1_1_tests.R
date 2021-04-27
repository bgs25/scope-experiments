


source("casanovafit.factor.crossval.R")

library(CatReg)

library(bazar)

library(randomForest)

library(tictoc)

library(DMRnet)

library(rpart)

library(effectFusion)


# This contains the lists that results will be stored in
scopecv_model_list = list()
scopes_model_list = list()
scopem_model_list = list()
scopel_model_list = list()
acc_model_list = list()
rf_model_list = list()
cart_model_list = list()
dmr_model_list = list()
bayes_model_list = list()
train.list = list()
response.list = list()



# signal regime 1, 2
# noise regime 1, 2, 3, 4

# Just fit models, on monday we'll compute prediction error, estimation error, signal proportion, clustering, et cetera

#with this regime, probability of not seeing category in training set is < 0.00001 in 1000 replicates so we ignore that it could happen... running it on loads of computers anyway so not an issue

n_jobs = 250

gammaseq = c(0.001, 2^(0:7), 1000) #for the cross-validating gamma

gamsol = list()
bestcv = rep(0, length(gammaseq))
grid.safe = 200



print("Pausing...")
Sys.sleep(rexp(1,0.1))
print("Go!")
for(job_id in 1:n_jobs) {
    
    
    
    
    
    model.index = 1
    
    
    # Main code to run. This should write to a file job.txt in the Results directory
    for ( setting in 1:3 ) {
        
        train.design = train.list[[ setting ]]
        for ( noise.level in 1:5 ) {
            # Now actually fit the models
            
            y = response.list[[ 5*(setting - 1) + noise.level ]]
            
            Sys.sleep(rexp(1,0.5))
            print(setting)
            print(noise.level)
            
            # Script for cross-validating gamma. Fixes random elements of the
            # cross-validation process to ensure that coordinate descent cycle order
            # is preserved, otherwise the randomness in the different CV curves depending
            # on this causes a lot of variance in the parameters selected.
            print("scope CV")
            cycleorder = sample(1:10)
            cvfold = as.integer(sample(ceiling((1:100)*5/100)))
            starttime = tic()
            for ( gam in 1:length(gammaseq) ) {
                print(paste0("gamma = ", gammaseq[gam]))
                gamsol[[ gam ]] = scope(y, data.frame(matrix(1, 500, 1) train.design), interceptxlinear = T,  gamma = gammaseq[gam], default.length = 150, BICterminate = 40, simply.cross.validated = TRUE, silent = TRUE, blockorder = cycleorder, FoldAssignment = cvfold)
                bestcv[ gam ] = min(gamsol[[ gam ]][ , 1 ])
            }
            bestgamind = which.min(bestcv)
            bestgam = gammaseq[ bestgamind ]
            bestlam = t(gamsol[[ bestgamind ]][ , -1 ])
            terminationpoint = which.min(gamsol[[ bestgamind ]][ , 1 ])
            grid.safe = 200
            bestlam = data.frame(matrix(0, 10, grid.safe), bestlam)
            lambdaratio = bestlam[ 1, grid.safe + 1 ] / bestlam[ 1, grid.safe + 2 ]
            for ( i in seq(grid.safe, 1, -1) ) {
                bestlam[ , i ] = lambdaratio * bestlam[ , i + 1 ]
            }
            bestlam = bestlam[ , 1:(terminationpoint + grid.safe) ]
            print("Cross-validated gamma and lambda, now fitting final version")
            solution = scope(y, data.frame(matrix(1, 500, 1), train.design), interceptxlinear = T,  gamma = bestgam, blockorder = cycleorder, FoldAssignment = cvfold, BICterminate = 40)
            stoptime = tic()
            duration = stoptime - starttime
            
            scopecv_model_list[[ model.index ]] = list(solution, bestgam, bestcv, duration)
            
            print("scope small gamma")
            starttime = tic()
            solution = scope(y, data.frame(matrix(1, 500, 1), train.design), interceptxlinear = T, default.length = 150, BICterminate = 40)
            stoptime = tic()
            duration = stoptime - starttime
            scopes_model_list[[ model.index ]] = list(solution, duration)
            
            
            print("scope medium gamma")
            starttime = tic()
            solution =scope(y, data.frame(matrix(1, 500, 1), train.design), interceptxlinear = T, default.length = 150, BICterminate = 40, gamma = 16)
            stoptime = tic()
            duration = stoptime - starttime
            scopem_model_list[[ model.index ]] = list(solution, duration)
            
            
            print("scope large gamma")
            solution =scope(y, data.frame(matrix(1, 500, 1), train.design), interceptxlinear = T, default.length = 150, BICterminate = 40, gamma = 32)
            stoptime = tic()
            duration = stoptime - starttime
            scopel_model_list[[ model.index ]] = list(solution, duration)
            
            
            print("Random Forest")
            starttime = tic()
            solution = randomForest(y ~ ., data = data.frame(y, train.design))
            stoptime = tic()
            duration = stoptime - starttime
            rf_model_list[[ model.index ]] = list(solution, duration)
            
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
            solution = prune(cpsolution, cp = cpthresh)
            stoptime = tic()
            duration = stoptime - starttime
            cart_model_list[[ model.index ]] = list(solution, cpsolution, duration)
            
            print("DMRnet")
            starttime = tic()
            if ( noise.level == 1 ) {
                print("adding small noise to y")
                cvy = y + 0.1 * rnorm(500) # This is required because DMRnet often errors in exact noiseless case due to diving through by 0 somewhere
            } else {
                cvy = y
            }
            print("fitting cv solution")
            cvsolution = cv.DMRnet(train.design, cvy, nfolds = 5)
            cvmaxp = cvsolution$df.min - 1
            print(cvmaxp)
            print("fitting full solution")
            solution = DMRnet(train.design, cvy, maxp = cvmaxp)
            solution = solution$beta[ , 1 ]
            stoptime = tic()
            duration = stoptime - starttime
            dmr_model_list[[ model.index ]] = list(solution, cvsolution, duration)
            
            print("Effect fusion")
            starttime = tic()
            if ( ( noise.level == 1 ) || ( noise.level == 5 ) ) {
                solution = NULL
            } else {
                solution = effectFusion(y, train.design, types = rep("n", 10), method = "FinMix")
                solution = solution$refit$beta
            }
            # Modelling category levels in a bayesian way as a sparse finite gaussian mixture model
            stoptime = tic()
            duration = stoptime - starttime
            
            bayes_model_list[[ model.index ]] = list(solution, duration)
            
            print("2-stage adaptive casanova")
            starttime = tic()
            if ( ( noise.level == 1 ) || ( noise.level == 5 ) ) {
                solution = NULL
            } else {
                solution = casanovafit(y, matrix(1, 500, 1), train.design, interceptxlinear = T, prev.coefficients = unlist(cas_model_list[[ model.index ]][[ 1 ]][[ 1 ]]), BICterminate = 50)
            }
            
            stoptime = tic()
            duration = stoptime - starttime
            acc_model_list[[ model.index ]] = list(solution, duration)
            
            
            model.index = model.index + 1
        }
    }
    
    save(lm_model_list, ols_model_list, cas_model_list, acc_model_list, scopecv_model_list, scopes_model_list, scopem_model_list, scopel_model_list, rf_model_list, cart_model_list, dmr_model_list, bayes_model_list, train.list, response.list, file=paste0("section_6_1_1_raw.Rdata"))
    
    
    
    
    
}



