# script for testing dummy examples on the prudential dataset. Save both the models and the predictions.

n_jobs = 50
train_proportion = 0.1
noise_sd = NULL # if NULL then it will be set to match the stnadard deviation of the signla to get a 1:1 signal to noise ratio
n_signal = 10
max_levels = function( j ) {
  return(floor(2 + 0.5*log(j)))
}

coef_vec = seq(0, 10, 1)
gammaseq = c(4, 8, 16, 32, 64)

load("prudential_processed_data.Rdata")
# categorical variables in this file are 6-116
library(CatReg)
library(glmnet)
library(ranger)
library(rpart)
library(DMRnet)
library(entropy)

vars_list = list()
cat_coefs = list()
cts_coefs = list()

response_list = list()
response_list_noise = list()

scopes_mods = list()
scopel_mods = list()
scopecv_mods = list()
glmnet_mods = list()
rf_mods = list()
cart_mods = list()
# dmr_mods = list()


scopes_pe = rep(0, n_jobs)
scopel_pe = rep(0, n_jobs)
scopecv_pe = rep(0, n_jobs)
glmnet_pe = rep(0, n_jobs)
rf_pe = rep(0, n_jobs)
cart_pe = rep(0, n_jobs)
# dmr_mods = rep(0, n_jobs)

gamma_cv = rep(0, n_jobs)

n = dim(x_train)[ 1 ]
n_train = floor(train_proportion * n)
n_test = n - n_train

# ignore this? apart from those that are actually missing
# x_train = x_train[, -c(6, 9, 11, 16, 19, 21, 23, 34, 35, 36, 37, 39, 40, 41, 43, 46,47,48,49,51,53,56,57,59,60,61)]

#gets rid of categorical variables with excessive proportions of observations missing
x_train = x_train[ , -c(39, 53, 61) ]

p = dim(x_train)[ 2 ]

comp_name = "modified_code_run_on_single_machine"

n_levels = rep(0, p - 5)
for (j in 6:p) {
    n_levels[ j - 5 ] = length(levels(x_train[ , j ]))
}
n_levels_norm = n_levels / sum(n_levels)

cat_tables = list()
var_entropy = rep(0, p - 5)
for (j in 6:p) {
    cat_tables[[ j ]] = table(x_train[,j])
    var_entropy[ j - 5 ] = entropy(cat_tables[[ j ]])
}
var_entropy_norm = var_entropy / sum(var_entropy)

for (job_id in 1:n_jobs) {
  print(paste0("Job: ", job_id))
  
  train_id = sample(c(rep(TRUE, n_train), rep(FALSE, n_test)))
  test_id = as.logical(1 - train_id)
  
  vars_list[[ job_id ]] = sample(6:p, n_signal, replace = FALSE, prob = n_levels_norm) # this is if we just want to select them with probability proportional to their number of levels
  #vars_list[[ job_id ]] = sample(6:p, n_signal, replace = FALSE, prob = var_entropy_norm) # this is if we want to select variables with probability proportional to their entropy
  
  cat_coefs[[ job_id ]] = list()
  for (k in 1:n_signal) {
    cat_coefs[[ job_id ]][[ k ]] = sample(1:max_levels(length(levels(x_train[,vars_list[[ job_id ]][ k ]]))), length(levels(x_train[,vars_list[[ job_id ]][ k ]])), replace = TRUE)
  }
  cts_coefs[[ job_id ]] = rnorm(5)
  
  response_vec = as.vector(as.matrix(x_train[ , 1:5 ]) %*% cts_coefs[[ job_id ]])
  for (k in 1:n_signal) {
    response_vec = response_vec + cat_coefs[[ job_id ]][[ k ]][ as.integer(x_train[ , vars_list[[ job_id ]][ k ]]) ]
  }
  response_list[[ job_id ]] = response_vec
  if ( is.null(noise_sd) ) {
      response_vec_noise = response_vec + sd(response_vec) * rnorm(n)
  } else {
      response_vec_noise = response_vec + noise_sd * rnorm(n)
  }

  response_list_noise[[ job_id ]] = response_vec_noise
  
  
  print("Scope S")
  scopes_mods[[ job_id ]] = scope(x_train[ train_id, ], response_vec_noise[ train_id ], early_stopping_rounds = 75, silent = F, lambda_min_ratio = 0.001)

  print("Scope L")
  scopel_mods[[ job_id ]] = scope(x_train[ train_id, ], response_vec_noise[ train_id ], early_stopping_rounds = 75, gamma = 32, silent = F, lambda_min_ratio = 0.001)

  # now the cross-validated gamma version of SCOPE
  print("Scope CV")
  cycleorder = sample(1:(p - 5))
  cvfold = as.integer(sample(ceiling((1:n_train)*5/n_train)))
  gamsol = list()
  bestcv = rep(0, 5)
  for ( gam in 1:length(gammaseq) ) {
    print(paste0("Gamma = ", gammaseq[ gam ]))
    gamsol[[ gam ]] = scope(x_train[ train_id, ], response_vec_noise[ train_id ], early_stopping_rounds = 75, gamma = gammaseq[ gam ], silent = F, only_cross_validate = TRUE, lambda_min_ratio = 0.001)
    #bestcv[ gam ] = min(gamsol[[ gam ]][ , 1 ])
    bestcv[ gam ] = min(gamsol[[ gam ]]$cverrors)
  }
  bestgamind = which.min(bestcv)
  bestgam = gammaseq[ bestgamind ]
  gamma_cv[ job_id ] = bestgam
  scopecv_mod = scope(x_train[ train_id, ], response_vec_noise[ train_id ], early_stopping_rounds = 75, gamma = bestgam, silent = F, lambda_min_ratio = 0.001)
  scopecv_mods[[ job_id ]] = scopecv_mod
  
  print("glmnet")
  options(na.action = "na.pass")
  glmnet_mods[[ job_id ]] = cv.glmnet(model.matrix(~ .-1, data=x_train[train_id,], contrasts.arg = lapply(x_train[train_id,6:p], contrasts, contrasts=FALSE)), response_vec_noise[ train_id ])
  #glmnet_mods[[ job_id ]] = cv.glmnet(model.matrix(response_vec_noise.train_id. ~ ., data.frame(response_vec_noise[train_id], x_train[train_id,])), response_vec_noise[ train_id ])
  
  print("Random Forests")
  rf_mods[[ job_id ]] = ranger(x = x_train[ train_id, ], y = response_vec_noise[ train_id ])
  
  print("CART")
  cpsolution = rpart(response_vec_noise.train_id. ~ ., data = data.frame(response_vec_noise[train_id], x_train[train_id,]))
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
  cart_mods[[ job_id ]] = cart_mod

  print("DMR")
  print("skip for now")
  #cvdmrsol = cv.DMRnet(x_train[ train_id, ], response_vec_noise[ train_id ], nfold = 5)
  #cvmaxp = length(cvdmrsol$cvm) - which.min(cvdmrsol$cvm)
  #dmr_mod = DMRnet(x_train[ train_id, ], response_vec_noise[ train_id ], maxp = cvmaxp)
  #dmr_mods[[ job_id ]] = dmr_mod
  
  # alternatively
  #dmr_mod = DMRnet(x_train[ train_id, ], response_vec_noise[ train_id ])
  #dmr_mods[[ job_id ]] = dmr_mod
  #gicdf = length(gic.DMR(dmr_mod)$gic) - which.min(gic.DMR(dmr_mod)$gic) + 1
  
  scopes_preds = predict(scopes_mods[[ job_id ]], x_train[ test_id, ])
  
  scopel_preds = predict(scopel_mods[[ job_id ]], x_train[ test_id, ])
  
  scopecv_preds = predict(scopecv_mods[[ job_id ]], x_train[ test_id, ])
  
  glmnet_preds = predict(glmnet_mods[[ job_id ]], model.matrix(~ .-1, data=x_train[test_id,], contrasts.arg = lapply(x_train[test_id,6:p], contrasts, contrasts=FALSE)))
  
  rf_preds =  predict(rf_mods[[ job_id ]], x_train[ test_id, ])$predictions
  
  cart_preds = predict(cart_mods[[ job_id ]], newdata = x_train[ test_id, ])
  
  #dmr_preds = dmr_pred = predict(dmr_mods[[ job_id ]], newx = x_train[ test_id, ])[ , 1 ]
  
  scopes_pe[ job_id ] = mean((response_vec[test_id] - scopes_preds)^2)
  
  scopel_pe[ job_id ] = mean((response_vec[test_id] - scopel_preds)^2)
  
  scopecv_pe[ job_id ] = mean((response_vec[test_id] - scopecv_preds)^2)
  
  glmnet_pe[ job_id ] = mean((response_vec[test_id] - glmnet_preds)^2)
  
  rf_pe[ job_id ] = mean((response_vec[test_id] - rf_preds)^2)
  
  cart_pe[ job_id ] = mean((response_vec[ test_id ] - cart_preds)^2)
  
  #dmr_pe[ job_id ] = mean((response_vec[ test_id ] - dmr_preds^2))
  
  save(job_id, response_list, response_list_noise, vars_list, cts_coefs, cat_coefs, scopes_pe, scopel_pe, scopecv_pe, glmnet_pe, rf_pe, cart_pe,  scopes_mods, scopel_mods, scopecv_mods,  glmnet_mods, rf_mods, cart_mods, gamma_cv, file=paste0("prudential_large_performance_experiments_", comp_name, ".Rdata"))
  
}
