# Now for fitting with the death data using the log-ratio
# cases -- now using location-week interaction as a variable (so now smoothing term for the week coefficient)

x = case_df[!is.na(case_df$value_7rollsum), ]
x$case_log_ratio = log(1 + x$value_7rollsum) - log(1 + x$value) # recall that value is the true death count, and value_7_rollsum is the weekly cumulative
# forecasts from the model

#CHANGEHERE
#y = x$death_error
y = x$case_log_ratio

x$model.target = paste0(x$model, " ", x$target)
x$model.target = as.factor(x$model.target)


min_date = min(x$target_end_date)
weeks_since_start = as.numeric(x$target_end_date - min_date) / 7
weeks_since_start = paste0(weeks_since_start, " weeks")
for (i in 1:length(weeks_since_start)) {
  if (nchar(weeks_since_start[ i ]) < 8) weeks_since_start[ i ] = paste0("0", weeks_since_start[i])
}
x$location.time = paste0(x$location_name, " ", weeks_since_start)

x$location.time = as.factor(x$location.time)

x_location.time = x$location.time

# Have done this because for some reason, glmnet::makeX has memory complaints and won't finish
x = sparse.model.matrix(~., data = data.frame(x$model.target))
unpenalise_vars = dim(x)[ 2 ]

# fitted very small ridge penalty just for computational convenience
ridge_lambda = exp(seq(log(100000), log(0.1), length = 100))


fit_ridge = function( x, y, lambda, penalty.factor ) {
  ridge_mod <<- glmnet(x, y, alpha = 0, penalty.factor = penalty.factor, lambda = lambda, maxit = 1e8, thresh = 1e-10, intercept = TRUE)
  return(predict(ridge_mod, x, s = min(lambda)))
}


weights = table(x_location.time)
weights = weights / sum(weights) 

#CHANGEHERE
#lambda = exp(seq(log(5000), log(20), length = 25)) # scaled based on fitting location-only model starting on 1000
#Now for log ratio
lambda = exp(seq(log(0.4), log(0.0008), length = 25)) # changed this now

term_eps = 1e-5
shrink_coefs = matrix(0, length(weights), length(lambda))
AIC = rep(0, 25)
BIC = rep(0, 25)
loglike = rep(0, 25)

rownames(shrink_coefs) = names(weights)
ridge_mods = list()
mu = mean(y)
y = y - mu
partial_residuals = y
preds2 = 0
for (l in 1:length(lambda)) {
  print(paste0("Lambda value ", l))
  old_partial_residuals = 0
  partial_residuals = y
  iter = 0
  preds1 = 0
  preds2 = 0
  while (mean((old_partial_residuals - partial_residuals)^2) >= term_eps) {
    print(iter)
    
    old_partial_residuals = partial_residuals
    iter = iter + 1
    print("A")
    print(mean(partial_residuals))
    partial_residuals = partial_residuals + preds1
    preds1 = fit_ridge(x, partial_residuals, ridge_lambda, rep(1, dim(x)[ 2 ])) # don't penalise anything
    partial_residuals = partial_residuals - preds1
    print("B")
    print(mean(partial_residuals))
    partial_residuals = partial_residuals + preds2
    print(sum(weights * tapply(partial_residuals, x_location.time, mean)))
    temp_coefs = CatReg:::DoBlock(weights, tapply(partial_residuals, x_location.time, mean), 8, lambda[l])
    if (mean(temp_coefs^2) <= 1e-7) {
      temp_coefs = rep(0, length(temp_coefs))
    }
    names(temp_coefs) = names(weights)
    print(sum(weights * temp_coefs))
    preds2 = temp_coefs[ x_location.time ]
    partial_residuals = partial_residuals - preds2
    print("C")
    print(mean(partial_residuals))
    print(mean((old_partial_residuals - partial_residuals)^2))
    
  }
  ridge_mods[[ l ]] = list(ridge_mod$a0[length(ridge_lambda)], ridge_mod$beta[,length(ridge_lambda)])
  shrink_coefs[ , l ] = temp_coefs
  
  print(mean(y^2))
  print(mean((preds1 + preds2)^2))
  print(mean((y - preds1 - preds2)^2))
  print(paste0("Number of location coefficients = ", length(unique(temp_coefs))))
  
  n = length(y)
  AIC[ l ] = n * (log(2 * pi) + log(sum((y - preds1 - preds2)^2)) - log(n)) + 2 * (n + 1 + dim(x)[ 2 ] + length(unique(temp_coefs)) + 1)
  BIC[ l ] = n * (log(2 * pi) + log(sum((y - preds1 - preds2)^2)) - log(n) + 1) + (log(n) * (1 + dim(x)[ 2 ] + length(unique(temp_coefs))))
  loglike[ l ] = -0.5 * n * (log(2 * pi) + log(sum((y - preds1 - preds2)^2)) - log(n) + 1)
  print(paste0("AIC: ", AIC[ l ]))
  print(paste0("BIC: ", BIC[ l ]))
}


# now do the plots etc
gam = 0.5
LogStirling2Approx = function(n, k) {
  ans = n * log(k)
  for (i in 1:k) {
    ans = ans - log(i)
  }
  return(ans)
}
gamBIC = rep(0, 25)
dimlist = unlist(lapply(1:25, function(x) length(unique(shrink_coefs[ , x ]))))
n = length(y)
for (l in 1:25) {
  gamBIC[ l ] = - 2 * loglike[ l ] + (log(n) * (1 + dim(x)[ 2 ] + dimlist[ l ])) + 2 * gam * LogStirling2Approx(1428, dimlist[ l ])
}
# in this case, min of gamBIC is at path point 14

shrink_mat = matrix(shrink_coefs[ , 14], 51, 28, byrow = TRUE)

# first wave
sim_mat = matrix(0, 51, 51)
rownames(sim_mat) = state_nam
colnames(sim_mat) = state_nam
for (i in 1:51) {
  for (j in 1:51) {
    sim_mat[i,j] = mean(shrink_mat[i,1:9]==shrink_mat[j,1:9])
  }
}
first_wave_cluster = list()
laplacian = diag(51) - sim_mat
laplacian = laplacian - diag(rowSums(laplacian))
first_wave_cluster$eigen = eigen(laplacian)
first_wave_cluster$z = first_wave_cluster$eigen$vectors[,48:50]
first_wave_cluster$k = 6
first_wave_cluster$kmeans = kmeans(first_wave_cluster$z, centers = 6, iter.max = 1e3, nstart = 1e5)

sim_mat = matrix(0, 51, 51)
rownames(sim_mat) = state_nam
colnames(sim_mat) = state_nam

for (i in 1:51) {
  for (j in 1:51) {
    sim_mat[i,j] = mean(shrink_mat[i,1:9]==shrink_mat[j,1:9])
  }
}
clusterOrder = c()
# compute median pairwise similarity score
median_pairwise_similarity = rep(0, first_wave_cluster$k)
mean_pairwise_similarity = rep(0, first_wave_cluster$k)
for (k in 1:first_wave_cluster$k) {
  if (first_wave_cluster$kmeans$size[ k ] > 1) {
    median_pairwise_similarity[ k ] = 1 - median(sim_mat[upper.tri(sim_mat[which(first_wave_cluster$kmeans$cluster == k), which(first_wave_cluster$kmeans$cluster == k)], diag = FALSE)])
    mean_pairwise_similarity[ k ] = 1 - mean(sim_mat[upper.tri(sim_mat[which(first_wave_cluster$kmeans$cluster == k), which(first_wave_cluster$kmeans$cluster == k)], diag = FALSE)])
  } else {
    median_pairwise_similarity[ k ] = 1
    mean_pairwise_similarity[ k ] = 1
  }
}
average_withinss = first_wave_cluster$kmeans$withinss / first_wave_cluster$kmeans$size
for (j in 1:first_wave_cluster$k) clusterOrder = c(clusterOrder, which(first_wave_cluster$kmeans$cluster == order(mean_pairwise_similarity)[ j ]))
corrplot(sim_mat[clusterOrder, clusterOrder], tl.col = "black", tl.cex = 0.7, cl.lim = c(0,1), method = "shade")
corrRect(table(first_wave_cluster$kmeans$cluster)[ order(mean_pairwise_similarity) ])




# second wave

sim_mat = matrix(0, 51, 51)
rownames(sim_mat) = state_nam
colnames(sim_mat) = state_nam
for (i in 1:51) {
  for (j in 1:51) {
    sim_mat[i,j] = mean(shrink_mat[i,12:21]==shrink_mat[j,12:21])
  }
}
second_wave_cluster = list()
laplacian = diag(51) - sim_mat
laplacian = laplacian - diag(rowSums(laplacian))
second_wave_cluster$eigen = eigen(laplacian)
second_wave_cluster$z = second_wave_cluster$eigen$vectors[,48:50]
second_wave_cluster$k = 8
second_wave_cluster$kmeans = kmeans(second_wave_cluster$z, centers = 8, iter.max = 1e3, nstart = 1e5)

sim_mat = matrix(0, 51, 51)
rownames(sim_mat) = state_nam
colnames(sim_mat) = state_nam

for (i in 1:51) {
  for (j in 1:51) {
    sim_mat[i,j] = mean(shrink_mat[i,12:21]==shrink_mat[j,12:21])
  }
}
clusterOrder = c()
# compute median pairwise similarity score
median_pairwise_similarity = rep(0, second_wave_cluster$k)
mean_pairwise_similarity = rep(0, second_wave_cluster$k)
for (k in 1:second_wave_cluster$k) {
  if (second_wave_cluster$kmeans$size[ k ] > 1) {
    median_pairwise_similarity[ k ] = 1 - median(sim_mat[upper.tri(sim_mat[which(second_wave_cluster$kmeans$cluster == k), which(second_wave_cluster$kmeans$cluster == k)], diag = FALSE)])
    mean_pairwise_similarity[ k ] = 1 - mean(sim_mat[upper.tri(sim_mat[which(second_wave_cluster$kmeans$cluster == k), which(second_wave_cluster$kmeans$cluster == k)], diag = FALSE)])
  } else {
    median_pairwise_similarity[ k ] = 1
    mean_pairwise_similarity[ k ] = 1
  }
}
average_withinss = second_wave_cluster$kmeans$withinss / second_wave_cluster$kmeans$size
for (j in 1:second_wave_cluster$k) clusterOrder = c(clusterOrder, which(second_wave_cluster$kmeans$cluster == order(mean_pairwise_similarity)[ j ]))
corrplot(sim_mat[clusterOrder, clusterOrder], tl.col = "black", tl.cex = 0.7, cl.lim = c(0,1), method = "shade")
corrRect(table(second_wave_cluster$kmeans$cluster)[ order(mean_pairwise_similarity) ])



