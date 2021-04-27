## Useful plotting code

# Old color vector
colvec = c(rgb(0.5,0,0, 0.3), rgb(0,0,0.5, 0.3), rgb(0,0.5,0.5, 0.3), rgb(0.5,0,0.7, 0.3), rgb(0.5,0.5,0, 0.3), rgb(0.7,0.4,0.1, 0.3), rgb(0.8,0.4,0.7, 0.3), rgb(0.8, 0.7, 0.2, 0.3), rgb(0.3, 0, 0.3, 0.3), rgb(0,0.5,0, 0.3),  rgb(0.35, 0.2, 0.15, 0.3))

# New color vector
colvec = c(rgb(0, 0.5, 0, 0.5), rgb(0.45, 0.85, 0.65, 0.5), rgb(0.7, 0.8, 0.4, 0.5), rgb(0, 0, 1, 0.5), rgb(0, 0.6, 0.8, 0.5), rgb(0.8, 0.2, 0, 0.5), rgb(0.85, 0.6, 0, 0.5), rgb(0.85, 0, 0.8, 0.5), rgb(0.75, 0.3, 0.4, 0.5), rgb(0.4, 0, 0.85, 0.5), rgb(0.6, 0.5, 0.7, 0.5), rgb(0.7, 0.7, 0.7, 0.5))

# Boxplots for gaussian simulation
errmat11 = cbind(ols_prederr1[1:150,1], lm_prederr1[1:150,1], cas_prederr1[1:150,1], acc_prederr1[1:150,1], dmr_prederr1[1:150,1], bayes_prederr1[1:150,1], scopes_prederr1[1:150,1], scopel_prederr1[1:150,1], scopecv_prederr1[1:150,1], cart_prederr1[1:150,1], rf_prederr1[1:150,1])
errmat12 = cbind(ols_prederr1[1:150,2], lm_prederr1[1:150,2], cas_prederr1[1:150,2], acc_prederr1[1:150,2], dmr_prederr1[1:150,2], bayes_prederr1[1:150,2], scopes_prederr1[1:150,2], scopel_prederr1[1:150,2], scopecv_prederr1[1:150,2], cart_prederr1[1:150,2], rf_prederr1[1:150,2])
errmat13 = cbind(ols_prederr1[1:150,3], lm_prederr1[1:150,3], cas_prederr1[1:150,3], acc_prederr1[1:150,3], dmr_prederr1[1:150,3], bayes_prederr1[1:150,3], scopes_prederr1[1:150,3], scopel_prederr1[1:150,3], scopecv_prederr1[1:150,3], cart_prederr1[1:150,3], rf_prederr1[1:150,3])
errmat14 = cbind(ols_prederr1[1:150,4], lm_prederr1[1:150,4], cas_prederr1[1:150,4], acc_prederr1[1:150,4], dmr_prederr1[1:150,4], bayes_prederr1[1:150,4], scopes_prederr1[1:150,4], scopel_prederr1[1:150,4], scopecv_prederr1[1:150,4], cart_prederr1[1:150,4], rf_prederr1[1:150,4])
errmat21 = cbind(ols_prederr1[1:150,5], lm_prederr1[1:150,5], cas_prederr1[1:150,5], acc_prederr1[1:150,5], dmr_prederr1[1:150,5], bayes_prederr1[1:150,5], scopes_prederr1[1:150,5], scopel_prederr1[1:150,5], scopecv_prederr1[1:150,5], cart_prederr1[1:150,5], rf_prederr1[1:150,5])
errmat22 = cbind(ols_prederr1[1:150,6], lm_prederr1[1:150,6], cas_prederr1[1:150,6], acc_prederr1[1:150,6], dmr_prederr1[1:150,6], bayes_prederr1[1:150,6], scopes_prederr1[1:150,6], scopel_prederr1[1:150,6], scopecv_prederr1[1:150,6], cart_prederr1[1:150,6], rf_prederr1[1:150,6])
errmat23 = cbind(ols_prederr1[1:150,7], lm_prederr1[1:150,7], cas_prederr1[1:150,7], acc_prederr1[1:150,7], dmr_prederr1[1:150,7], bayes_prederr1[1:150,7], scopes_prederr1[1:150,7], scopel_prederr1[1:150,7], scopecv_prederr1[1:150,7], cart_prederr1[1:150,7], rf_prederr1[1:150,7])
errmat24 = cbind(ols_prederr1[1:150,8], lm_prederr1[1:150,8], cas_prederr1[1:150,8], acc_prederr1[1:150,8], dmr_prederr1[1:150,8], bayes_prederr1[1:150,8], scopes_prederr1[1:150,8], scopel_prederr1[1:150,8], scopecv_prederr1[1:150,8], cart_prederr1[1:150,8], rf_prederr1[1:150,8])
errmat31 = cbind(ols_prederr1[1:150,9], lm_prederr1[1:150,9], cas_prederr1[1:150,9], acc_prederr1[1:150,9], dmr_prederr1[1:150,9], bayes_prederr1[1:150,9], scopes_prederr1[1:150,9], scopel_prederr1[1:150,9], scopecv_prederr1[1:150,9], cart_prederr1[1:150,9], rf_prederr1[1:150,9])
errmat32 = cbind(ols_prederr1[1:150,10], lm_prederr1[1:150,10], cas_prederr1[1:150,10], acc_prederr1[1:150,10], dmr_prederr1[1:150,10], bayes_prederr1[1:150,10], scopes_prederr1[1:150,10], scopel_prederr1[1:150,10], scopecv_prederr1[1:150,10], cart_prederr1[1:150,10], rf_prederr1[1:150,10])
errmat33 = cbind(ols_prederr1[1:150,11], lm_prederr1[1:150,11], cas_prederr1[1:150,11], acc_prederr1[1:150,11], dmr_prederr1[1:150,11], bayes_prederr1[1:150,11], scopes_prederr1[1:150,11], scopel_prederr1[1:150,11], scopecv_prederr1[1:150,11], cart_prederr1[1:150,11], rf_prederr1[1:150,11])
errmat34 = cbind(ols_prederr1[1:150,12], lm_prederr1[1:150,12], cas_prederr1[1:150,12], acc_prederr1[1:150,12], dmr_prederr1[1:150,12], bayes_prederr1[1:150,12], scopes_prederr1[1:150,12], scopel_prederr1[1:150,12], scopecv_prederr1[1:150,12], cart_prederr1[1:150,12], rf_prederr1[1:150,12])
# High noise data

# Now permute the columns:
errmat11 = errmat11[ , c(7, 8, 9, 2, 1, 3, 4, 5, 6, 10, 11) ]
errmat12 = errmat12[ , c(7, 8, 9, 2, 1, 3, 4, 5, 6, 10, 11) ]
errmat13 = errmat13[ , c(7, 8, 9, 2, 1, 3, 4, 5, 6, 10, 11) ]
errmat14 = errmat14[ , c(7, 8, 9, 2, 1, 3, 4, 5, 6, 10, 11) ]
errmat21 = errmat21[ , c(7, 8, 9, 2, 1, 3, 4, 5, 6, 10, 11) ]
errmat22 = errmat22[ , c(7, 8, 9, 2, 1, 3, 4, 5, 6, 10, 11) ]
errmat23 = errmat23[ , c(7, 8, 9, 2, 1, 3, 4, 5, 6, 10, 11) ]
errmat24 = errmat24[ , c(7, 8, 9, 2, 1, 3, 4, 5, 6, 10, 11) ]
errmat31 = errmat31[ , c(7, 8, 9, 2, 1, 3, 4, 5, 6, 10, 11) ]
errmat32 = errmat32[ , c(7, 8, 9, 2, 1, 3, 4, 5, 6, 10, 11) ]
errmat33 = errmat33[ , c(7, 8, 9, 2, 1, 3, 4, 5, 6, 10, 11) ]
errmat34 = errmat34[ , c(7, 8, 9, 2, 1, 3, 4, 5, 6, 10, 11) ]



nam.txt=rep("",11)
for (j in 1:11) nam.txt[j] = paste0("(", LETTERS[j], ")")

par(mar = c(2,4,3,1))
par(mfrow = c(4,3))
boxplot(errmat11,names=nam.txt,ylab="MSPE",main=expression("Setting 1, "*sigma^2*" = 1"), ylim=c(0,0.1), col = colvec[ 1:11 ])
grid(nx = NA, ny = NULL, col="lightgray", lty="dotted")
boxplot(errmat21,names=nam.txt,ylab="MSPE",main=expression("Setting 2, "*sigma^2*" = 1"), ylim=c(0,0.1), col = colvec[ 1:11 ])
grid(nx = NA, ny = NULL, col="lightgray", lty="dotted")
boxplot(errmat31,names=nam.txt,ylab="MSPE",main=expression("Setting 3, "*sigma^2*" = 1"), ylim=c(0,0.1), col = colvec[ 1:11 ])
grid(nx = NA, ny = NULL, col="lightgray", lty="dotted")
boxplot(errmat12,names=nam.txt,ylab="MSPE",main=expression("Setting 1, "*sigma^2*" = 6.25"), ylim=c(0,3), col = colvec[ 1:11 ])
grid(nx = NA, ny = NULL, col="lightgray", lty="dotted")
boxplot(errmat22,names=nam.txt,ylab="MSPE",main=expression("Setting 2, "*sigma^2*" = 6.25"), ylim=c(0,3), col = colvec[ 1:11 ])
grid(nx = NA, ny = NULL, col="lightgray", lty="dotted")
boxplot(errmat32,names=nam.txt,ylab="MSPE",main=expression("Setting 3, "*sigma^2*" = 6.25"), ylim=c(0,3), col = colvec[ 1:11 ])
grid(nx = NA, ny = NULL, col="lightgray", lty="dotted")
boxplot(errmat13,names=nam.txt,ylab="MSPE",main=expression("Setting 1, "*sigma^2*" = 25"), ylim=c(0,12), col = colvec[ 1:11 ])
grid(nx = NA, ny = NULL, col="lightgray", lty="dotted")
boxplot(errmat23,names=nam.txt,ylab="MSPE",main=expression("Setting 2, "*sigma^2*" = 25"), ylim=c(0,12), col = colvec[ 1:11 ])
grid(nx = NA, ny = NULL, col="lightgray", lty="dotted")
boxplot(errmat33,names=nam.txt,ylab="MSPE",main=expression("Setting 3, "*sigma^2*" = 25"), ylim=c(0,12), col = colvec[ 1:11 ])
grid(nx = NA, ny = NULL, col="lightgray", lty="dotted")

# High noise
boxplot(errmat14,names=nam.txt,ylab="MSPE",main=expression("Setting 1, "*sigma^2*" = 100"), ylim=c(0,24), col = colvec[1:11])
grid(nx = NA, ny = NULL, col="lightgray", lty="dotted")
boxplot(errmat24,names=nam.txt,ylab="MSPE",main=expression("Setting 2, "*sigma^2*" = 100"), ylim=c(0,24), col = colvec[1:11])
grid(nx = NA, ny = NULL, col="lightgray", lty="dotted")
boxplot(errmat34,names=nam.txt,ylab="MSPE",main=expression("Setting 3, "*sigma^2*" = 100"), ylim=c(0,24), col = colvec[1:11])
grid(nx = NA, ny = NULL, col="lightgray", lty="dotted")

# Tables for prediction error gaussian simulation
printdp = function(inputmat, dp = 3, n = 500) {
  roundnos = round(colMeans(inputmat[1:n,]), dp)
  print(paste0(roundnos[1]," & ", roundnos[2], " & " , roundnos[3] , " & ", roundnos[ 4 ], " & ", roundnos[5]," & ", roundnos[6], " & " , roundnos[7] , " & ", roundnos[ 8])) #, " & & ", roundnos[ 9], " & ", roundnos[ 10], " & ", roundnos[11 ]," & ", roundnos[ 12]))
}


# Categorical variable multiplier
plot(1:4,colMeans(mb_dim), type='l', col=rgb(0,0,0.5),ylim=c(0,400), lwd = 3, xlab = "Categorical variable multiplier", ylab = "Model dimension", main="Model dimension")
grid(ny = NULL, nx = NA, col = "lightgray")
for (i in 1:17) abline(h=25*(i-1), col="lightgray", lty="dotted")
arrows(1:4,mbiqr2[1,],1:4,mbiqr2[2,], length = 0.05, angle = 90, code = 3)
lines(1:4, colMeans(glm_dim), col=rgb(0.5,0,0), lwd = 3)
# arrows(1:4,glmiqr[1,],1:4,glmiqr[2,], length = 0.05, angle = 90, code = 3)
legend(1,400,legend=c("SCOPE", "Logistic regression"), col=c(rgb(0,0,0.5),rgb(0.5,0,0)), lwd = 3)

plot(1:4,colMeans(mb_errors), type='l', col=rgb(0,0,0.5),ylim=c(0.15,0.28), lwd = 3, xlab = "Categorical variable multiplier", ylab = "Misclassification error", main="Misclassification error")
grid(ny = NULL, nx = NA, col = "lightgray")
# for (i in 1:17) abline(h=25*(i-1), col="lightgray", lty="dotted")
arrows(1:4,mbiqr[1,],1:4,mbiqr[2,], length = 0.05, angle = 90, code = 3)
lines(1:4, colMeans(glm_errors), col=rgb(0.5,0,0), lwd = 3)
arrows(1:4,glmiqr[1,],1:4,glmiqr[2,], length = 0.05, angle = 90, code = 3)
legend(1,0.28,legend=c("SCOPE", "Logistic regression"), col=c(rgb(0,0,0.5),rgb(0.5,0,0)), lwd = 3)

# Computation time graphs
colvec = c(rgb(0.8,0,0), rgb(0,0.4,0), rgb(0,0,0.8), rgb(0.8, 0.6, 0.1), rgb(0.8, 0.1, 0.5), rgb(0.3, 0.75, 0.75))
par(mar = c(5,5,4,2))
catseq = 14 * c(1, seq(5, 65, 5))
time_mat_mean = list()
for (i in 1:3) {
    time_mat_mean[[i]] = list()
    for (j in 1:3) {
        time_mat_mean[[i]][[j]] = list()
        for (k in 1:2) time_mat_mean[[i]][[j]][[k]] = colMeans(time_matrix[[i]][[j]][[k]])
    }
}

for (i in 1:3) {
  for (j in 1:2) {
    if ( ( i == 1 ) && ( j == 1 ) ) {
        plot(catseq, (time_mat_mean[[1]][[i]][[j]]), type='l', lwd = 2, col = colvec[2*(i-1)+j], xlab="Number of categories", ylab="Computation time (s)", main="Setting 1", ylim=c(0,6), cex.main = 2, cex.axis = 2, cex.lab = 2)
      grid(ny = NULL, nx = NA, col = "lightgray")
    } else {
      lines(catseq, (time_mat_mean[[1]][[i]][[j]]), lwd = 2, col = colvec[2*(i-1)+j])
    }

  }
}


legend(0,6,legend=c(expression("("*gamma*", "*lambda*") = (1, 0.1)"),expression("("*gamma*", "*lambda*") = (1, 0.3)"),expression("("*gamma*", "*lambda*") = (8, 0.1)"),expression("("*gamma*", "*lambda*") = (8, 0.3)"),expression("("*gamma*", "*lambda*") = (64, 0.1)"),expression("("*gamma*", "*lambda*") = (64, 0.3)")), col=colvec, lwd=2, cex = 2)

for (i in 1:3) {
  for (j in 1:2) {
    if ( ( i == 1 ) && ( j == 1 ) ) {
        plot(catseq, (time_mat_mean[[2]][[i]][[j]]), type='l', lwd = 2, col = colvec[2*(i-1)+j], xlab="Number of categories", ylab="Computation time (s)", main="Setting 2", ylim=c(0,6), cex.main = 2, cex.axis = 2, cex.lab = 2)
      grid(ny = NULL, nx = NA, col = "lightgray")
    } else {
      lines(catseq, (time_mat_mean[[2]][[i]][[j]]), lwd = 2, col = colvec[2*(i-1)+j])
    }

  }
}


legend(0,6,legend=c(expression("("*gamma*", "*lambda*") = (1, 0.1)"),expression("("*gamma*", "*lambda*") = (1, 0.3)"),expression("("*gamma*", "*lambda*") = (8, 0.1)"),expression("("*gamma*", "*lambda*") = (8, 0.3)"),expression("("*gamma*", "*lambda*") = (64, 0.1)"),expression("("*gamma*", "*lambda*") = (64, 0.3)")), col=colvec, lwd=2, cex = 2)

for (i in 1:3) {
  for (j in 1:2) {
    if ( ( i == 1 ) && ( j == 1 ) ) {
        plot(catseq, (time_mat_mean[[3]][[i]][[j]]), type='l', lwd = 2, col = colvec[2*(i-1)+j], xlab="Number of categories", ylab="Computation time (s)", main="Setting 3", ylim=c(0,6), cex.main = 2, cex.axis = 2, cex.lab = 2)
      grid(ny = NULL, nx = NA, col = "lightgray")
    } else {
      lines(catseq, (time_mat_mean[[3]][[i]][[j]]), lwd = 2, col = colvec[2*(i-1)+j])
    }

  }
}


legend(0,6,legend=c(expression("("*gamma*", "*lambda*") = (1, 0.1)"),expression("("*gamma*", "*lambda*") = (1, 0.3)"),expression("("*gamma*", "*lambda*") = (8, 0.1)"),expression("("*gamma*", "*lambda*") = (8, 0.3)"),expression("("*gamma*", "*lambda*") = (64, 0.1)"),expression("("*gamma*", "*lambda*") = (64, 0.3)")), col=colvec, lwd=2, cex = 2)


# Small diagrams at start
plot(c(rep(mean(smallex_cas[,1]),20),smallex_cas[1,]), 1:500, type='l', xlim=c(-2,2), xlab="", ylab="", yaxt="n", main="CAS-ANOVA", lwd=5, col=rgb(1,0,0,0.6), cex.axis = 2, cex.main = 2) # previously xlim=c(2,6)
lines(c(rep(mean(smallex_cas[,1]),20),smallex_cas[2,]), 1:500, lwd=5, col=rgb(1,0,0,0.6))
lines(c(rep(mean(smallex_cas[,1]),20),smallex_cas[3,]), 1:500, lwd=5, col=rgb(1,0,0,0.6))
lines(c(rep(mean(smallex_cas[,1]),20),smallex_cas[4,]), 1:500, lwd=5, col=rgb(1,0,0,0.6))
lines(c(rep(mean(smallex_cas[,1]),20),smallex_cas[5,]), 1:500, lwd=5, col=rgb(0,0.5,0,0.6))
lines(c(rep(mean(smallex_cas[,1]),20),smallex_cas[6,]), 1:500, lwd=5, col=rgb(0,0.5,0,0.6))
lines(c(rep(mean(smallex_cas[,1]),20),smallex_cas[7,]), 1:500, lwd=5, col=rgb(0,0,1,0.6))
lines(c(rep(mean(smallex_cas[,1]),20),smallex_cas[8,]), 1:500, lwd=5, col=rgb(0,0,1,0.6))
lines(c(rep(mean(smallex_cas[,1]),20),smallex_cas[9,]), 1:500, lwd=5, col=rgb(0,0,1,0.6))
lines(c(rep(mean(smallex_cas[,1]),20),smallex_cas[10,]), 1:500, lwd=5, col=rgb(0,0,1,0.6))
plot(smallex_range[1,], rev(scopeseq), type='l', xlim=c(-2,2), xlab="", ylab="", yaxt="n", main="Range penalty", lwd=5, col=rgb(1,0,0,0.6),  cex.axis = 2, cex.main = 2)
lines(smallex_range[2,], rev(scopeseq), lwd=5, col=rgb(1,0,0,0.6))
lines(smallex_range[3,], rev(scopeseq), lwd=5, col=rgb(1,0,0,0.6))
lines(smallex_range[4,], rev(scopeseq), lwd=5, col=rgb(1,0,0,0.6))
lines(smallex_range[5,], rev(scopeseq), lwd=5, col=rgb(0,0.5,0,0.6))
lines(smallex_range[6,], rev(scopeseq), lwd=5, col=rgb(0,0.5,0,0.6))
lines(smallex_range[7,], rev(scopeseq), lwd=5, col=rgb(0,0,1,0.6))
lines(smallex_range[8,], rev(scopeseq), lwd=5, col=rgb(0,0,1,0.6))
lines(smallex_range[9,], rev(scopeseq), lwd=5, col=rgb(0,0,1,0.6))
lines(smallex_range[10,], rev(scopeseq), lwd=5, col=rgb(0,0,1,0.6))
plot(smallex_kmeans[1,], rev(scopeseq), type='l', xlim=c(-2,2), xlab="", ylab="", yaxt="n", main="k-means", lwd=5, col=rgb(1,0,0,0.6),  cex.axis = 1.5, cex.main = 1.5)
lines(smallex_kmeans[2,], rev(scopeseq), lwd=5, col=rgb(1,0,0,0.6))
lines(smallex_kmeans[3,], rev(scopeseq), lwd=5, col=rgb(1,0,0,0.6))
lines(smallex_kmeans[4,], rev(scopeseq), lwd=5, col=rgb(1,0,0,0.6))
lines(smallex_kmeans[5,], rev(scopeseq), lwd=5, col=rgb(0,0.5,0,0.6))
lines(smallex_kmeans[6,], rev(scopeseq), lwd=5, col=rgb(0,0.5,0,0.6))
lines(smallex_kmeans[7,], rev(scopeseq), lwd=5, col=rgb(0,0,1,0.6))
lines(smallex_kmeans[8,], rev(scopeseq), lwd=5, col=rgb(0,0,1,0.6))
lines(smallex_kmeans[9,], rev(scopeseq), lwd=5, col=rgb(0,0,1,0.6))
lines(smallex_kmeans[10,], rev(scopeseq), lwd=5, col=rgb(0,0,1,0.6))
plot(smallex_scope[1,], rev(scopeseq), type='l', xlim=c(-2,2), xlab="", ylab="", yaxt="n", main="SCOPE", lwd=5, col=rgb(1,0,0,0.6),  cex.axis = 2, cex.main = 2)
lines(smallex_scope[2,], rev(scopeseq), lwd=5, col=rgb(1,0,0,0.6))
lines(smallex_scope[3,], rev(scopeseq), lwd=5, col=rgb(1,0,0,0.6))
lines(smallex_scope[4,], rev(scopeseq), lwd=5, col=rgb(1,0,0,0.6))
lines(smallex_scope[5,], rev(scopeseq), lwd=5, col=rgb(0,0.5,0,0.6))
lines(smallex_scope[6,], rev(scopeseq), lwd=5, col=rgb(0,0.5,0,0.6))
lines(smallex_scope[7,], rev(scopeseq), lwd=5, col=rgb(0,0,1,0.6))
lines(smallex_scope[8,], rev(scopeseq), lwd=5, col=rgb(0,0,1,0.6))
lines(smallex_scope[9,], rev(scopeseq), lwd=5, col=rgb(0,0,1,0.6))
lines(smallex_scope[10,], rev(scopeseq), lwd=5, col=rgb(0,0,1,0.6))


# REVISED DIAGRAMS
function(x) {
    n = length(x)
    return(seq(x[1], x[n], length=n))
}

colvec = c(rep(rgb(1,0,0,0.5), 4), rep(rgb(0,0.5,0,0.5), 6), rep(rgb(0,0,1,0.5), 6), rep(rgb(1,0.5,0,0.5), 4))

matplot(t(cbind(0,casanova_coefs2)), c(-20,casanova_lambda2), type='l', lty=1, col=colvec, ylim=c(68,-20), xlab="", ylab="", yaxt="n", main="CAS-ANOVA", lwd=2.5, cex.axis = 2, cex.main = 2)
matplot(t(range_coefs), range_lambda, type='l', lty=1, col=colvec, xlab="", ylab="", yaxt="n", main="Range penalty", lwd=2.5, cex.axis = 2, cex.main = 2)
matplot(t(scope_coefs), scope_lambda, type='l', lty=1, col=colvec, xlab="", ylab="", yaxt="n", main="SCOPE", lwd=2.5, cex.axis = 2, cex.main = 2)



# Boxplots for adult dataset
errmat = cbind(glm_err[ 1:250 ], cas_err[ 1:250 ], acc_err[ 1:250 ], dmr_err[ 1:250 ], bayes_err[ 1:250 ], mb_err[ 1:250 ], mbl_err[ 1:250 ], cart_err[ 1:250 ], rf_err[ 1:250 ])
# new one
errmat = cbind(mb_err[ 1:250 ], mbl_err[ 1:250 ], glm_err[ 1:250 ], cas_err[ 1:250 ], acc_err[ 1:250 ], dmr_err[ 1:250 ], bayes_err[ 1:250 ], cart_err[ 1:250 ], rf_err[ 1:250 ])
#errmat = errmat[1:250]
boxplot(errmat, names = nam.txt[1:9], ylim=c(0.165, 0.26), ylab="Misclassification error", main = "Test error", col = colvec[c(1,2,5,6,7,8,9,10,11)])
grid(nx = NA, ny = NULL, col="lightgray", lty="dotted")

dimmat = cbind(mb_dim[ 1:250 ], mbl_dim[ 1:250 ], glm_dim[ 1:250 ], cas_dim[ 1:250 ], acc_dim[ 1:250 ], dmr_dim[ 1:250 ], bayes_dim[ 1:250 ])
#dimmat = dimmat[1:250]
boxplot(dimmat, names = nam.txt[1:7], ylim=c(0,80), ylab="Dimension", main = "Model dimension", col = colvec[c(1,2,5,6,7,8,9)], yaxt = "n")
ticks = seq(0, 80, 10)
axis(2, at = ticks, labels = ticks)
for (i in ticks) abline(h=i, lty="dotted", col="lightgray")


par(mar = c(2,4,3,1))
par(mfrow = c(4,2))
# Boxplots for high-dimensional tests
errmats = list()
for (i in 1:8) {
  errmats[[ i ]] = cbind(dmr_pe[1:500, i], scopes_pe[1:500, i], scopel_pe[1:500, i], scopecv_pe[1:500, i], cart_pe[1:500, i], rf_pe[1:500, i])
  # if ( i %in% c(1, 2, 4, 5, 6) ) errmats[[ i ]] = cbind(errmats[[ i ]], glmnet_pe[1:500, i]) OLD
  errmats[[ i ]] = cbind(errmats[[ i ]], glmnet_pe[1:500, i])
}

for (i in 1:8) {
  errmats[[ i ]] = cbind(scopes_pe[1:500, i], scopel_pe[1:500, i], scopecv_pe[1:500, i], dmr_pe[1:500, i], cart_pe[1:500, i], rf_pe[1:500, i])
  # if ( i %in% c(1, 2, 4, 5, 6) ) errmats[[ i ]] = cbind(errmats[[ i ]], glmnet_pe[1:500, i]) OLD
  errmats[[ i ]] = cbind(errmats[[ i ]], glmnet_pe[1:500, i])
}

#boxplot(errmats[[ 1 ]],names=nam.txt[ 1:7 ],ylab="MSPE",main="Setting 1", ylim=c(0,25), col = colvec[ c(1, 2, 3, 8, 10, 11, 12) ])
#grid(nx = NA, ny = NULL, col="lightgray", lty="dotted")
#boxplot(errmats[[ 2 ]],names=nam.txt[ 1:7 ],ylab="MSPE",main="Setting 2", ylim=c(0,25), col = colvec[ c(1, 2, 3, 8, 10, 11, 12) ])
#grid(nx = NA, ny = NULL, col="lightgray", lty="dotted")
#boxplot(errmats[[ 3 ]],names=nam.txt[ 1:6 ],ylab="MSPE",main="Setting 3", ylim=c(0,50), col = colvec[ c(1, 2, 3, 8, 10, 11) ])
#grid(nx = NA, ny = NULL, col="lightgray", lty="dotted")
#boxplot(errmats[[ 4 ]],names=nam.txt[ 1:7 ],ylab="MSPE",main="Setting 4", ylim=c(0,15), col = colvec[ c(1, 2, 3, 8, 10, 11, 12) ])
#grid(nx = NA, ny = NULL, col="lightgray", lty="dotted")
#boxplot(errmats[[ 5 ]],names=nam.txt[ 1:7 ],ylab="MSPE",main="Setting 5", ylim=c(0,150), col = colvec[ c(1, 2, 3, 8, 10, 11, 12) ])
#grid(nx = NA, ny = NULL, col="lightgray", lty="dotted")
#boxplot(errmats[[ 6 ]],names=nam.txt[ 1:7 ],ylab="MSPE",main="Setting 6", ylim=c(0,150), col = colvec[ c(1, 2, 3, 8, 10, 11, 12) ])
#grid(nx = NA, ny = NULL, col="lightgray", lty="dotted")
#boxplot(errmats[[ 7 ]],names=nam.txt[ 1:6 ],ylab="MSPE",main="Setting 7", ylim=c(0,25), col = colvec[ c(1, 2, 3, 8, 10, 11) ])
#grid(nx = NA, ny = NULL, col="lightgray", lty="dotted")
#boxplot(errmats[[ 8 ]],names=nam.txt[ 1:6 ],ylab="MSPE",main="Setting 8", ylim=c(0,30), col = colvec[ c(1, 2, 3, 8, 10, 11) ])
#grid(nx = NA, ny = NULL, col="lightgray", lty="dotted")

boxplot(errmats[[ 1 ]],names=nam.txt[ 1:7 ],ylab="MSPE",main="Setting 1", ylim=c(0,25), col = colvec[ c(1, 2, 3, 8, 10, 11, 12) ])
grid(nx = NA, ny = NULL, col="lightgray", lty="dotted")
boxplot(errmats[[ 2 ]],names=nam.txt[ 1:7 ],ylab="MSPE",main="Setting 2", ylim=c(0,25), col = colvec[ c(1, 2, 3, 8, 10, 11, 12) ])
grid(nx = NA, ny = NULL, col="lightgray", lty="dotted")
boxplot(errmats[[ 3 ]],names=nam.txt[ 1:7 ],ylab="MSPE",main="Setting 3", ylim=c(0,50), col = colvec[ c(1, 2, 3, 8, 10, 11, 12) ])
grid(nx = NA, ny = NULL, col="lightgray", lty="dotted")
boxplot(errmats[[ 4 ]],names=nam.txt[ 1:7 ],ylab="MSPE",main="Setting 4", ylim=c(0,15), col = colvec[ c(1, 2, 3, 8, 10, 11, 12) ])
grid(nx = NA, ny = NULL, col="lightgray", lty="dotted")
boxplot(errmats[[ 5 ]],names=nam.txt[ 1:7 ],ylab="MSPE",main="Setting 5", ylim=c(0,150), col = colvec[ c(1, 2, 3, 8, 10, 11, 12) ])
grid(nx = NA, ny = NULL, col="lightgray", lty="dotted")
boxplot(errmats[[ 6 ]],names=nam.txt[ 1:7 ],ylab="MSPE",main="Setting 6", ylim=c(0,150), col = colvec[ c(1, 2, 3, 8, 10, 11, 12) ])
grid(nx = NA, ny = NULL, col="lightgray", lty="dotted")
boxplot(errmats[[ 7 ]],names=nam.txt[ 1:7 ],ylab="MSPE",main="Setting 7", ylim=c(0,25), col = colvec[ c(1, 2, 3, 8, 10, 11, 12) ])
grid(nx = NA, ny = NULL, col="lightgray", lty="dotted")
boxplot(errmats[[ 8 ]],names=nam.txt[ 1:7 ],ylab="MSPE",main="Setting 8", ylim=c(0,30), col = colvec[ c(1, 2, 3, 8, 10, 11, 12) ])
grid(nx = NA, ny = NULL, col="lightgray", lty="dotted")


# revised version including oracle least-squares estimator

par(mar = c(2,4,3,1))
par(mfrow = c(4,2))
# Boxplots for high-dimensional tests
errmats = list()


for (i in 1:8) {
  errmats[[ i ]] = cbind(scopes_pe[1:500, i], scopel_pe[1:500, i], scopecv_pe[1:500, i], ols_pe[1:500, i],dmr_pe[1:500, i], cart_pe[1:500, i], rf_pe[1:500, i], glmnet_pe[1:500, i])
  # if ( i %in% c(1, 2, 4, 5, 6) ) errmats[[ i ]] = cbind(errmats[[ i ]], glmnet_pe[1:500, i]) OLD

}



boxplot(errmats[[ 1 ]],names=nam.txt[ 1:8 ],ylab="MSPE",main="Setting 1", ylim=c(0,25), col = colvec[ c(1, 2, 3, 5, 8, 10, 11, 12) ])
grid(nx = NA, ny = NULL, col="lightgray", lty="dotted")
boxplot(errmats[[ 2 ]],names=nam.txt[ 1:8 ],ylab="MSPE",main="Setting 2", ylim=c(0,25), col = colvec[ c(1, 2, 3, 5, 8, 10, 11, 12) ])
grid(nx = NA, ny = NULL, col="lightgray", lty="dotted")
boxplot(errmats[[ 3 ]],names=nam.txt[ 1:8 ],ylab="MSPE",main="Setting 3", ylim=c(0,50), col = colvec[ c(1, 2, 3, 5, 8, 10, 11, 12) ])
grid(nx = NA, ny = NULL, col="lightgray", lty="dotted")
boxplot(errmats[[ 4 ]],names=nam.txt[ 1:8 ],ylab="MSPE",main="Setting 4", ylim=c(0,15), col = colvec[ c(1, 2, 3, 5, 8, 10, 11, 12) ])
grid(nx = NA, ny = NULL, col="lightgray", lty="dotted")
boxplot(errmats[[ 5 ]],names=nam.txt[ 1:8 ],ylab="MSPE",main="Setting 5", ylim=c(0,150), col = colvec[ c(1, 2, 3, 5, 8, 10, 11, 12) ])
grid(nx = NA, ny = NULL, col="lightgray", lty="dotted")
boxplot(errmats[[ 6 ]],names=nam.txt[ 1:8 ],ylab="MSPE",main="Setting 6", ylim=c(0,150), col = colvec[ c(1, 2, 3, 5, 8, 10, 11, 12) ])
grid(nx = NA, ny = NULL, col="lightgray", lty="dotted")
boxplot(errmats[[ 7 ]],names=nam.txt[ 1:8 ],ylab="MSPE",main="Setting 7", ylim=c(0,25), col = colvec[ c(1, 2, 3, 5, 8, 10, 11, 12) ])
grid(nx = NA, ny = NULL, col="lightgray", lty="dotted")
boxplot(errmats[[ 8 ]],names=nam.txt[ 1:8 ],ylab="MSPE",main="Setting 8", ylim=c(0,30), col = colvec[ c(1, 2, 3, 5, 8, 10, 11, 12) ])
grid(nx = NA, ny = NULL, col="lightgray", lty="dotted")


#updated code for the above
# New color vector
colvec = c(rgb(0, 0.5, 0, 0.5), rgb(0.45, 0.85, 0.65, 0.5), rgb(0.7, 0.8, 0.4, 0.5), rgb(0, 0, 1, 0.5), rgb(0, 0.6, 0.8, 0.5), rgb(0.8, 0.2, 0, 0.5), rgb(0.85, 0.6, 0, 0.5), rgb(0.85, 0, 0.8, 0.5), rgb(0.75, 0.3, 0.4, 0.5), rgb(0.4, 0, 0.85, 0.5), rgb(0.6, 0.5, 0.7, 0.5), rgb(0.7, 0.7, 0.7, 0.5))

par(mar = c(2,4,3,1))
par(mfrow = c(4,2))
# Boxplots for high-dimensional tests
errmats = list()
# for (i in 1:8) {
#   errmats[[ i ]] = cbind(dmr_pe[1:500, i], scopes_pe[1:500, i], scopel_pe[1:500, i], scopecv_pe[1:500, i], cart_pe[1:500, i], rf_pe[1:500, i], glmnet)
#   # if ( i %in% c(1, 2, 4, 5, 6) ) errmats[[ i ]] = cbind(errmats[[ i ]], glmnet_pe[1:500, i]) OLD
#   errmats[[ i ]] = cbind(errmats[[ i ]], glmnet_pe[1:500, i])
# }

for (i in 1:8) {
  errmats[[ i ]] = cbind(scopes_pe[1:500, i], scopel_pe[1:500, i], scopecv_pe[1:500, i], ols_pe[1:500, i],dmr_pe[1:500, i], cart_pe[1:500, i], rf_pe[1:500, i], glmnet_pe[1:500, i])
  # if ( i %in% c(1, 2, 4, 5, 6) ) errmats[[ i ]] = cbind(errmats[[ i ]], glmnet_pe[1:500, i]) OLD

}



boxplot(errmats[[ 1 ]],names=nam.txt[ 1:8 ],ylab="MSPE",main="Setting 1", ylim=c(0,25), col = colvec[ c(1, 2, 3, 5, 8, 10, 11, 12) ])
grid(nx = NA, ny = NULL, col="lightgray", lty="dotted")
boxplot(errmats[[ 2 ]],names=nam.txt[ 1:8 ],ylab="MSPE",main="Setting 2", ylim=c(0,25), col = colvec[ c(1, 2, 3, 5, 8, 10, 11, 12) ])
grid(nx = NA, ny = NULL, col="lightgray", lty="dotted")
boxplot(errmats[[ 3 ]],names=nam.txt[ 1:8 ],ylab="MSPE",main="Setting 3", ylim=c(0,50), col = colvec[ c(1, 2, 3, 5, 8, 10, 11, 12) ])
grid(nx = NA, ny = NULL, col="lightgray", lty="dotted")
boxplot(errmats[[ 4 ]],names=nam.txt[ 1:8 ],ylab="MSPE",main="Setting 4", ylim=c(0,15), col = colvec[ c(1, 2, 3, 5, 8, 10, 11, 12) ])
grid(nx = NA, ny = NULL, col="lightgray", lty="dotted")
boxplot(errmats[[ 5 ]],names=nam.txt[ 1:8 ],ylab="MSPE",main="Setting 5", ylim=c(0,150), col = colvec[ c(1, 2, 3, 5, 8, 10, 11, 12) ])
grid(nx = NA, ny = NULL, col="lightgray", lty="dotted")
boxplot(errmats[[ 6 ]],names=nam.txt[ 1:8 ],ylab="MSPE",main="Setting 6", ylim=c(0,150), col = colvec[ c(1, 2, 3, 5, 8, 10, 11, 12) ])
grid(nx = NA, ny = NULL, col="lightgray", lty="dotted")
boxplot(errmats[[ 7 ]],names=nam.txt[ 1:8 ],ylab="MSPE",main="Setting 7", ylim=c(0,25), col = colvec[ c(1, 2, 3, 5, 8, 10, 11, 12) ])
grid(nx = NA, ny = NULL, col="lightgray", lty="dotted")
boxplot(errmats[[ 8 ]],names=nam.txt[ 1:8 ],ylab="MSPE",main="Setting 8", ylim=c(0,30), col = colvec[ c(1, 2, 3, 5, 8, 10, 11, 12) ])
grid(nx = NA, ny = NULL, col="lightgray", lty="dotted")

# ALTERNATIVE
# or alternatively bar plots:
se = function(x) {
    return(sd(x) / sqrt(length(x)))
}

barCenters = barplot(colMeans(errmats[[ 1 ]]),names=nam.txt[ 1:8 ],ylab="MSPE",main="Setting 1", ylim=c(0,25), col = colvec[ c(1, 2, 3, 5, 8, 10, 11, 12) ])
arrows(barCenters, colMeans(errmats[[1]]) - apply(errmats[[1]], 2, se),barCenters, colMeans(errmats[[1]]) + apply(errmats[[1]], 2, se), length = 0.05, angle = 90, code = 3)
grid(nx = NA, ny = NULL, col="lightgray", lty="dotted")
barCenters = barplot(colMeans(errmats[[ 2 ]]),names=nam.txt[ 1:8 ],ylab="MSPE",main="Setting 2", ylim=c(0,25), col = colvec[ c(1, 2, 3, 5, 8, 10, 11, 12) ])
arrows(barCenters, colMeans(errmats[[2]]) - apply(errmats[[2]], 2, se),barCenters, colMeans(errmats[[2]]) + apply(errmats[[2]], 2, se), length = 0.05, angle = 90, code = 3)
grid(nx = NA, ny = NULL, col="lightgray", lty="dotted")
barCenters = barplot(colMeans(errmats[[ 3 ]]),names=nam.txt[ 1:8 ],ylab="MSPE",main="Setting 3", ylim=c(0,50), col = colvec[ c(1, 2, 3, 5, 8, 10, 11, 12) ])
arrows(barCenters, colMeans(errmats[[3]]) - apply(errmats[[3]], 2, se),barCenters, colMeans(errmats[[3]]) + apply(errmats[[3]], 2, se), length = 0.05, angle = 90, code = 3)
grid(nx = NA, ny = NULL, col="lightgray", lty="dotted")
barCenters = barplot(colMeans(errmats[[ 4 ]]),names=nam.txt[ 1:8 ],ylab="MSPE",main="Setting 4", ylim=c(0,15), col = colvec[ c(1, 2, 3, 5, 8, 10, 11, 12) ])
arrows(barCenters, colMeans(errmats[[4]]) - apply(errmats[[4]], 2, se),barCenters, colMeans(errmats[[4]]) + apply(errmats[[4]], 2, se), length = 0.05, angle = 90, code = 3)
grid(nx = NA, ny = NULL, col="lightgray", lty="dotted")
barCenters = barplot(colMeans(errmats[[ 5 ]]),names=nam.txt[ 1:8 ],ylab="MSPE",main="Setting 5", ylim=c(0,150), col = colvec[ c(1, 2, 3, 5, 8, 10, 11, 12) ])
arrows(barCenters, colMeans(errmats[[5]]) - apply(errmats[[5]], 2, se),barCenters, colMeans(errmats[[5]]) + apply(errmats[[5]], 2, se), length = 0.05, angle = 90, code = 3)
grid(nx = NA, ny = NULL, col="lightgray", lty="dotted")
barCenters = barplot(colMeans(errmats[[ 6 ]]),names=nam.txt[ 1:8 ],ylab="MSPE",main="Setting 6", ylim=c(0,150), col = colvec[ c(1, 2, 3, 5, 8, 10, 11, 12) ])
arrows(barCenters, colMeans(errmats[[6]]) - apply(errmats[[6]], 2, se),barCenters, colMeans(errmats[[6]]) + apply(errmats[[6]], 2, se), length = 0.05, angle = 90, code = 3)
grid(nx = NA, ny = NULL, col="lightgray", lty="dotted")
barCenters = barplot(colMeans(errmats[[ 7 ]]),names=nam.txt[ 1:8 ],ylab="MSPE",main="Setting 7", ylim=c(0,25), col = colvec[ c(1, 2, 3, 5, 8, 10, 11, 12) ])
arrows(barCenters, colMeans(errmats[[7]]) - apply(errmats[[7]], 2, se),barCenters, colMeans(errmats[[7]]) + apply(errmats[[7]], 2, se), length = 0.05, angle = 90, code = 3)
grid(nx = NA, ny = NULL, col="lightgray", lty="dotted")
barCenters = barplot(colMeans(errmats[[ 8 ]]),names=nam.txt[ 1:8 ],ylab="MSPE",main="Setting 8", ylim=c(0,30), col = colvec[ c(1, 2, 3, 5, 8, 10, 11, 12) ])
arrows(barCenters, colMeans(errmats[[8]]) - apply(errmats[[8]], 2, se),barCenters, colMeans(errmats[[8]]) + apply(errmats[[8]], 2, se), length = 0.05, angle = 90, code = 3)
grid(nx = NA, ny = NULL, col="lightgray", lty="dotted")


# prudential experiment box plot
# plotmat = cbind(scopes_error / response_sd , scopel_error / response_sd, scopecv_error / response_sd, cart_error / response_sd, rf_error / response_sd, glmnet_error/ response_sd)[1:2000,]

plotmat = cbind(scopes_norm_error, scopel_norm_error, scopecv_norm_error, cart_norm_error, rf_norm_error, glmnet_norm_error)[1:1000,]


boxplot(plotmat, names = nam.txt[1:6], ylab = "MSPE", main = "Prudential Life Insurance Assessment example", ylim = c(0, 0.6), col = colvec[c(1,2,3,10,11,12)])
grid(nx = NA, ny = NULL, col="lightgray", lty="dotted")



x = seq(0, 10, 0.01)

fn1 = function(x) return(0.5 * (x - 2)^2 + 1)
fn2 = function(x) return(11)
fn3 = function(x) return(1.5 * (x - 4)^2 + 9)
fn4 = function(x) return(3.5* (x - 6)^2 + 5)
fn5 = function(x) return((x - 9)^2 + 9.5)

gn1 = function(x) {
    if ( (x < 5.333333)) return(fn1(x)) else return(Inf)
}
gn2 = function(x) {
  if ( x > 3) return(fn2(x)) else return(Inf)
}
gn3 = function(x) {
  if ( x < 6 ) return(fn3(x)) else return(Inf)
}
gn4 = function(x) {
    if (( x > 5.333333333) && ( x < 8 )) return(fn4(x)) else return(Inf)
}
gn5 = function(x) {
  if ( x > (9 - sqrt(3/2))) return(fn5(x)) else return(Inf)
}

gn1 = Vectorize(gn1)
gn2 = Vectorize(gn2)
gn3 = Vectorize(gn3)
gn4 = Vectorize(gn4)
gn5 = Vectorize(gn5)

plot(x, gn1(x), xlim=c(0,10), ylim=c(0,20), type='l', xlab='', ylab = '')
lines(x, gn2(x), type='l')
lines(x, gn3(x), type='l')
lines(x, gn4(x), type='l')
lines(x, gn5(x), type='l')

dotlin = c(3, 5.3333333, 6, 9 - sqrt(3/2))
for (j in dotlin) abline(v = j, lty='dotted')

# Intersections at x = 5.333333333
# x = 6 + sqrt(12/7)
# x = 9 - sqrt(3/2)

lines(seq(0,5.33333,0.01), gn1(seq(0,5.333333,0.01)), col="red", lwd = 2)
lines(seq(5.333333,6+sqrt(12/7),0.01), gn4(seq(5.333333, 6+sqrt(12/7),0.01)), col="red", lwd = 2)
lines(seq(6+sqrt(12/7),9-sqrt(3/2), 0.01), gn2(seq(6+sqrt(12/7),9-sqrt(3/2),0.01)), col="red", lwd = 2)
lines(seq(9-sqrt(3/2),10, 0.01), gn5(seq(9-sqrt(3/2),10,0.01)), col="red", lwd = 2)


points(c(5.3333333333, 6 + sqrt(12/7), 9 - sqrt(3/2)),c(6.5555555, 11,11), col="red", pch=18,cex=2)


# NEWER SIMPLER CURVE PLOT

x = seq(0, 10, 0.01)

fn1 = function(x) return(0.5 * (x - 2)^2 + 1)
fn2 = function(x) return(11)
fn3 = function(x) return(1.5 * (x - 4)^2 + 9)
fn4 = function(x) return(3.5* (x - 6)^2 + 5)
fn5 = function(x) return((x - 9)^2 + 9.5)

gn1 = function(x) {
    if ( (x < 6)) return(fn1(x)) else return(Inf)
}

gn1dot = function(x) {
    if ( (x >= 6)) return(fn1(x)) else return(Inf)
}

gn2 = function(x) {
  if ( x > 3) return(fn2(x)) else return(Inf)
}
gn3 = function(x) {
  if ( x < 6 ) return(fn3(x)) else return(Inf)
}
gn4 = function(x) {
    if (( x >= 4.5) && ( x < 6 + sqrt(12/7) )) return(fn4(x)) else return(Inf)
}
gn4dot = function(x) {
    if ( x >= 6 + sqrt(12/7)) return(fn4(x)) else return(Inf)
}
gn5 = function(x) {
  if ( x > (9 - sqrt(3/2))) return(fn5(x)) else return(Inf)
}

gn1 = Vectorize(gn1)
gn1dot = Vectorize(gn1dot)
gn2 = Vectorize(gn2)
gn3 = Vectorize(gn3)
gn4 = Vectorize(gn4)
gn4dot = Vectorize(gn4dot)
gn5 = Vectorize(gn5)

plot(x, gn1(x), xlim=c(0,10), ylim=c(0,20), type='l', xlab='', ylab = '', yaxt = 'n', xaxt = 'n', ann = F)
lines(x, gn2(x), type='l')
# lines(x, gn3(x), type='l')
lines(x, gn4(x), type='l')
lines(x, gn5(x), type='l')
lines(x, gn1dot(x), type='l', lty=3)
lines(x, gn4dot(x), type='l', lty=3)

dotlin = c(3, 4.5, 6, 6 + sqrt(12/7), 9 - sqrt(3/2))
for (j in dotlin) abline(v = j, lty='dotted')

# Intersections at x = 5.333333333
# x = 6 + sqrt(12/7)
# x = 9 - sqrt(3/2)

lines(seq(0,5.33333,0.01), gn1(seq(0,5.333333,0.01)), col="red", lwd = 2)
lines(seq(5.333333,6+sqrt(12/7),0.01), gn4(seq(5.333333, 6+sqrt(12/7),0.01)), col="red", lwd = 2)
lines(seq(6+sqrt(12/7),9-sqrt(3/2), 0.01), gn2(seq(6+sqrt(12/7),9-sqrt(3/2),0.01)), col="red", lwd = 2)
lines(seq(9-sqrt(3/2),10, 0.01), gn5(seq(9-sqrt(3/2),10,0.01)), col="red", lwd = 2)



