# This is only needed for when tests are run on a large number of machines. Create a string vector filelist with each entry the name of the results file, then run this and it combines all of them together into one file.

library(mclust)

adjustedRandIndex_Round = function(x, y, digits = 4) {
    x = round(x, digits = digits)
    y = round(y, digits = digits)
    return(adjustedRandIndex(x, y))
}



is_signal = function(x, digits = 4) {
    return(length(unique(round(x, digits = digits))) > 1)
}

n_signal = c(6, 6, 6, 5, 25, 25, 10, 5)

signal = list()
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

scopes_pe = matrix(0,0,8)
scopel_pe = matrix(0,0,8)
scopecv_pe = matrix(0,0,8)
dmr_pe = matrix(0,0,8)
cart_pe = matrix(0,0,8)
rf_pe = matrix(0,0,8)
glmnet_pe = matrix(0,0,8)
ols_pe = matrix(0, 0, 8)

scopes_t = matrix(0,0,8)
scopel_t = matrix(0,0,8)
scopecv_t = matrix(0,0,8)
dmr_t = matrix(0,0,8)
cart_t = matrix(0,0,8)
rf_t = matrix(0,0,8)
glmnet_t = matrix(0,0,8)
ols_t = matrix(0, 0, 8)

scopes_d = matrix(0,0,8)
scopel_d = matrix(0,0,8)
scopecv_d = matrix(0,0,8)
dmr_d = matrix(0,0,8)
glmnet_d = matrix(0,0,8)

scopecv_gamma = matrix(0, 0, 8)

scopes_fn = matrix(0, 0, 8)
scopel_fn = matrix(0, 0, 8)
scopecv_fn = matrix(0, 0, 8)
dmr_fn = matrix(0, 0, 8)
glmnet_fn = matrix(0, 0, 8)

scopes_fp = matrix(0, 0, 8)
scopel_fp = matrix(0, 0, 8)
scopecv_fp = matrix(0, 0, 8)
dmr_fp = matrix(0, 0, 8)
glmnet_fp = matrix(0, 0, 8)

scopes_rand = list()
scopel_rand = list()
scopecv_rand = list()
dmr_rand = list()
glmnet_rand = list()

scopes_rand_all = list()
scopel_rand_all = list()
scopecv_rand_all = list()
dmr_rand_all = list()
glmnet_rand_all = list()


for (setting in 1:8) {
    scopes_rand[[ setting ]] = list()
    scopel_rand[[ setting ]] = list()
    scopecv_rand[[ setting ]] = list()
    dmr_rand[[ setting ]] = list()
    glmnet_rand[[ setting ]] = list()
    
    scopes_rand_all[[ setting ]] = list()
    scopel_rand_all[[ setting ]] = list()
    scopecv_rand_all[[ setting ]] = list()
    dmr_rand_all[[ setting ]] = list()
    glmnet_rand_all[[ setting ]] = list()
    
}


filelist = c("section_6_1_2_raw_modified_code_to_run_on_single_machine.Rdata")

for (i in 1:length(filelist)){
    print(paste0("File ", i))
    load(filelist[i])
    scopes_pe = rbind(scopes_pe, scopes_prederr[ 1:number_finished, ])
    scopel_pe = rbind(scopel_pe, scopel_prederr[ 1:number_finished, ])
    scopecv_pe = rbind(scopecv_pe, scopecv_prederr[ 1:number_finished, ])
    dmr_pe = rbind(dmr_pe, dmr_prederr[ 1:number_finished, ])
    cart_pe = rbind(cart_pe, cart_prederr[ 1:number_finished, ])
    rf_pe = rbind(rf_pe, rf_prederr[ 1:number_finished, ])
    glmnet_pe = rbind(glmnet_pe, glmnet_prederr[ 1:number_finished, ])
    ols_pe = rbind(ols_pe, ols_prederr[ 1:number_finished, ])
    
    scopes_t = rbind(scopes_t, scopes_time[ 1:number_finished, ])
    scopel_t = rbind(scopel_t, scopel_time[ 1:number_finished, ])
    scopecv_t = rbind(scopecv_t, scopecv_time[ 1:number_finished, ])
    dmr_t = rbind(dmr_t, dmr_time[ 1:number_finished, ])
    cart_t = rbind(cart_t, cart_time[ 1:number_finished, ])
    rf_t = rbind(rf_t, rf_time[ 1:number_finished, ])
    glmnet_t = rbind(glmnet_t, glmnet_time[ 1:number_finished, ])
    ols_t = rbind(ols_t, ols_time[ 1:number_finished, ])
    
    scopes_d = rbind(scopes_d, scopes_dim[ 1:number_finished, ])
    scopel_d = rbind(scopel_d, scopel_dim[ 1:number_finished, ])
    scopecv_d = rbind(scopecv_d, scopecv_dim[ 1:number_finished, ])
    dmr_d = rbind(dmr_d, dmr_dim[ 1:number_finished, ])
    glmnet_d = rbind(glmnet_d, glmnet_dim[ 1:number_finished, ])
    
    scopecv_gamma = rbind(scopecv_gamma, scopecv_gam[ 1:number_finished, ])
    
    for (j in 1:number_finished) {
        print(paste0("Example ", j))
        scopes_fn_row = rep(0, 8)
        scopel_fn_row = rep(0, 8)
        scopecv_fn_row = rep(0, 8)
        dmr_fn_row = rep(0, 8)
        glmnet_fn_row = rep(0, 8)
        
        scopes_fp_row = rep(0, 8)
        scopel_fp_row = rep(0, 8)
        scopecv_fp_row = rep(0, 8)
        dmr_fp_row = rep(0, 8)
        glmnet_fp_row = rep(0, 8)
        
        for (setting in 1:8) {
            print(paste0("Setting ", setting))
            # scopes
            print("Scope S")
            act_sig = c(rep(TRUE, n_signal[ setting ]), rep(FALSE, 100 - n_signal[ setting ]))
            est_sig = rep(FALSE, 100)
            for (k in 1:100) {
                est_sig[ k ] = is_signal(scopes_models[[ setting ]][[ j ]]$beta.best[[ 2 ]][[ k ]])
            }
            scopes_fp_row[ setting ] = sum(est_sig[act_sig == FALSE])
            scopes_fn_row[ setting ] = sum(1 - est_sig[act_sig == TRUE])
            
            rand_all = rep(0, n_signal[ setting ])
            match_all = rep(0, n_signal[ setting ])
            print("Computing cluster scores for variables...")
            for (k in 1:n_signal[ setting ]) {
                print(k)
                rand_all[ k ] = adjustedRandIndex_Round(signal[[ setting ]][ , k ], scopes_models[[ setting ]][[ j ]]$beta.best[[ 2 ]][[ k ]])

            }
            scopes_rand_all[[ setting ]] = append(scopes_rand_all[[ setting ]], list(rand_all))
            if (sum(est_sig[ 1:n_signal[ setting ] ]) >= 1) {
                scopes_rand[[ setting ]] = append(scopes_rand[[ setting ]], list(rand_all[ 1:n_signal[ setting ] ][ est_sig[ 1:n_signal[ setting ] ] ]))

            } else {
                scopes_rand[[ setting ]] = append(scopes_rand[[ setting ]], list(NULL))

            }
            
            # scopel
            print("Scope L")
            act_sig = c(rep(TRUE, n_signal[ setting ]), rep(FALSE, 100 - n_signal[ setting ]))
            est_sig = rep(FALSE, 100)
            for (k in 1:100) {
                est_sig[ k ] = is_signal(scopel_models[[ setting ]][[ j ]]$beta.best[[ 2 ]][[ k ]])
            }
            scopel_fp_row[ setting ] = sum(est_sig[act_sig == FALSE])
            scopel_fn_row[ setting ] = sum(1 - est_sig[act_sig == TRUE])
            
            rand_all = rep(0, n_signal[ setting ])

            print("Computing cluster scores for variables...")
            for (k in 1:n_signal[ setting ]) {
                print(k)
                rand_all[ k ] = adjustedRandIndex_Round(signal[[ setting ]][ , k ], scopel_models[[ setting ]][[ j ]]$beta.best[[ 2 ]][[ k ]])

            }
            scopel_rand_all[[ setting ]] = append(scopel_rand_all[[ setting ]], list(rand_all))
            if (sum(est_sig[ 1:n_signal[ setting ] ]) >= 1) {
                scopel_rand[[ setting ]] = append(scopel_rand[[ setting ]], list(rand_all[ 1:n_signal[ setting ] ][ est_sig[ 1:n_signal[ setting ] ] ]))

            } else {
                scopel_rand[[ setting ]] = append(scopel_rand[[ setting ]], list(NULL))

            }
            
            # scopecv
            print("Scope CV")
            act_sig = c(rep(TRUE, n_signal[ setting ]), rep(FALSE, 100 - n_signal[ setting ]))
            est_sig = rep(FALSE, 100)
            for (k in 1:100) {
                est_sig[ k ] = is_signal(scopecv_models[[ setting ]][[ j ]]$beta.best[[ 2 ]][[ k ]])
            }
            scopecv_fp_row[ setting ] = sum(est_sig[act_sig == FALSE])
            scopecv_fn_row[ setting ] = sum(1 - est_sig[act_sig == TRUE])
            
            rand_all = rep(0, n_signal[ setting ])

            print("Computing cluster scores for variables...")
            for (k in 1:n_signal[ setting ]) {
                print(k)
                rand_all[ k ] = adjustedRandIndex_Round(signal[[ setting ]][ , k ], scopecv_models[[ setting ]][[ j ]]$beta.best[[ 2 ]][[ k ]])

            }
            scopecv_rand_all[[ setting ]] = append(scopecv_rand_all[[ setting ]], list(rand_all))
            
            if (sum(est_sig[ 1:n_signal[ setting ] ]) >= 1) {
                scopecv_rand[[ setting ]] = append(scopecv_rand[[ setting ]], list(rand_all[ 1:n_signal[ setting ] ][ est_sig[ 1:n_signal[ setting ] ] ]))

            } else {
                scopecv_rand[[ setting ]] = append(scopecv_rand[[ setting ]], list(NULL))
            }
            
            # dmr
            # first turn the beta into a list
            print("DMR")
            beta_list = list()
            for (k in 1:100) {
                beta_list[[ k ]] = c(0, dmr_models[[ setting ]][[ j ]]$beta[(23*(k-1) + 2):(23*k + 1)])
            }
            act_sig = c(rep(TRUE, n_signal[ setting ]), rep(FALSE, 100 - n_signal[ setting ]))
            est_sig = rep(FALSE, 100)
            for (k in 1:100) {
                est_sig[ k ] = is_signal(beta_list[[ k ]])
            }
            dmr_fp_row[ setting ] = sum(est_sig[act_sig == FALSE])
            dmr_fn_row[ setting ] = sum(1 - est_sig[act_sig == TRUE])
            
            rand_all = rep(0, n_signal[ setting ])

            print("Computing cluster scores for variables...")
            for (k in 1:n_signal[ setting ]) {
                print(k)
                rand_all[ k ] = adjustedRandIndex_Round(signal[[ setting ]][ , k ], beta_list[[ k ]])

            }
            dmr_rand_all[[ setting ]] = append(dmr_rand_all[[ setting ]], list(rand_all))

            if (sum(est_sig[ 1:n_signal[ setting ] ]) >= 1) {
                dmr_rand[[ setting ]] = append(dmr_rand[[ setting ]], list(rand_all[ 1:n_signal[ setting ] ][ est_sig[ 1:n_signal[ setting ] ] ]))

            } else {
                dmr_rand[[ setting ]] = append(dmr_rand[[ setting ]], list(NULL))
            }
            
            # glmnet
            # first turn the beta into a list
            # There is an issue here with the full coefficient vector not being saved in some cases
            print("glmnet")
            beta_list = list()
            for (k in 1:100) {
                beta_list[[ k ]] = as.vector(glmnet_models[[ setting ]][[ j ]]$beta.best)[(24*(k-1) + 1):(24*k)]
            }
            
            
            act_sig = c(rep(TRUE, n_signal[ setting ]), rep(FALSE, 100 - n_signal[ setting ]))
            est_sig = rep(FALSE, 100)
            for (k in 1:100) {
                est_sig[ k ] = is_signal(beta_list[[ k ]])
            }
            glmnet_fp_row[ setting ] = sum(est_sig[act_sig == FALSE])
            glmnet_fn_row[ setting ] = sum(1 - est_sig[act_sig == TRUE])
            
            rand_all = rep(0, n_signal[ setting ])
            print("Computing cluster scores for variables...")
            for (k in 1:n_signal[ setting ]) {
                print(k)
                rand_all[ k ] = adjustedRandIndex_Round(signal[[ setting ]][ , k ], beta_list[[ k ]])

            }
            glmnet_rand_all[[ setting ]] = append(glmnet_rand_all[[ setting ]], list(rand_all))
            if (sum(est_sig[ 1:n_signal[ setting ] ]) >= 1) {
                glmnet_rand[[ setting ]] = append(glmnet_rand[[ setting ]], list(rand_all[ 1:n_signal[ setting ] ][ est_sig[ 1:n_signal[ setting ] ] ]))

            } else {
                glmnet_rand[[ setting ]] = append(glmnet_rand[[ setting ]], list(NULL))
                
            }
            
            
            
            
        }
        scopes_fn = rbind(scopes_fn, scopes_fn_row)
        scopel_fn = rbind(scopel_fn, scopel_fn_row)
        scopecv_fn = rbind(scopecv_fn, scopecv_fn_row)
        dmr_fn = rbind(dmr_fn, dmr_fn_row)
        glmnet_fn = rbind(glmnet_fn, glmnet_fn_row)
        
        scopes_fp = rbind(scopes_fp, scopes_fp_row)
        scopel_fp = rbind(scopel_fp, scopel_fp_row)
        scopecv_fp = rbind(scopecv_fp, scopecv_fp_row)
        dmr_fp = rbind(dmr_fp, dmr_fp_row)
        glmnet_fp = rbind(glmnet_fp, glmnet_fp_row)
        
        
        
    }
    
    
    
}

save(scopes_pe, scopel_pe, scopecv_pe, dmr_pe, cart_pe, rf_pe, glmnet_pe, ols_pe, scopes_t, scopel_t, scopecv_t, dmr_t, cart_t, rf_t, glmnet_t, ols_t, scopes_d, scopel_d, scopecv_d, dmr_d, glmnet_d, scopecv_gamma, scopes_fn, scopel_fn, scopecv_fn, dmr_fn, glmnet_fn, scopes_fp, scopel_fp, scopecv_fp, dmr_fp, glmnet_fp, scopes_rand, scopel_rand, scopecv_rand, dmr_rand, glmnet_rand, scopes_rand_all, scopel_rand_all, scopecv_rand_all, dmr_rand_all, glmnet_rand_all, file = "section_6_1_2_full_results.Rdata")


