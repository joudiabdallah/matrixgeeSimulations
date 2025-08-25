
# =============================================================================
# simfct_nokron_parallel()
# -----------------------------------------------------------------------------
# Purpose
#   Run a parallel Monte Carlo study for the non-Kronecker matrix-valued GEE
#   estimator implemented in matrixgee::matrixgee_cpp_nokron. For each replicate:
#     1) Simulate Y using datagen_nokron() 
#     2) Fit the  GEE with user-specified working correlation `corstr`.
#     3) Fit an independence GEE (corstr = "independence").
#     4) Gather β̂, robust covariance, Wald p-values, and efficiency metric.s
#        
#
# Data model in generation (via datagen_nokron)
#   For subject i:
#     M_i = a_R 1_c^T  +  1_r (B^T x_i) 1_c^T   
#  
#
# Parallelization
#   Uses  clusters via parallel::{detectCores, makeCluster, parLapply}.
#   Each worker loads matrixgee and MASS
#
# Arguments
#   sample_size : integer N ≥ 1. Number of matrices per replicate.
#   rows_no     : integer r ≥ 1. Rows of each matrix response.
#   cols_no     : integer c ≥ 1. Columns of each matrix response.
#   rho         : numeric. Off-diagonal correlation for vec(Y) in datagen_nokron.
#   covariates  : numeric vector of length N*q (stacked x_1,…,x_N) or NULL.
#   intercept   : character. Intercept type expected by matrixgee_cpp_nokron
#                 ("row_intercept", "column_intercept", "matrix_intercept", "scalar_intercept").
#   max_iter    : integer. Maximum iterations in matrixgee_cpp_nokron.
#   corstr      : character. Working correlation for the fitted non-Kronecker GEE
#                 ("exchangeable", "ar1", "independence", ...).
#   tol         : numeric. Convergence tolerance in matrixgee_cpp_nokron.
#   family      : Placeholder for future GLM families. Unused for now.
#   n_sim       : integer S ≥ 1. Number of Monte Carlo replicates.
#   alpha       : Nominal level for tests; p-value threshold. Unused for now.
#                 It is set at 0.05 below.
#   a_R         : numeric vector length r. Row intercepts passed to datagen_nokron.
#   B           : numeric matrix q×1 or NULL.
#                 If either covariates or B is NULL, covariate effect is neglected.
#
# Shapes
#   - datagen_nokron() must return Y_big, an r × (N*c) matrix. For subject i,
#     block columns ((i−1)*c+1):(i*c) correspond to Y_i.
#   - matrixgee::matrixgee_cpp_nokron(Y_big, covariates, intercept, sample_size,
#       rows_no, cols_no, max_iter, tol, corstr)
#     returns a list with:
#       $beta_hat          : numeric vector of length r+q (parameter numbers)
#       $robust_covariance : rcxrc sandwich covariance matrix
#
# True parameters
#   θ* = (a_R, vec(B)) if covariates are present; otherwise θ* = a_R.
#
# Metrics computed per replicate
#   - Bias: β̂ − θ*
#   - Frobenius (ℓ2) norm of bias: ||β̂ − θ*||_2
#   - Robust SEs: sqrt(diag(robust_covariance))
#   - Wald p-values per parameter: ((β̂_j − θ*_j)^2 / Var_j) ~ χ^2_1
#   - ARE per parameter: diag(robust_cov_indep) / diag(robust_cov_working)
#   - ARE_det: det(robust_cov_indep) / det(robust_cov_working)
#
# Aggregate summaries across all S replicates. Check Simulation chapter for more information
#   - ARE_det_calc_averaged : mean(ARE_det)
#   - Average_RMSE          : per-parameter RMSE across sims, then averaged
#                             over parameters (“method 1”)
#   - Frob_method2          : || mean(β̂) − θ* ||_2 (“method 2” bias)
#   - MAB                   : mean over parameters of mean absolute bias
#   - MSE                   : mean over parameters of mean squared error
#   - p_freq                : overall non-rejection frequency at 0.05
#                             averaged over parameters
#   - p_freq_min/max/median : min/max/median non-rejection across parameters
#   - ARE_robust_vs_empirical :
#         det( Cov_empirical(β̂_indep) ) / det( Cov_empirical(β̂_working) )
#
# Dependencies
#   parallel   : detectCores, makeCluster, clusterEvalQ, clusterExport,
#                parLapply, stopCluster
#   matrixgee  : matrixgee::matrixgee_cpp_nokron
#   MASS       : MASS::mvrnorm (used by datagen_nokron)
#
# Example 
#   library(parallel)
#   set.seed(1)
#   N <- 10; r <- 6; c <- 5; q <- 3
#   a_R <- rnorm(r)
#   B   <- matrix(c(0.5, -0.3, 0.2), nrow = q)  
#   covs <- rnorm(N*q)
#   J <- simfct_nokron_parallel(sample_size = N, rows_no = r, cols_no = c,
#                                 rho = 0.25,
#                                 covariates = covs,
#                                 intercept = "row_intercept",
#                                 max_iter = 20, corstr = "exchangeable",
#                                 tol = 1e-6, family = NULL, n_sim = 50, alpha = 0.05,
#                                 a_R = a_R, B = B)
#   str(J)
#
# Notes
#   - `family` and `alpha` are currently unused;They are fixed in the main package.
#      we keep them for future use.
# =============================================================================




library(parallel)


simfct_nokron_parallel <-  function(sample_size, rows_no, cols_no, rho,  covariates, intercept, max_iter,
                                    corstr, tol, family, n_sim, alpha, a_R, B) {
  has_covariates <- !is.null(covariates) && !is.null(B)
  true_params <- if (has_covariates) c(a_R, as.vector(B)) else a_R
  parameters_no <- length(true_params)
  
  
  no_cores <- detectCores() - 1
  cl <- makeCluster(no_cores)
  clusterEvalQ(cl, {
    library(matrixgee)
    library(MASS)
  })
  
  
  clusterExport(cl, varlist = c("datagen_nokron", "sample_size", "rows_no", "cols_no", "rho",
                                "a_R", "covariates", "intercept", "max_iter", "corstr", "tol",
                                "B", "true_params", "parameters_no","has_covariates","matrixgee_cpp_nokron"), envir =  environment())
  
  sim_results <- parLapply(cl, 1:n_sim, function(n) {
    
    data_sim <- datagen_nokron(sample_size, rows_no, cols_no, rho, a_R, covariates, B)
    #long_d <- data_sim$y
    model <- matrixgee::matrixgee_cpp_nokron(data_sim, covariates, intercept, sample_size,
                                             rows_no, cols_no, max_iter, tol,
                                             corstr)
    
    estimates <- as.numeric(model$beta_hat)
    
    bias <- estimates - true_params
    frobenius <- sqrt(sum(bias^2))
    
    var_result <- model$robust_covariance
    var_betas <- sqrt(diag(var_result))
    se <- var_betas
    sd <- var_betas^2
    
    independent_model <- matrixgee::matrixgee_cpp_nokron(data_sim, covariates, intercept, sample_size,
                                                         rows_no, cols_no, max_iter, tol,
                                                         corstr = "independence")
    var_indp <- independent_model$robust_covariance
    estimates_indep <- independent_model$beta_hat
    are <- diag(var_indp) / diag(var_result)
    ARE_det <- det(var_indp) / det(var_result)
    
    wald <- (estimates - true_params)^2 / sd
    pvals <- 1 - pchisq(wald, df = 1)
    
    list(estimates = estimates,
         estimates_indep = estimates_indep,
         bias = bias,
         frobenius = frobenius,
         se = se,
         var = sd,
         are = are,
         ARE_det = ARE_det,
         pvals = pvals,
         robust_cov = model$robust_covariance,
         robust_cov_indep = independent_model$robust_covariance,
         estimates_indep = independent_model$beta_hat)
    
  })
  
  stopCluster(cl)
  
  extract_mat <- function(name) do.call(cbind, lapply(sim_results, `[[`, name))
  ### RMSE per parameter then average across parameters
  estimates_mat <- extract_mat("estimates") 
  errors_sq <- (estimates_mat - matrix(true_params, nrow = parameters_no, ncol = n_sim))^2
  rmse_per_param <- sqrt(rowMeans(errors_sq))  
  method1_total <- mean(rmse_per_param) 
  
  ### Bias method 2
  averaged_estimates <- rowMeans(estimates_mat)
  method2_bias_frob <- sqrt(sum((averaged_estimates - true_params)^2))
  
  
  averaged_bias <- rowMeans(extract_mat("bias"))
  averaged_se <- rowMeans(extract_mat("se"))
  averaged_var <- rowMeans(extract_mat("var"))
  averaged_are <- rowMeans(extract_mat("are"))
  frobenius_all <- sapply(sim_results, `[[`, "frobenius")
  ARE_det_all <- sapply(sim_results, `[[`, "ARE_det")
  ###P-values 
  p_matrix <- extract_mat("pvals")
  no_reject_H0_freq <- rowMeans(p_matrix > 0.05)
  min_freq <- min(no_reject_H0_freq)
  max_freq <- max(no_reject_H0_freq)
  median_freq <- median(no_reject_H0_freq)
  
  overall_non_rejection_freq <- mean(no_reject_H0_freq)
  ##MAE
  abs_bias_mat <- abs(estimates_mat - matrix(true_params, nrow = parameters_no, ncol = n_sim))
  mae_per_param <- rowMeans(abs_bias_mat) 
  MAE <- mean(mae_per_param)              
  
  
  #MSE 
  errors_sq <- (estimates_mat - matrix(true_params, nrow = parameters_no, ncol = n_sim))^2
  mse_per_param <- rowMeans(errors_sq) 
  overall_MSE <- mean(mse_per_param) 
  
  
  
  
  
  estimates_df <- as.data.frame(t(extract_mat("estimates")))  
  estimates_indp_df <- as.data.frame(t(extract_mat("estimates_indep")))
  empirical_cov <- cov(estimates_df)  
  indp_cov <- cov(estimates_indp_df)
  ARE_empirical <- det(indp_cov)/det(empirical_cov)
  
  
  
  
  
  
  
  
  
  
  list(
    ARE_det_calc_averaged = mean(ARE_det_all),
    Average_RMSE = method1_total,
    Frob_method2 = method2_bias_frob,
    MAE = MAE,
    MSE = overall_MSE,
    p_freq = overall_non_rejection_freq,
    p_freq_min = min_freq,
    p_freq_max = max_freq,
    p_freq_median = median_freq,
    ARE_robust_vs_empirical = ARE_empirical
    
  )
}



####Non parallel fct
simfct_nokron_seq <- function(sample_size, rows_no, cols_no, rho, covariates, intercept, max_iter,
                              corstr, tol, family, n_sim, alpha, a_R, B) {
  has_covariates <- !is.null(covariates) && !is.null(B)
  true_params <- if (has_covariates) c(a_R, as.vector(B)) else a_R
  parameters_no <- length(true_params)
  
  sim_results <- vector("list", n_sim)
  
  for (n in 1:n_sim) {
    data_sim <- datagen_nokron(sample_size, rows_no, cols_no, rho, a_R, covariates, B)
    print(is.matrix(data_sim))
    
    #y_matrix <- data_sim$t
    #y_matrix <- matrix(as.numeric(y_matrix), nrow = nrow(y_matrix), ncol = ncol(y_matrix))
    #colnames(y_matrix) <- NULL
    #print(class(y_matrix))
    #print(dim(y_matrix))
    #print(head(y_matrix))
    
    model <- matrixgee::matrixgee_cpp_nokron(data_sim, covariates, intercept, sample_size,
                                             rows_no, cols_no, max_iter, tol,
                                             corstr)
    
    estimates <- as.numeric(model$beta_hat)
    bias <- estimates - true_params
    frobenius <- sqrt(sum(bias^2))
    
    var_result <- model$robust_covariance
    var_betas <- sqrt(diag(var_result))
    se <- var_betas
    sd <- var_betas^2
    
    independent_model <- matrixgee::matrixgee_cpp_nokron(data_sim, covariates, intercept, sample_size,
                                                         rows_no, cols_no, max_iter, tol,
                                                         corstr = "independence")
    var_indp <- independent_model$robust_covariance
    estimates_indep <- independent_model$beta_hat
    are <- diag(var_indp) / diag(var_result)
    ARE_det <- det(var_indp) / det(var_result)
    
    wald <- (estimates - true_params)^2 / sd
    pvals <- 1 - pchisq(wald, df = 1)
    
    sim_results[[n]] <- list(
      estimates = estimates,
      estimates_indep = estimates_indep,
      bias = bias,
      frobenius = frobenius,
      se = se,
      var = sd,
      are = are,
      ARE_det = ARE_det,
      pvals = pvals,
      robust_cov = model$robust_covariance,
      robust_cov_indep = independent_model$robust_covariance
    )
  }
  
  extract_mat <- function(name) do.call(cbind, lapply(sim_results, `[[`, name))
  
  estimates_mat <- extract_mat("estimates") 
  errors_sq <- (estimates_mat - matrix(true_params, nrow = parameters_no, ncol = n_sim))^2
  rmse_per_param <- sqrt(rowMeans(errors_sq))  
  method1_total <- mean(rmse_per_param)
  
  averaged_estimates <- rowMeans(estimates_mat)
  method2_bias_frob <- sqrt(sum((averaged_estimates - true_params)^2))
  
  averaged_bias <- rowMeans(extract_mat("bias"))
  averaged_se <- rowMeans(extract_mat("se"))
  averaged_var <- rowMeans(extract_mat("var"))
  averaged_are <- rowMeans(extract_mat("are"))
  frobenius_all <- sapply(sim_results, `[[`, "frobenius")
  ARE_det_all <- sapply(sim_results, `[[`, "ARE_det")
  
  p_matrix <- extract_mat("pvals")
  no_reject_H0_freq <- rowMeans(p_matrix > 0.05)
  min_freq <- min(no_reject_H0_freq)
  max_freq <- max(no_reject_H0_freq)
  median_freq <- median(no_reject_H0_freq)
  overall_non_rejection_freq <- mean(no_reject_H0_freq)
  
  abs_bias_mat <- abs(estimates_mat - matrix(true_params, nrow = parameters_no, ncol = n_sim))
  mab_per_param <- rowMeans(abs_bias_mat) 
  MAB <- mean(mab_per_param)
  
  mse_per_param <- rowMeans(errors_sq) 
  overall_MSE <- mean(mse_per_param) 
  
  estimates_df <- as.data.frame(t(estimates_mat))  
  estimates_indp_df <- as.data.frame(t(extract_mat("estimates_indep")))
  empirical_cov <- cov(estimates_df)  
  indp_cov <- cov(estimates_indp_df)
  ARE_empirical <- det(indp_cov)/det(empirical_cov)
  
  list(
    ARE_det_calc_averaged = mean(ARE_det_all),
    Average_RMSE = method1_total,
    Frob_method2 = method2_bias_frob,
    MAB = MAB,
    MSE = overall_MSE,
    p_freq = overall_non_rejection_freq,
    p_freq_min = min_freq,
    p_freq_max = max_freq,
    p_freq_median = median_freq,
    ARE_robust_vs_empirical = ARE_empirical
  )
}








