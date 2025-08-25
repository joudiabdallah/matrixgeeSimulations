# =============================================================================
# simfct_parallel()
# -----------------------------------------------------------------------------
# Purpose
#   Run a parallel Monte Carlo study for the matrix-valued GEE estimator
#   implemented in matrixgee::matrixgee_cpp. For each replicate:
#     1) Simulate Y using `datagen()` (Kronecker model with row-Exch and col-AR(1)).
#     2) Fit the GEE with user-specified (corstr_rows, corstr_cols).
#     3) Fit an independence GEE (rows=Indep, cols=Indep).
#     4) Collect parameter estimates, robust covariance, Wald p-values, and
#        efficiency metrics.
#
# Data model in generation
#   The generator `datagen()` is expected to produce, for subject i:
#     M_i = a_R 1_c^T + 1_r (B^T x_i) 1_c^T 
#   and a matrix-normal error with Σ_R(ρ) (exchangeable) and Σ_C(φ) (AR(1)).
#
# Parallelization
#   Uses clusters via parallel::{detectCores, makeCluster, parLapply}.
#   Each worker loads `matrixgee`.
#
# Arguments
#   sample_size  : integer N ≥ 1. Number of matrices in each replicate.
#   rows_no      : integer r ≥ 1. Rows of each matrix response.
#   cols_no      : integer c ≥ 1. Columns of each matrix response.
#   rho          : numeric. Row exchangeable correlation parameter used in datagen().
#   phi          : numeric. Column AR(1) correlation parameter used in datagen().
#   covariates   : numeric vector of length N*q (stacked x_1,…,x_N) or NULL.
#   intercept    : character/string. Intercept type expected by matrixgee_cpp
#                  ("row_intercept", "column_intercept", ...).
#   max_iter     : integer. Maximum iterations in matrixgee_cpp.
#   corstr_rows  : character. Working correlation for rows in the fitted GEE
#                  ("exchangeable", "ar1", "independence").
#   corstr_cols  : character. Working correlation for cols in the fitted GEE
#                  ("exchangeable", "ar1", "independence").
#   tol          : numeric. Convergence tolerance in matrixgee_cpp.
#   family       : Placeholder for future GLM families. Unused for now.
#   n_sim        : integer S ≥ 1. Number of Monte Carlo replicates.
#   alpha        : Nominal level for tests; fixed at 0.05 inside. Unused for now.
#   a_R          : numeric length r. Row-intercept vector passed to datagen().
#   B            : numeric q×1. Covariate coefficients
#                  used by datagen(); if NULL (or covariates NULL), no covariates.
#
# Shapes 
#   - datagen() must return a list with $y = Y_big of size r × (N*c), where
#     block columns ((i−1)*c+1):(i*c) correspond to subject i.
#   - matrixgee::matrixgee_cpp(long_d, covariates, intercept, sample_size,
#       rows_no, cols_no, max_iter, tol, corstr_rows, corstr_cols)
#     should return a list with:
#         $beta_hat            : numeric vector of length r+q  (parameter numbers)
#         $robust_covariance   : rcxrc robust (sandwich) covariance matrix
#
# Metrics computed per replicate
#   Let θ* be the true parameters (a_R, vec(B))
#     - Bias: β̂ − θ*
#     - Frobenius of bias vector: ||β̂ − θ*||_2
#     - Robust SEs: sqrt(diag(robust_cov))
#     - Wald p-values: for each component j, (β̂_j − θ*_j)^2 / Var_j  ~ χ^2_1
#     - ARE (per-parameter): diag(robust_cov_indep) / diag(robust_cov_working)
#       (> 1 indicates efficiency gain over independence)
#     - ARE_det: det(robust_cov_indep) / det(robust_cov_working)
#
# Aggregate summaries across replicates: For full formulas check simulation chapter
#   - ARE_det_calc_averaged : mean of ARE_det over replicates.
#   - Average_RMSE          : "method 1" RMSE computed per parameter across sims
#                             then averaged across parameters.
#   - Frob_method2          : Frobenius norm of (avg(β̂) − θ*)  ("method 2" bias).
#   - MAE                   : Mean Absolute error — avg over parameters of
#                             avg_i |β̂_i − θ*_i|.
#   - MSE                   : Mean Squared Error — avg over parameters of
#                             avg_i (β̂_i − θ*_i)^2.
#   - p_freq                : Overall non-rejection frequency at 0.05
#                             (averaged over parameters).
#   - p_freq_min/max/median : Min/Max/Median non-rejection frequency across params.
#   - ARE_robust_vs_empirical :
#         det( Cov_empirical(β̂_indep) ) / det( Cov_empirical(β̂_working) )
#
# Dependencies
#   parallel      : detectCores, makeCluster, clusterEvalQ, clusterExport,
#                   parLapply, stopCluster
#   matrixgee     : matrixgee::matrixgee_cpp 
#   datagen       : must be defined 
#
# Example
#   library(parallel)
#   # assume `datagen()` is defined in the same script
#   set.seed(1)
#   N <- 10; r <- 5; c <- 4; q <- 3
#   a_R <- rnorm(r)
#   B   <- matrix(c(0.5, -0.3, 0.2), nrow = q)
#   covs <- rnorm(N*q)
#   J <- simfct_parallel(sample_size = N, rows_no = r, cols_no = c,
#                          rho = 0.2, phi = 0.5, covariates = covs,
#                          intercept = "row_intercept",
#                          max_iter = 20, corstr_rows = "exchangeable",
#                          corstr_cols = "ar1", tol = 1e-6,
#                          family = NULL, n_sim = 50, alpha = 0.05,
#                          a_R = a_R, B = B)
#   str(J)
#
# Notes 
#   - `family` and `alpha` are currently unused in the code; We define them
#      for future use when we can alternate the family function and alpha value.
# =============================================================================



library(parallel)

simfct_parallel <- function(sample_size, rows_no, cols_no, rho, phi, covariates, intercept, max_iter,
                            corstr_rows, corstr_cols, tol, family, n_sim, alpha, a_R, B) {
  has_covariates <- !is.null(covariates) && !is.null(B)
  true_params <- if (has_covariates) c(a_R, as.vector(B)) else a_R
  parameters_no <- length(true_params)
  
  
  
  
  no_cores <- detectCores() - 1
  cl <- makeCluster(no_cores)
  clusterEvalQ(cl, {
    library(matrixgee)
  })
  
  clusterExport(cl, varlist = c("datagen", "sample_size", "rows_no", "cols_no", "rho", "phi",
                                "a_R", "covariates", "intercept", "max_iter", "corstr_rows", "corstr_cols", "tol",
                                "B", "true_params", "parameters_no","has_covariates"), envir = environment())
  
  sim_results <- parLapply(cl, 1:n_sim, function(n) {
    data_sim <- datagen(sample_size, rows_no, cols_no, rho, phi, a_R, covariates, B)

    long_d <- data_sim$y
    
    model <- matrixgee::matrixgee_cpp(long_d, covariates, intercept, sample_size,
                                      rows_no, cols_no, max_iter, tol,
                                      corstr_rows, corstr_cols)
    
    estimates <- model$beta_hat
    bias <- estimates - true_params
    frobenius <- sqrt(sum(bias^2))
    
    var_result <- model$robust_covariance
    var_betas <- sqrt(diag(var_result))
    se <- var_betas
    sd <- var_betas^2
    
    independent_model <- matrixgee::matrixgee_cpp(long_d, covariates, intercept, sample_size,
                                                  rows_no, cols_no, max_iter, tol,
                                                  corstr_rows = "independence",
                                                  corstr_cols = "independence")
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
  #print(estimates_mat)
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
### Non-parallel fct
simfunction <- function(sample_size, rows_no, cols_no, rho, phi, covariates, intercept, max_iter,
                        corstr_rows, corstr_cols, tol, family, n_sim, alpha, a_R, B) {
  
  has_covariates <- !is.null(covariates) && !is.null(B)
  true_params <- if (has_covariates) c(a_R, as.vector(B)) else a_R
  parameters_no <- length(true_params)
  
  
  sim_results <- vector("list", n_sim)
  
  for (n in 1:n_sim) {
    data_sim <- datagen(sample_size, rows_no, cols_no, rho, phi, a_R, covariates, B)
    long_d <- data_sim$y
    
    model <- matrixgee::matrixgee_cpp(long_d, covariates, intercept, sample_size,
                                      rows_no, cols_no, max_iter, tol,
                                      corstr_rows, corstr_cols)
    
    estimates <- model$beta_hat
    print(estimates)
    bias <- estimates - true_params
    frobenius <- sqrt(sum(bias^2))
    
    var_result <- model$robust_covariance
    var_betas <- sqrt(diag(var_result))
    se <- var_betas
    sd <- var_betas^2
    
    independent_model <- matrixgee::matrixgee_cpp(long_d, covariates, intercept, sample_size,
                                                  rows_no, cols_no, max_iter, tol,
                                                  corstr_rows = "independence",
                                                  corstr_cols = "independence")
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
  
  estimates_df <- as.data.frame(t(extract_mat("estimates")))
  estimates_indp_df <- as.data.frame(t(extract_mat("estimates_indep")))
  empirical_cov <- cov(estimates_df)
  indp_cov <- cov(estimates_indp_df)
  ARE_empirical <- det(indp_cov) / det(empirical_cov)
  
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
  
  
  
  
  
  
  
  
  
  
  

