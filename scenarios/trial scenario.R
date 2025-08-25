library(foreach)
library(parallel)
library(iterators)
library(doParallel)
source("set-up/set_up_design_kron.R")
rows_no <- rows_small
a_R_1 <- a_R_const(rows_no)

row_corrs <- c("independence", "Exchangeable", "ar1")
col_corrs <- c("independence", "Exchangeable", "ar1")
combinations <- expand.grid(row = row_corrs, col = col_corrs, stringsAsFactors = FALSE)

num_cores <- detectCores() - 1
cl <- makeCluster(num_cores)
registerDoParallel(cl)



results <- foreach(i = 1:nrow(combinations), .packages = c("matrixgee", "tibble")) %dopar% {
  row_corr <- combinations$row[i]
  col_corr <- combinations$col[i]
  result <- simfunction(
    sample_size = N_small,
    rows_no = rows_small,
    cols_no = cols_small,
    rho = 0.3,
    phi = 0.3,
    n_sim = 10000,
    covariates = p_50,
    intercept = "row_intercept",
    max_iter = 20,
    tol = 1e-6,
    corstr_rows = row_corr,
    corstr_cols = col_corr,
    family = gaussian(),
    alpha = 0.05,
    a_R = a_R_1,
    B = B
  )
  
  file_name <- paste0("results/scenario_1_A_", row_corr, "_", col_corr, ".rds")
  saveRDS(result, file = file_name)
  
  # Return summary info for table
  tibble(
    RowCorr = row_corr,
    ColCorr = col_corr,
    SampleSize = "N=50",
    ARE = result$ARE_det_calc_averaged,
    RMSE = result$Average_RMSE,
    Frob = result$Frob_method2,
    MAB = result$MAB,
    MSE = result$MSE,
    Errmean = result$p_freq,
    MinErr = result$p_freq_min,
    MaxErr = result$p_freq_max,
    MedianErr = result$p_freq_median,
    AREemp = result$ARE_robust_vs_empirical
  )
}



A_summary_50 <- bind_rows(results)
stopCluster(cl)


