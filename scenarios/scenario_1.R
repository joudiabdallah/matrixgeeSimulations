# scenario_1:  a_R = constant, r = 15, c = 5, rho = 0.3, phi = 0.3, covers case 1,3
# and 5
library(matrixgee)
library(dplyr)
library(stringr)
source("set-up/set_up_design_kron.R")
rows_no_1 <- rows_small
a_R_1 <- a_R_const(rows_no)
############################## N_small#########################




##independence, independence
system.time ({
  A11 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_1, B=B)
})
#Save results
saveRDS(A11, file = "results/scenario_1_A11.rds")
#Read results
A11 <- readRDS("results/scenario_1_A11.rds")
A11


##independence, ar1
system.time ({
  A12 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_1, B=B)
})
#Save results
saveRDS(A12, file = "results/scenario_1_A12.rds")
#Read results
A12 <- readRDS("results/scenario_1_A12.rds")
A12




##independence, Exchangeable
system.time ({
  A13 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_1, B=B)
})
#Save results
saveRDS(A13, file = "results/scenario_1_A13.rds")
#Read results
A13 <- readRDS("results/scenario_1_A13.rds")
A13





##Exchangeable, ar1
system.time ({
  A14 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_1, B=B)
})
#Save results
saveRDS(A14, file = "results/scenario_1_A14.rds")
#Read results
A14 <- readRDS("results/scenario_1_A14.rds")
A14



##Exchangeable, Exchangeable
system.time ({
  A15 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_1, B=B)
})
#Save results
saveRDS(A15, file = "results/scenario_1_A15.rds")
#Read results
A15 <- readRDS("results/scenario_1_A15.rds")
A15


##ar1, Exchangeable
system.time ({
  A16 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_1, B=B)
})
#Save results
saveRDS(A16, file = "results/scenario_1_A16.rds")
#Read results
A16 <- readRDS("results/scenario_1_A16.rds")
A16


##Exchangeable, independence
system.time ({
  A17 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_1, B=B)
})
#Save results
saveRDS(A17, file = "results/scenario_1_A17.rds")
#Read results
A17 <- readRDS("results/scenario_1_A17.rds")
A17






##ar1, independence
system.time ({
  A18 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_1, B=B)
})
#Save results
saveRDS(A18, file = "results/scenario_1_A18.rds")
#Read results
A18 <- readRDS("results/scenario_1_A18.rds")
A18




extract_results <- function(file_path, row_corr, col_corr, N) {
  result <- readRDS(file_path)
  
  tibble(
    RowCorr = row_corr,
    ColCorr = col_corr,
    SampleSize = paste0("N=", N),
    ARE = result$ARE_det_calc_averaged,
    RMSE = result$Average_RMSE,
    Frob = result$Frob,
    MAB = result$MAB,
    MSE = result$MSE,
    Errmean = result$p_freq,
    MinErr = result$p_freq_min,
    MaxErr = result$p_freq_max,
    MedianErr = result$p_freq_median,
    AREemp = result$ARE_robust_vs_empirical
  )
}



A_summary_50 <- bind_rows(
  extract_results("results/scenario_1_A11.rds", "independence", "independence", N=50),
  extract_results("results/scenario_1_A12.rds", "independence", "ar1", N=50),
  extract_results("results/scenario_1_A13.rds", "independence", "Exchangeable", N=50),
  extract_results("results/scenario_1_A14.rds", "Exchangeable", "ar1", N=50),
  extract_results("results/scenario_1_A15.rds", "Exchangeable", "Exchangeable", N=50),
  extract_results("results/scenario_1_A16.rds", "ar1", "Exchangeable", N=50),
  extract_results("results/scenario_1_A17.rds", "Exchangeable", "independence", N=50),
  extract_results("results/scenario_1_A18.rds", "ar1", "independence", N=50)
)
A_summary_50










############################## N_medium #########################
##independence, independence
system.time ({
  A21 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_1, B=B)
})
#Save results
saveRDS(A21, file = "results/scenario_1_A21.rds")
#Read results
A21 <- readRDS("results/scenario_1_A21.rds")
A21

##independence, ar1
system.time ({
  A22 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_1, B=B)
})
#Save results
saveRDS(A22, file = "results/scenario_1_A22.rds")
#Read results
A22 <- readRDS("results/scenario_1_A22.rds")
A22



##independence, Exchangeable
system.time ({
  A23 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_1, B=B)
})
#Save results
saveRDS(A23, file = "results/scenario_1_A23.rds")
#Read results
A23 <- readRDS("results/scenario_1_A23.rds")
A23






##Exchangeable, ar1
system.time ({
  A24 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_1, B=B)
})
#Save results
saveRDS(A24, file = "results/scenario_1_A24.rds")
#Read results
A24 <- readRDS("results/scenario_1_A24.rds")
A24





##Exchangeable, Exchangeable
system.time ({
  A25 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_1, B=B)
})
#Save results
saveRDS(A25, file = "results/scenario_1_A25.rds")
#Read results
A25 <- readRDS("results/scenario_1_A25.rds")
A25




## ar1, Exchangeable
system.time ({
  A26 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_1, B=B)
})
#Save results
saveRDS(A26, file = "results/scenario_1_A26.rds")
#Read results
A26 <- readRDS("results/scenario_1_A26.rds")
A26



##Exchangeable, independence
system.time ({
  A27 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_1, B=B)
})
#Save results
saveRDS(A27, file = "results/scenario_1_A27.rds")
#Read results
A27 <- readRDS("results/scenario_1_A27.rds")
A27



##ar1, independence
system.time ({
  A28 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_1, B=B)
})
#Save results
saveRDS(A28, file = "results/scenario_1_A28.rds")
#Read results
A28 <- readRDS("results/scenario_1_A28.rds")
A28


A_summary_100 <- bind_rows(
  extract_results("results/scenario_1_A21.rds", "independence", "independence", N=100),
  extract_results("results/scenario_1_A22.rds", "independence", "ar1", N=100),
  extract_results("results/scenario_1_A23.rds", "independence", "Exchangeable", N=100),
  extract_results("results/scenario_1_A24.rds", "Exchangeable", "ar1", N=100),
  extract_results("results/scenario_1_A25.rds", "Exchangeable", "Exchangeable", N=100),
  extract_results("results/scenario_1_A26.rds", "ar1", "Exchangeable", N=100),
  extract_results("results/scenario_1_A27.rds", "Exchangeable", "independence", N=100),
  extract_results("results/scenario_1_A28.rds", "ar1", "independence", N=100)
)
A_summary_100















############################## N_large #########################
##independence, independence
system.time ({
  A31 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_1, B=B)
})
#Save results
saveRDS(A31, file = "results/scenario_1_A31.rds")
#Read results
A31 <- readRDS("results/scenario_1_A31.rds")
A31

##independence, ar1
system.time ({
  A32 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_1, B=B)
})
#Save results
saveRDS(A32, file = "results/scenario_1_A32.rds")
#Read results
A32 <- readRDS("results/scenario_1_A32.rds")
A32



##independence, Exchangeable
system.time ({
  A33 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_1, B=B)
})
#Save results
saveRDS(A33, file = "results/scenario_1_A33.rds")
#Read results
A33 <- readRDS("results/scenario_1_A33.rds")
A33






##Exchangeable, ar1
system.time ({
  A34 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_1, B=B)
})
#Save results
saveRDS(A34, file = "results/scenario_1_A34.rds")
#Read results
A34 <- readRDS("results/scenario_1_A34.rds")
A34





##Exchangeable, Exchangeable
system.time ({
  A35 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_1, B=B)
})
#Save results
saveRDS(A35, file = "results/scenario_1_A35.rds")
#Read results
A35 <- readRDS("results/scenario_1_A35.rds")
A35




## ar1, Exchangeable
system.time ({
  A36 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_1, B=B)
})
#Save results
saveRDS(A36, file = "results/scenario_1_A36.rds")
#Read results
A36 <- readRDS("results/scenario_1_A36.rds")
A36



##Exchangeable, independence
system.time ({
  A37 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_1, B=B)
})
#Save results
saveRDS(A37, file = "results/scenario_1_A37.rds")
#Read results
A37 <- readRDS("results/scenario_1_A37.rds")
A37


##ar1, independence
system.time ({
  A38 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_1, B=B)
})
#Save results
saveRDS(A38, file = "results/scenario_1_A38.rds")
#Read results
A38 <- readRDS("results/scenario_1_A38.rds")
A38



A_summary_500 <- bind_rows(
  extract_results("results/scenario_1_A31.rds", "independence", "independence", N=500),
  extract_results("results/scenario_1_A32.rds", "independence", "ar1", N=500),
  extract_results("results/scenario_1_A33.rds", "independence", "Exchangeable", N=500),
  extract_results("results/scenario_1_A34.rds", "Exchangeable", "ar1", N=500),
  extract_results("results/scenario_1_A35.rds", "Exchangeable", "Exchangeable", N=500),
  extract_results("results/scenario_1_A36.rds", "ar1", "Exchangeable", N=500),
  extract_results("results/scenario_1_A37.rds", "Exchangeable", "independence", N=500),
  extract_results("results/scenario_1_A38.rds", "ar1", "independence", N=500)
)
A_summary_500






A_all <- bind_rows(A_summary_50 %>% mutate(N = 50),
                   A_summary_100 %>% mutate(N = 100),
                   A_summary_500 %>% mutate(N = 500))
latex_table <- A_all %>%
  arrange(RowCorr, ColCorr, N) %>%
  mutate(SampleSize = paste0("N=", N)) %>%
  select(RowCorr, ColCorr, SampleSize, 
         ARE, RMSE, Frob, MAB, MSE, Errmean,
         MinErr, MaxErr, MedianErr, AREemp) %>%
  group_by(RowCorr, ColCorr) %>%
  mutate(
    RowCorr = replace(RowCorr, row_number() != 1, ""),
    ColCorr = replace(ColCorr, row_number() != 1, "")
  ) %>%
  ungroup()

#latex_table[, 4:ncol(latex_table)] <- round(latex_table[, 4:ncol(latex_table)], 4)


#latex_table <- latex_table %>%
#mutate(across(where(is.numeric), ~ round(.x, 4)))
#latex_table[, 4:ncol(latex_table)] <- format(round(latex_table[, 4:ncol(latex_table)], 4), nsmall = 4)
latex_table <- latex_table %>%
  mutate(across(
    .cols = 4:ncol(.),
    .fns = ~ format(round(.x, 4), nsmall = 4),
    .names = "{.col}"
  ))


xtab <- xtable(latex_table,
               caption = "Simulation Results Across Sample Sizes",
               label = "tab:sim-results")


#print(xtab,
#include.rownames = FALSE,
#sanitize.colnames.function = identity,
#caption.placement = "top",
#booktabs = TRUE)

cat("\\begin{flushleft}\n")

print(xtab,
      include.rownames = FALSE,
      booktabs = TRUE,
      caption.placement = "top",
      sanitize.colnames.function = identity,
      add.to.row = list(pos = list(0), command = "\\begin{flushleft}\\scriptsize\n"),
      table.placement = "H")


cat("\\end{flushleft}\n")








