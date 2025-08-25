# scenario_2:  a_R  increasing, r = 15, c = 5, rho = 0.3, phi = 0.3 covers case 2,
#              4, and 6

library(dplyr)
library(stringr)
source("set-up/set_up_design_kron.R")
rows_no_2 <- rows_small
a_R_2 <- a_R_linear(rows_no_2)
############################## N_small#########################




##independence, independence
system.time ({
  B11 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_2, B=B)
})
#Save results
saveRDS(B11, file = "results/scenario_2_B11.rds")
#Read results
B11 <- readRDS("results/scenario_2_B11.rds")
B11


##independence, ar1
system.time ({
  B12 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_2, B=B)
})
#Save results
saveRDS(B12, file = "results/scenario_2_B12.rds")
#Read results
B12 <- readRDS("results/scenario_2_B12.rds")
B12




##independence, Exchangeable
system.time ({
  B13 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_2, B=B)
})
#Save results
saveRDS(B13, file = "results/scenario_2_B13.rds")
#Read results
B13 <- readRDS("results/scenario_2_B13.rds")
B13





##Exchangeable, ar1
system.time ({
  B14 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_2, B=B)
})
#Save results
saveRDS(B14, file = "results/scenario_2_B14.rds")
#Read results
B14 <- readRDS("results/scenario_2_B14.rds")
B14



##Exchangeable, Exchangeable
system.time ({
  B15 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_2, B=B)
})
#Save results
saveRDS(B15, file = "results/scenario_2_B15.rds")
#Read results
B15 <- readRDS("results/scenario_2_B15.rds")
B15


##ar1, Exchangeable
system.time ({
  B16 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_2, B=B)
})
#Save results
saveRDS(B16, file = "results/scenario_2_B16.rds")
#Read results
B16 <- readRDS("results/scenario_2_B16.rds")
B16


##Exchangeable, independence
system.time ({
  B17 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_2, B=B)
})
#Save results
saveRDS(B17, file = "results/scenario_2_B17.rds")
#Read results
B17 <- readRDS("results/scenario_2_B17.rds")
B17






##ar1, independence
system.time ({
  B18 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_2, B=B)
})
#Save results
saveRDS(B18, file = "results/scenario_2_B18.rds")
#Read results
B18 <- readRDS("results/scenario_2_B18.rds")
B18




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



B_summary_50 <- bind_rows(
  extract_results("results/scenario_2_B11.rds", "independence", "independence", N=50),
  extract_results("results/scenario_2_B12.rds", "independence", "ar1", N=50),
  extract_results("results/scenario_2_B13.rds", "independence", "Exchangeable", N=50),
  extract_results("results/scenario_2_B14.rds", "Exchangeable", "ar1", N=50),
  extract_results("results/scenario_2_B15.rds", "Exchangeable", "Exchangeable", N=50),
  extract_results("results/scenario_2_B16.rds", "ar1", "Exchangeable", N=50),
  extract_results("results/scenario_2_B17.rds", "Exchangeable", "independence", N=50),
  extract_results("results/scenario_2_B18.rds", "ar1", "independence", N=50)
)
B_summary_50










############################## N_medium #########################
##independence, independence
system.time ({
  B21 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_2, B=B)
})
#Save results
saveRDS(B21, file = "results/scenario_2_B21.rds")
#Read results
B21 <- readRDS("results/scenario_2_B21.rds")
B21

##independence, ar1
system.time ({
  B22 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_2, B=B)
})
#Save results
saveRDS(B22, file = "results/scenario_2_B22.rds")
#Read results
B22 <- readRDS("results/scenario_2_B22.rds")
B22



##independence, Exchangeable
system.time ({
  B23 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_2, B=B)
})
#Save results
saveRDS(B23, file = "results/scenario_2_B23.rds")
#Read results
B23 <- readRDS("results/scenario_2_B23.rds")
B23






##Exchangeable, ar1
system.time ({
  B24 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_2, B=B)
})
#Save results
saveRDS(B24, file = "results/scenario_2_B24.rds")
#Read results
B24 <- readRDS("results/scenario_2_B24.rds")
B24





##Exchangeable, Exchangeable
system.time ({
  B25 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_2, B=B)
})
#Save results
saveRDS(B25, file = "results/scenario_2_B25.rds")
#Read results
B25 <- readRDS("results/scenario_2_B25.rds")
B25




## ar1, Exchangeable
system.time ({
  B26 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_2, B=B)
})
#Save results
saveRDS(B26, file = "results/scenario_2_B26.rds")
#Read results
B26 <- readRDS("results/scenario_2_B26.rds")
B26



##Exchangeable, independence
system.time ({
  B27 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_2, B=B)
})
#Save results
saveRDS(B27, file = "results/scenario_2_B27.rds")
#Read results
B27 <- readRDS("results/scenario_2_B27.rds")
B27



##ar1, independence
system.time ({
  B28 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_2, B=B)
})
#Save results
saveRDS(B28, file = "results/scenario_2_B28.rds")
#Read results
B28 <- readRDS("results/scenario_2_B28.rds")
B28


B_summary_100 <- bind_rows(
  extract_results("results/scenario_2_B21.rds", "independence", "independence", N=100),
  extract_results("results/scenario_2_B22.rds", "independence", "ar1", N=100),
  extract_results("results/scenario_2_B23.rds", "independence", "Exchangeable", N=100),
  extract_results("results/scenario_2_B24.rds", "Exchangeable", "ar1", N=100),
  extract_results("results/scenario_2_B25.rds", "Exchangeable", "Exchangeable", N=100),
  extract_results("results/scenario_2_B26.rds", "ar1", "Exchangeable", N=100),
  extract_results("results/scenario_2_B27.rds", "Exchangeable", "independence", N=100),
  extract_results("results/scenario_2_B28.rds", "ar1", "independence", N=100)
)
B_summary_100















############################## N_large #########################
##independence, independence
system.time ({
  B31 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_2, B=B)
})
#Save results
saveRDS(B31, file = "results/scenario_2_B31.rds")
#Read results
B31 <- readRDS("results/scenario_2_B31.rds")
B31

##independence, ar1
system.time ({
  B32 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_2, B=B)
})
#Save results
saveRDS(B32, file = "results/scenario_2_B32.rds")
#Read results
B32 <- readRDS("results/scenario_2_B32.rds")
B32



##independence, Exchangeable
system.time ({
  B33 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_2, B=B)
})
#Save results
saveRDS(B33, file = "results/scenario_2_B33.rds")
#Read results
B33 <- readRDS("results/scenario_2_B33.rds")
B33






##Exchangeable, ar1
system.time ({
  B34 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_2, B=B)
})
#Save results
saveRDS(B34, file = "results/scenario_2_B34.rds")
#Read results
B34 <- readRDS("results/scenario_2_B34.rds")
B34





##Exchangeable, Exchangeable
system.time ({
  B35 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_2, B=B)
})
#Save results
saveRDS(B35, file = "results/scenario_2_B35.rds")
#Read results
B35 <- readRDS("results/scenario_2_B35.rds")
B35




## ar1, Exchangeable
system.time ({
  B36 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_2, B=B)
})
#Save results
saveRDS(B36, file = "results/scenario_2_B36.rds")
#Read results
B36 <- readRDS("results/scenario_2_B36.rds")
B36



##Exchangeable, independence
system.time ({
  B37 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_2, B=B)
})
#Save results
saveRDS(B37, file = "results/scenario_2_B37.rds")
#Read results
B37 <- readRDS("results/scenario_2_B37.rds")
B37


##ar1, independence
system.time ({
  B38 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_2, B=B)
})
#Save results
saveRDS(B38, file = "results/scenario_2_B38.rds")
#Read results
B38 <- readRDS("results/scenario_2_B38.rds")
B38



B_summary_500 <- bind_rows(
  extract_results("results/scenario_2_B31.rds", "independence", "independence", N=500),
  extract_results("results/scenario_2_B32.rds", "independence", "ar1", N=500),
  extract_results("results/scenario_2_B33.rds", "independence", "Exchangeable", N=500),
  extract_results("results/scenario_2_B34.rds", "Exchangeable", "ar1", N=500),
  extract_results("results/scenario_2_B35.rds", "Exchangeable", "Exchangeable", N=500),
  extract_results("results/scenario_2_B36.rds", "ar1", "Exchangeable", N=500),
  extract_results("results/scenario_2_B37.rds", "Exchangeable", "independence", N=500),
  extract_results("results/scenario_2_B38.rds", "ar1", "independence", N=500)
)
B_summary_500






B_all <- bind_rows(B_summary_50 %>% mutate(N = 50),
                   B_summary_100 %>% mutate(N = 100),
                   B_summary_500 %>% mutate(N = 500))
latex_table <- B_all %>%
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








