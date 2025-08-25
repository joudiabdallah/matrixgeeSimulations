# scenario_4:  a_R  constant, r = 15, c = 5, rho = 0.3, phi = 0.8 covers case 19,
#              21, and 23

library(dplyr)
library(stringr)
library(matrixgee)
library(xtable)
source("set-up/set_up_design_kron.R")
rows_no_4 <- rows_small
a_R_4 <- a_R_const(rows_no_4)
############################## N_small#########################




##independence, independence
system.time ({
  D11 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_4, B=B)
})
#Save results
saveRDS(D11, file = "results/scenario_4_D11.rds")
#Read results
D11 <- readRDS("results/scenario_4_D11.rds")
D11


##independence, ar1
system.time ({
  D12 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_4, B=B)
})
#Save results
saveRDS(D12, file = "results/scenario_4_D12.rds")
#Read results
D12 <- readRDS("results/scenario_4_D12.rds")
D12




##independence, Exchangeable
system.time ({
  D13 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_4, B=B)
})
#Save results
saveRDS(D13, file = "results/scenario_4_D13.rds")
#Read results
D13 <- readRDS("results/scenario_4_D13.rds")
D13





##Exchangeable, ar1
system.time ({
  D14 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_4, B=B)
})
#Save results
saveRDS(D14, file = "results/scenario_4_D14.rds")
#Read results
D14 <- readRDS("results/scenario_4_D14.rds")
D14



##Exchangeable, Exchangeable
system.time ({
  D15 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_4, B=B)
})
#Save results
saveRDS(D15, file = "results/scenario_4_D15.rds")
#Read results
D15 <- readRDS("results/scenario_4_D15.rds")
D15


##ar1, Exchangeable
system.time ({
  D16 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_4, B=B)
})
#Save results
saveRDS(D16, file = "results/scenario_4_D16.rds")
#Read results
D16 <- readRDS("results/scenario_4_D16.rds")
D16


##Exchangeable, independence
system.time ({
  D17 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_4, B=B)
})
#Save results
saveRDS(D17, file = "results/scenario_4_D17.rds")
#Read results
D17 <- readRDS("results/scenario_4_D17.rds")
D17






##ar1, independence
system.time ({
  D18 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_4, B=B)
})
#Save results
saveRDS(D18, file = "results/scenario_4_D18.rds")
#Read results
D18 <- readRDS("results/scenario_4_D18.rds")
D18




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



D_summary_50 <- bind_rows(
  extract_results("results/scenario_4_D11.rds", "independence", "independence", N=50),
  extract_results("results/scenario_4_D12.rds", "independence", "ar1", N=50),
  extract_results("results/scenario_4_D13.rds", "independence", "Exchangeable", N=50),
  extract_results("results/scenario_4_D14.rds", "Exchangeable", "ar1", N=50),
  extract_results("results/scenario_4_D15.rds", "Exchangeable", "Exchangeable", N=50),
  extract_results("results/scenario_4_D16.rds", "ar1", "Exchangeable", N=50),
  extract_results("results/scenario_4_D17.rds", "Exchangeable", "independence", N=50),
  extract_results("results/scenario_4_D18.rds", "ar1", "independence", N=50)
)
D_summary_50










############################## N_medium #########################
##independence, independence
system.time ({
  D21 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_4, B=B)
})
#Save results
saveRDS(D21, file = "results/scenario_4_D21.rds")
#Read results
D21 <- readRDS("results/scenario_4_D21.rds")
D21

##independence, ar1
system.time ({
  D22 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_4, B=B)
})
#Save results
saveRDS(D22, file = "results/scenario_4_D22.rds")
#Read results
D22 <- readRDS("results/scenario_4_D22.rds")
D22



##independence, Exchangeable
system.time ({
  D23 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_4, B=B)
})
#Save results
saveRDS(D23, file = "results/scenario_4_D23.rds")
#Read results
D23 <- readRDS("results/scenario_4_D23.rds")
D23






##Exchangeable, ar1
system.time ({
  D24 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_4, B=B)
})
#Save results
saveRDS(D24, file = "results/scenario_4_D24.rds")
#Read results
D24 <- readRDS("results/scenario_4_D24.rds")
D24





##Exchangeable, Exchangeable
system.time ({
  D25 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_4, B=B)
})
#Save results
saveRDS(D25, file = "results/scenario_4_D25.rds")
#Read results
D25 <- readRDS("results/scenario_4_D25.rds")
D25




## ar1, Exchangeable
system.time ({
  D26 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_4, B=B)
})
#Save results
saveRDS(D26, file = "results/scenario_4_D26.rds")
#Read results
D26 <- readRDS("results/scenario_4_D26.rds")
D26



##Exchangeable, independence
system.time ({
  D27 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_4, B=B)
})
#Save results
saveRDS(D27, file = "results/scenario_4_D27.rds")
#Read results
D27 <- readRDS("results/scenario_4_D27.rds")
D27



##ar1, independence
system.time ({
  D28 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_4, B=B)
})
#Save results
saveRDS(D28, file = "results/scenario_4_D28.rds")
#Read results
D28 <- readRDS("results/scenario_4_D28.rds")
D28


D_summary_100 <- bind_rows(
  extract_results("results/scenario_4_D21.rds", "independence", "independence", N=100),
  extract_results("results/scenario_4_D22.rds", "independence", "ar1", N=100),
  extract_results("results/scenario_4_D23.rds", "independence", "Exchangeable", N=100),
  extract_results("results/scenario_4_D24.rds", "Exchangeable", "ar1", N=100),
  extract_results("results/scenario_4_D25.rds", "Exchangeable", "Exchangeable", N=100),
  extract_results("results/scenario_4_D26.rds", "ar1", "Exchangeable", N=100),
  extract_results("results/scenario_4_D27.rds", "Exchangeable", "independence", N=100),
  extract_results("results/scenario_4_D28.rds", "ar1", "independence", N=100)
)
D_summary_100















############################## N_large #########################
##independence, independence
system.time ({
  D31 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_4, B=B)
})
#Save results
saveRDS(D31, file = "results/scenario_4_D31.rds")
#Read results
D31 <- readRDS("results/scenario_4_D31.rds")
D31

##independence, ar1
system.time ({
  D32 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_4, B=B)
})
#Save results
saveRDS(D32, file = "results/scenario_4_D32.rds")
#Read results
D32 <- readRDS("results/scenario_4_D32.rds")
D32



##independence, Exchangeable
system.time ({
  D33 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_4, B=B)
})
#Save results
saveRDS(D33, file = "results/scenario_4_D33.rds")
#Read results
D33 <- readRDS("results/scenario_4_D33.rds")
D33






##Exchangeable, ar1
system.time ({
  D34 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_4, B=B)
})
#Save results
saveRDS(D34, file = "results/scenario_4_D34.rds")
#Read results
D34 <- readRDS("results/scenario_4_D34.rds")
D34





##Exchangeable, Exchangeable
system.time ({
  D35 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_4, B=B)
})
#Save results
saveRDS(D35, file = "results/scenario_4_D35.rds")
#Read results
D35 <- readRDS("results/scenario_4_D35.rds")
D35




## ar1, Exchangeable
system.time ({
  D36 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_4, B=B)
})
#Save results
saveRDS(D36, file = "results/scenario_4_D36.rds")
#Read results
D36 <- readRDS("results/scenario_4_D36.rds")
D36



##Exchangeable, independence
system.time ({
  D37 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_4, B=B)
})
#Save results
saveRDS(D37, file = "results/scenario_4_D37.rds")
#Read results
D37 <- readRDS("results/scenario_4_D37.rds")
D37


##ar1, independence
system.time ({
  D38 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_4, B=B)
})
#Save results
saveRDS(D38, file = "results/scenario_4_D38.rds")
#Read results
D38 <- readRDS("results/scenario_4_D38.rds")
D38



D_summary_500 <- bind_rows(
  extract_results("results/scenario_4_D31.rds", "independence", "independence", N=500),
  extract_results("results/scenario_4_D32.rds", "independence", "ar1", N=500),
  extract_results("results/scenario_4_D33.rds", "independence", "Exchangeable", N=500),
  extract_results("results/scenario_4_D34.rds", "Exchangeable", "ar1", N=500),
  extract_results("results/scenario_4_D35.rds", "Exchangeable", "Exchangeable", N=500),
  extract_results("results/scenario_4_D36.rds", "ar1", "Exchangeable", N=500),
  extract_results("results/scenario_4_D37.rds", "Exchangeable", "independence", N=500),
  extract_results("results/scenario_4_D38.rds", "ar1", "independence", N=500)
)
D_summary_500






D_all <- bind_rows(D_summary_50 %>% mutate(N = 50),
                   D_summary_100 %>% mutate(N = 100),
                   D_summary_500 %>% mutate(N = 500))
latex_table <- D_all %>%
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
               caption = "Simulation Results Across Sample Sizes-sCENARIO4",
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








