# scenario_6:  a_R  constant, r = 15, c = 5, rho = 0.8, phi = 0.3 covers case 37,
#              39, and 41

library(dplyr)
library(stringr)
source("set-up/set_up_design_kron.R")
rows_no_6 <- rows_small
a_R_6 <- a_R_const(rows_no_6)
############################## N_small#########################




##independence, independence
system.time ({
  F11 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_6, B=B)
})
#Save results
saveRDS(F11, file = "results/scenario_6_F11.rds")
#Read results
F11 <- readRDS("results/scenario_6_F11.rds")
F11


##independence, ar1
system.time ({
  F12 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_6, B=B)
})
#Save results
saveRDS(F12, file = "results/scenario_6_F12.rds")
#Read results
F12 <- readRDS("results/scenario_6_F12.rds")
F12




##independence, Exchangeable
system.time ({
  F13 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_6, B=B)
})
#Save results
saveRDS(F13, file = "results/scenario_6_F13.rds")
#Read results
F13 <- readRDS("results/scenario_6_F13.rds")
F13





##Exchangeable, ar1
system.time ({
  F14 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_6, B=B)
})
#Save results
saveRDS(F14, file = "results/scenario_6_F14.rds")
#Read results
F14 <- readRDS("results/scenario_6_F14.rds")
F14



##Exchangeable, Exchangeable
system.time ({
  F15 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_6, B=B)
})
#Save results
saveRDS(F15, file = "results/scenario_6_F15.rds")
#Read results
F15 <- readRDS("results/scenario_6_F15.rds")
F15


##ar1, Exchangeable
system.time ({
  F16 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_6, B=B)
})
#Save results
saveRDS(F16, file = "results/scenario_6_F16.rds")
#Read results
F16 <- readRDS("results/scenario_6_F16.rds")
F16


##Exchangeable, independence
system.time ({
  F17 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_6, B=B)
})
#Save results
saveRDS(F17, file = "results/scenario_6_F17.rds")
#Read results
F17 <- readRDS("results/scenario_6_F17.rds")
F17






##ar1, independence
system.time ({
  F18 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_6, B=B)
})
#Save results
saveRDS(F18, file = "results/scenario_6_F18.rds")
#Read results
F18 <- readRDS("results/scenario_6_F18.rds")
F18




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



F_summary_50 <- bind_rows(
  extract_results("results/scenario_6_F11.rds", "independence", "independence", N=50),
  extract_results("results/scenario_6_F12.rds", "independence", "ar1", N=50),
  extract_results("results/scenario_6_F13.rds", "independence", "Exchangeable", N=50),
  extract_results("results/scenario_6_F14.rds", "Exchangeable", "ar1", N=50),
  extract_results("results/scenario_6_F15.rds", "Exchangeable", "Exchangeable", N=50),
  extract_results("results/scenario_6_F16.rds", "ar1", "Exchangeable", N=50),
  extract_results("results/scenario_6_F17.rds", "Exchangeable", "independence", N=50),
  extract_results("results/scenario_6_F18.rds", "ar1", "independence", N=50)
)
F_summary_50










############################## N_medium #########################
##independence, independence
system.time ({
  F21 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_6, B=B)
})
#Save results
saveRDS(F21, file = "results/scenario_6_F21.rds")
#Read results
F21 <- readRDS("results/scenario_6_F21.rds")
F21

##independence, ar1
system.time ({
  F22 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_6, B=B)
})
#Save results
saveRDS(F22, file = "results/scenario_6_F22.rds")
#Read results
F22 <- readRDS("results/scenario_6_F22.rds")
F22



##independence, Exchangeable
system.time ({
  F23 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_6, B=B)
})
#Save results
saveRDS(F23, file = "results/scenario_6_F23.rds")
#Read results
F23 <- readRDS("results/scenario_6_F23.rds")
F23






##Exchangeable, ar1
system.time ({
  F24 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_6, B=B)
})
#Save results
saveRDS(F24, file = "results/scenario_6_F24.rds")
#Read results
F24 <- readRDS("results/scenario_6_F24.rds")
F24





##Exchangeable, Exchangeable
system.time ({
  F25 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_6, B=B)
})
#Save results
saveRDS(F25, file = "results/scenario_6_F25.rds")
#Read results
F25 <- readRDS("results/scenario_6_F25.rds")
F25




## ar1, Exchangeable
system.time ({
  F26 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_6, B=B)
})
#Save results
saveRDS(F26, file = "results/scenario_6_F26.rds")
#Read results
F26 <- readRDS("results/scenario_6_F26.rds")
F26



##Exchangeable, independence
system.time ({
  F27 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_6, B=B)
})
#Save results
saveRDS(F27, file = "results/scenario_6_F27.rds")
#Read results
F27 <- readRDS("results/scenario_6_F27.rds")
F27



##ar1, independence
system.time ({
  F28 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_6, B=B)
})
#Save results
saveRDS(F28, file = "results/scenario_6_F28.rds")
#Read results
F28 <- readRDS("results/scenario_6_F28.rds")
F28


F_summary_100 <- bind_rows(
  extract_results("results/scenario_6_F21.rds", "independence", "independence", N=100),
  extract_results("results/scenario_6_F22.rds", "independence", "ar1", N=100),
  extract_results("results/scenario_6_F23.rds", "independence", "Exchangeable", N=100),
  extract_results("results/scenario_6_F24.rds", "Exchangeable", "ar1", N=100),
  extract_results("results/scenario_6_F25.rds", "Exchangeable", "Exchangeable", N=100),
  extract_results("results/scenario_6_F26.rds", "ar1", "Exchangeable", N=100),
  extract_results("results/scenario_6_F27.rds", "Exchangeable", "independence", N=100),
  extract_results("results/scenario_6_F28.rds", "ar1", "independence", N=100)
)
F_summary_100















############################## N_large #########################
##independence, independence
system.time ({
  F31 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_6, B=B)
})
#Save results
saveRDS(F31, file = "results/scenario_6_F31.rds")
#Read results
F31 <- readRDS("results/scenario_6_F31.rds")
F31

##independence, ar1
system.time ({
  F32 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_6, B=B)
})
#Save results
saveRDS(F32, file = "results/scenario_6_F32.rds")
#Read results
F32 <- readRDS("results/scenario_6_F32.rds")
F32



##independence, Exchangeable
system.time ({
  F33 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_6, B=B)
})
#Save results
saveRDS(F33, file = "results/scenario_6_F33.rds")
#Read results
F33 <- readRDS("results/scenario_6_F33.rds")
F33






##Exchangeable, ar1
system.time ({
  F34 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_6, B=B)
})


#Save results
saveRDS(F34, file = "results/scenario_6_F34.rds")
#Read results
F34 <- readRDS("results/scenario_6_F34.rds")
F34





##Exchangeable, Exchangeable
system.time ({
  F35 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_6, B=B)
})
#Save results
saveRDS(F35, file = "results/scenario_6_F35.rds")
#Read results
F35 <- readRDS("results/scenario_6_F35.rds")
F35




## ar1, Exchangeable
system.time ({
  F36 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_6, B=B)
})
#Save results
saveRDS(F36, file = "results/scenario_6_F36.rds")
#Read results
F36 <- readRDS("results/scenario_6_F36.rds")
F36



##Exchangeable, independence
system.time ({
  F37 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_6, B=B)
})
#Save results
saveRDS(F37, file = "results/scenario_6_F37.rds")
#Read results
F37 <- readRDS("results/scenario_6_F37.rds")
F37


##ar1, independence
system.time ({
  F38 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_6, B=B)
})
#Save results
saveRDS(F38, file = "results/scenario_6_F38.rds")
#Read results
F38 <- readRDS("results/scenario_6_F38.rds")
F38



F_summary_500 <- bind_rows(
  extract_results("results/scenario_6_F31.rds", "independence", "independence", N=500),
  extract_results("results/scenario_6_F32.rds", "independence", "ar1", N=500),
  extract_results("results/scenario_6_F33.rds", "independence", "Exchangeable", N=500),
  extract_results("results/scenario_6_F34.rds", "Exchangeable", "ar1", N=500),
  extract_results("results/scenario_6_F35.rds", "Exchangeable", "Exchangeable", N=500),
  extract_results("results/scenario_6_F36.rds", "ar1", "Exchangeable", N=500),
  extract_results("results/scenario_6_F37.rds", "Exchangeable", "independence", N=500),
  extract_results("results/scenario_6_F38.rds", "ar1", "independence", N=500)
)
F_summary_500






F_all <- bind_rows(F_summary_50 %>% mutate(N = 50),
                   F_summary_100 %>% mutate(N = 100),
                   F_summary_500 %>% mutate(N = 500))
latex_table <- F_all %>%
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








