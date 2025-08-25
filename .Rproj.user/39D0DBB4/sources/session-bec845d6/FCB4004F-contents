# scenario_11:  a_R  increasing, r = 15, c = 5, rho = 0.8, phi = 0.8 covers case 44,
#              46, and 48

library(dplyr)
library(matrixgee)
library(stringr)
library(xtable)
source("set-up/set_up_design_kron.R")
rows_no_18 <- rows_small
a_R_18 <- a_R_linear(rows_no_18)
############################## N_small#########################




##independence, independence
system.time ({
  R11 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_18, B=B)
})
#Save results
saveRDS(R11, file = "results/scenario_18_R11.rds")
#Read results
R11 <- readRDS("results/scenario_18_R11.rds")
R11


##independence, ar1
system.time ({
  R12 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_18, B=B)
})
#Save results
saveRDS(R12, file = "results/scenario_18_R12.rds")
#Read results
R12 <- readRDS("results/scenario_18_R12.rds")
R12




##independence, Exchangeable
system.time ({
  R13 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_18, B=B)
})
#Save results
saveRDS(R13, file = "results/scenario_18_R13.rds")
#Read results
R13 <- readRDS("results/scenario_18_R13.rds")
R13





##Exchangeable, ar1
system.time ({
  R14 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_18, B=B)
})
#Save results
saveRDS(R14, file = "results/scenario_18_R14.rds")
#Read results
R14 <- readRDS("results/scenario_18_R14.rds")
R14



##Exchangeable, Exchangeable
system.time ({
  R15 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_18, B=B)
})
#Save results
saveRDS(R15, file = "results/scenario_18_R15.rds")
#Read results
R15 <- readRDS("results/scenario_18_R15.rds")
R15


##ar1, Exchangeable
system.time ({
  R16 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_18, B=B)
})
#Save results
saveRDS(R16, file = "results/scenario_18_R16.rds")
#Read results
R16 <- readRDS("results/scenario_18_R16.rds")
R16


##Exchangeable, independence
system.time ({
  R17 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_18, B=B)
})
#Save results
saveRDS(R17, file = "results/scenario_18_R17.rds")
#Read results
R17 <- readRDS("results/scenario_18_R17.rds")
R17






##ar1, independence
system.time ({
  R18 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_18, B=B)
})
#Save results
saveRDS(R18, file = "results/scenario_18_R18.rds")
#Read results
R18 <- readRDS("results/scenario_18_R18.rds")
R18




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



R_summary_50 <- bind_rows(
  extract_results("results/scenario_18_R11.rds", "independence", "independence", N=50),
  extract_results("results/scenario_18_R12.rds", "independence", "ar1", N=50),
  extract_results("results/scenario_18_R13.rds", "independence", "Exchangeable", N=50),
  extract_results("results/scenario_18_R14.rds", "Exchangeable", "ar1", N=50),
  extract_results("results/scenario_18_R15.rds", "Exchangeable", "Exchangeable", N=50),
  extract_results("results/scenario_18_R16.rds", "ar1", "Exchangeable", N=50),
  extract_results("results/scenario_18_R17.rds", "Exchangeable", "independence", N=50),
  extract_results("results/scenario_18_R18.rds", "ar1", "independence", N=50)
)
R_summary_50










############################## N_medium #########################
##independence, independence
system.time ({
  R21 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_18, B=B)
})
#Save results
saveRDS(R21, file = "results/scenario_18_R21.rds")
#Read results
R21 <- readRDS("results/scenario_18_R21.rds")
R21

##independence, ar1
system.time ({
  R22 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_18, B=B)
})
#Save results
saveRDS(R22, file = "results/scenario_18_R22.rds")
#Read results
R22 <- readRDS("results/scenario_18_R22.rds")
R22



##independence, Exchangeable
system.time ({
  R23 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_18, B=B)
})
#Save results
saveRDS(R23, file = "results/scenario_18_R23.rds")
#Read results
R23 <- readRDS("results/scenario_18_R23.rds")
R23






##Exchangeable, ar1
system.time ({
  R24 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_18, B=B)
})
#Save results
saveRDS(R24, file = "results/scenario_18_R24.rds")
#Read results
R24 <- readRDS("results/scenario_18_R24.rds")
R24





##Exchangeable, Exchangeable
system.time ({
  R25 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_18, B=B)
})
#Save results
saveRDS(R25, file = "results/scenario_18_R25.rds")
#Read results
R25 <- readRDS("results/scenario_18_R25.rds")
R25




## ar1, Exchangeable
system.time ({
  R26 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_18, B=B)
})
#Save results
saveRDS(R26, file = "results/scenario_18_R26.rds")
#Read results
R26 <- readRDS("results/scenario_18_R26.rds")
R26



##Exchangeable, independence
system.time ({
  R27 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_18, B=B)
})
#Save results
saveRDS(R27, file = "results/scenario_18_R27.rds")
#Read results
R27 <- readRDS("results/scenario_18_R27.rds")
R27



##ar1, independence
system.time ({
  R28 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_18, B=B)
})#Save results
saveRDS(R28, file = "results/scenario_18_R28.rds")
#Read results
R28 <- readRDS("results/scenario_18_R28.rds")
R28


R_summary_100 <- bind_rows(
  extract_results("results/scenario_18_R21.rds", "independence", "independence", N=100),
  extract_results("results/scenario_18_R22.rds", "independence", "ar1", N=100),
  extract_results("results/scenario_18_R23.rds", "independence", "Exchangeable", N=100),
  extract_results("results/scenario_18_R24.rds", "Exchangeable", "ar1", N=100),
  extract_results("results/scenario_18_R25.rds", "Exchangeable", "Exchangeable", N=100),
  extract_results("results/scenario_18_R26.rds", "ar1", "Exchangeable", N=100),
  extract_results("results/scenario_18_R27.rds", "Exchangeable", "independence", N=100),
  extract_results("results/scenario_18_R28.rds", "ar1", "independence", N=100)
)
R_summary_100















############################## N_large #########################
##independence, independence
system.time ({
  R31 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_18, B=B)
})
#Save results
saveRDS(R31, file = "results/scenario_18_R31.rds")
#Read results
R31 <- readRDS("results/scenario_18_R31.rds")
R31

##independence, ar1
system.time ({
  R32 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_18, B=B)
})

#Save results
saveRDS(R32, file = "results/scenario_18_R32.rds")
#Read results
R32 <- readRDS("results/scenario_18_R32.rds")
R32



##independence, Exchangeable
system.time ({
  R33 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_18, B=B)
})
#Save results
saveRDS(R33, file = "results/scenario_18_R33.rds")
#Read results
R33 <- readRDS("results/scenario_18_R33.rds")
R33






##Exchangeable, ar1
system.time ({
  R34 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_18, B=B)
})


#Save results
saveRDS(R34, file = "results/scenario_18_R34.rds")
#Read results
R34 <- readRDS("results/scenario_18_R34.rds")
R34





##Exchangeable, Exchangeable
system.time ({
  R35 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_18, B=B)
})
#Save results
saveRDS(R35, file = "results/scenario_18_R35.rds")
#Read results
R35 <- readRDS("results/scenario_18_R35.rds")
R35




## ar1, Exchangeable
system.time ({
  R36 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_18, B=B)
})
#Save results
saveRDS(R36, file = "results/scenario_18_R36.rds")
#Read results
R36 <- readRDS("results/scenario_18_R36.rds")
R36



##Exchangeable, independence
system.time ({
  R37 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_18, B=B)
})
#Save results
saveRDS(R37, file = "results/scenario_18_R37.rds")
#Read results
R37 <- readRDS("results/scenario_18_R37.rds")
R37


##ar1, independence
system.time ({
  R38 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_18, B=B)
})
#Save results
saveRDS(R38, file = "results/scenario_18_R38.rds")
#Read results
R38 <- readRDS("results/scenario_18_R38.rds")
R38



R_summary_500 <- bind_rows(
  extract_results("results/scenario_18_R31.rds", "independence", "independence", N=500),
  extract_results("results/scenario_18_R32.rds", "independence", "ar1", N=500),
  extract_results("results/scenario_18_R33.rds", "independence", "Exchangeable", N=500),
  extract_results("results/scenario_18_R34.rds", "Exchangeable", "ar1", N=500),
  extract_results("results/scenario_18_R35.rds", "Exchangeable", "Exchangeable", N=500),
  extract_results("results/scenario_18_R36.rds", "ar1", "Exchangeable", N=500),
  extract_results("results/scenario_18_R37.rds", "Exchangeable", "independence", N=500),
  extract_results("results/scenario_18_R38.rds", "ar1", "independence", N=500)
)
R_summary_500






R_all <- bind_rows(R_summary_50 %>% mutate(N = 50),
                   R_summary_100 %>% mutate(N = 100),
                   R_summary_500 %>% mutate(N = 500))
latex_table <- R_all %>%
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








