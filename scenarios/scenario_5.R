# scenario_5:  a_R  increasing, r = 15, c = 5, rho = 0.3, phi = 0.8 covers case 20,
#              22, and 24
library(matrixgee)
library(xtable)
library(dplyr)
library(stringr)
source("set-up/set_up_design_kron.R")
rows_no_5 <- rows_small
a_R_5 <- a_R_linear(rows_no_5)
############################## N_small#########################




##independence, independence
system.time ({
  E11 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_5, B=B)
})
#Save results
saveRDS(E11, file = "results/scenario_5_E11.rds")
#Read results
E11 <- readRDS("results/scenario_5_E11.rds")
E11


##independence, ar1
system.time ({
  E12 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_5, B=B)
})
#Save results
saveRDS(E12, file = "results/scenario_5_E12.rds")
#Read results
E12 <- readRDS("results/scenario_5_E12.rds")
E12




##independence, Exchangeable
system.time ({
  E13 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_5, B=B)
})
#Save results
saveRDS(E13, file = "results/scenario_5_E13.rds")
#Read results
E13 <- readRDS("results/scenario_5_E13.rds")
E13





##Exchangeable, ar1
system.time ({
  E14 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_5, B=B)
})
#Save results
saveRDS(E14, file = "results/scenario_5_E14.rds")
#Read results
E14 <- readRDS("results/scenario_5_E14.rds")
E14



##Exchangeable, Exchangeable
system.time ({
  E15 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_5, B=B)
})
#Save results
saveRDS(E15, file = "results/scenario_5_E15.rds")
#Read results
E15 <- readRDS("results/scenario_5_E15.rds")
E15


##ar1, Exchangeable
system.time ({
  E16 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_5, B=B)
})
#Save results
saveRDS(E16, file = "results/scenario_5_E16.rds")
#Read results
E16 <- readRDS("results/scenario_5_E16.rds")
E16


##Exchangeable, independence
system.time ({
  E17 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_5, B=B)
})
#Save results
saveRDS(E17, file = "results/scenario_5_E17.rds")
#Read results
E17 <- readRDS("results/scenario_5_E17.rds")
E17






##ar1, independence
system.time ({
  E18 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_5, B=B)
})
#Save results
saveRDS(E18, file = "results/scenario_5_E18.rds")
#Read results
E18 <- readRDS("results/scenario_5_E18.rds")
E18




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



E_summary_50 <- bind_rows(
  extract_results("results/scenario_5_E11.rds", "independence", "independence", N=50),
  extract_results("results/scenario_5_E12.rds", "independence", "ar1", N=50),
  extract_results("results/scenario_5_E13.rds", "independence", "Exchangeable", N=50),
  extract_results("results/scenario_5_E14.rds", "Exchangeable", "ar1", N=50),
  extract_results("results/scenario_5_E15.rds", "Exchangeable", "Exchangeable", N=50),
  extract_results("results/scenario_5_E16.rds", "ar1", "Exchangeable", N=50),
  extract_results("results/scenario_5_E17.rds", "Exchangeable", "independence", N=50),
  extract_results("results/scenario_5_E18.rds", "ar1", "independence", N=50)
)
E_summary_50










############################## N_medium #########################
##independence, independence
system.time ({
  E21 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_5, B=B)
})
#Save results
saveRDS(E21, file = "results/scenario_5_E21.rds")
#Read results
E21 <- readRDS("results/scenario_5_E21.rds")
E21

##independence, ar1
system.time ({
  E22 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_5, B=B)
})
#Save results
saveRDS(E22, file = "results/scenario_5_E22.rds")
#Read results
E22 <- readRDS("results/scenario_5_E22.rds")
E22



##independence, Exchangeable
system.time ({
  E23 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_5, B=B)
})
#Save results
saveRDS(E23, file = "results/scenario_5_E23.rds")
#Read results
E23 <- readRDS("results/scenario_5_E23.rds")
E23






##Exchangeable, ar1
system.time ({
  E24 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_5, B=B)
})
#Save results
saveRDS(E24, file = "results/scenario_5_E24.rds")
#Read results
E24 <- readRDS("results/scenario_5_E24.rds")
E24





##Exchangeable, Exchangeable
system.time ({
  E25 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_5, B=B)
})
#Save results
saveRDS(E25, file = "results/scenario_5_E25.rds")
#Read results
E25 <- readRDS("results/scenario_5_E25.rds")
E25




## ar1, Exchangeable
system.time ({
  E26 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_5, B=B)
})
#Save results
saveRDS(E26, file = "results/scenario_5_E26.rds")
#Read results
E26 <- readRDS("results/scenario_5_E26.rds")
E26



##Exchangeable, independence
system.time ({
  E27 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_5, B=B)
})
#Save results
saveRDS(E27, file = "results/scenario_5_E27.rds")
#Read results
E27 <- readRDS("results/scenario_5_E27.rds")
E27



##ar1, independence
system.time ({
  E28 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_5, B=B)
})
#Save results
saveRDS(E28, file = "results/scenario_5_E28.rds")
#Read results
E28 <- readRDS("results/scenario_5_E28.rds")
E28


E_summary_100 <- bind_rows(
  extract_results("results/scenario_5_E21.rds", "independence", "independence", N=100),
  extract_results("results/scenario_5_E22.rds", "independence", "ar1", N=100),
  extract_results("results/scenario_5_E23.rds", "independence", "Exchangeable", N=100),
  extract_results("results/scenario_5_E24.rds", "Exchangeable", "ar1", N=100),
  extract_results("results/scenario_5_E25.rds", "Exchangeable", "Exchangeable", N=100),
  extract_results("results/scenario_5_E26.rds", "ar1", "Exchangeable", N=100),
  extract_results("results/scenario_5_E27.rds", "Exchangeable", "independence", N=100),
  extract_results("results/scenario_5_E28.rds", "ar1", "independence", N=100)
)
E_summary_100















############################## N_large #########################
##independence, independence
system.time ({
  E31 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_5, B=B)
})
#Save results
saveRDS(E31, file = "results/scenario_5_E31.rds")
#Read results
E31 <- readRDS("results/scenario_5_E31.rds")
E31

##independence, ar1
system.time ({
  E32 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_5, B=B)
})
#Save results
saveRDS(E32, file = "results/scenario_5_E32.rds")
#Read results
E32 <- readRDS("results/scenario_5_E32.rds")
E32



##independence, Exchangeable
system.time ({
  E33 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_5, B=B)
})
#Save results
saveRDS(E33, file = "results/scenario_5_E33.rds")
#Read results
E33 <- readRDS("results/scenario_5_E33.rds")
E33






##Exchangeable, ar1
system.time ({
  E34 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_5, B=B)
})
#Save results
saveRDS(E34, file = "results/scenario_5_E34.rds")
#Read results
E34 <- readRDS("results/scenario_5_E34.rds")
E34





##Exchangeable, Exchangeable
system.time ({
  E35 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_5, B=B)
})
#Save results
saveRDS(E35, file = "results/scenario_5_E35.rds")
#Read results
E35 <- readRDS("results/scenario_5_E35.rds")
E35




## ar1, Exchangeable
system.time ({
  E36 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_5, B=B)
})
#Save results
saveRDS(E36, file = "results/scenario_5_E36.rds")
#Read results
E36 <- readRDS("results/scenario_5_E36.rds")
E36



##Exchangeable, independence
system.time ({
  E37 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_5, B=B)
})
#Save results
saveRDS(E37, file = "results/scenario_5_E37.rds")
#Read results
E37 <- readRDS("results/scenario_5_E37.rds")
E37


##ar1, independence
system.time ({
  E38 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_5, B=B)
})
#Save results
saveRDS(E38, file = "results/scenario_5_E38.rds")
#Read results
E38 <- readRDS("results/scenario_5_E38.rds")
E38



E_summary_500 <- bind_rows(
  extract_results("results/scenario_5_E31.rds", "independence", "independence", N=500),
  extract_results("results/scenario_5_E32.rds", "independence", "ar1", N=500),
  extract_results("results/scenario_5_E33.rds", "independence", "Exchangeable", N=500),
  extract_results("results/scenario_5_E34.rds", "Exchangeable", "ar1", N=500),
  extract_results("results/scenario_5_E35.rds", "Exchangeable", "Exchangeable", N=500),
  extract_results("results/scenario_5_E36.rds", "ar1", "Exchangeable", N=500),
  extract_results("results/scenario_5_E37.rds", "Exchangeable", "independence", N=500),
  extract_results("results/scenario_5_E38.rds", "ar1", "independence", N=500)
)
E_summary_500






E_all <- bind_rows(E_summary_50 %>% mutate(N = 50),
                   E_summary_100 %>% mutate(N = 100),
                   E_summary_500 %>% mutate(N = 500))
latex_table <- E_all %>%
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








