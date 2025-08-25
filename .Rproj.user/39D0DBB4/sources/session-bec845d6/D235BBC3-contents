# scenario_8:  a_R  constant, r = 15, c = 5, rho = 0.8, phi = 0.8 covers case 55,
#              57, and 59

library(dplyr)
library(stringr)
library(xtable)
source("set-up/set_up_design_kron.R")
rows_no_8 <- rows_small
a_R_8 <- a_R_const(rows_no_8)
############################## N_small#########################




##independence, independence
system.time ({
  H11 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_high, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_8, B=B)
})
#Save results
saveRDS(H11, file = "results/scenario_8_H11.rds")
#Read results
H11 <- readRDS("results/scenario_8_H11.rds")
H11


##independence, ar1
system.time ({
  H12 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_high, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_8, B=B)
})
#Save results
saveRDS(H12, file = "results/scenario_8_H12.rds")
#Read results
H12 <- readRDS("results/scenario_8_H12.rds")
H12




##independence, Exchangeable
system.time ({
  H13 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_high, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_8, B=B)
})
#Save results
saveRDS(H13, file = "results/scenario_8_H13.rds")
#Read results
H13 <- readRDS("results/scenario_8_H13.rds")
H13





##Exchangeable, ar1
system.time ({
  H14 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_high, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_8, B=B)
})
#Save results
saveRDS(H14, file = "results/scenario_8_H14.rds")
#Read results
H14 <- readRDS("results/scenario_8_H14.rds")
H14



##Exchangeable, Exchangeable
system.time ({
  H15 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_high, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_8, B=B)
})
#Save results
saveRDS(H15, file = "results/scenario_8_H15.rds")
#Read results
H15 <- readRDS("results/scenario_8_H15.rds")
H15


##ar1, Exchangeable
system.time ({
  H16 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_high, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_8, B=B)
})
#Save results
saveRDS(H16, file = "results/scenario_8_H16.rds")
#Read results
H16 <- readRDS("results/scenario_8_H16.rds")
H16


##Exchangeable, independence
system.time ({
  H17 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_high, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_8, B=B)
})
#Save results
saveRDS(H17, file = "results/scenario_8_H17.rds")
#Read results
H17 <- readRDS("results/scenario_8_H17.rds")
H17






##ar1, independence
system.time ({
  H18 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_high, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_8, B=B)
})
#Save results
saveRDS(H18, file = "results/scenario_8_H18.rds")
#Read results
H18 <- readRDS("results/scenario_8_H18.rds")
H18




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



H_summary_50 <- bind_rows(
  extract_results("results/scenario_8_H11.rds", "independence", "independence", N=50),
  extract_results("results/scenario_8_H12.rds", "independence", "ar1", N=50),
  extract_results("results/scenario_8_H13.rds", "independence", "Exchangeable", N=50),
  extract_results("results/scenario_8_H14.rds", "Exchangeable", "ar1", N=50),
  extract_results("results/scenario_8_H15.rds", "Exchangeable", "Exchangeable", N=50),
  extract_results("results/scenario_8_H16.rds", "ar1", "Exchangeable", N=50),
  extract_results("results/scenario_8_H17.rds", "Exchangeable", "independence", N=50),
  extract_results("results/scenario_8_H18.rds", "ar1", "independence", N=50)
)
H_summary_50










############################## N_medium #########################
##independence, independence
system.time ({
  H21 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_high, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_8, B=B)
})
#Save results
saveRDS(H21, file = "results/scenario_8_H21.rds")
#Read results
H21 <- readRDS("results/scenario_8_H21.rds")
H21

##independence, ar1
system.time ({
  H22 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_high, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_8, B=B)
})
#Save results
saveRDS(H22, file = "results/scenario_8_H22.rds")
#Read results
H22 <- readRDS("results/scenario_8_H22.rds")
H22



##independence, Exchangeable
system.time ({
  H23 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_high, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_8, B=B)
})
#Save results
saveRDS(H23, file = "results/scenario_8_H23.rds")
#Read results
H23 <- readRDS("results/scenario_8_H23.rds")
H23






##Exchangeable, ar1
system.time ({
  H24 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_high, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_8, B=B)
})
#Save results
saveRDS(H24, file = "results/scenario_8_H24.rds")
#Read results
H24 <- readRDS("results/scenario_8_H24.rds")
H24





##Exchangeable, Exchangeable
system.time ({
  H25 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_high, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_8, B=B)
})
#Save results
saveRDS(H25, file = "results/scenario_8_H25.rds")
#Read results
H25 <- readRDS("results/scenario_8_H25.rds")
H25




## ar1, Exchangeable
system.time ({
  H26 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_high, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_8, B=B)
})
#Save results
saveRDS(H26, file = "results/scenario_8_H26.rds")
#Read results
H26 <- readRDS("results/scenario_8_H26.rds")
H26



##Exchangeable, independence
system.time ({
  H27 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_high, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_8, B=B)
})
#Save results
saveRDS(H27, file = "results/scenario_8_H27.rds")
#Read results
H27 <- readRDS("results/scenario_8_H27.rds")
H27



##ar1, independence
system.time ({
  H28 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_high, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_8, B=B)
})
#Save results
saveRDS(H28, file = "results/scenario_8_H28.rds")
#Read results
H28 <- readRDS("results/scenario_8_H28.rds")
H28


H_summary_100 <- bind_rows(
  extract_results("results/scenario_8_H21.rds", "independence", "independence", N=100),
  extract_results("results/scenario_8_H22.rds", "independence", "ar1", N=100),
  extract_results("results/scenario_8_H23.rds", "independence", "Exchangeable", N=100),
  extract_results("results/scenario_8_H24.rds", "Exchangeable", "ar1", N=100),
  extract_results("results/scenario_8_H25.rds", "Exchangeable", "Exchangeable", N=100),
  extract_results("results/scenario_8_H26.rds", "ar1", "Exchangeable", N=100),
  extract_results("results/scenario_8_H27.rds", "Exchangeable", "independence", N=100),
  extract_results("results/scenario_8_H28.rds", "ar1", "independence", N=100)
)
H_summary_100















############################## N_large #########################
##independence, independence
system.time ({
  H31 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_high, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_8, B=B)
})
#Save results
saveRDS(H31, file = "results/scenario_8_H31.rds")
#Read results
H31 <- readRDS("results/scenario_8_H31.rds")
H31

##independence, ar1
system.time ({
  H32 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_high, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_8, B=B)
})
#Save results
saveRDS(H32, file = "results/scenario_8_H32.rds")
#Read results
H32 <- readRDS("results/scenario_8_H32.rds")
H32



##independence, Exchangeable
system.time ({
  H33 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_high, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_8, B=B)
})
#Save results
saveRDS(H33, file = "results/scenario_8_H33.rds")
#Read results
H33 <- readRDS("results/scenario_8_H33.rds")
H33






##Exchangeable, ar1
system.time ({
  H34 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_high, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_8, B=B)
})


#Save results
saveRDS(H34, file = "results/scenario_8_H34.rds")
#Read results
H34 <- readRDS("results/scenario_8_H34.rds")
H34





##Exchangeable, Exchangeable
system.time ({
  H35 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_high, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_8, B=B)
})
#Save results
saveRDS(H35, file = "results/scenario_8_H35.rds")
#Read results
H35 <- readRDS("results/scenario_8_H35.rds")
H35




## ar1, Exchangeable
system.time ({
  H36 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_high, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_8, B=B)
})
#Save results
saveRDS(H36, file = "results/scenario_8_H36.rds")
#Read results
H36 <- readRDS("results/scenario_8_H36.rds")
H36



##Exchangeable, independence
system.time ({
  H37 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_high, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_8, B=B)
})
#Save results
saveRDS(H37, file = "results/scenario_8_H37.rds")
#Read results
H37 <- readRDS("results/scenario_8_H37.rds")
H37


##ar1, independence
system.time ({
  H38 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_high, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_8, B=B)
})
#Save results
saveRDS(H38, file = "results/scenario_8_H38.rds")
#Read results
H38 <- readRDS("results/scenario_8_H38.rds")
H38



H_summary_500 <- bind_rows(
  extract_results("results/scenario_8_H31.rds", "independence", "independence", N=500),
  extract_results("results/scenario_8_H32.rds", "independence", "ar1", N=500),
  extract_results("results/scenario_8_H33.rds", "independence", "Exchangeable", N=500),
  extract_results("results/scenario_8_H34.rds", "Exchangeable", "ar1", N=500),
  extract_results("results/scenario_8_H35.rds", "Exchangeable", "Exchangeable", N=500),
  extract_results("results/scenario_8_H36.rds", "ar1", "Exchangeable", N=500),
  extract_results("results/scenario_8_H37.rds", "Exchangeable", "independence", N=500),
  extract_results("results/scenario_8_H38.rds", "ar1", "independence", N=500)
)
H_summary_500






H_all <- bind_rows(H_summary_50 %>% mutate(N = 50),
                   H_summary_100 %>% mutate(N = 100),
                   H_summary_500 %>% mutate(N = 500))
latex_table <- H_all %>%
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








