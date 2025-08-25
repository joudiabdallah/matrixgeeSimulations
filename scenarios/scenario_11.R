# scenario_11:  a_R  constant, r = 15, c = 15, rho = 0.3, phi = 0.8 covers case 25,
#              27, and 29

library(dplyr)
library(stringr)
library(xtable)
source("set-up/set_up_design_kron.R")
rows_no_11 <- rows_small
a_R_11 <- a_R_const(rows_no_11)
############################## N_small#########################




##independence, independence
system.time ({
  K11 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_11, B=B)
})
#Save results
saveRDS(K11, file = "results/scenario_11_K11.rds")
#Read results
K11 <- readRDS("results/scenario_11_K11.rds")
K11


##independence, ar1
system.time ({
  K12 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_11, B=B)
})
#Save results
saveRDS(K12, file = "results/scenario_11_K12.rds")
#Read results
K12 <- readRDS("results/scenario_11_K12.rds")
K12




##independence, Exchangeable
system.time ({
  K13 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_11, B=B)
})
#Save results
saveRDS(K13, file = "results/scenario_11_K13.rds")
#Read results
K13 <- readRDS("results/scenario_11_K13.rds")
K13





##Exchangeable, ar1
system.time ({
  K14 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_11, B=B)
})
#Save results
saveRDS(K14, file = "results/scenario_11_K14.rds")
#Read results
K14 <- readRDS("results/scenario_11_K14.rds")
K14



##Exchangeable, Exchangeable
system.time ({
  K15 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_11, B=B)
})
#Save results
saveRDS(K15, file = "results/scenario_11_K15.rds")
#Read results
K15 <- readRDS("results/scenario_11_K15.rds")
K15


##ar1, Exchangeable
system.time ({
  K16 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_11, B=B)
})
#Save results
saveRDS(K16, file = "results/scenario_11_K16.rds")
#Read results
K16 <- readRDS("results/scenario_11_K16.rds")
K16


##Exchangeable, independence
system.time ({
  K17 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_11, B=B)
})
#Save results
saveRDS(K17, file = "results/scenario_11_K17.rds")
#Read results
K17 <- readRDS("results/scenario_11_K17.rds")
K17






##ar1, independence
system.time ({
  K18 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_11, B=B)
})
#Save results
saveRDS(K18, file = "results/scenario_11_K18.rds")
#Read results
K18 <- readRDS("results/scenario_11_K18.rds")
K18




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



K_summary_50 <- bind_rows(
  extract_results("results/scenario_11_K11.rds", "independence", "independence", N=50),
  extract_results("results/scenario_11_K12.rds", "independence", "ar1", N=50),
  extract_results("results/scenario_11_K13.rds", "independence", "Exchangeable", N=50),
  extract_results("results/scenario_11_K14.rds", "Exchangeable", "ar1", N=50),
  extract_results("results/scenario_11_K15.rds", "Exchangeable", "Exchangeable", N=50),
  extract_results("results/scenario_11_K16.rds", "ar1", "Exchangeable", N=50),
  extract_results("results/scenario_11_K17.rds", "Exchangeable", "independence", N=50),
  extract_results("results/scenario_11_K18.rds", "ar1", "independence", N=50)
)
K_summary_50










############################## N_medium #########################
##independence, independence
system.time ({
  K21 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_11, B=B)
})
#Save results
saveRDS(K21, file = "results/scenario_11_K21.rds")
#Read results
K21 <- readRDS("results/scenario_11_K21.rds")
K21

##independence, ar1
system.time ({
  K22 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_11, B=B)
})
#Save results
saveRDS(K22, file = "results/scenario_11_K22.rds")
#Read results
K22 <- readRDS("results/scenario_11_K22.rds")
K22



##independence, Exchangeable
system.time ({
  K23 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_11, B=B)
})
#Save results
saveRDS(K23, file = "results/scenario_11_K23.rds")
#Read results
K23 <- readRDS("results/scenario_11_K23.rds")
K23






##Exchangeable, ar1
system.time ({
  K24 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_11, B=B)
})
#Save results
saveRDS(K24, file = "results/scenario_11_K24.rds")
#Read results
K24 <- readRDS("results/scenario_11_K24.rds")
K24





##Exchangeable, Exchangeable
system.time ({
  K25 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_11, B=B)
})
#Save results
saveRDS(K25, file = "results/scenario_11_K25.rds")
#Read results
K25 <- readRDS("results/scenario_11_K25.rds")
K25




## ar1, Exchangeable
system.time ({
  K26 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_11, B=B)
})
#Save results
saveRDS(K26, file = "results/scenario_11_K26.rds")
#Read results
K26 <- readRDS("results/scenario_11_K26.rds")
K26



##Exchangeable, independence
system.time ({
  K27 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_11, B=B)
})
#Save results
saveRDS(K27, file = "results/scenario_11_K27.rds")
#Read results
K27 <- readRDS("results/scenario_11_K27.rds")
K27



##ar1, independence
system.time ({
  K28 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_11, B=B)
})#Save results
saveRDS(K28, file = "results/scenario_11_K28.rds")
#Read results
K28 <- readRDS("results/scenario_11_K28.rds")
K28


K_summary_100 <- bind_rows(
  extract_results("results/scenario_11_K21.rds", "independence", "independence", N=100),
  extract_results("results/scenario_11_K22.rds", "independence", "ar1", N=100),
  extract_results("results/scenario_11_K23.rds", "independence", "Exchangeable", N=100),
  extract_results("results/scenario_11_K24.rds", "Exchangeable", "ar1", N=100),
  extract_results("results/scenario_11_K25.rds", "Exchangeable", "Exchangeable", N=100),
  extract_results("results/scenario_11_K26.rds", "ar1", "Exchangeable", N=100),
  extract_results("results/scenario_11_K27.rds", "Exchangeable", "independence", N=100),
  extract_results("results/scenario_11_K28.rds", "ar1", "independence", N=100)
)
K_summary_100















############################## N_large #########################
##independence, independence
system.time ({
  K31 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_11, B=B)
})
#Save results
saveRDS(K31, file = "results/scenario_11_K31.rds")
#Read results
K31 <- readRDS("results/scenario_11_K31.rds")
K31

##independence, ar1
system.time ({
  K32 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_11, B=B)
})

#Save results
saveRDS(K32, file = "results/scenario_11_K32.rds")
#Read results
K32 <- readRDS("results/scenario_11_K32.rds")
K32



##independence, Exchangeable
system.time ({
  K33 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_11, B=B)
})
#Save results
saveRDS(K33, file = "results/scenario_11_K33.rds")
#Read results
K33 <- readRDS("results/scenario_11_K33.rds")
K33






##Exchangeable, ar1
system.time ({
  K34 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_11, B=B)
})


#Save results
saveRDS(K34, file = "results/scenario_11_K34.rds")
#Read results
K34 <- readRDS("results/scenario_11_K34.rds")
K34





##Exchangeable, Exchangeable
system.time ({
  K35 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_11, B=B)
})
#Save results
saveRDS(K35, file = "results/scenario_11_K35.rds")
#Read results
K35 <- readRDS("results/scenario_11_K35.rds")
K35




## ar1, Exchangeable
system.time ({
  K36 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_11, B=B)
})
#Save results
saveRDS(K36, file = "results/scenario_11_K36.rds")
#Read results
K36 <- readRDS("results/scenario_11_K36.rds")
K36



##Exchangeable, independence
system.time ({
  K37 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_11, B=B)
})
#Save results
saveRDS(K37, file = "results/scenario_11_K37.rds")
#Read results
K37 <- readRDS("results/scenario_11_K37.rds")
K37


##ar1, independence
system.time ({
  K38 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_11, B=B)
})
#Save results
saveRDS(K38, file = "results/scenario_11_K38.rds")
#Read results
K38 <- readRDS("results/scenario_11_K38.rds")
K38



K_summary_500 <- bind_rows(
  extract_results("results/scenario_11_K31.rds", "independence", "independence", N=500),
  extract_results("results/scenario_11_K32.rds", "independence", "ar1", N=500),
  extract_results("results/scenario_11_K33.rds", "independence", "Exchangeable", N=500),
  extract_results("results/scenario_11_K34.rds", "Exchangeable", "ar1", N=500),
  extract_results("results/scenario_11_K35.rds", "Exchangeable", "Exchangeable", N=500),
  extract_results("results/scenario_11_K36.rds", "ar1", "Exchangeable", N=500),
  extract_results("results/scenario_11_K37.rds", "Exchangeable", "independence", N=500),
  extract_results("results/scenario_11_K38.rds", "ar1", "independence", N=500)
)
K_summary_500






K_all <- bind_rows(K_summary_50 %>% mutate(N = 50),
                   K_summary_100 %>% mutate(N = 100),
                   K_summary_500 %>% mutate(N = 500))
latex_table <- K_all %>%
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








