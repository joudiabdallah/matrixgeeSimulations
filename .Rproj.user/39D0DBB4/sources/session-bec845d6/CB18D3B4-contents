# scenario_9:  a_R  increasing, r = 15, c = 15, rho = 0.3, phi = 0.3 covers case 8,
#              10, and 12

library(dplyr)
library(stringr)
library(xtable)
source("set-up/set_up_design_kron.R")
rows_no_9 <- rows_small
a_R_9 <- a_R_linear(rows_no_9)
############################## N_small#########################




##independence, independence
system.time ({
  I11 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_9, B=B)
})
#Save results
saveRDS(I11, file = "results/scenario_9_I11.rds")
#Read results
I11 <- readRDS("results/scenario_9_I11.rds")
I11


##independence, ar1
system.time ({
  I12 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_9, B=B)
})
#Save results
saveRDS(I12, file = "results/scenario_9_I12.rds")
#Read results
I12 <- readRDS("results/scenario_9_I12.rds")
I12




##independence, Exchangeable
system.time ({
  I13 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_9, B=B)
})
#Save results
saveRDS(I13, file = "results/scenario_9_I13.rds")
#Read results
I13 <- readRDS("results/scenario_9_I13.rds")
I13





##Exchangeable, ar1
system.time ({
  I14 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_9, B=B)
})
#Save results
saveRDS(I14, file = "results/scenario_9_I14.rds")
#Read results
I14 <- readRDS("results/scenario_9_I14.rds")
I14



##Exchangeable, Exchangeable
system.time ({
  I15 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_9, B=B)
})
#Save results
saveRDS(I15, file = "results/scenario_9_I15.rds")
#Read results
I15 <- readRDS("results/scenario_9_I15.rds")
I15


##ar1, Exchangeable
system.time ({
  I16 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_9, B=B)
})
#Save results
saveRDS(I16, file = "results/scenario_9_I16.rds")
#Read results
I16 <- readRDS("results/scenario_9_I16.rds")
I16


##Exchangeable, independence
system.time ({
  I17 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_9, B=B)
})
#Save results
saveRDS(I17, file = "results/scenario_9_I17.rds")
#Read results
I17 <- readRDS("results/scenario_9_I17.rds")
I17






##ar1, independence
system.time ({
  I18 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_9, B=B)
})
#Save results
saveRDS(I18, file = "results/scenario_9_I18.rds")
#Read results
I18 <- readRDS("results/scenario_9_I18.rds")
I18




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



I_summary_50 <- bind_rows(
  extract_results("results/scenario_9_I11.rds", "independence", "independence", N=50),
  extract_results("results/scenario_9_I12.rds", "independence", "ar1", N=50),
  extract_results("results/scenario_9_I13.rds", "independence", "Exchangeable", N=50),
  extract_results("results/scenario_9_I14.rds", "Exchangeable", "ar1", N=50),
  extract_results("results/scenario_9_I15.rds", "Exchangeable", "Exchangeable", N=50),
  extract_results("results/scenario_9_I16.rds", "ar1", "Exchangeable", N=50),
  extract_results("results/scenario_9_I17.rds", "Exchangeable", "independence", N=50),
  extract_results("results/scenario_9_I18.rds", "ar1", "independence", N=50)
)
I_summary_50










############################## N_medium #########################
##independence, independence
system.time ({
  I21 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_9, B=B)
})
#Save results
saveRDS(I21, file = "results/scenario_9_I21.rds")
#Read results
I21 <- readRDS("results/scenario_9_I21.rds")
I21

##independence, ar1
system.time ({
  I22 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_9, B=B)
})
#Save results
saveRDS(I22, file = "results/scenario_9_I22.rds")
#Read results
I22 <- readRDS("results/scenario_9_I22.rds")
I22



##independence, Exchangeable
system.time ({
  I23 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_9, B=B)
})
#Save results
saveRDS(I23, file = "results/scenario_9_I23.rds")
#Read results
I23 <- readRDS("results/scenario_9_I23.rds")
I23






##Exchangeable, ar1
system.time ({
  I24 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_9, B=B)
})
#Save results
saveRDS(I24, file = "results/scenario_9_I24.rds")
#Read results
I24 <- readRDS("results/scenario_9_I24.rds")
I24





##Exchangeable, Exchangeable
system.time ({
  I25 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_9, B=B)
})
#Save results
saveRDS(I25, file = "results/scenario_9_I25.rds")
#Read results
I25 <- readRDS("results/scenario_9_I25.rds")
I25




## ar1, Exchangeable
system.time ({
  I26 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_9, B=B)
})
#Save results
saveRDS(I26, file = "results/scenario_9_I26.rds")
#Read results
I26 <- readRDS("results/scenario_9_I26.rds")
I26



##Exchangeable, independence
system.time ({
  I27 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_9, B=B)
})
#Save results
saveRDS(I27, file = "results/scenario_9_I27.rds")
#Read results
I27 <- readRDS("results/scenario_9_I27.rds")
I27



##ar1, independence
system.time ({
  I28 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_9, B=B)
})#Save results
saveRDS(I28, file = "results/scenario_9_I28.rds")
#Read results
I28 <- readRDS("results/scenario_9_I28.rds")
I28


I_summary_100 <- bind_rows(
  extract_results("results/scenario_9_I21.rds", "independence", "independence", N=100),
  extract_results("results/scenario_9_I22.rds", "independence", "ar1", N=100),
  extract_results("results/scenario_9_I23.rds", "independence", "Exchangeable", N=100),
  extract_results("results/scenario_9_I24.rds", "Exchangeable", "ar1", N=100),
  extract_results("results/scenario_9_I25.rds", "Exchangeable", "Exchangeable", N=100),
  extract_results("results/scenario_9_I26.rds", "ar1", "Exchangeable", N=100),
  extract_results("results/scenario_9_I27.rds", "Exchangeable", "independence", N=100),
  extract_results("results/scenario_9_I28.rds", "ar1", "independence", N=100)
)
I_summary_100















############################## N_large #########################
##independence, independence
system.time ({
  I31 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_9, B=B)
})
#Save results
saveRDS(I31, file = "results/scenario_9_I31.rds")
#Read results
I31 <- readRDS("results/scenario_9_I31.rds")
I31

##independence, ar1
system.time ({
  I32 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_9, B=B)
})

#Save results
saveRDS(I32, file = "results/scenario_9_I32.rds")
#Read results
I32 <- readRDS("results/scenario_9_I32.rds")
I32



##independence, Exchangeable
system.time ({
  I33 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_9, B=B)
})
#Save results
saveRDS(I33, file = "results/scenario_9_I33.rds")
#Read results
I33 <- readRDS("results/scenario_9_I33.rds")
I33






##Exchangeable, ar1
system.time ({
  I34 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_9, B=B)
})


#Save results
saveRDS(I34, file = "results/scenario_9_I34.rds")
#Read results
I34 <- readRDS("results/scenario_9_I34.rds")
I34





##Exchangeable, Exchangeable
system.time ({
  I35 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_9, B=B)
})
#Save results
saveRDS(I35, file = "results/scenario_9_I35.rds")
#Read results
I35 <- readRDS("results/scenario_9_I35.rds")
I35




## ar1, Exchangeable
system.time ({
  I36 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_9, B=B)
})
#Save results
saveRDS(I36, file = "results/scenario_9_I36.rds")
#Read results
I36 <- readRDS("results/scenario_9_I36.rds")
I36



##Exchangeable, independence
system.time ({
  I37 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_9, B=B)
})
#Save results
saveRDS(I37, file = "results/scenario_9_I37.rds")
#Read results
I37 <- readRDS("results/scenario_9_I37.rds")
I37


##ar1, independence
system.time ({
  I38 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_9, B=B)
})
#Save results
saveRDS(I38, file = "results/scenario_9_I38.rds")
#Read results
I38 <- readRDS("results/scenario_9_I38.rds")
I38



I_summary_500 <- bind_rows(
  extract_results("results/scenario_9_I31.rds", "independence", "independence", N=500),
  extract_results("results/scenario_9_I32.rds", "independence", "ar1", N=500),
  extract_results("results/scenario_9_I33.rds", "independence", "Exchangeable", N=500),
  extract_results("results/scenario_9_I34.rds", "Exchangeable", "ar1", N=500),
  extract_results("results/scenario_9_I35.rds", "Exchangeable", "Exchangeable", N=500),
  extract_results("results/scenario_9_I36.rds", "ar1", "Exchangeable", N=500),
  extract_results("results/scenario_9_I37.rds", "Exchangeable", "independence", N=500),
  extract_results("results/scenario_9_I38.rds", "ar1", "independence", N=500)
)
I_summary_500






I_all <- bind_rows(I_summary_50 %>% mutate(N = 50),
                   I_summary_100 %>% mutate(N = 100),
                   I_summary_500 %>% mutate(N = 500))
latex_table <- I_all %>%
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








