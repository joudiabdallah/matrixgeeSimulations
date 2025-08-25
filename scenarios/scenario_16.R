# scenario_11:  a_R  constant, r = 15, c = 15, rho = 0.8, phi = 0.3 covers case 43,
#              45, and 47

library(dplyr)
library(stringr)
library(xtable)
source("set-up/set_up_design_kron.R")
rows_no_16 <- rows_small
a_R_16 <- a_R_const(rows_no_16)
############################## N_small#########################




##independence, independence
system.time ({
  P11 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_16, B=B)
})
#Save results
saveRDS(P11, file = "results/scenario_16_P11.rds")
#Read results
P11 <- readRDS("results/scenario_16_P11.rds")
P11


##independence, ar1
system.time ({
  P12 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_16, B=B)
})
#Save results
saveRDS(P12, file = "results/scenario_16_P12.rds")
#Read results
P12 <- readRDS("results/scenario_16_P12.rds")
P12




##independence, Exchangeable
system.time ({
  P13 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_16, B=B)
})
#Save results
saveRDS(P13, file = "results/scenario_16_P13.rds")
#Read results
P13 <- readRDS("results/scenario_16_P13.rds")
P13





##Exchangeable, ar1
system.time ({
  P14 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_16, B=B)
})
#Save results
saveRDS(P14, file = "results/scenario_16_P14.rds")
#Read results
P14 <- readRDS("results/scenario_16_P14.rds")
P14



##Exchangeable, Exchangeable
system.time ({
  P15 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_16, B=B)
})
#Save results
saveRDS(P15, file = "results/scenario_16_P15.rds")
#Read results
P15 <- readRDS("results/scenario_16_P15.rds")
P15


##ar1, Exchangeable
system.time ({
  P16 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_16, B=B)
})
#Save results
saveRDS(P16, file = "results/scenario_16_P16.rds")
#Read results
P16 <- readRDS("results/scenario_16_P16.rds")
P16


##Exchangeable, independence
system.time ({
  P17 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_16, B=B)
})
#Save results
saveRDS(P17, file = "results/scenario_16_P17.rds")
#Read results
P17 <- readRDS("results/scenario_16_P17.rds")
P17






##ar1, independence
system.time ({
  P18 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_16, B=B)
})
#Save results
saveRDS(P18, file = "results/scenario_16_P18.rds")
#Read results
P18 <- readRDS("results/scenario_16_P18.rds")
P18




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



P_summary_50 <- bind_rows(
  extract_results("results/scenario_16_P11.rds", "independence", "independence", N=50),
  extract_results("results/scenario_16_P12.rds", "independence", "ar1", N=50),
  extract_results("results/scenario_16_P13.rds", "independence", "Exchangeable", N=50),
  extract_results("results/scenario_16_P14.rds", "Exchangeable", "ar1", N=50),
  extract_results("results/scenario_16_P15.rds", "Exchangeable", "Exchangeable", N=50),
  extract_results("results/scenario_16_P16.rds", "ar1", "Exchangeable", N=50),
  extract_results("results/scenario_16_P17.rds", "Exchangeable", "independence", N=50),
  extract_results("results/scenario_16_P18.rds", "ar1", "independence", N=50)
)
P_summary_50










############################## N_medium #########################
##independence, independence
system.time ({
  P21 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_16, B=B)
})
#Save results
saveRDS(P21, file = "results/scenario_16_P21.rds")
#Read results
P21 <- readRDS("results/scenario_16_P21.rds")
P21

##independence, ar1
system.time ({
  P22 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_16, B=B)
})
#Save results
saveRDS(P22, file = "results/scenario_16_P22.rds")
#Read results
P22 <- readRDS("results/scenario_16_P22.rds")
P22



##independence, Exchangeable
system.time ({
  P23 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_16, B=B)
})
#Save results
saveRDS(P23, file = "results/scenario_16_P23.rds")
#Read results
P23 <- readRDS("results/scenario_16_P23.rds")
P23






##Exchangeable, ar1
system.time ({
  P24 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_16, B=B)
})
#Save results
saveRDS(P24, file = "results/scenario_16_P24.rds")
#Read results
P24 <- readRDS("results/scenario_16_P24.rds")
P24





##Exchangeable, Exchangeable
system.time ({
  P25 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_16, B=B)
})
#Save results
saveRDS(P25, file = "results/scenario_16_P25.rds")
#Read results
P25 <- readRDS("results/scenario_16_P25.rds")
P25




## ar1, Exchangeable
system.time ({
  P26 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_16, B=B)
})
#Save results
saveRDS(P26, file = "results/scenario_16_P26.rds")
#Read results
P26 <- readRDS("results/scenario_16_P26.rds")
P26



##Exchangeable, independence
system.time ({
  P27 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_16, B=B)
})
#Save results
saveRDS(P27, file = "results/scenario_16_P27.rds")
#Read results
P27 <- readRDS("results/scenario_16_P27.rds")
P27



##ar1, independence
system.time ({
  P28 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_16, B=B)
})#Save results
saveRDS(P28, file = "results/scenario_16_P28.rds")
#Read results
P28 <- readRDS("results/scenario_16_P28.rds")
P28


P_summary_100 <- bind_rows(
  extract_results("results/scenario_16_P21.rds", "independence", "independence", N=100),
  extract_results("results/scenario_16_P22.rds", "independence", "ar1", N=100),
  extract_results("results/scenario_16_P23.rds", "independence", "Exchangeable", N=100),
  extract_results("results/scenario_16_P24.rds", "Exchangeable", "ar1", N=100),
  extract_results("results/scenario_16_P25.rds", "Exchangeable", "Exchangeable", N=100),
  extract_results("results/scenario_16_P26.rds", "ar1", "Exchangeable", N=100),
  extract_results("results/scenario_16_P27.rds", "Exchangeable", "independence", N=100),
  extract_results("results/scenario_16_P28.rds", "ar1", "independence", N=100)
)
P_summary_100















############################## N_large #########################
##independence, independence
system.time ({
  P31 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_16, B=B)
})
#Save results
saveRDS(P31, file = "results/scenario_16_P31.rds")
#Read results
P31 <- readRDS("results/scenario_16_P31.rds")
P31

##independence, ar1
system.time ({
  P32 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_16, B=B)
})

#Save results
saveRDS(P32, file = "results/scenario_16_P32.rds")
#Read results
P32 <- readRDS("results/scenario_16_P32.rds")
P32



##independence, Exchangeable
system.time ({
  P33 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_16, B=B)
})
#Save results
saveRDS(P33, file = "results/scenario_16_P33.rds")
#Read results
P33 <- readRDS("results/scenario_16_P33.rds")
P33






##Exchangeable, ar1
system.time ({
  P34 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_16, B=B)
})


#Save results
saveRDS(P34, file = "results/scenario_16_P34.rds")
#Read results
P34 <- readRDS("results/scenario_16_P34.rds")
P34





##Exchangeable, Exchangeable
system.time ({
  P35 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_16, B=B)
})
#Save results
saveRDS(P35, file = "results/scenario_16_P35.rds")
#Read results
P35 <- readRDS("results/scenario_16_P35.rds")
P35




## ar1, Exchangeable
system.time ({
  P36 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_16, B=B)
})
#Save results
saveRDS(P36, file = "results/scenario_16_P36.rds")
#Read results
P36 <- readRDS("results/scenario_16_P36.rds")
P36



##Exchangeable, independence
system.time ({
  P37 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_16, B=B)
})
#Save results
saveRDS(P37, file = "results/scenario_16_P37.rds")
#Read results
P37 <- readRDS("results/scenario_16_P37.rds")
P37


##ar1, independence
system.time ({
  P38 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_16, B=B)
})
#Save results
saveRDS(P38, file = "results/scenario_16_P38.rds")
#Read results
P38 <- readRDS("results/scenario_16_P38.rds")
P38



P_summary_500 <- bind_rows(
  extract_results("results/scenario_16_P31.rds", "independence", "independence", N=500),
  extract_results("results/scenario_16_P32.rds", "independence", "ar1", N=500),
  extract_results("results/scenario_16_P33.rds", "independence", "Exchangeable", N=500),
  extract_results("results/scenario_16_P34.rds", "Exchangeable", "ar1", N=500),
  extract_results("results/scenario_16_P35.rds", "Exchangeable", "Exchangeable", N=500),
  extract_results("results/scenario_16_P36.rds", "ar1", "Exchangeable", N=500),
  extract_results("results/scenario_16_P37.rds", "Exchangeable", "independence", N=500),
  extract_results("results/scenario_16_P38.rds", "ar1", "independence", N=500)
)
P_summary_500






P_all <- bind_rows(P_summary_50 %>% mutate(N = 50),
                   P_summary_100 %>% mutate(N = 100),
                   P_summary_500 %>% mutate(N = 500))
latex_table <- P_all %>%
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








