# scenario_11:  a_R  increasing, r = 15, c = 5, rho = 0.8, phi = 0.8 covers case 56,
#              58, and 60

library(dplyr)
library(matrixgee)
library(stringr)
library(xtable)
source("set-up/set_up_design_kron.R")
rows_no_17 <- rows_small
a_R_17 <- a_R_linear(rows_no_17)
############################## N_small#########################




##independence, independence
system.time ({
  Q11 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_high, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_17, B=B)
})
#Save results
saveRDS(Q11, file = "results/scenario_17_Q11.rds")
#Read results
Q11 <- readRDS("results/scenario_17_Q11.rds")
Q11


##independence, ar1
system.time ({
  Q12 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_high, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_17, B=B)
})
#Save results
saveRDS(Q12, file = "results/scenario_17_Q12.rds")
#Read results
Q12 <- readRDS("results/scenario_17_Q12.rds")
Q12




##independence, Exchangeable
system.time ({
  Q13 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_high, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_17, B=B)
})
#Save results
saveRDS(Q13, file = "results/scenario_17_Q13.rds")
#Read results
Q13 <- readRDS("results/scenario_17_Q13.rds")
Q13





##Exchangeable, ar1
system.time ({
  Q14 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_high, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_17, B=B)
})
#Save results
saveRDS(Q14, file = "results/scenario_17_Q14.rds")
#Read results
Q14 <- readRDS("results/scenario_17_Q14.rds")
Q14



##Exchangeable, Exchangeable
system.time ({
  Q15 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_high, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_17, B=B)
})
#Save results
saveRDS(Q15, file = "results/scenario_17_Q15.rds")
#Read results
Q15 <- readRDS("results/scenario_17_Q15.rds")
Q15


##ar1, Exchangeable
system.time ({
  Q16 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_high, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_17, B=B)
})
#Save results
saveRDS(Q16, file = "results/scenario_17_Q16.rds")
#Read results
Q16 <- readRDS("results/scenario_17_Q16.rds")
Q16


##Exchangeable, independence
system.time ({
  Q17 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_high, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_17, B=B)
})
#Save results
saveRDS(Q17, file = "results/scenario_17_Q17.rds")
#Read results
Q17 <- readRDS("results/scenario_17_Q17.rds")
Q17






##ar1, independence
system.time ({
  Q18 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_high, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_17, B=B)
})
#Save results
saveRDS(Q18, file = "results/scenario_17_Q18.rds")
#Read results
Q18 <- readRDS("results/scenario_17_Q18.rds")
Q18




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



Q_summary_50 <- bind_rows(
  extract_results("results/scenario_17_Q11.rds", "independence", "independence", N=50),
  extract_results("results/scenario_17_Q12.rds", "independence", "ar1", N=50),
  extract_results("results/scenario_17_Q13.rds", "independence", "Exchangeable", N=50),
  extract_results("results/scenario_17_Q14.rds", "Exchangeable", "ar1", N=50),
  extract_results("results/scenario_17_Q15.rds", "Exchangeable", "Exchangeable", N=50),
  extract_results("results/scenario_17_Q16.rds", "ar1", "Exchangeable", N=50),
  extract_results("results/scenario_17_Q17.rds", "Exchangeable", "independence", N=50),
  extract_results("results/scenario_17_Q18.rds", "ar1", "independence", N=50)
)
Q_summary_50










############################## N_medium #########################
##independence, independence
system.time ({
  Q21 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_high, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_17, B=B)
})
#Save results
saveRDS(Q21, file = "results/scenario_17_Q21.rds")
#Read results
Q21 <- readRDS("results/scenario_17_Q21.rds")
Q21

##independence, ar1
system.time ({
  Q22 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_high, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_17, B=B)
})
#Save results
saveRDS(Q22, file = "results/scenario_17_Q22.rds")
#Read results
Q22 <- readRDS("results/scenario_17_Q22.rds")
Q22



##independence, Exchangeable
system.time ({
  Q23 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_high, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_17, B=B)
})
#Save results
saveRDS(Q23, file = "results/scenario_17_Q23.rds")
#Read results
Q23 <- readRDS("results/scenario_17_Q23.rds")
Q23






##Exchangeable, ar1
system.time ({
  Q24 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_high, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_17, B=B)
})
#Save results
saveRDS(Q24, file = "results/scenario_17_Q24.rds")
#Read results
Q24 <- readRDS("results/scenario_17_Q24.rds")
Q24





##Exchangeable, Exchangeable
system.time ({
  Q25 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_high, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_17, B=B)
})
#Save results
saveRDS(Q25, file = "results/scenario_17_Q25.rds")
#Read results
Q25 <- readRDS("results/scenario_17_Q25.rds")
Q25




## ar1, Exchangeable
system.time ({
  Q26 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_high, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_17, B=B)
})
#Save results
saveRDS(Q26, file = "results/scenario_17_Q26.rds")
#Read results
Q26 <- readRDS("results/scenario_17_Q26.rds")
Q26



##Exchangeable, independence
system.time ({
  Q27 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_high, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_17, B=B)
})
#Save results
saveRDS(Q27, file = "results/scenario_17_Q27.rds")
#Read results
Q27 <- readRDS("results/scenario_17_Q27.rds")
Q27



##ar1, independence
system.time ({
  Q28 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_high, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_17, B=B)
})#Save results
saveRDS(Q28, file = "results/scenario_17_Q28.rds")
#Read results
Q28 <- readRDS("results/scenario_17_Q28.rds")
Q28


Q_summary_100 <- bind_rows(
  extract_results("results/scenario_17_Q21.rds", "independence", "independence", N=100),
  extract_results("results/scenario_17_Q22.rds", "independence", "ar1", N=100),
  extract_results("results/scenario_17_Q23.rds", "independence", "Exchangeable", N=100),
  extract_results("results/scenario_17_Q24.rds", "Exchangeable", "ar1", N=100),
  extract_results("results/scenario_17_Q25.rds", "Exchangeable", "Exchangeable", N=100),
  extract_results("results/scenario_17_Q26.rds", "ar1", "Exchangeable", N=100),
  extract_results("results/scenario_17_Q27.rds", "Exchangeable", "independence", N=100),
  extract_results("results/scenario_17_Q28.rds", "ar1", "independence", N=100)
)
Q_summary_100















############################## N_large #########################
##independence, independence
system.time ({
  Q31 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_high, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_17, B=B)
})
#Save results
saveRDS(Q31, file = "results/scenario_17_Q31.rds")
#Read results
Q31 <- readRDS("results/scenario_17_Q31.rds")
Q31

##independence, ar1
system.time ({
  Q32 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_high, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_17, B=B)
})

#Save results
saveRDS(Q32, file = "results/scenario_17_Q32.rds")
#Read results
Q32 <- readRDS("results/scenario_17_Q32.rds")
Q32



##independence, Exchangeable
system.time ({
  Q33 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_high, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_17, B=B)
})
#Save results
saveRDS(Q33, file = "results/scenario_17_Q33.rds")
#Read results
Q33 <- readRDS("results/scenario_17_Q33.rds")
Q33






##Exchangeable, ar1
system.time ({
  Q34 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_high, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_17, B=B)
})


#Save results
saveRDS(Q34, file = "results/scenario_17_Q34.rds")
#Read results
Q34 <- readRDS("results/scenario_17_Q34.rds")
Q34





##Exchangeable, Exchangeable
system.time ({
  Q35 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_high, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_17, B=B)
})
#Save results
saveRDS(Q35, file = "results/scenario_17_Q35.rds")
#Read results
Q35 <- readRDS("results/scenario_17_Q35.rds")
Q35




## ar1, Exchangeable
system.time ({
  Q36 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_high, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_17, B=B)
})
#Save results
saveRDS(Q36, file = "results/scenario_17_Q36.rds")
#Read results
Q36 <- readRDS("results/scenario_17_Q36.rds")
Q36



##Exchangeable, independence
system.time ({
  Q37 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_high, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_17, B=B)
})
#Save results
saveRDS(Q37, file = "results/scenario_17_Q37.rds")
#Read results
Q37 <- readRDS("results/scenario_17_Q37.rds")
Q37


##ar1, independence
system.time ({
  Q38 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_high, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_17, B=B)
})
#Save results
saveRDS(Q38, file = "results/scenario_17_Q38.rds")
#Read results
Q38 <- readRDS("results/scenario_17_Q38.rds")
Q38



Q_summary_500 <- bind_rows(
  extract_results("results/scenario_17_Q31.rds", "independence", "independence", N=500),
  extract_results("results/scenario_17_Q32.rds", "independence", "ar1", N=500),
  extract_results("results/scenario_17_Q33.rds", "independence", "Exchangeable", N=500),
  extract_results("results/scenario_17_Q34.rds", "Exchangeable", "ar1", N=500),
  extract_results("results/scenario_17_Q35.rds", "Exchangeable", "Exchangeable", N=500),
  extract_results("results/scenario_17_Q36.rds", "ar1", "Exchangeable", N=500),
  extract_results("results/scenario_17_Q37.rds", "Exchangeable", "independence", N=500),
  extract_results("results/scenario_17_Q38.rds", "ar1", "independence", N=500)
)
Q_summary_500






Q_all <- bind_rows(Q_summary_50 %>% mutate(N = 50),
                   Q_summary_100 %>% mutate(N = 100),
                   Q_summary_500 %>% mutate(N = 500))
latex_table <- Q_all %>%
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








