# scenario_3:  a_R = constant vector, r = 15, c = 15, rho = 0.8, phi = 0.8
#              Case 61, 63 and 65

library(dplyr)
library(stringr)
library(matrixgee)
library(xtable)
source("set-up/set_up_design_kron.R")
rows_no_19 <- rows_small
a_R_19 <- a_R_const(rows_no_19)
############################## N_small#########################


##independence, independence
system.time ({
  S11 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_high,
                         phi = phi_high, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_19, B=B)
})
#Save results
saveRDS(S11, file = "results/scenario_19_S11.rds")
#Read results
S11 <- readRDS("results/scenario_19_S11.rds")
S11


##independence, ar1
system.time ({
  S12 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_high,
                         phi = phi_high, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_19, B=B)
})
#Save results
saveRDS(S12, file = "results/scenario_19_S12.rds")
#Read results
S12 <- readRDS("results/scenario_19_S12.rds")
S12




##independence, Exchangeable
system.time ({
  S13 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_high,
                         phi = phi_high, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_19, B=B)
})
#Save results
saveRDS(S13, file = "results/scenario_19_S13.rds")
#Read results
S13 <- readRDS("results/scenario_19_S13.rds")
S13





##Exchangeable, ar1
system.time ({
  S14 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_high,
                         phi = phi_high, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_19, B=B)
})
#Save results
saveRDS(S14, file = "results/scenario_19_S14.rds")
#Read results
S14 <- readRDS("results/scenario_19_S14.rds")
S14



##Exchangeable, Exchangeable
system.time ({
  S15 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_high,
                         phi = phi_high, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_19, B=B)
})
#Save results
saveRDS(S15, file = "results/scenario_19_S15.rds")
#Read results
S15 <- readRDS("results/scenario_19_S15.rds")
S15


##ar1, Exchangeable
system.time ({
  S16 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_high,
                         phi = phi_high, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_19, B=B)
})
#Save results
saveRDS(S16, file = "results/scenario_19_S16.rds")
#Read results
S16 <- readRDS("results/scenario_19_S16.rds")
S16


##Exchangeable, independence
system.time ({
  S17 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_high,
                         phi = phi_high, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_19, B=B)
})
#Save results
saveRDS(S17, file = "results/scenario_19_S17.rds")
#Read results
S17 <- readRDS("results/scenario_19_S17.rds")
S17






##ar1, independence
system.time ({
  S18 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_high,
                         phi = phi_high, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_19, B=B)
})
#Save results
saveRDS(S18, file = "results/scenario_19_S18.rds")
#Read results
S18 <- readRDS("results/scenario_19_S18.rds")
S18

S_summary_50 <- bind_rows(
  extract_results("results/scenario_19_S11.rds", "independence", "independence", N=50),
  extract_results("results/scenario_19_S12.rds", "independence", "ar1", N=50),
  extract_results("results/scenario_19_S13.rds", "independence", "Exchangeable", N=50),
  extract_results("results/scenario_19_S14.rds", "Exchangeable", "ar1", N=50),
  extract_results("results/scenario_19_S15.rds", "Exchangeable", "Exchangeable", N=50),
  extract_results("results/scenario_19_S16.rds", "ar1", "Exchangeable", N=50),
  extract_results("results/scenario_19_S17.rds", "Exchangeable", "independence", N=50),
  extract_results("results/scenario_19_S18.rds", "ar1", "independence", N=50)
)
S_summary_50













############################## N_medium #########################
##independence, independence
system.time ({
  S21 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_high,
                         phi = phi_high, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_19, B=B)
})
#Save results
saveRDS(S21, file = "results/scenario_19_S21.rds")
#Read results
S21 <- readRDS("results/scenario_19_S21.rds")
S21

##independence, ar1
system.time ({
  S22 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_high,
                         phi = phi_high, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_19, B=B)
})
#Save results
saveRDS(S22, file = "results/scenario_19_S22.rds")
#Read results
S22 <- readRDS("results/scenario_19_S22.rds")
S22



##independence, Exchangeable
system.time ({
  S23 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_high,
                         phi = phi_high, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_19, B=B)
})
#Save results
saveRDS(S23, file = "results/scenario_19_S23.rds")
#Read results
S23 <- readRDS("results/scenario_19_S23.rds")
S23






##Exchangeable, ar1
system.time ({
  S24 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_high,
                         phi = phi_high, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_19, B=B)
})
#Save results
saveRDS(S24, file = "results/scenario_19_S24.rds")
#Read results
S24 <- readRDS("results/scenario_19_S24.rds")
S24





##Exchangeable, Exchangeable
system.time ({
  S25 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_high,
                         phi = phi_high, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_19, B=B)
})
#Save results
saveRDS(S25, file = "results/scenario_19_S25.rds")
#Read results
S25 <- readRDS("results/scenario_19_S25.rds")
S25




## ar1, Exchangeable
system.time ({
  S26 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_high,
                         phi = phi_high, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_19, B=B)
})
#Save results
saveRDS(S26, file = "results/scenario_19_S26.rds")
#Read results
S26 <- readRDS("results/scenario_19_S26.rds")
S26



##Exchangeable, independence
system.time ({
  S27 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_high,
                         phi = phi_high, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_19, B=B)
})
#Save results
saveRDS(S27, file = "results/scenario_19_S27.rds")
#Read results
S27 <- readRDS("results/scenario_19_S27.rds")
S27



##ar1, independence
system.time ({
  S28 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_high,
                         phi = phi_high, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_19, B=B)
})
#Save results
saveRDS(S28, file = "results/scenario_19_S28.rds")
#Read results
S28 <- readRDS("results/scenario_19_S28.rds")
S28


S_summary_100 <- bind_rows(
  extract_results("results/scenario_19_S21.rds", "independence", "independence", N=100),
  extract_results("results/scenario_19_S22.rds", "independence", "ar1", N=100),
  extract_results("results/scenario_19_S23.rds", "independence", "Exchangeable", N=100),
  extract_results("results/scenario_19_S24.rds", "Exchangeable", "ar1", N=100),
  extract_results("results/scenario_19_S25.rds", "Exchangeable", "Exchangeable", N=100),
  extract_results("results/scenario_19_S26.rds", "ar1", "Exchangeable", N=100),
  extract_results("results/scenario_19_S27.rds", "Exchangeable", "independence", N=100),
  extract_results("results/scenario_19_S28.rds", "ar1", "independence", N=100)
)
S_summary_100








############################## N_large #########################
##independence, independence
system.time ({
  S31 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_high,
                         phi = phi_high, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_19, B=B)
})
#Save results
saveRDS(S31, file = "results/scenario_19_S31.rds")
#Read results
S31 <- readRDS("results/scenario_19_S31.rds")
S31

##independence, ar1
system.time ({
  S32 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_high,
                         phi = phi_high, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_19, B=B)
})
#Save results
saveRDS(S32, file = "results/scenario_19_S32.rds")
#Read results
S32 <- readRDS("results/scenario_19_S32.rds")
S32



##independence, Exchangeable
system.time ({
  S33 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_high,
                         phi = phi_high, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_19, B=B)
})
#Save results
saveRDS(S33, file = "results/scenario_19_S33.rds")
#Read results
S33 <- readRDS("results/scenario_19_S33.rds")
S33






##Exchangeable, ar1
system.time ({
  S34 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_high,
                         phi = phi_high, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_19, B=B)
})
#Save results
saveRDS(S34, file = "results/scenario_19_S34.rds")
#Read results
S34 <- readRDS("results/scenario_19_S34.rds")
S34





##Exchangeable, Exchangeable
system.time ({
  S35 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_high,
                         phi = phi_high, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_19, B=B)
})
#Save results
saveRDS(S35, file = "results/scenario_19_S35.rds")
#Read results
S35 <- readRDS("results/scenario_19_S35.rds")
S35




## ar1, Exchangeable
system.time ({
  S36 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_high,
                         phi = phi_high, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_19, B=B)
})
#Save results
saveRDS(S36, file = "results/scenario_19_S36.rds")
#Read results
S36 <- readRDS("results/scenario_19_S36.rds")
S36



##Exchangeable, independence
system.time ({
  S37 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_high,
                         phi = phi_high, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_19, B=B)
})
#Save results
saveRDS(S37, file = "results/scenario_19_S37.rds")
#Read results
S37 <- readRDS("results/scenario_19_S37.rds")
S37


##ar1, independence
system.time ({
  S38 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_high,
                         phi = phi_high, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_19, B=B)
})
#Save results
saveRDS(S38, file = "results/scenario_19_S38.rds")
#Read results
S38 <- readRDS("results/scenario_19_S38.rds")
S38




S_summary_500 <- bind_rows(
  extract_results("results/scenario_19_S31.rds", "independence", "independence", N=500),
  extract_results("results/scenario_19_S32.rds", "independence", "ar1", N=500),
  extract_results("results/scenario_19_S33.rds", "independence", "Exchangeable", N=500),
  extract_results("results/scenario_19_S34.rds", "Exchangeable", "ar1", N=500),
  extract_results("results/scenario_19_S35.rds", "Exchangeable", "Exchangeable", N=500),
  extract_results("results/scenario_19_S36.rds", "ar1", "Exchangeable", N=500),
  extract_results("results/scenario_19_S37.rds", "Exchangeable", "independence", N=500),
  extract_results("results/scenario_19_S38.rds", "ar1", "independence", N=500)
)
S_summary_500

S_all <- bind_rows(S_summary_50 %>% mutate(N = 50),
                   S_summary_100 %>% mutate(N = 100),
                   S_summary_500 %>% mutate(N = 500))
latex_table <- S_all %>%
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















