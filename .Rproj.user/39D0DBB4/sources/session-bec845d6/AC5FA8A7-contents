# scenario_3:  a_R = constant vector, r = 15, c = 15, rho = 0.3, phi = 0.3
#              Case 7, 9 and 11

library(dplyr)
library(stringr)
library(matrixgee)
library(xtable)
source("set-up/set_up_design_kron.R")
rows_no_3 <- rows_small
a_R_3 <- a_R_const(rows_no)
############################## N_small#########################


##independence, independence
system.time ({
  C11 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_3, B=B)
})
#Save results
saveRDS(C11, file = "results/scenario_3_C11.rds")
#Read results
C11 <- readRDS("results/scenario_3_C11.rds")
C11


##independence, ar1
system.time ({
  C12 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_3, B=B)
})
#Save results
saveRDS(C12, file = "results/scenario_3_C12.rds")
#Read results
C12 <- readRDS("results/scenario_3_C12.rds")
C12




##independence, Exchangeable
system.time ({
  C13 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_3, B=B)
})
#Save results
saveRDS(C13, file = "results/scenario_3_C13.rds")
#Read results
C13 <- readRDS("results/scenario_3_C13.rds")
C13





##Exchangeable, ar1
system.time ({
  C14 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_3, B=B)
})
#Save results
saveRDS(C14, file = "results/scenario_3_C14.rds")
#Read results
C14 <- readRDS("results/scenario_3_C14.rds")
C14



##Exchangeable, Exchangeable
system.time ({
  C15 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_3, B=B)
})
#Save results
saveRDS(C15, file = "results/scenario_3_C15.rds")
#Read results
C15 <- readRDS("results/scenario_3_C15.rds")
C15


##ar1, Exchangeable
system.time ({
  C16 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_1, B=B)
})
#Save results
saveRDS(C16, file = "results/scenario_3_C16.rds")
#Read results
C16 <- readRDS("results/scenario_3_C16.rds")
C16


##Exchangeable, independence
system.time ({
  C17 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_3, B=B)
})
#Save results
saveRDS(C17, file = "results/scenario_3_C17.rds")
#Read results
C17 <- readRDS("results/scenario_3_C17.rds")
C17






##ar1, independence
system.time ({
  C18 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_3, B=B)
})
#Save results
saveRDS(C18, file = "results/scenario_3_C18.rds")
#Read results
C18 <- readRDS("results/scenario_3_C18.rds")
C18

C_summary_50 <- bind_rows(
  extract_results("results/scenario_3_C11.rds", "independence", "independence", N=50),
  extract_results("results/scenario_3_C12.rds", "independence", "ar1", N=50),
  extract_results("results/scenario_3_C13.rds", "independence", "Exchangeable", N=50),
  extract_results("results/scenario_3_C14.rds", "Exchangeable", "ar1", N=50),
  extract_results("results/scenario_3_C15.rds", "Exchangeable", "Exchangeable", N=50),
  extract_results("results/scenario_3_C16.rds", "ar1", "Exchangeable", N=50),
  extract_results("results/scenario_3_C17.rds", "Exchangeable", "independence", N=50),
  extract_results("results/scenario_3_C18.rds", "ar1", "independence", N=50)
)
C_summary_50













############################## N_medium #########################
##independence, independence
system.time ({
  C21 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_3, B=B)
})
#Save results
saveRDS(C21, file = "results/scenario_3_C21.rds")
#Read results
C21 <- readRDS("results/scenario_3_C21.rds")
C21

##independence, ar1
system.time ({
  C22 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_3, B=B)
})
#Save results
saveRDS(C22, file = "results/scenario_3_C22.rds")
#Read results
C22 <- readRDS("results/scenario_3_C22.rds")
C22



##independence, Exchangeable
system.time ({
  C23 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_3, B=B)
})
#Save results
saveRDS(C23, file = "results/scenario_3_C23.rds")
#Read results
C23 <- readRDS("results/scenario_3_C23.rds")
C23






##Exchangeable, ar1
system.time ({
  C24 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_3, B=B)
})
#Save results
saveRDS(C24, file = "results/scenario_3_C24.rds")
#Read results
C24 <- readRDS("results/scenario_3_C24.rds")
C24





##Exchangeable, Exchangeable
system.time ({
  C25 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_3, B=B)
})
#Save results
saveRDS(C25, file = "results/scenario_3_C25.rds")
#Read results
C25 <- readRDS("results/scenario_3_C25.rds")
C25




## ar1, Exchangeable
system.time ({
  C26 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_3, B=B)
})
#Save results
saveRDS(C26, file = "results/scenario_3_C26.rds")
#Read results
C26 <- readRDS("results/scenario_3_C26.rds")
C26



##Exchangeable, independence
system.time ({
  C27 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_3, B=B)
})
#Save results
saveRDS(C27, file = "results/scenario_3_C27.rds")
#Read results
C27 <- readRDS("results/scenario_3_C27.rds")
C27



##ar1, independence
system.time ({
  C28 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_3, B=B)
})
#Save results
saveRDS(C28, file = "results/scenario_3_C28.rds")
#Read results
C28 <- readRDS("results/scenario_3_C28.rds")
C28


C_summary_100 <- bind_rows(
  extract_results("results/scenario_3_C21.rds", "independence", "independence", N=100),
  extract_results("results/scenario_3_C22.rds", "independence", "ar1", N=100),
  extract_results("results/scenario_3_C23.rds", "independence", "Exchangeable", N=100),
  extract_results("results/scenario_3_C24.rds", "Exchangeable", "ar1", N=100),
  extract_results("results/scenario_3_C25.rds", "Exchangeable", "Exchangeable", N=100),
  extract_results("results/scenario_3_C26.rds", "ar1", "Exchangeable", N=100),
  extract_results("results/scenario_3_C27.rds", "Exchangeable", "independence", N=100),
  extract_results("results/scenario_3_C28.rds", "ar1", "independence", N=100)
)
C_summary_100








############################## N_large #########################
##independence, independence
system.time ({
  C31 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_3, B=B)
})
#Save results
saveRDS(C31, file = "results/scenario_3_C31.rds")
#Read results
C31 <- readRDS("results/scenario_3_C31.rds")
C31

##independence, ar1
system.time ({
  C32 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_3, B=B)
})
#Save results
saveRDS(C32, file = "results/scenario_3_C32.rds")
#Read results
C32 <- readRDS("results/scenario_3_C32.rds")
C32



##independence, Exchangeable
system.time ({
  C33 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_3, B=B)
})
#Save results
saveRDS(C33, file = "results/scenario_3_C33.rds")
#Read results
C33 <- readRDS("results/scenario_3_C33.rds")
C33






##Exchangeable, ar1
system.time ({
  C34 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_3, B=B)
})
#Save results
saveRDS(C34, file = "results/scenario_3_C34.rds")
#Read results
C34 <- readRDS("results/scenario_3_C34.rds")
C34





##Exchangeable, Exchangeable
system.time ({
  C35 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_3, B=B)
})
#Save results
saveRDS(C35, file = "results/scenario_3_C35.rds")
#Read results
C35 <- readRDS("results/scenario_3_C35.rds")
C35




## ar1, Exchangeable
system.time ({
  C36 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_3, B=B)
})
#Save results
saveRDS(C36, file = "results/scenario_3_C36.rds")
#Read results
C36 <- readRDS("results/scenario_3_C36.rds")
C36



##Exchangeable, independence
system.time ({
  C37 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_3, B=B)
})
#Save results
saveRDS(C37, file = "results/scenario_3_C37.rds")
#Read results
C37 <- readRDS("results/scenario_3_C37.rds")
C37


##ar1, independence
system.time ({
  C38 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_low, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_3, B=B)
})
#Save results
saveRDS(C38, file = "results/scenario_3_C38.rds")
#Read results
C38 <- readRDS("results/scenario_3_C38.rds")
C38




C_summary_500 <- bind_rows(
  extract_results("results/scenario_3_C31.rds", "independence", "independence", N=500),
  extract_results("results/scenario_3_C32.rds", "independence", "ar1", N=500),
  extract_results("results/scenario_3_C33.rds", "independence", "Exchangeable", N=500),
  extract_results("results/scenario_3_C34.rds", "Exchangeable", "ar1", N=500),
  extract_results("results/scenario_3_C35.rds", "Exchangeable", "Exchangeable", N=500),
  extract_results("results/scenario_3_C36.rds", "ar1", "Exchangeable", N=500),
  extract_results("results/scenario_3_C37.rds", "Exchangeable", "independence", N=500),
  extract_results("results/scenario_3_C38.rds", "ar1", "independence", N=500)
)
C_summary_500

C_all <- bind_rows(C_summary_50 %>% mutate(N = 50),
                   C_summary_100 %>% mutate(N = 100),
                   C_summary_500 %>% mutate(N = 500))
latex_table <- C_all %>%
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















