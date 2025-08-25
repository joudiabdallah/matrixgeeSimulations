# scenario_13:  a_R  constant, r = 15, c = 5, rho = 0.8 
library(MASS)
library(matrixgee)
library(dplyr)
library(stringr)
library(xtable)
library(parallel)
source("set-up/set_up_design_kron.R")
rows_no_13 <- rows_small
a_R_13 <- a_R_const(rows_no_13)
############################## N_small#########################




##independence
system.time ({
  M11 <- simfct_nokron_parallel(sample_size = N_small,rows_no = rows_small,
                                cols_no = cols_small, rho = rho_high,
                                n_sim =10000, covariates=p_50, 
                                intercept="row_intercept",
                                max_iter = 20, tol = 1e-6, 
                                corstr = "independence", 
                                family = gaussian(), alpha=0.05, a_R=a_R_13, B=B)
})
#Save results
saveRDS(M11, file = "results/NoKronCase/scenario_13_M11.rds")
#Read results
M11 <- readRDS("results//NoKronCase/scenario_13_M11.rds")
M11















## ar1
system.time ({
  M12 <- simfct_nokron_parallel(sample_size = N_small,rows_no = rows_small,
                                cols_no = cols_small, rho = rho_high,
                                n_sim =10000, covariates=p_50, 
                                intercept="row_intercept",
                                max_iter = 20, tol = 1e-6, 
                                corstr = "ar1", 
                                family = gaussian(), alpha=0.05, a_R=a_R_13, B=B)
})
#Save results
saveRDS(M12, file = "results/NoKronCase/scenario_13_M12.rds")
#Read results
M12 <- readRDS("results/NoKronCase/scenario_13_M12.rds")
M12




## Exchangeable
system.time ({
  M13 <- simfct_nokron_parallel(sample_size = N_small,rows_no = rows_small,
                                cols_no = cols_small, rho = rho_high,
                                n_sim =10000, covariates=p_50, 
                                intercept="row_intercept",
                                max_iter = 20, tol = 1e-6, 
                                corstr = "Exchangeable", 
                                family = gaussian(), alpha=0.05, a_R=a_R_13, B=B)
})
#Save results
saveRDS(M13, file = "results/NoKronCase/scenario_13_M13.rds")
#Read results
M13 <- readRDS("results/NoKronCase/scenario_13_M13.rds")
M13









extract_results <- function(file_path, corstr, N) {
  result <- readRDS(file_path)
  
  tibble(
    corstr = corstr,
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



M_summary_50 <- bind_rows(
  extract_results("results/NoKronCase/scenario_13_M11.rds", "independence",  N=50),
  extract_results("results/NoKronCase/scenario_13_M12.rds",  "ar1", N=50),
  extract_results("results/NoKronCase/scenario_13_M13.rds",  "Exchangeable", N=50),
  
)
M_summary_50










############################## N_medium #########################
##independence
system.time ({
  M21 <- simfct_nokron_parallel(sample_size = N_medium,rows_no = rows_small,
                                cols_no = cols_small, rho = rho_high,
                                n_sim =10000, covariates=p_100, 
                                intercept="row_intercept",
                                max_iter = 20, tol = 1e-6, 
                                corstr = "independence", 
                                family = gaussian(), alpha=0.05, a_R=a_R_13, B=B)
})
#Save results
saveRDS(M21, file = "results/NoKronCase/scenario_13_M21.rds")
#Read results
M21 <- readRDS("results/NoKronCase/scenario_13_M21.rds")
M21

## ar1
system.time ({
  M22 <- simfct_nokron_parallel(sample_size = N_medium,rows_no = rows_small,
                                cols_no = cols_small, rho = rho_high,
                                n_sim =10000, covariates=p_100, 
                                intercept="row_intercept",
                                max_iter = 20, tol = 1e-6, 
                                corstr = "ar1", 
                                family = gaussian(), alpha=0.05, a_R=a_R_13, B=B)
})
#Save results
saveRDS(M22, file = "results/NoKronCase/scenario_13_M22.rds")
#Read results
M22 <- readRDS("results/NoKronCase/scenario_13_M22.rds")
M22



## Exchangeable
system.time ({
  M23 <- simfct_nokron_parallel(sample_size = N_medium,rows_no = rows_small,
                                cols_no = cols_small, rho = rho_high,
                                n_sim =10000, covariates=p_100, 
                                intercept="row_intercept",
                                max_iter = 20, tol = 1e-6, 
                                corstr = "Exchangeable", 
                                family = gaussian(), alpha=0.05, a_R=a_R_13, B=B)
})
#Save results
saveRDS(M23, file = "results/NoKronCase/scenario_13_M23.rds")
#Read results
M23 <- readRDS("results/NoKronCase/scenario_13_M23.rds")
M23







M_summary_100 <- bind_rows(
  extract_results("results/NoKronCase/scenario_13_M21.rds", "independence",  N=100),
  extract_results("results/NoKronCase/scenario_13_M22.rds",  "ar1", N=100),
  extract_results("results/NoKronCase/scenario_13_M23.rds",  "Exchangeable", N=100),
  
)
M_summary_100















############################## N_large #########################
##independence
system.time ({
  M31 <- simfct_nokron_parallel(sample_size = N_large,rows_no = rows_small,
                                cols_no = cols_small, rho = rho_high,
                                n_sim =10000, covariates=p_500, 
                                intercept="row_intercept",
                                max_iter = 20, tol = 1e-6, 
                                corstr = "independence", 
                                family = gaussian(), alpha=0.05, a_R=a_R_13, B=B)
})
#Save results
saveRDS(M31, file = "results/NoKronCase/scenario_13_M31.rds")
#Read results
M31 <- readRDS("results/NoKronCase/scenario_13_M31.rds")
M31

## ar1
system.time ({
  M32 <- simfct_nokron_parallel(sample_size = N_large,rows_no = rows_small,
                                cols_no = cols_small, rho = rho_high,
                                n_sim =10000, covariates=p_500, 
                                intercept="row_intercept",
                                max_iter = 20, tol = 1e-6, 
                                corstr = "ar1", 
                                family = gaussian(), alpha=0.05, a_R=a_R_13, B=B)
})
#Save results
saveRDS(M32, file = "results/NoKronCase/scenario_13_M32.rds")
#Read results
M32 <- readRDS("results/NoKronCase/scenario_13_M32.rds")
M32



## Exchangeable
system.time ({
  M33 <- simfct_nokron_parallel(sample_size = N_large,rows_no = rows_small,
                                cols_no = cols_small, rho = rho_high,
                                n_sim =10000, covariates=p_500, 
                                intercept="row_intercept",
                                max_iter = 20, tol = 1e-6, 
                                corstr = "Exchangeable", 
                                family = gaussian(), alpha=0.05, a_R=a_R_13, B=B)
})
#Save results
saveRDS(M33, file = "results/NoKronCase/scenario_13_M33.rds")
#Read results
M33 <- readRDS("results/NoKronCase/scenario_13_M33.rds")
M33









M_summary_500 <- bind_rows(
  extract_results("results/NoKronCase/scenario_13_M31.rds", "independence",  N=500),
  extract_results("results/NoKronCase/scenario_13_M32.rds",  "ar1", N=500),
  extract_results("results/NoKronCase/scenario_13_M33.rds",  "Exchangeable", N=500),
  
)
M_summary_500






M_all <- bind_rows(M_summary_50 %>% mutate(N = 50),
                   M_summary_100 %>% mutate(N = 100),
                   M_summary_500 %>% mutate(N = 500))
latex_table <- M_all %>%
  arrange(corstr, N) %>%
  mutate(SampleSize = paste0("N=", N)) %>%
  select(corstr, SampleSize, ARE, RMSE, Frob, MAB, MSE, Errmean,
         MinErr, MaxErr, MedianErr, AREemp) %>%
  group_by(corstr) %>%
  mutate(corstr = if_else(row_number() == 1, corstr, "")) %>%
  ungroup() %>%
  mutate(across(
    .cols = ARE:AREemp,
    .fns = ~ format(round(.x, 4), nsmall = 4)
  ))

# Step 4: Create xtable
xtab <- xtable(
  latex_table,
  caption = "Simulation Results Across Sample Sizes - Scenario 10 (No Kronecker Case)",
  label = "tab:sim-results"
)

# Step 5: Print LaTeX table with formatting
cat("\\begin{flushleft}\n")

print(
  xtab,
  include.rownames = FALSE,
  booktabs = TRUE,
  caption.placement = "top",
  sanitize.colnames.function = identity,
  add.to.row = list(pos = list(0), command = "\\scriptsize\n"),
  table.placement = "H"
)

cat("\\end{flushleft}\n")





