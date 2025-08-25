# scenario_12:  a_R  linear, r = 15, c = 5, rho = 0.3 
library(MASS)
library(matrixgee)
library(dplyr)
library(stringr)
library(xtable)
library(parallel)
source("set-up/set_up_design_kron.R")
rows_no_12 <- rows_small
a_R_12 <- a_R_linear(rows_no_12)
############################## N_small#########################




##independence
system.time ({
  L11 <- simfct_nokron_parallel(sample_size = N_small,rows_no = rows_small,
                                cols_no = cols_small, rho = rho_low,
                                n_sim =10000, covariates=p_50, 
                                intercept="row_intercept",
                                max_iter = 20, tol = 1e-6, 
                                corstr = "independence", 
                                family = gaussian(), alpha=0.05, a_R=a_R_12, B=B)
})
#Save results
saveRDS(L11, file = "results/NoKronCase/scenario_12_L11.rds")
#Read results
L11 <- readRDS("results//NoKronCase/scenario_12_L11.rds")
L11















## ar1
system.time ({
  L12 <- simfct_nokron_parallel(sample_size = N_small,rows_no = rows_small,
                                cols_no = cols_small, rho = rho_low,
                                n_sim =10000, covariates=p_50, 
                                intercept="row_intercept",
                                max_iter = 20, tol = 1e-6, 
                                corstr = "ar1", 
                                family = gaussian(), alpha=0.05, a_R=a_R_12, B=B)
})
#Save results
saveRDS(L12, file = "results/NoKronCase/scenario_12_L12.rds")
#Read results
L12 <- readRDS("results/NoKronCase/scenario_12_L12.rds")
L12




## Exchangeable
system.time ({
  L13 <- simfct_nokron_parallel(sample_size = N_small,rows_no = rows_small,
                                cols_no = cols_small, rho = rho_low,
                                n_sim =10000, covariates=p_50, 
                                intercept="row_intercept",
                                max_iter = 20, tol = 1e-6, 
                                corstr = "Exchangeable", 
                                family = gaussian(), alpha=0.05, a_R=a_R_12, B=B)
})
#Save results
saveRDS(L13, file = "results/NoKronCase/scenario_12_L13.rds")
#Read results
L13 <- readRDS("results/NoKronCase/scenario_12_L13.rds")
L13









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



L_summary_50 <- bind_rows(
  extract_results("results/NoKronCase/scenario_12_L11.rds", "independence",  N=50),
  extract_results("results/NoKronCase/scenario_12_L12.rds",  "ar1", N=50),
  extract_results("results/NoKronCase/scenario_12_L13.rds",  "Exchangeable", N=50),
  
)
L_summary_50










############################## N_medium #########################
##independence
system.time ({
  L21 <- simfct_nokron_parallel(sample_size = N_medium,rows_no = rows_small,
                                cols_no = cols_small, rho = rho_low,
                                n_sim =10000, covariates=p_100, 
                                intercept="row_intercept",
                                max_iter = 20, tol = 1e-6, 
                                corstr = "independence", 
                                family = gaussian(), alpha=0.05, a_R=a_R_12, B=B)
})
#Save results
saveRDS(L21, file = "results/NoKronCase/scenario_12_L21.rds")
#Read results
L21 <- readRDS("results/NoKronCase/scenario_12_L21.rds")
L21

## ar1
system.time ({
  L22 <- simfct_nokron_parallel(sample_size = N_medium,rows_no = rows_small,
                                cols_no = cols_small, rho = rho_low,
                                n_sim =10000, covariates=p_100, 
                                intercept="row_intercept",
                                max_iter = 20, tol = 1e-6, 
                                corstr = "ar1", 
                                family = gaussian(), alpha=0.05, a_R=a_R_12, B=B)
})
#Save results
saveRDS(L22, file = "results/NoKronCase/scenario_12_L22.rds")
#Read results
L22 <- readRDS("results/NoKronCase/scenario_12_L22.rds")
L22



## Exchangeable
system.time ({
  L23 <- simfct_nokron_parallel(sample_size = N_medium,rows_no = rows_small,
                                cols_no = cols_small, rho = rho_low,
                                n_sim =10000, covariates=p_100, 
                                intercept="row_intercept",
                                max_iter = 20, tol = 1e-6, 
                                corstr = "Exchangeable", 
                                family = gaussian(), alpha=0.05, a_R=a_R_12, B=B)
})
#Save results
saveRDS(L23, file = "results/NoKronCase/scenario_12_L23.rds")
#Read results
L23 <- readRDS("results/NoKronCase/scenario_12_L23.rds")
L23







L_summary_100 <- bind_rows(
  extract_results("results/NoKronCase/scenario_12_L21.rds", "independence",  N=100),
  extract_results("results/NoKronCase/scenario_12_L22.rds",  "ar1", N=100),
  extract_results("results/NoKronCase/scenario_12_L23.rds",  "Exchangeable", N=100),
  
)
L_summary_100















############################## N_large #########################
##independence
system.time ({
  L31 <- simfct_nokron_parallel(sample_size = N_large,rows_no = rows_small,
                                cols_no = cols_small, rho = rho_low,
                                n_sim =10000, covariates=p_500, 
                                intercept="row_intercept",
                                max_iter = 20, tol = 1e-6, 
                                corstr = "independence", 
                                family = gaussian(), alpha=0.05, a_R=a_R_12, B=B)
})
#Save results
saveRDS(L31, file = "results/NoKronCase/scenario_12_L31.rds")
#Read results
L31 <- readRDS("results/NoKronCase/scenario_12_L31.rds")
L31

## ar1
system.time ({
  L32 <- simfct_nokron_parallel(sample_size = N_large,rows_no = rows_small,
                                cols_no = cols_small, rho = rho_low,
                                n_sim =10000, covariates=p_500, 
                                intercept="row_intercept",
                                max_iter = 20, tol = 1e-6, 
                                corstr = "ar1", 
                                family = gaussian(), alpha=0.05, a_R=a_R_12, B=B)
})
#Save results
saveRDS(L32, file = "results/NoKronCase/scenario_12_L32.rds")
#Read results
L32 <- readRDS("results/NoKronCase/scenario_12_L32.rds")
L32



## Exchangeable
system.time ({
  L33 <- simfct_nokron_parallel(sample_size = N_large,rows_no = rows_small,
                                cols_no = cols_small, rho = rho_low,
                                n_sim =10000, covariates=p_500, 
                                intercept="row_intercept",
                                max_iter = 20, tol = 1e-6, 
                                corstr = "Exchangeable", 
                                family = gaussian(), alpha=0.05, a_R=a_R_12, B=B)
})
#Save results
saveRDS(L33, file = "results/NoKronCase/scenario_12_L33.rds")
#Read results
L33 <- readRDS("results/NoKronCase/scenario_12_L33.rds")
L33









L_summary_500 <- bind_rows(
  extract_results("results/NoKronCase/scenario_12_L31.rds", "independence",  N=500),
  extract_results("results/NoKronCase/scenario_12_L32.rds",  "ar1", N=500),
  extract_results("results/NoKronCase/scenario_12_L33.rds",  "Exchangeable", N=500),
  
)
L_summary_500






L_all <- bind_rows(L_summary_50 %>% mutate(N = 50),
                   L_summary_100 %>% mutate(N = 100),
                   L_summary_500 %>% mutate(N = 500))
latex_table <- L_all %>%
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





