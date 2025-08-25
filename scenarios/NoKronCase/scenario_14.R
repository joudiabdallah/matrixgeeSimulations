# scenario_14:  a_R  linear, r = 15, c = 5, rho = 0.8 
library(MASS)
library(matrixgee)
library(dplyr)
library(stringr)
library(xtable)
library(parallel)
source("set-up/set_up_design_kron.R")
rows_no_14 <- rows_small
a_R_14 <- a_R_linear(rows_no_14)
############################## N_small#########################




##independence
system.time ({
  N11 <- simfct_nokron_parallel(sample_size = N_small,rows_no = rows_small,
                                cols_no = cols_small, rho = rho_high,
                                n_sim =10000, covariates=p_50, 
                                intercept="row_intercept",
                                max_iter = 20, tol = 1e-6, 
                                corstr = "independence", 
                                family = gaussian(), alpha=0.05, a_R=a_R_14, B=B)
})
#Save results
saveRDS(N11, file = "results/NoKronCase/scenario_14_N11.rds")
#Read results
N11 <- readRDS("results//NoKronCase/scenario_14_N11.rds")
N11















## ar1
system.time ({
  N12 <- simfct_nokron_parallel(sample_size = N_small,rows_no = rows_small,
                                cols_no = cols_small, rho = rho_high,
                                n_sim =10000, covariates=p_50, 
                                intercept="row_intercept",
                                max_iter = 20, tol = 1e-6, 
                                corstr = "ar1", 
                                family = gaussian(), alpha=0.05, a_R=a_R_14, B=B)
})
#Save results
saveRDS(N12, file = "results/NoKronCase/scenario_14_N12.rds")
#Read results
N12 <- readRDS("results/NoKronCase/scenario_14_N12.rds")
N12




## Exchangeable
system.time ({
  N13 <- simfct_nokron_parallel(sample_size = N_small,rows_no = rows_small,
                                cols_no = cols_small, rho = rho_high,
                                n_sim =10000, covariates=p_50, 
                                intercept="row_intercept",
                                max_iter = 20, tol = 1e-6, 
                                corstr = "Exchangeable", 
                                family = gaussian(), alpha=0.05, a_R=a_R_14, B=B)
})
#Save results
saveRDS(N13, file = "results/NoKronCase/scenario_14_N13.rds")
#Read results
N13 <- readRDS("results/NoKronCase/scenario_14_N13.rds")
N13









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



N_summary_50 <- bind_rows(
  extract_results("results/NoKronCase/scenario_14_N11.rds", "independence",  N=50),
  extract_results("results/NoKronCase/scenario_14_N12.rds",  "ar1", N=50),
  extract_results("results/NoKronCase/scenario_14_N13.rds",  "Exchangeable", N=50),
  
)
N_summary_50










############################## N_medium #########################
##independence
system.time ({
  N21 <- simfct_nokron_parallel(sample_size = N_medium,rows_no = rows_small,
                                cols_no = cols_small, rho = rho_high,
                                n_sim =10000, covariates=p_100, 
                                intercept="row_intercept",
                                max_iter = 20, tol = 1e-6, 
                                corstr = "independence", 
                                family = gaussian(), alpha=0.05, a_R=a_R_14, B=B)
})
#Save results
saveRDS(N21, file = "results/NoKronCase/scenario_14_N21.rds")
#Read results
N21 <- readRDS("results/NoKronCase/scenario_14_N21.rds")
N21

## ar1
system.time ({
  N22 <- simfct_nokron_parallel(sample_size = N_medium,rows_no = rows_small,
                                cols_no = cols_small, rho = rho_high,
                                n_sim =10000, covariates=p_100, 
                                intercept="row_intercept",
                                max_iter = 20, tol = 1e-6, 
                                corstr = "ar1", 
                                family = gaussian(), alpha=0.05, a_R=a_R_14, B=B)
})
#Save results
saveRDS(N22, file = "results/NoKronCase/scenario_14_N22.rds")
#Read results
N22 <- readRDS("results/NoKronCase/scenario_14_N22.rds")
N22



## Exchangeable
system.time ({
  N23 <- simfct_nokron_parallel(sample_size = N_medium,rows_no = rows_small,
                                cols_no = cols_small, rho = rho_high,
                                n_sim =10000, covariates=p_100, 
                                intercept="row_intercept",
                                max_iter = 20, tol = 1e-6, 
                                corstr = "Exchangeable", 
                                family = gaussian(), alpha=0.05, a_R=a_R_14, B=B)
})
#Save results
saveRDS(N23, file = "results/NoKronCase/scenario_14_N23.rds")
#Read results
N23 <- readRDS("results/NoKronCase/scenario_14_N23.rds")
N23







N_summary_100 <- bind_rows(
  extract_results("results/NoKronCase/scenario_14_N21.rds", "independence",  N=100),
  extract_results("results/NoKronCase/scenario_14_N22.rds",  "ar1", N=100),
  extract_results("results/NoKronCase/scenario_14_N23.rds",  "Exchangeable", N=100),
  
)
N_summary_100















############################## N_large #########################
##independence
system.time ({
  N31 <- simfct_nokron_parallel(sample_size = N_large,rows_no = rows_small,
                                cols_no = cols_small, rho = rho_high,
                                n_sim =10000, covariates=p_500, 
                                intercept="row_intercept",
                                max_iter = 20, tol = 1e-6, 
                                corstr = "independence", 
                                family = gaussian(), alpha=0.05, a_R=a_R_14, B=B)
})
#Save results
saveRDS(N31, file = "results/NoKronCase/scenario_14_N31.rds")
#Read results
N31 <- readRDS("results/NoKronCase/scenario_14_N31.rds")
N31

## ar1
system.time ({
  N32 <- simfct_nokron_parallel(sample_size = N_large,rows_no = rows_small,
                                cols_no = cols_small, rho = rho_high,
                                n_sim =10000, covariates=p_500, 
                                intercept="row_intercept",
                                max_iter = 20, tol = 1e-6, 
                                corstr = "ar1", 
                                family = gaussian(), alpha=0.05, a_R=a_R_14, B=B)
})

#Save results
saveRDS(N32, file = "results/NoKronCase/scenario_14_N32.rds")
#Read results
N32 <- readRDS("results/NoKronCase/scenario_14_N32.rds")
N32



## Exchangeable
system.time ({
  N33 <- simfct_nokron_parallel(sample_size = N_large,rows_no = rows_small,
                                cols_no = cols_small, rho = rho_high,
                                n_sim =10000, covariates=p_500, 
                                intercept="row_intercept",
                                max_iter = 20, tol = 1e-6, 
                                corstr = "Exchangeable", 
                                family = gaussian(), alpha=0.05, a_R=a_R_14, B=B)
})
#Save results
saveRDS(N33, file = "results/NoKronCase/scenario_14_N33.rds")
#Read results
N33 <- readRDS("results/NoKronCase/scenario_14_N33.rds")
N33









N_summary_500 <- bind_rows(
  extract_results("results/NoKronCase/scenario_14_N31.rds", "independence",  N=500),
  extract_results("results/NoKronCase/scenario_14_N32.rds",  "ar1", N=500),
  extract_results("results/NoKronCase/scenario_14_N33.rds",  "Exchangeable", N=500),
  
)
N_summary_500






N_all <- bind_rows(N_summary_50 %>% mutate(N = 50),
                   N_summary_100 %>% mutate(N = 100),
                   N_summary_500 %>% mutate(N = 500))
latex_table <- N_all %>%
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





