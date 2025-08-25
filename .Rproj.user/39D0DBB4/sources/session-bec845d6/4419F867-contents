# scenario_10:  a_R  constant, r = 15, c = 5, rho = 0.3 covers case 73,
#              75, and 77
library(MASS)
library(matrixgee)
library(dplyr)
library(stringr)
library(xtable)
library(parallel)
source("set-up/set_up_design_kron.R")
rows_no_10 <- rows_small
a_R_10 <- a_R_const(rows_no_10)
############################## N_small#########################




##independence
system.time ({
  J11 <- simfct_nokron_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                          n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_10, B=B)
})
#Save results
saveRDS(J11, file = "results/NoKronCase/scenario_10_J11.rds")
#Read results
J11 <- readRDS("results//NoKronCase/scenario_10_J11.rds")
J11















## ar1
system.time ({
  J12 <- simfct_nokron_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_10, B=B)
})
#Save results
saveRDS(J12, file = "results/NoKronCase/scenario_10_J12.rds")
#Read results
J12 <- readRDS("results/NoKronCase/scenario_10_J12.rds")
J12




## Exchangeable
system.time ({
  J13 <- simfct_nokron_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                          n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_10, B=B)
})
#Save results
saveRDS(J13, file = "results/NoKronCase/scenario_10_J13.rds")
#Read results
J13 <- readRDS("results/NoKronCase/scenario_10_J13.rds")
J13









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



J_summary_50 <- bind_rows(
  extract_results("results/NoKronCase/scenario_10_J11.rds", "independence",  N=50),
  extract_results("results/NoKronCase/scenario_10_J12.rds",  "ar1", N=50),
  extract_results("results/NoKronCase/scenario_10_J13.rds",  "Exchangeable", N=50),
  
)
J_summary_50










############################## N_medium #########################
##independence
system.time ({
  J21 <- simfct_nokron_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                          n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_10, B=B)
})
#Save results
saveRDS(J21, file = "results/NoKronCase/scenario_10_J21.rds")
#Read results
J21 <- readRDS("results/NoKronCase/scenario_10_J21.rds")
J21

## ar1
system.time ({
  J22 <- simfct_nokron_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_10, B=B)
})
#Save results
saveRDS(J22, file = "results/NoKronCase/scenario_10_J22.rds")
#Read results
J22 <- readRDS("results/NoKronCase/scenario_10_J22.rds")
J22



## Exchangeable
system.time ({
  J23 <- simfct_nokron_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_10, B=B)
})
#Save results
saveRDS(J23, file = "results/NoKronCase/scenario_10_J23.rds")
#Read results
J23 <- readRDS("results/NoKronCase/scenario_10_J23.rds")
J23







J_summary_100 <- bind_rows(
  extract_results("results/NoKronCase/scenario_10_J21.rds", "independence",  N=100),
  extract_results("results/NoKronCase/scenario_10_J22.rds",  "ar1", N=100),
  extract_results("results/NoKronCase/scenario_10_J23.rds",  "Exchangeable", N=100),
  
)
J_summary_100















############################## N_large #########################
##independence
system.time ({
  J31 <- simfct_nokron_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_10, B=B)
})
#Save results
saveRDS(J31, file = "results/NoKronCase/scenario_10_J31.rds")
#Read results
J31 <- readRDS("results/NoKronCase/scenario_10_J31.rds")
J31

## ar1
system.time ({
  J32 <- simfct_nokron_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                          n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_10, B=B)
})
#Save results
saveRDS(J32, file = "results/NoKronCase/scenario_10_J32.rds")
#Read results
J32 <- readRDS("results/NoKronCase/scenario_10_J32.rds")
J32



## Exchangeable
system.time ({
  J33 <- simfct_nokron_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_low,
                         n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_10, B=B)
})
#Save results
saveRDS(J33, file = "results/NoKronCase/scenario_10_J33.rds")
#Read results
J33 <- readRDS("results/NoKronCase/scenario_10_J33.rds")
J33









J_summary_500 <- bind_rows(
  extract_results("results/NoKronCase/scenario_10_J31.rds", "independence",  N=500),
  extract_results("results/NoKronCase/scenario_10_J32.rds",  "ar1", N=500),
  extract_results("results/NoKronCase/scenario_10_J33.rds",  "Exchangeable", N=500),
  
)
J_summary_500






J_all <- bind_rows(J_summary_50 %>% mutate(N = 50),
                   J_summary_100 %>% mutate(N = 100),
                   J_summary_500 %>% mutate(N = 500))
latex_table <- J_all %>%
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





