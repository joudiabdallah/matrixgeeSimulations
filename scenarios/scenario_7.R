# scenario_7:  a_R  increasing, r = 15, c = 5, rho = 0.8, phi = 0.3 covers case 38,
#              40, and 42

library(dplyr)
library(stringr)
library(xtable)
source("set-up/set_up_design_kron.R")
rows_no_7 <- rows_small
a_R_7 <- a_R_linear(rows_no_7)
############################## N_small#########################




##independence, independence
system.time ({
  G11 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_7, B=B)
})
#Save results
saveRDS(G11, file = "results/scenario_7_G11.rds")
#Read results
G11 <- readRDS("results/scenario_7_G11.rds")
G11


##independence, ar1
system.time ({
  G12 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_7, B=B)
})
#Save results
saveRDS(G12, file = "results/scenario_7_G12.rds")
#Read results
G12 <- readRDS("results/scenario_7_G12.rds")
G12




##independence, Exchangeable
system.time ({
  G13 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_7, B=B)
})
#Save results
saveRDS(G13, file = "results/scenario_7_G13.rds")
#Read results
G13 <- readRDS("results/scenario_7_G13.rds")
G13





##Exchangeable, ar1
system.time ({
  G14 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_7, B=B)
})
#Save results
saveRDS(G14, file = "results/scenario_7_G14.rds")
#Read results
G14 <- readRDS("results/scenario_7_G14.rds")
G14



##Exchangeable, Exchangeable
system.time ({
  G15 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_7, B=B)
})
#Save results
saveRDS(G15, file = "results/scenario_7_G15.rds")
#Read results
G15 <- readRDS("results/scenario_7_G15.rds")
G15


##ar1, Exchangeable
system.time ({
  G16 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_7, B=B)
})
#Save results
saveRDS(G16, file = "results/scenario_7_G16.rds")
#Read results
G16 <- readRDS("results/scenario_7_G16.rds")
G16


##Exchangeable, independence
system.time ({
  G17 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_7, B=B)
})
#Save results
saveRDS(G17, file = "results/scenario_7_G17.rds")
#Read results
G17 <- readRDS("results/scenario_7_G17.rds")
G17






##ar1, independence
system.time ({
  G18 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_7, B=B)
})
#Save results
saveRDS(G18, file = "results/scenario_7_G18.rds")
#Read results
G18 <- readRDS("results/scenario_7_G18.rds")
G18




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



G_summary_50 <- bind_rows(
  extract_results("results/scenario_7_G11.rds", "independence", "independence", N=50),
  extract_results("results/scenario_7_G12.rds", "independence", "ar1", N=50),
  extract_results("results/scenario_7_G13.rds", "independence", "Exchangeable", N=50),
  extract_results("results/scenario_7_G14.rds", "Exchangeable", "ar1", N=50),
  extract_results("results/scenario_7_G15.rds", "Exchangeable", "Exchangeable", N=50),
  extract_results("results/scenario_7_G16.rds", "ar1", "Exchangeable", N=50),
  extract_results("results/scenario_7_G17.rds", "Exchangeable", "independence", N=50),
  extract_results("results/scenario_7_G18.rds", "ar1", "independence", N=50)
)
G_summary_50










############################## N_medium #########################
##independence, independence
system.time ({
  G21 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_7, B=B)
})
#Save results
saveRDS(G21, file = "results/scenario_7_G21.rds")
#Read results
G21 <- readRDS("results/scenario_7_G21.rds")
G21

##independence, ar1
system.time ({
  G22 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_7, B=B)
})
#Save results
saveRDS(G22, file = "results/scenario_7_G22.rds")
#Read results
G22 <- readRDS("results/scenario_7_G22.rds")
G22



##independence, Exchangeable
system.time ({
  G23 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_7, B=B)
})
#Save results
saveRDS(G23, file = "results/scenario_7_G23.rds")
#Read results
G23 <- readRDS("results/scenario_7_G23.rds")
G23






##Exchangeable, ar1
system.time ({
  G24 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_7, B=B)
})
#Save results
saveRDS(G24, file = "results/scenario_7_G24.rds")
#Read results
G24 <- readRDS("results/scenario_7_G24.rds")
G24





##Exchangeable, Exchangeable
system.time ({
  G25 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_7, B=B)
})
#Save results
saveRDS(G25, file = "results/scenario_7_G25.rds")
#Read results
G25 <- readRDS("results/scenario_7_G25.rds")
G25




## ar1, Exchangeable
system.time ({
  G26 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_7, B=B)
})
#Save results
saveRDS(G26, file = "results/scenario_7_G26.rds")
#Read results
G26 <- readRDS("results/scenario_7_G26.rds")
G26



##Exchangeable, independence
system.time ({
  G27 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_7, B=B)
})
#Save results
saveRDS(G27, file = "results/scenario_7_G27.rds")
#Read results
G27 <- readRDS("results/scenario_7_G27.rds")
G27



##ar1, independence
system.time ({
  G28 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_7, B=B)
})
#Save results
saveRDS(G28, file = "results/scenario_7_G28.rds")
#Read results
G28 <- readRDS("results/scenario_7_G28.rds")
G28


G_summary_100 <- bind_rows(
  extract_results("results/scenario_7_G21.rds", "independence", "independence", N=100),
  extract_results("results/scenario_7_G22.rds", "independence", "ar1", N=100),
  extract_results("results/scenario_7_G23.rds", "independence", "Exchangeable", N=100),
  extract_results("results/scenario_7_G24.rds", "Exchangeable", "ar1", N=100),
  extract_results("results/scenario_7_G25.rds", "Exchangeable", "Exchangeable", N=100),
  extract_results("results/scenario_7_G26.rds", "ar1", "Exchangeable", N=100),
  extract_results("results/scenario_7_G27.rds", "Exchangeable", "independence", N=100),
  extract_results("results/scenario_7_G28.rds", "ar1", "independence", N=100)
)
G_summary_100















############################## N_large #########################
##independence, independence
system.time ({
  G31 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_7, B=B)
})
#Save results
saveRDS(G31, file = "results/scenario_7_G31.rds")
#Read results
G31 <- readRDS("results/scenario_7_G31.rds")
G31

##independence, ar1
system.time ({
  G32 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_7, B=B)
})
#Save results
saveRDS(G32, file = "results/scenario_7_G32.rds")
#Read results
G32 <- readRDS("results/scenario_7_G32.rds")
G32



##independence, Exchangeable
system.time ({
  G33 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_7, B=B)
})
#Save results
saveRDS(G33, file = "results/scenario_7_G33.rds")
#Read results
G33 <- readRDS("results/scenario_7_G33.rds")
G33






##Exchangeable, ar1
system.time ({
  G34 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_7, B=B)
})


#Save results
saveRDS(G34, file = "results/scenario_7_G34.rds")
#Read results
G34 <- readRDS("results/scenario_7_G34.rds")
G34





##Exchangeable, Exchangeable
system.time ({
  G35 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_7, B=B)
})
#Save results
saveRDS(G35, file = "results/scenario_7_G35.rds")
#Read results
G35 <- readRDS("results/scenario_7_G35.rds")
G35




## ar1, Exchangeable
system.time ({
  G36 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_7, B=B)
})
#Save results
saveRDS(G36, file = "results/scenario_7_G36.rds")
#Read results
G36 <- readRDS("results/scenario_7_G36.rds")
G36



##Exchangeable, independence
system.time ({
  G37 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_7, B=B)
})
#Save results
saveRDS(G37, file = "results/scenario_7_G37.rds")
#Read results
G37 <- readRDS("results/scenario_7_G37.rds")
G37


##ar1, independence
system.time ({
  G38 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_small, rho = rho_high,
                         phi = phi_low, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_7, B=B)
})
#Save results
saveRDS(G38, file = "results/scenario_7_G38.rds")
#Read results
G38 <- readRDS("results/scenario_7_G38.rds")
G38



G_summary_500 <- bind_rows(
  extract_results("results/scenario_7_G31.rds", "independence", "independence", N=500),
  extract_results("results/scenario_7_G32.rds", "independence", "ar1", N=500),
  extract_results("results/scenario_7_G33.rds", "independence", "Exchangeable", N=500),
  extract_results("results/scenario_7_G34.rds", "Exchangeable", "ar1", N=500),
  extract_results("results/scenario_7_G35.rds", "Exchangeable", "Exchangeable", N=500),
  extract_results("results/scenario_7_G36.rds", "ar1", "Exchangeable", N=500),
  extract_results("results/scenario_7_G37.rds", "Exchangeable", "independence", N=500),
  extract_results("results/scenario_7_G38.rds", "ar1", "independence", N=500)
)
G_summary_500






G_all <- bind_rows(G_summary_50 %>% mutate(N = 50),
                   G_summary_100 %>% mutate(N = 100),
                   G_summary_500 %>% mutate(N = 500))
latex_table <- G_all %>%
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








