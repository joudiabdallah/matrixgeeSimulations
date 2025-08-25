# scenario_11:  a_R  increasing, r = 15, c = 15, rho = 0.3, phi = 0.8 covers case 26,
#              28, and 30

library(dplyr)
library(stringr)
library(xtable)
source("set-up/set_up_design_kron.R")
rows_no_15 <- rows_small
a_R_15 <- a_R_linear(rows_no_15)
############################## N_small#########################




##independence, independence
system.time ({
  O11 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_15, B=B)
})
#Save results
saveRDS(O11, file = "results/scenario_15_O11.rds")
#Read results
O11 <- readRDS("results/scenario_15_O11.rds")
O11


##independence, ar1
system.time ({
  O12 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_15, B=B)
})
#Save results
saveRDS(O12, file = "results/scenario_15_O12.rds")
#Read results
O12 <- readRDS("results/scenario_15_O12.rds")
O12




##independence, Exchangeable
system.time ({
  O13 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_15, B=B)
})
#Save results
saveRDS(O13, file = "results/scenario_15_O13.rds")
#Read results
O13 <- readRDS("results/scenario_15_O13.rds")
O13





##Exchangeable, ar1
system.time ({
  O14 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_15, B=B)
})
#Save results
saveRDS(O14, file = "results/scenario_15_O14.rds")
#Read results
O14 <- readRDS("results/scenario_15_O14.rds")
O14



##Exchangeable, Exchangeable
system.time ({
  O15 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_15, B=B)
})
#Save results
saveRDS(O15, file = "results/scenario_15_O15.rds")
#Read results
O15 <- readRDS("results/scenario_15_O15.rds")
O15


##ar1, Exchangeable
system.time ({
  O16 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_15, B=B)
})
#Save results
saveRDS(O16, file = "results/scenario_15_O16.rds")
#Read results
O16 <- readRDS("results/scenario_15_O16.rds")
O16


##Exchangeable, independence
system.time ({
  O17 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_15, B=B)
})
#Save results
saveRDS(O17, file = "results/scenario_15_O17.rds")
#Read results
O17 <- readRDS("results/scenario_15_O17.rds")
O17






##ar1, independence
system.time ({
  O18 <- simfct_parallel(sample_size = N_small,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_50, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_15, B=B)
})
#Save results
saveRDS(O18, file = "results/scenario_15_O18.rds")
#Read results
O18 <- readRDS("results/scenario_15_O18.rds")
O18




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



O_summary_50 <- bind_rows(
  extract_results("results/scenario_15_O11.rds", "independence", "independence", N=50),
  extract_results("results/scenario_15_O12.rds", "independence", "ar1", N=50),
  extract_results("results/scenario_15_O13.rds", "independence", "Exchangeable", N=50),
  extract_results("results/scenario_15_O14.rds", "Exchangeable", "ar1", N=50),
  extract_results("results/scenario_15_O15.rds", "Exchangeable", "Exchangeable", N=50),
  extract_results("results/scenario_15_O16.rds", "ar1", "Exchangeable", N=50),
  extract_results("results/scenario_15_O17.rds", "Exchangeable", "independence", N=50),
  extract_results("results/scenario_15_O18.rds", "ar1", "independence", N=50)
)
O_summary_50










############################## N_medium #########################
##independence, independence
system.time ({
  O21 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_15, B=B)
})
#Save results
saveRDS(O21, file = "results/scenario_15_O21.rds")
#Read results
O21 <- readRDS("results/scenario_15_O21.rds")
O21

##independence, ar1
system.time ({
  O22 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_15, B=B)
})
#Save results
saveRDS(O22, file = "results/scenario_15_O22.rds")
#Read results
O22 <- readRDS("results/scenario_15_O22.rds")
O22



##independence, Exchangeable
system.time ({
  O23 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_15, B=B)
})
#Save results
saveRDS(O23, file = "results/scenario_15_O23.rds")
#Read results
O23 <- readRDS("results/scenario_15_O23.rds")
O23






##Exchangeable, ar1
system.time ({
  O24 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_15, B=B)
})
#Save results
saveRDS(O24, file = "results/scenario_15_O24.rds")
#Read results
O24 <- readRDS("results/scenario_15_O24.rds")
O24





##Exchangeable, Exchangeable
system.time ({
  O25 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_15, B=B)
})
#Save results
saveRDS(O25, file = "results/scenario_15_O25.rds")
#Read results
O25 <- readRDS("results/scenario_15_O25.rds")
O25




## ar1, Exchangeable
system.time ({
  O26 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_15, B=B)
})
#Save results
saveRDS(O26, file = "results/scenario_15_O26.rds")
#Read results
O26 <- readRDS("results/scenario_15_O26.rds")
O26



##Exchangeable, independence
system.time ({
  O27 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_15, B=B)
})
#Save results
saveRDS(O27, file = "results/scenario_15_O27.rds")
#Read results
O27 <- readRDS("results/scenario_15_O27.rds")
O27



##ar1, independence
system.time ({
  O28 <- simfct_parallel(sample_size = N_medium,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_100, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_15, B=B)
})#Save results
saveRDS(O28, file = "results/scenario_15_O28.rds")
#Read results
O28 <- readRDS("results/scenario_15_O28.rds")
O28


O_summary_100 <- bind_rows(
  extract_results("results/scenario_15_O21.rds", "independence", "independence", N=100),
  extract_results("results/scenario_15_O22.rds", "independence", "ar1", N=100),
  extract_results("results/scenario_15_O23.rds", "independence", "Exchangeable", N=100),
  extract_results("results/scenario_15_O24.rds", "Exchangeable", "ar1", N=100),
  extract_results("results/scenario_15_O25.rds", "Exchangeable", "Exchangeable", N=100),
  extract_results("results/scenario_15_O26.rds", "ar1", "Exchangeable", N=100),
  extract_results("results/scenario_15_O27.rds", "Exchangeable", "independence", N=100),
  extract_results("results/scenario_15_O28.rds", "ar1", "independence", N=100)
)
O_summary_100















############################## N_large #########################
##independence, independence
system.time ({
  O31 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_15, B=B)
})
#Save results
saveRDS(O31, file = "results/scenario_15_O31.rds")
#Read results
O31 <- readRDS("results/scenario_15_O31.rds")
O31

##independence, ar1
system.time ({
  O32 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_15, B=B)
})

#Save results
saveRDS(O32, file = "results/scenario_15_O32.rds")
#Read results
O32 <- readRDS("results/scenario_15_O32.rds")
O32



##independence, Exchangeable
system.time ({
  O33 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "independence", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_15, B=B)
})
#Save results
saveRDS(O33, file = "results/scenario_15_O33.rds")
#Read results
O33 <- readRDS("results/scenario_15_O33.rds")
O33






##Exchangeable, ar1
system.time ({
  O34 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "ar1", 
                         family = gaussian(), alpha=0.05, a_R=a_R_15, B=B)
})


#Save results
saveRDS(O34, file = "results/scenario_15_O34.rds")
#Read results
O34 <- readRDS("results/scenario_15_O34.rds")
O34





##Exchangeable, Exchangeable
system.time ({
  O35 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_15, B=B)
})
#Save results
saveRDS(O35, file = "results/scenario_15_O35.rds")
#Read results
O35 <- readRDS("results/scenario_15_O35.rds")
O35




## ar1, Exchangeable
system.time ({
  O36 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "Exchangeable", 
                         family = gaussian(), alpha=0.05, a_R=a_R_15, B=B)
})
#Save results
saveRDS(O36, file = "results/scenario_15_O36.rds")
#Read results
O36 <- readRDS("results/scenario_15_O36.rds")
O36



##Exchangeable, independence
system.time ({
  O37 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "Exchangeable", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_15, B=B)
})
#Save results
saveRDS(O37, file = "results/scenario_15_O37.rds")
#Read results
O37 <- readRDS("results/scenario_15_O37.rds")
O37


##ar1, independence
system.time ({
  O38 <- simfct_parallel(sample_size = N_large,rows_no = rows_small,
                         cols_no = cols_large, rho = rho_low,
                         phi = phi_high, n_sim =10000, covariates=p_500, 
                         intercept="row_intercept",
                         max_iter = 20, tol = 1e-6, 
                         corstr_rows = "ar1", 
                         corstr_cols = "independence", 
                         family = gaussian(), alpha=0.05, a_R=a_R_15, B=B)
})
#Save results
saveRDS(O38, file = "results/scenario_15_O38.rds")
#Read results
O38 <- readRDS("results/scenario_15_O38.rds")
O38



O_summary_500 <- bind_rows(
  extract_results("results/scenario_15_O31.rds", "independence", "independence", N=500),
  extract_results("results/scenario_15_O32.rds", "independence", "ar1", N=500),
  extract_results("results/scenario_15_O33.rds", "independence", "Exchangeable", N=500),
  extract_results("results/scenario_15_O34.rds", "Exchangeable", "ar1", N=500),
  extract_results("results/scenario_15_O35.rds", "Exchangeable", "Exchangeable", N=500),
  extract_results("results/scenario_15_O36.rds", "ar1", "Exchangeable", N=500),
  extract_results("results/scenario_15_O37.rds", "Exchangeable", "independence", N=500),
  extract_results("results/scenario_15_O38.rds", "ar1", "independence", N=500)
)
O_summary_500






O_all <- bind_rows(O_summary_50 %>% mutate(N = 50),
                   O_summary_100 %>% mutate(N = 100),
                   O_summary_500 %>% mutate(N = 500))
latex_table <- O_all %>%
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








