# =============================================================================
# datagen_nokron() Data generator function for Setting 2(Non-Seperable correlation)
# -----------------------------------------------------------------------------
# Purpose
#   Simulate N (= sample_size) matrix-valued responses Y_i ∈ R^{r × c} from a
#   non-Kronecker, general correlation model. The vectorized response vec(Y_i)
#   has a exchangeable correlation matrix of size (rc)×(rc)
#   with off-diagonal parameter rho and unit variances.
#
# Mean structure
#   For subject i:
#     M_i = a_R 1_c^T  +  1_r (B^T x_i) 1_c^T
#   where:
#     a_R ∈ R^r is a row-intercept vector,
#     x_i ∈ R^q is the subject-level covariate vector,
#     B ∈ R^{q×1} (so B^T x_i is a scalar),
#     1_r and 1_c are vectors of ones of lengths r and c.
#
# Vectorized model
#   vec(Y_i) ~ N( vec(M_i),  V ),
#   V = Cor matrix of size rcxrc with ones on the diagonal and rho elsewhere,
#    This is a full, non-Kronecker correlation
#
# Arguments
#   sample_size : integer N ≥ 1. Number of matrices.
#   rows_no     : integer r ≥ 1. Rows in each Y_i.
#   cols_no     : integer c ≥ 1. Columns in each Y_i.
#   rho         : numeric. Exchangeable  
#                 correlation for vec(Y_i).
#   a_R         : numeric vector of length r. Row intercepts.
#   covariates  : numeric vector of length N*q (stacked x_1, …, x_N) or NULL.
#   B           : numeric matrix q×1 (or numeric vector length q) or NULL.
#                 If covariates or B are NULL, the covariate effect is neglected.
#
# Covariate handling
#   If covariates and B are provided:
#     - q is set to be nrow(B).
#     - For subject i, x_i = covariates[( (i−1)*q + 1 ) : (i*q )].
#   
#
# Returns
#   A numeric matrix Y_big of size r × (N*c).
#   The columns are grouped by subject; for subject i, columns
#   ((i−1)*c + 1) : (i*c) correspond to Y_i (in column-wise order).
#
# Dependencies
#   Uses MASS::mvrnorm() for multivariate normal sampling.
#
# Example (no covariates)
#   set.seed(1)
#   J <- datagen_nokron(sample_size = 5, rows_no = 10, cols_no = 4,
#                         rho = 0.2,
#                         a_R = rnorm(10),
#                         covariates = NULL, B = NULL)
#   dim(J)       # 10 x 20
# 
#
# Example (with covariates; q = 3)
#   set.seed(123)
#   N <- 5; r <- 8; c <- 6; q <- 3
#   a_R <- rep(0.5, r)
#   B   <- matrix(c(0.4, -0.2, 0.1), nrow = q)  
#   covs <- rnorm(N*q)                           
#   J <- datagen_nokron(sample_size = N, rows_no = r, cols_no = c,
#                         rho = 0.3,
#                         a_R = a_R, covariates = covs, B = B)
#
# Notes 
#   - If using covariates: nrow(B) defines q, and length(covariates) must be N*q.
# =============================================================================




datagen_nokron <- function(sample_size, rows_no, cols_no, rho,
                           a_R, covariates, B) {
  
  
  v <- matrix(rho, nrow = rows_no * cols_no, ncol = rows_no * cols_no)
  diag(v) <- 1
  
  
  one_C_T <- matrix(1, nrow = 1, ncol = cols_no)
  intercept_mat <- a_R %*% one_C_T  
  
  has_covariates <- !is.null(covariates) && !is.null(B)
  f <- if (has_covariates) nrow(B) else 0
  
  Y_big <- matrix(NA, nrow = rows_no, ncol = sample_size * cols_no)
  
  for (i in 1:sample_size) {
    if (has_covariates) {
      x_i <- covariates[((i - 1) * f + 1):(i * f)]
      cov_effect_i <- matrix(1, rows_no, 1) %*% (t(B) %*% x_i) %*% one_C_T
    } else {
      cov_effect_i <- matrix(0, nrow = rows_no, ncol = cols_no)
    }
    
    expected_i <- intercept_mat + cov_effect_i
    
   
    Y_i_vec <- mvrnorm(1, mu = as.vector(expected_i), Sigma = v)
    Y_i <- matrix(Y_i_vec, nrow = rows_no, ncol = cols_no)
    
    col_range <- ((i - 1) * cols_no + 1):(i * cols_no)
    Y_big[, col_range] <- Y_i
  }
  if (any(is.na(Y_big))) {
    stop("datagen_nokron: Y_big contains NA — something failed during simulation")
  }
  return(Y_big)
}

