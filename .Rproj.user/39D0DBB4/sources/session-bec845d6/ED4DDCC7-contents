# =============================================================================
# datagen() Data generator function for Setting 1 (Separable Correlation matrix)
# -----------------------------------------------------------------------------
# Purpose
#   Simulate N (= sample_size) matrix-valued responses Y_i ∈ R^{r × c} from a
#   Matrix-Normal model with:
#     - Row correlation: Exchangeable with parameter rho
#     - Column correlation: AR(1) with parameter phi
#   The mean for subject i is:
#     M_i = a_R 1_c^T  +  1_r (B^T x_i) 1_c^T
#   where:
#     a_R ∈ R^r is a row-intercept vector,
#     x_i ∈ R^q is the subject-level covariate vector for subject i,
#     B ∈ R^{q×1} (so B^T x_i is a scalar),
#     1_r and 1_c are vectors of ones of lengths r and c respectively.
#
# Model
#   Y_i ~ MN( M_i,  U = Σ_R,  V = Σ_C ),
#   with Σ_R having 1 on the diagonal and rho off-diagonal (exchangeable),
#        Σ_C having entries (φ^{|j-k|}) for j,k = 1..c (AR(1)).
#
# Arguments
#   sample_size : integer N ≥ 1. Number of matrices.
#   rows_no     : integer r ≥ 1. Number of rows in each Y_i.
#   cols_no     : integer c ≥ 1. Number of columns in each Y_i.
#   rho         : numeric. Exchangeable correlation for rows. For positive
#                 definiteness typically -1/(r-1) < rho < 1.
#   phi         : numeric. AR(1) correlation for columns. Typically |phi| < 1.
#   a_R         : numeric vector of length r. Row intercepts.
#   covariates  : numeric vector of length N*q (stacked as x_1, …, x_N) or NULL.
#   B           : numeric matrix of size q×1 (or a numeric vector length q) or NULL.
#                 If covariates or B are NULL, the covariate effect is neglected.
#
# Details on covariates
#   If covariates and B are provided:
#     - q is set to be nrow(B)
#     - For subject i, x_i = covariates[( (i-1)*q + 1 ) : (i*q )].
#     - The resulting covariate effect is a scalar (B^T x_i) replicated to an
#       r×c matrix, constant within the matrix for that subject.
#
# Returns
#   A list with:
#     $y         : numeric matrix of size r × (N*c).
#                  The columns are grouped by subject; for subject i, the block
#                  columns ((i-1)*c + 1) : (i*c) correspond to Y_i.
#     $intercept : the input a_R (length r).
#
# Dependencies
#   Uses matrixNormal::rmatnorm(). 
#
# Example (without covariates)
#   set.seed(1)
#   J <- datagen(sample_size = 5, rows_no = 10, cols_no = 4,
#                  rho = 0.3, phi = 0.6,
#                  a_R = rnorm(10),
#                  covariates = NULL, B = NULL)
#   dim(J$y)   # 10 x 20
#
# Example (with covariates; q = 3)
#   set.seed(123)
#   N <- 5; r <- 8; c <- 6; q <- 3
#   a_R <- rep(0.5, r)
#   B   <- matrix(c(0.4, -0.2, 0.1), nrow = q)  
#   covs <- rnorm(N*q)                           
#   J <- datagen(sample_size = N, rows_no = r, cols_no = c,
#                  rho = 0.2, phi = 0.5,
#                  a_R = a_R, covariates = covs, B = B)
#   
#
# Notes 
#   - If using covariates: nrow(B) defines q, and length(covariates) must be N*q.
#   - The covariate effect here is subject-level (constant across the r×c cells).
# =============================================================================





datagen <- function(sample_size, rows_no, cols_no, rho, phi,
                      a_R, covariates, B) {
  
  sigma_R <- matrix(rho, nrow = rows_no, ncol = rows_no)
  diag(sigma_R) <- 1
  sigma_C <- outer(1:cols_no, 1:cols_no, function(i, j) phi^abs(i - j))
  one_C_T <- matrix(1, nrow = 1, ncol = cols_no)
  
  intercept_mat <- a_R %*% one_C_T  
  
  Y_big <- matrix(NA, nrow = rows_no, ncol = sample_size * cols_no)
  has_covariates <- !is.null(covariates) && !is.null(B)
  
  if (has_covariates) {
    q <- nrow(B)
  } else {
    q <- 0
  }
  
  for (i in 1:sample_size) {
    
    if (has_covariates) {
      X_i <- covariates[((i - 1) * q + 1):(i * q)]
      cov_effect_i <- matrix(1, rows_no, 1) %*% (t(B) %*% X_i) %*% one_C_T
    } else {
      cov_effect_i <- matrix(0, nrow = rows_no, ncol = cols_no)
    }  
    
    expected_i <- intercept_mat + cov_effect_i
    
    Y_i <- matrixNormal::rmatnorm(M = expected_i, U = sigma_R, V = sigma_C)
    
    col_range <- ((i - 1) * cols_no + 1):(i * cols_no)
    Y_big[, col_range] <- Y_i
  }
  
  return(list(
    y = Y_big,
    intercept = a_R
    
  ))
}



