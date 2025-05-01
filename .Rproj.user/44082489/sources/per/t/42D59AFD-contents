# Create Date: 2025-04-21
# Last Edit Date: 2025-04-22
# Creator: Robert
# Last Editor: Robert

# Purpose: genearte synthetic data
# If helpful, we can think of rows as cells and columns as features

gen_synthetic_data <- function(n_rows, n_cols, noise_level=0.5) {
  # Generate matrix A
  A <- matrix(rnorm(n_rows * n_cols), nrow = n_rows, ncol = n_cols)
  
  # Generate matrix B by adding small noise
  B <- A + matrix(rnorm(n_rows * n_cols, sd = noise_level), nrow = n_rows, ncol = n_cols)
  
  return(list(A = A, B = B))
}

# # Testing A and B
# # Compute average row-wise correlation for matched rows
# rowwise_corr <- function(X, Y) {
#   sapply(1:nrow(X), function(i) cor(X[i, ], Y[i, ]))
# }
# 
# mean(rowwise_corr(A, B))
# 
# # Shuffle rows of B and compute again
# B_shuffled <- B[sample(1:nrow(B)), ]
# mean(rowwise_corr(A, B_shuffled))
