# Create Date: 2025-04-22
# Last Edit Date: 2025-04-22
# Creator: Robert
# Last Editor: Robert

# Scratch code to test the code I wrote elsewhere as I develop the project

# Source modules and read packages ####
library(microbenchmark)

source(here::here("10_source", "generate_synthetic_data.R"))
source(here::here("10_source", "hungarian_algorithm.R"))
source(here::here("10_source", "jv_algorithm.R"))

# Gen synthetic data
A_B <- gen_synthetic_data(n_rows = 100, n_cols = 25, noise_level = 2)
A <- A_B[[1]]
B <- A_B[[2]]
# Shuffle B
B_shuffled <- B[sample(1:nrow(B)), ]

# Calculate cost, defined as row-wise correlation
cost_matrix <- cor(t(A), t(B_shuffled))
image(cor(t(A), t(B)))

# Transform cost matrix so that we maximize cost_matrix 
# by minimizing cost_matrix_transformed
cost_matrix_transformed <- max(cost_matrix) - cost_matrix

# Hungarian
hungarian_assign <- run_hungarian(cost_matrix_transformed)
sum(B_shuffled[hungarian_assign,1] == B[,1])

microbenchmark(
  run_hungarian(cost_matrix_transformed),
  times = 500
)

# JV
jv_assign <- run_jv(cost_matrix_transformed)
sum(B_shuffled[jv_assign$matching,1] == B[,1])

microbenchmark(
  run_jv(cost_matrix_transformed),
  times = 500
)

# Auction
auction_assign <- run_auction(cost_matrix, epsilon=0.01)
sum(B_shuffled[auction_assign$assignment,1] == B[,1])
microbenchmark(
  run_auction(cost_matrix,0.01),
  times = 500
)
