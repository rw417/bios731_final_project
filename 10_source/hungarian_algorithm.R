# Create Date: 2025-04-21
# Last Edit Date: 2025-04-22
# Creator: Robert
# Last Editor: Robert

library(clue)

run_hungarian <- function(cost_matrix){
  clue::solve_LSAP(cost_matrix)
}
