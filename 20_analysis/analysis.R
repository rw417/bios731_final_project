# Source scripts and load libraries
library(ggplot2)
source(here::here("10_source", "simulations.R"))


# Noise Levels ####
n_row <- 100
n_col <- 25
noise_level = 2
iter <- 1000

noise_levels <- c(0.5,1,1.5,2,2.5,3,5,10)
noise_level_results <- data.frame()

for (noise_level in noise_levels) {
  set.seed(123)
  result <- run_sim(n_rows = n_row, n_cols = n_col, noise_level = noise_level, iter = iter)
  noise_level_results <- rbind(noise_level_results, result)
}

saveRDS(noise_level_results, file = here::here("30_results", "noise_level_results.RDS"))

# Plot boxplots, where y-axis is the noise_levels, x-axis is the correctness,
# for each noise level, we create side-by-side boxplots for each method
ggplot(noise_level_results, aes(y = factor(noise_level), x = correct, fill = method)) +
  geom_boxplot() +
  labs(title = "Correctness of LAP Algorithms by Noise Level",
       y = "Noise Level",
       x = "Correctness") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2", name = "Method") +
  theme(legend.position = "right")

ggplot(noise_level_results, aes(y = factor(noise_level), x = log(time), fill = method)) +
  geom_boxplot() +
  labs(title = "Computation Times by Noise Level",
       y = "Noise Level",
       x = "Log Computation Time (microseconds)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2", name = "Method") +
  theme(legend.position = "right")


# Number of Rows ####
n_row <- 100
n_col <- 25
noise_level = 2
iter <- 1000

n_rows <- c(50, 100, 200, 500, 1000)
n_row_results <- data.frame()

for (n_row in n_rows) {
  set.seed(123)
  result <- run_sim(n_rows = n_row, n_cols = n_col, noise_level = noise_level, iter = iter)
  n_row_results <- rbind(n_row_results, result)
}

saveRDS(n_row_results, file = here::here("30_results", "n_row_results.RDS"))

# Plotting
ggplot(n_row_results, aes(y = factor(n_rows), x = correct, fill = method)) +
  geom_boxplot() +
  labs(title = "Correctness of Matching Algorithms by Number of Rows",
       y = "Number of Rows",
       x = "Correctness") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2", name = "Method") +
  theme(legend.position = "right")

ggplot(n_row_results, aes(y = factor(n_rows), x = time, fill = method)) +
  geom_boxplot() +
  labs(title = "Computation Times by Number of Columns",
       y = "Number of Rows",
       x = "Computation Time (microseconds)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2", name = "Method") +
  theme(legend.position = "right")

# Number of Columns ####
n_row <- 100
n_col <- 25
noise_level = 2
iter <- 1000

n_cols <- c(10, 25, 50, 100, 200)
n_col_results <- data.frame()
for (n_col in n_cols) {
  set.seed(123)
  result <- run_sim(n_rows = n_row, n_cols = n_col, noise_level = noise_level, iter = iter)
  n_col_results <- rbind(n_col_results, result)
}

saveRDS(n_col_results, file = here::here("30_results", "n_col_results.RDS"))

# Plotting
ggplot(n_col_results, aes(y = factor(n_cols), x = correct, fill = method)) +
  geom_boxplot() +
  labs(title = "Correctness of Matching Algorithms by Number of Columns",
       y = "Number of Columns",
       x = "Correctness") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2", name = "Method") +
  theme(legend.position = "right")

ggplot(n_col_results, aes(y = factor(n_cols), x = time, fill = method)) +
  geom_boxplot() +
  labs(title = "Computation Times by Number of Columns",
       y = "Number of Columns",
       x = "Computation Time (microseconds)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2", name = "Method") +
  theme(legend.position = "right")

# Number of rows and number of columns ####
library(future.apply)

noise_level = 3
iter <- 500
n_rows <- c(50, 100, 200, 500)
n_cols <- c(10, 25, 50, 100)
param_grid <- expand.grid(n_row = n_rows, n_col = n_cols)
row_col_results <- data.frame()

plan(multisession)

results_list <- future_lapply(1:nrow(param_grid), function(i) {
  row <- param_grid[i, ]
  run_sim_jv(n_rows = row$n_row, n_cols = row$n_col, noise_level = noise_level, iter = iter)
}, future.seed=TRUE)
row_col_results <- do.call(rbind, results_list)

saveRDS(row_col_results, file = here::here("30_results", "row_col_results.RDS"))

# Plot
ggplot(row_col_results, aes(y = factor(n_cols), x = correct, fill = factor(n_rows))) +
  geom_boxplot() +
  labs(title = "Grouped by Number of Features",
       y = "Number of Features",
       x = "Correctness") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2", name = "Number of Observations") +
  theme(legend.position = "right")



# Plot Figure 1 for the report
# Run final-presentation.Rmd to obtain the plots needed here
library(gridExtra)
library(grid)
library(ggplot2)
library(cowplot)
# Add title
row1_title <- textGrob("Simulation Results by Noise Level", gp = gpar(fontsize = 12, fontface = "bold"))
row2_title <- textGrob("Simulation Results by Number of Observations", gp = gpar(fontsize = 12, fontface = "bold"))
row3_title <- textGrob("Simulation Results by Number of Features", gp = gpar(fontsize = 12, fontface = "bold"))

# Extract the legend from one plot
legend <- get_legend(p1_n + theme(legend.position = "bottom"))

# Remove legends from all individual plots
p1_n <- p1_n + theme(legend.position = "none")
p2_n <- p2_n + theme(legend.position = "none")
p1_o <- p1_o + theme(legend.position = "none")
p2_o <- p2_o + theme(legend.position = "none")
p1_f <- p1_f + theme(legend.position = "none")
p2_f <- p2_f + theme(legend.position = "none")

# Arrange plots in a grid, adding row titles as separate rows using heights
plot_grid(
  row1_title,
  arrangeGrob(p1_n, p2_n, ncol = 2),
  row2_title,
  arrangeGrob(p1_o, p2_o, ncol = 2),
  row3_title,
  arrangeGrob(p1_f, p2_f, ncol = 2),
  legend,
  ncol = 1,
  rel_heights = c(0.1, 1, 0.1, 1, 0.1, 1, 0.1)
)
