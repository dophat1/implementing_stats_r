# ============================================
# Central Limit Theorem Simulation
# ============================================

library(ggplot2)
library(gridExtra)

set.seed(42)  # for reproducibility

# --- 1. Define the population ---
# Exponential distribution: rate = 1 (mean = 1, sd = 1)
# Highly right-skewed -- a good stress test for CLT
rate_param <- 1
pop_mean <- 1 / rate_param
pop_sd   <- 1 / rate_param

pop_samples <- rexp(1000000, rate = rate_param)

p_pop <- ggplot(data.frame(x = pop_samples), aes(x = x)) +
  geom_histogram(aes(y = after_stat(density)), bins = 60,
                 fill = "steelblue", alpha = 0.7) +
  labs(title = "Population Distribution (Exponential, rate = 1)",
       x = "Value", y = "Density") +
  theme_minimal()

print(p_pop)

# --- 2. Simulation function ---
# For a given sample size n, draw `n_sims` samples of size n,
# compute the mean of each, and return the vector of sample means
simulate_sample_means <- function(n, n_sims = 10000, rate = 1) {
  replicate(n_sims, mean(rexp(n, rate = rate)))
}

# --- 3. Run simulations across different sample sizes ---
sample_sizes <- c(1, 5, 30, 100)
n_sims <- 10000

results <- lapply(sample_sizes, function(n) {
  means <- simulate_sample_means(n, n_sims, rate_param)
  data.frame(sample_mean = means, n = paste0("n = ", n))
})

# Combine into one long dataframe
df_all <- do.call(rbind, results)
df_all$n <- factor(df_all$n, levels = paste0("n = ", sample_sizes))

# --- 4. Plot: histograms of sample means, faceted by n ---
# Overlay the theoretical normal curve CLT predicts:
# mean = pop_mean, sd = pop_sd / sqrt(n)

theoretical_curves <- do.call(rbind, lapply(sample_sizes, function(n) {
  x_vals <- seq(min(df_all$sample_mean[df_all$n == paste0("n = ", n)]),
                max(df_all$sample_mean[df_all$n == paste0("n = ", n)]),
                length.out = 200)
  data.frame(
    x = x_vals,
    y = dnorm(x_vals, mean = pop_mean, sd = pop_sd / sqrt(n)),
    n = paste0("n = ", n)
  )
}))
theoretical_curves$n <- factor(theoretical_curves$n, levels = paste0("n = ", sample_sizes))

p_clt <- ggplot(df_all, aes(x = sample_mean)) +
  geom_histogram(aes(y = after_stat(density)), bins = 50,
                 fill = "darkorange", alpha = 0.6, color = "white") +
  geom_line(data = theoretical_curves, aes(x = x, y = y),
            color = "black", linewidth = 1) +
  facet_wrap(~ n, scales = "free") +
  labs(title = "Distribution of Sample Means as n Increases",
       subtitle = "Orange = simulated sample means | Black line = CLT-predicted normal curve",
       x = "Sample Mean", y = "Density") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold", size = 11))

print(p_clt)

# --- 5. Quantify the convergence: summary stats ---
summary_stats <- do.call(rbind, lapply(sample_sizes, function(n) {
  means <- df_all$sample_mean[df_all$n == paste0("n = ", n)]
  data.frame(
    n = n,
    sim_mean = mean(means),
    sim_sd = sd(means),
    theoretical_sd = pop_sd / sqrt(n)
  )
}))

print(summary_stats)


