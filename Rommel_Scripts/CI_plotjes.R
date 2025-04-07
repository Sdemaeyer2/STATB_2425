library(ggplot2)
library(dplyr)

set.seed(6)  # Voor reproduceerbaarheid

# Parameters
pop_mean <- 3
pop_sd <- 2.5
sample_size <- 100
n_intervals <- 100
conf_level <- 0.95
z_score <- qnorm(1 - (1 - conf_level)/2)

# Simuleer 100 steekproeven en hun betrouwbaarheidsintervallen
intervals <- data.frame(
  id = 1:n_intervals,
  sample_mean = replicate(n_intervals, mean(rnorm(sample_size, mean = pop_mean, sd = pop_sd)))
) %>%
  mutate(
    se = pop_sd / sqrt(sample_size),
    lower = sample_mean - z_score * se,
    upper = sample_mean + z_score * se,
    contains_mean = ifelse(lower <= pop_mean & upper >= pop_mean, TRUE, FALSE)
  )

# Plot
ggplot(intervals, aes(x = id, ymin = lower, ymax = upper, color = contains_mean)) +
  geom_errorbar(width = 0.4) +
  geom_point(aes(y = sample_mean), size = 1) +
  geom_hline(yintercept = pop_mean, linetype = "dashed", color = "black") +
  scale_color_manual(values = c("FALSE" = "red", "TRUE" = "green")) +
  labs(
    title = "95% Betrouwbaarheidsintervallen voor 100 steekproeven",
    x = "Steekproef nummer",
    y = "Gemiddelde",
    color = "Bevat populatiegemiddelde?"
  ) +
  theme_minimal()

