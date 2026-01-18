# analysis/01_simulation.R
set.seed(123)

# packages
library(dplyr)
library(readr)
library(tidyr)

# Read prompts
prompts <- read_csv("stimuli/prompts.csv", show_col_types = FALSE)

N <- 600  # participants
J <- nrow(prompts)

# Create latent trait theta (vividness)
theta <- rnorm(N, mean = 0, sd = 1)

# Define groups as VVIQ High vs Low (median split on theta)
group <- ifelse(theta >= median(theta), "HighVVIQ", "LowVVIQ")
group <- factor(group, levels = c("LowVVIQ", "HighVVIQ"))

# item parameters (discrimination-like and difficulty-like)
a <- runif(J, 0.8, 2.0)                 # discrimination
b <- rnorm(J, mean = 0, sd = 0.6)       # location

# thresholds for 5-point scale (4 thresholds)
tau <- c(-1.2, -0.4, 0.4, 1.2)

# function to generate ordinal response 1..5
gen_resp <- function(th, aj, bj, tau) {
  z <- aj * (th - bj) + rnorm(length(th), 0, 1) # latent response
  cut(z, breaks = c(-Inf, tau, Inf), labels = FALSE)
}

# simulate response matrix: N x J (each column is an item, values 1..5)
resp <- sapply(1:J, function(j) gen_resp(theta, a[j], b[j], tau))

# Name item columns
if ("item_id" %in% names(prompts)) {
  colnames(resp) <- prompts$item_id
} else {
  colnames(resp) <- sprintf("I%02d", 1:J)
}

# Build final dataset
dat <- as.data.frame(resp) %>%
  mutate(id = 1:N, group = group, theta = theta) %>%
  relocate(id, group, theta)

# Write the FULL simulated responses dataset
write_csv(dat, "data/simulated_responses.csv")

print(head(dat))
