# analysis/02_efa.R

# packages
library(readr)
library(dplyr)
library(psych)

# Load data
dat <- read_csv("data/simulated_responses.csv", show_col_types = FALSE)

# Keep ONLY item columns
items <- dat %>%
  select(starts_with("I")) %>%
  select(where(is.numeric)) %>%
  select(-id)

# Correlation matrix
R <- cor(items, use = "pairwise.complete.obs")

# 1) Parallel analysis
png("C:/Users/louis/Desktop/efa_parallel.png", width = 900, height = 700)
fa.parallel(R, n.obs = nrow(items), fm = "minres", fa = "fa")
dev.off()

# 2) Fit EFA models to compare (1-factor vs 2-factor)
efa1 <- fa(R, nfactors = 1, fm = "minres", rotate = "oblimin")
efa2 <- fa(R, nfactors = 2, fm = "minres", rotate = "oblimin")

# Save results for later
saveRDS(list(efa1 = efa1, efa2 = efa2), "efa_results.rds")

# Print loadings + fit
cat("\n=== 1-factor EFA ===\n")
print(efa1, cut = 0.30, sort = TRUE)

cat("\n=== 2-factor EFA ===\n")
print(efa2, cut = 0.30, sort = TRUE)
