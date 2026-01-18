# analysis/04_irt_grm.R
# GRM IRT analysis (mirt) 

library(readr)
library(dplyr)
library(mirt)

# ---- INPUT / OUTPUT PATHS
DATA_IN <- "data/simulated_responses.csv"

OUT_PARAM <- "irt_item_parameters.csv"
OUT_INFO  <- "irt_test_information.png"
OUT_TRACE <- "irt_item_traces.png"

# ---- Load data
dat <- read_csv(DATA_IN, show_col_types = FALSE)

# Keep ONLY item columns (I01, I02, ...)
items <- dat %>%
  select(matches("^I\\d+")) %>%
  mutate(across(everything(), ~ as.numeric(as.character(.))))

# Sanity check: must be 1..5
bad <- sapply(items, function(x) any(!(x %in% 1:5) & !is.na(x)))
if (any(bad)) {
  print("Items with values outside 1..5:")
  print(names(bad[bad]))
  stop("Fix item values before fitting GRM.")
}

# ---- Fit 1-factor GRM
mod_grm <- mirt(items, 1, itemtype = "graded", verbose = FALSE)

coef_obj <- coef(mod_grm, IRTpars = TRUE, simplify = TRUE)
pars_items <- coef_obj$items

param_table <- as.data.frame(pars_items, stringsAsFactors = FALSE)
param_table$item_id <- rownames(param_table)
row.names(param_table) <- NULL

keep <- c("item_id", "a", "b1", "b2", "b3", "b4")
param_table <- param_table[, keep]

# Force numeric
num_cols <- c("a","b1","b2","b3","b4")
param_table[num_cols] <- lapply(param_table[num_cols], as.numeric)

# Sort by discrimination
param_table <- param_table[order(param_table$a, decreasing = TRUE), ]
row.names(param_table) <- NULL

print(param_table)
readr::write_csv(param_table, OUT_PARAM)

# Print + Save
print(param_table)
readr::write_csv(param_table, OUT_PARAM)

# ---- Plots
png(OUT_INFO, width = 900, height = 700)
plot(mod_grm, type = "info")
dev.off()

png(OUT_TRACE, width = 1100, height = 800)
plot(mod_grm, type = "trace", which.items = 1:min(4, ncol(items)))
dev.off()
