# analysis/03_cfa_invariance.R

library(readr)
library(dplyr)
library(lavaan)

# Load data
dat <- read_csv("data/simulated_responses.csv", show_col_types = FALSE)

# Item names
item_names <- names(dat)[grepl("^I\\d+", names(dat))]

# Ensure group is factor
dat$group <- factor(dat$group, levels = c("LowVVIQ", "HighVVIQ"))

# Collapse categories globally if any category is empty in any group (WLSMV requirement)
collapse_empty_globally <- function(x, group, max_cat = 5) {
  x <- as.integer(x)
  
  # Tabulate category counts by group
  tabs <- lapply(levels(group), function(grp) {
    table(factor(x[group == grp], levels = 1:max_cat))
  })
  tabmat <- do.call(cbind, tabs)
  colnames(tabmat) <- levels(group)
  
  # If any category is empty in any group, collapse it globally
  # (5->4, 4->3, ...; 1->2)
  for (k in max_cat:1) {
    if (any(tabmat[as.character(k), ] == 0)) {
      if (k == 1) {
        x[x == 1] <- 2
      } else {
        x[x == k] <- k - 1
      }
      
      # recompute after collapsing
      tabs <- lapply(levels(group), function(grp) {
        table(factor(x[group == grp], levels = 1:max_cat))
      })
      tabmat <- do.call(cbind, tabs)
      colnames(tabmat) <- levels(group)
    }
  }
  
  # Drop unused levels and make ordered factor
  present_levels <- sort(unique(x))
  factor(x, levels = present_levels, ordered = TRUE)
}

# Apply to all items (this uses dat$group, so no 'g not found' issue)
dat[item_names] <- lapply(dat[item_names], collapse_empty_globally, group = dat$group, max_cat = 5)


# 1-factor CFA model
model_1f <- paste0("Vivid =~ ", paste(item_names, collapse = " + "))

# ---- Configural
fit_config <- cfa(
  model_1f,
  data = dat,
  group = "group",
  ordered = item_names,
  estimator = "WLSMV"
)

# ---- Metric (equal loadings)
fit_metric <- cfa(
  model_1f,
  data = dat,
  group = "group",
  ordered = item_names,
  estimator = "WLSMV",
  group.equal = c("loadings")
)

# ---- Scalar for ordinal (equal loadings + thresholds)
fit_scalar <- cfa(
  model_1f,
  data = dat,
  group = "group",
  ordered = item_names,
  estimator = "WLSMV",
  group.equal = c("loadings", "thresholds")
)

# ---- Fit measures (robust rbind)

getfits <- function(fit) {
  fm <- fitMeasures(fit)
  out <- c(
    cfi  = unname(fm["cfi"]),
    tli  = unname(fm["tli"]),
    rmsea = unname(fm["rmsea"]),
    srmr = unname(fm["srmr"])
  )
  # ensure named numeric vector always has the same names
  out
}

fits <- data.frame(
  model = c("configural", "metric", "scalar"),
  rbind(
    getfits(fit_config),
    getfits(fit_metric),
    getfits(fit_scalar)
  ),
  row.names = NULL
)

fits$delta_cfi   <- c(NA, fits$cfi[2] - fits$cfi[1], fits$cfi[3] - fits$cfi[2])
fits$delta_rmsea <- c(NA, fits$rmsea[2] - fits$rmsea[1], fits$rmsea[3] - fits$rmsea[2])

print(fits)

write.csv(fits, "cfa_invariance_fit.csv", row.names = FALSE)

