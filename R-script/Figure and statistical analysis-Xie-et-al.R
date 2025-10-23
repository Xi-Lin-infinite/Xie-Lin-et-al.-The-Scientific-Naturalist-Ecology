# This code belongs to the paper “A Monogamous Male's Dilemma: Managing Two Females and Parental Care in White-Faced Plovers” in Ecology
# update 2025-10-23 Xi Lin. if you have any question, please contact: linx69@mail2.sysu.edu.cn
# ps. For column name explanations, refer to "Column Name Explanation.txt"

# Set your working directory
setwd("")
# Set the path for saving images
dir.create("paper plot", recursive = TRUE, showWarnings = FALSE)




# *Figure2 and Table S2*----

rm(list=ls())

## Load packages and data preprocessing --------------------------------------------------------------
library(tidyverse)   # data manipulation
library(lubridate)   # date-time handling
library(lme4)        # mixed-effect models
library(arm)         # sim() for model parameter simulation
library(ggplot2)     # plotting
library(performance)
library(stringr)



## Two helper functions for data preparation

## for total nest attendance
make_df_rs <- function(summary_incu_hours,
                       label_value = "Biparental monogamy",
                       season_start_month = 3,
                       seed = 42,
                       label_col = "lable",   # your current column name is 'lable' (if renamed to 'label', modify here accordingly)
                       nest_col  = "NEST") {
  
  # 1) Filter target mating type and safely remove unnecessary columns
  hourly_stats2 <- summary_incu_hours %>%
    dplyr::filter(.data[[label_col]] == label_value) %>%
    dplyr::select(-tidyselect::any_of(c("count_4", "count_0", "count_3")))# any_of() automatically ignores non-existent columns instead of throwing an error
  
  # 2) Convert wide to long format + calculate attendance
  hourly_stats_long <- hourly_stats2 %>%
    pivot_longer(
      cols = starts_with("count_"),
      names_to = "type",
      values_to = "count"
    ) %>%
    mutate(attendance = (count / total_count) * 100)
  
  # 3) Keep total incubation only and set sex label
  df <- hourly_stats_long %>%
    filter(type == c("count_total_incu")) %>%
    mutate(
      sex = "Total"
    ) %>%
    filter(!is.na(sex))
  
  # 4) Calculate day_in_season (based on breeding year)
  df <- df %>%
    mutate(date = as_date(date),
           season_year = year(date %m-% months(season_start_month - 1L))) %>%
    group_by(season_year) %>%
    mutate(day_in_season = factor(as.integer(date - min(date, na.rm = TRUE)) + 1L)) %>%
    ungroup()
  
  # 6) Generate circular time terms for 12h and 24h cycles
  df_rs2 <- df %>%
    mutate(
      rad12 = 2 * pi * hour / 12,  sin12 = sin(rad12),  cos12 = cos(rad12),
      rad24 = 2 * pi * hour / 24,  sin24 = sin(rad24),  cos24 = cos(rad24)
    )
  
  return(df_rs2)
}

## for individual nest attendance
make_df <- function(summary_incu_hours,
                    label_value,
                    season_start_month = 3,
                    #seed = 42,
                    label_col = "lable",   # your current column name is 'lable' (if renamed to 'label', modify here accordingly)
                    nest_col  = "NEST") {
  
  # 1) Filter target mating type and safely remove unnecessary columns
  hourly_stats2 <- summary_incu_hours %>%
    dplyr::filter(.data[[label_col]] == label_value) %>%
    dplyr::select(-tidyselect::any_of(c("count_4", "count_0")))# any_of() automatically ignores non-existent columns instead of throwing an error
  
  # Remove β individual if not E009
  if(length(unique(hourly_stats2$count_3))==1){
    hourly_stats2 <- hourly_stats2 %>%
      dplyr::select(-tidyselect::any_of(c("count_3")))
  }
  
  
  # 2) Convert wide to long format + calculate attendance
  hourly_stats_long <- hourly_stats2 %>%
    pivot_longer(
      cols = starts_with("count_"),
      names_to = "type",
      values_to = "count"
    ) %>%
    mutate(attendance = (count / total_count) * 100)
  
  # 3) Keep only male/female records and assign sex labels
  df <- hourly_stats_long %>%
    filter(type != "count_total_incu") %>%
    mutate(
      sex = case_when(
        str_detect(type, "count_1") ~ "male",
        str_detect(type, "count_2") ~ "female",
        str_detect(type, "count_3") ~ "female2",
        TRUE                        ~ NA_character_
      )
    ) %>%
    filter(!is.na(sex))
  
  
  # 4) Calculate day_in_season (based on breeding year)
  df <- df %>%
    mutate(date = as_date(date),
           season_year = year(date %m-% months(season_start_month - 1L))) %>%
    group_by(season_year) %>%
    mutate(day_in_season = factor(as.integer(date - min(date, na.rm = TRUE)) + 1L)) %>%
    ungroup()
  
  return(df)
}


## Figure2a Biparental monogamy----

##1.1 sex difference in nest attendance----
###  model----

load("Organized data/summary_incu_hours.Rdata")

df <- make_df(summary_incu_hours, label_value = "Biparental monogamy")

library(dplyr)
library(lme4)
library(lmerTest)
library(arm)
library(ggplot2)

### Adjustable parameters 
B     <- 200   # Number of bootstrap replicates (bagging iterations)
nsim  <- 5000  # Number of parameter draws per sim() (5000 similar to 500)
seed0 <- 2024  # Global seed for reproducibility
sex_levels <- c("male","female")  # two individual labels



df$sex <- factor(df$sex, levels = sex_levels)
df$day_in_season <- factor(df$day_in_season)


# Prediction grid (0–24h × two individuals)
newdat <- expand.grid(
  hour = seq(0, 24, by = 1),
  sex  = sex_levels,
  KEEP.OUT.ATTRS = FALSE
) %>%
  mutate(
    sin12 = sin(2*pi*hour/12),  cos12 = cos(2*pi*hour/12),
    sin24 = sin(2*pi*hour/24),  cos24 = cos(2*pi*hour/24),
    sex   = factor(sex, levels = sex_levels)
  )

# Design matrix consistent with the model fixed effects (computed once)
X_new <- model.matrix(~ sex * (sin12 + cos12 + sin24 + cos24), data = newdat)

# Store all replicate predictions
all_preds <- vector("list", B)
used <- 0L
# Store all model results
#all_models <- vector("list", B)
fixefs_all <- vector("list", B)  # Fixed effects for each iteration
ranefs_all <- vector("list", B)  # Random effects for each iteration

for (b in seq_len(B)) {
  set.seed(seed0 + b)
  
  # Randomly keep one record per NEST × date × hour
  df_rs_b <- df %>%
    group_by(NEST, date, hour) %>%
    slice_sample(n = 1) %>%
    ungroup() %>%
    mutate(
      sin12 = sin(2*pi*hour/12),  cos12 = cos(2*pi*hour/12),
      sin24 = sin(2*pi*hour/24),  cos24 = cos(2*pi*hour/24),
      sex   = factor(sex, levels = sex_levels),
      day_in_season = factor(day_in_season)
    )
  
  # Skip iteration if any sex is missing, otherwise fixed effects cannot be identified
  if (nlevels(droplevels(df_rs_b$sex)) < length(sex_levels)) next
  
  # Fit model
  mod_b <- try(
    lmer(attendance ~ sex * (sin12 + cos12 + sin24 + cos24) + (1 | day_in_season),
         data = df_rs_b, REML = TRUE),
    silent = TRUE
  )
  if (inherits(mod_b, "try-error")) next
  
  
  
  # Parameter simulation and storage
  sim_b <- try(arm::sim(mod_b, n.sims = nsim), silent = TRUE)
  if (inherits(sim_b, "try-error")) next
  
  fixefs <- sim_b@fixef  # nsim × p
  
  # Align column names (prevent issues due to contrast or column order)
  common <- intersect(colnames(fixefs), colnames(X_new))
  if (length(common) == 0L) next
  fits   <- fixefs[, common, drop = FALSE] %*% t(X_new[, common, drop = FALSE])  # nsim × n_new
  
  # Reshape to long format: each row = 1 sample × newdata
  fits_vec <- as.vector(t(fits))  # length = nsim * nrow(newdat)
  pred_b <- newdat[rep(seq_len(nrow(newdat)), times = nsim), ]
  pred_b$draw      <- rep(seq_len(nsim), each = nrow(newdat))
  pred_b$replicate <- b
  pred_b$fit       <- fits_vec
  
  used <- used + 1L
  all_preds[[b]] <- pred_b
  
  
  # Save fixed effects coefficients
  fixefs_all[[b]] <-  sim_b@fixef
  
  # Save random-effect variance and SD
  var_corr <- as.data.frame(VarCorr(mod_b))
  # Extract variance (Intercept) for day_in_season random effect
  ranefs_all[[b]] <- var_corr$sdcor[1]
}


### Summarize model results----
# Combine fixed effects (row bind)
fixefs_combined <- do.call(rbind, fixefs_all)

# Combine random effects (row bind)
ranefs_combined <- do.call(rbind, ranefs_all)



# Compute summary statistics for fixed effects (mean, SD, 95% CI)
fixefs_stats <- data.frame(
  mean = apply(fixefs_combined, 2, mean, na.rm = TRUE),
  sd = apply(fixefs_combined, 2, sd, na.rm = TRUE),
  lower_95ci = apply(fixefs_combined, 2, function(x) quantile(x, 0.025, na.rm = TRUE)),
  upper_95ci = apply(fixefs_combined, 2, function(x) quantile(x, 0.975, na.rm = TRUE))
)


# Compute summary statistics for random effects
ranefs_stats <- data.frame(
  mean = apply(ranefs_combined, 2, mean, na.rm = TRUE),
  sd = apply(ranefs_combined, 2, sd, na.rm = TRUE),
  lower_95ci = apply(ranefs_combined, 2, function(x) quantile(x, 0.025, na.rm = TRUE)),
  upper_95ci = apply(ranefs_combined, 2, function(x) quantile(x, 0.975, na.rm = TRUE))
)


write.csv(fixefs_stats, "biparental_monogamy_fixefs_stats.csv", row.names = TRUE)
write.csv(ranefs_stats, "biparental_monogamy_ranefs_stats.csv", row.names = TRUE)







### Prepare data for plotting----
preds_all <- dplyr::bind_rows(all_preds)

message("Effective replicates: ", used, " / ", B)

# Summarize predictions from all simulations and bootstraps
pred_summary <- pred_b %>%
  group_by(sex, hour) %>%
  summarise(
    fits   = median(.data$fit, na.rm = TRUE),
    lower = quantile(.data$fit, 0.025, na.rm = TRUE, names = FALSE),
    upper = quantile(.data$fit, 0.975, na.rm = TRUE, names = FALSE),
    .groups = "drop"
  )



pred_df<-pred_summary








##1.2 total nest attendance----

### model----
keep <- c("pred_df","make_df_rs","make_df")
rm(list = setdiff(ls(envir=.GlobalEnv, all.names=TRUE), keep),
   envir=.GlobalEnv); gc()# free memory


load("Organized data/summary_incu_hours.Rdata")

df_rs2 <- make_df_rs(summary_incu_hours, label_value = "Biparental monogamy")


library(lmerTest)
model2 <- lmer(
  attendance ~(sin12 + cos12+sin24 + cos24)+
    (1 | day_in_season),
  data = df_rs2,
  REML = T
)
summary(model2)

# Model diagnostics
library(performance)
check_autocorrelation(model2)  # passed
result <- check_collinearity(model2)
check_heteroscedasticity(model2) # test for heteroscedasticity
r2(model2)


# Posterior simulation of model parameters for confidence intervals 
nsim    <- 5000 # number of draws
sim_mod2 <- arm::sim(model2, n.sims = nsim) # draw parameter samples jointly from the fixed effects and variance components based on model estimates

# Extract simulated fixed-effect coefficients (rows = draws, columns = parameters)
fixefs2 <- sim_mod2@fixef


# Construct prediction dataset (0–24 h)
newdat <- expand.grid(
  hour = seq(0, 24, by = 1)
)
newdat <- newdat %>%
  mutate(
    sin12 = sin(2*pi*hour/12),
    cos12 = cos(2*pi*hour/12),
    sin24 = sin(2*pi*hour/24),
    cos24 = cos(2*pi*hour/24)
  )

# Design matrix and compute simulated predictions
# Only fixed effects used (population-level predictions, equivalent to re.form = NA)
X_new <- model.matrix(~ (sin12 + cos12+sin24 + cos24), data = newdat)   # n_new × p

# Align column names to ensure matching order
common <- intersect(colnames(fixefs2), colnames(X_new))
fixefs2  <- fixefs2[, common, drop = FALSE]
X_new   <- X_new[,  common, drop = FALSE]

# Compute nsim prediction curves: matrix multiplication (nsim × n_new)
fits_mat2 <- fixefs2 %*% t(X_new)



# Summarize median and 95% CI 
pred_df2 <- newdat %>%
  mutate(
    fits   = apply(fits_mat2, 2, median),
    lower = apply(fits_mat2, 2, quantile, probs = 0.025),
    upper = apply(fits_mat2, 2, quantile, probs = 0.975)
  )


# Extract fixed effects
fixefs_all <-  sim_mod2@fixef

# Extract random-effect variance and SD
var_corr <- as.data.frame(VarCorr(model2))
# Extract variance (Intercept) for day_in_season random effect
ranefs_all <- var_corr$sdcor[1]


# Compute summary statistics for fixed effects (mean, SD, 95% CI)
fixefs_stats <- data.frame(
  mean = apply(fixefs_all, 2, mean, na.rm = TRUE),
  sd = apply(fixefs_all, 2, sd, na.rm = TRUE),
  lower_95ci = apply(fixefs_all, 2, function(x) quantile(x, 0.025, na.rm = TRUE)),
  upper_95ci = apply(fixefs_all, 2, function(x) quantile(x, 0.975, na.rm = TRUE))
)


write.csv(fixefs_stats, "biparental_monogamy_fixefs_stats2.csv", row.names = TRUE)
write.csv(ranefs_all, "biparental_monogamy_ranefs_stats2.csv", row.names = TRUE)











## Combine plots----
# 1) Standardize group column and merge
plot_df3 <- bind_rows(
  pred_df  %>% mutate(sex = tolower(sex)),        # "male"/"female"
  pred_df2 %>% mutate(sex = "Total")            # "total"
) %>%
  mutate(
    group = recode(sex, male = "Male", female = "Female", overall = "Total"),
    group = factor(group, levels = c("Male","Female","Total"))
  )


# 2) Color palette
cols <- c(Male = "#4682B4", Female = "#99322E", Overall = "#D3D3D3")

# 3) Plot three curves
library(scales)

fig2_1<- ggplot(plot_df3, aes(x = hour, y = fits, color = group, fill = group)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.4, color = NA) +
  geom_line(size = 1) +
  scale_color_manual(values = cols, name = "Sex") +
  scale_fill_manual(values = cols,  name = "Sex") +
  scale_x_continuous(limits = c(0, 24), breaks = seq(0, 24, 4)) +
  scale_y_continuous(breaks = seq(0, 100, 25),
                     limits = c(0, 100),
                     oob = scales::squish) +   # key
  labs(x = "Time of day (h)", y = "Hourly nest attendance (%)") +
  ggtitle("Biparental monogamy") +
  theme_classic() +
  theme(
    text = element_text(size = 9),
    legend.position = "bottom",
    axis.text.x = element_text(margin = margin(t = 0)),
    axis.text.y = element_text(margin = margin(r = 0)),
    plot.title   = element_text(hjust = 0.5, size = 8)
  )
fig2_1


##2. Same-nest polygyny----

keep <- c("fig2_1","make_df_rs","make_df")
rm(list = setdiff(ls(envir=.GlobalEnv, all.names=TRUE), keep),
   envir=.GlobalEnv); gc()# free memory


## 2.1 sex difference in nest attendance----

### model----

load("Organized data/summary_incu_hours.Rdata")

df <- make_df(summary_incu_hours, label_value = "Same-nest polygyny")

library(dplyr)
library(lme4)
library(lmerTest)
library(arm)
library(ggplot2)

### Adjustable parameters
B     <- 200   # Number of bootstrap replicates (bagging iterations)
nsim  <- 5000  # Number of parameter draws per sim()
seed0 <- 2024  # Global seed for reproducibility
sex_levels <- c("male","female","female2")  # Three individual labels

df$sex <- factor(df$sex, levels = sex_levels)
df$day_in_season <- factor(df$day_in_season)

# Unified prediction grid (0–24h × 3 individuals)
newdat <- expand.grid(
  hour = seq(0, 24, by = 1),
  sex  = sex_levels,
  KEEP.OUT.ATTRS = FALSE
) %>%
  mutate(
    sin12 = sin(2*pi*hour/12),  cos12 = cos(2*pi*hour/12),
    sin24 = sin(2*pi*hour/24),  cos24 = cos(2*pi*hour/24),
    sex   = factor(sex, levels = sex_levels)
  )

# Design matrix consistent with model fixed effects (computed once)
X_new <- model.matrix(~ sex * (sin12 + cos12 + sin24 + cos24), data = newdat)

# Container for all replicate predictions
all_preds <- vector("list", B)
used <- 0L
# Store all model results
#all_models <- vector("list", B)
fixefs_all <- vector("list", B)  # Store fixed effects from each iteration
ranefs_all <- vector("list", B)  # Store random effects from each iteration

for (b in seq_len(B)) {
  set.seed(seed0 + b)
  
  # Randomly keep one record per NEST × date × hour
  df_rs_b <- df %>%
    group_by(NEST, date, hour) %>%
    slice_sample(n = 1) %>%
    ungroup() %>%
    mutate(
      sin12 = sin(2*pi*hour/12),  cos12 = cos(2*pi*hour/12),
      sin24 = sin(2*pi*hour/24),  cos24 = cos(2*pi*hour/24),
      sex   = factor(sex, levels = sex_levels),
      day_in_season = factor(day_in_season)
    )
  
  # Skip this iteration if any individual is completely missing; otherwise, fixed effects cannot be identified
  if (nlevels(droplevels(df_rs_b$sex)) < length(sex_levels)) next
  
  # Fit model
  mod_b <- try(
    lmer(attendance ~ sex * (sin12 + cos12 + sin24 + cos24) + (1 | day_in_season),
         data = df_rs_b, REML = TRUE),
    silent = TRUE
  )
  if (inherits(mod_b, "try-error")) next
  
  # Parameter simulation
  sim_b <- try(arm::sim(mod_b, n.sims = nsim), silent = TRUE)
  if (inherits(sim_b, "try-error")) next
  
  fixefs <- sim_b@fixef  # nsim × p
  
  # Align column names (to prevent mismatch in order or contrast coding)
  common <- intersect(colnames(fixefs), colnames(X_new))
  if (length(common) == 0L) next
  fits   <- fixefs[, common, drop = FALSE] %*% t(X_new[, common, drop = FALSE])  # nsim × n_new
  
  # Reshape to long format: each row = one “simulation × new data point”
  fits_vec <- as.vector(t(fits))  # length = nsim * nrow(newdat)
  pred_b <- newdat[rep(seq_len(nrow(newdat)), times = nsim), ]
  pred_b$draw      <- rep(seq_len(nsim), each = nrow(newdat))
  pred_b$replicate <- b
  pred_b$fit       <- fits_vec
  
  used <- used + 1L
  all_preds[[b]] <- pred_b
  
  # Extract fixed-effect coefficients and store in list
  fixefs_all[[b]] <-  sim_b@fixef
  
  # Extract random-effect variance and SD
  var_corr <- as.data.frame(VarCorr(mod_b))
  # Extract variance (Intercept) of day_in_season random effect
  ranefs_all[[b]] <- var_corr$sdcor[1]
}

### Report model results----
# Combine fixed-effect coefficients (row bind)
fixefs_combined <- do.call(rbind, fixefs_all)

# Combine random-effect results (row bind)
ranefs_combined <- do.call(rbind, ranefs_all)

# Compute summary statistics for fixed effects (mean, SD, 95% CI)
fixefs_stats <- data.frame(
  mean = apply(fixefs_combined, 2, mean, na.rm = TRUE),
  sd = apply(fixefs_combined, 2, sd, na.rm = TRUE),
  lower_95ci = apply(fixefs_combined, 2, function(x) quantile(x, 0.025, na.rm = TRUE)),
  upper_95ci = apply(fixefs_combined, 2, function(x) quantile(x, 0.975, na.rm = TRUE))
)

# Compute summary statistics for random effects
ranefs_stats <- data.frame(
  mean = apply(ranefs_combined, 2, mean, na.rm = TRUE),
  sd = apply(ranefs_combined, 2, sd, na.rm = TRUE),
  lower_95ci = apply(ranefs_combined, 2, function(x) quantile(x, 0.025, na.rm = TRUE)),
  upper_95ci = apply(ranefs_combined, 2, function(x) quantile(x, 0.975, na.rm = TRUE))
)

write.csv(fixefs_stats, "Same-nest polygyny_fixefs_stats.csv", row.names = TRUE)
write.csv(ranefs_stats, "Same-nest polygyny_ranefs_stats.csv", row.names = TRUE)

### Prepare data for plotting----
preds_all <- dplyr::bind_rows(all_preds)

message("Effective replicates: ", used, " / ", B)

# Summarize predictions across bootstrap × parameter draws
pred_summary <- pred_b %>%
  group_by(sex, hour) %>%
  summarise(
    fits   = median(.data$fit, na.rm = TRUE),
    lower = quantile(.data$fit, 0.025, na.rm = TRUE, names = FALSE),
    upper = quantile(.data$fit, 0.975, na.rm = TRUE, names = FALSE),
    .groups = "drop"
  )

pred_df<-pred_summary

## 2.2 total nest attendance----
keep <- c("fig2_1","pred_df","make_df_rs","make_df")
rm(list = setdiff(ls(envir=.GlobalEnv, all.names=TRUE), keep),
   envir=.GlobalEnv); gc()# free memory

### model----

load("Organized data/summary_incu_hours.Rdata")

df_rs2 <- make_df_rs(summary_incu_hours, label_value = "Same-nest polygyny")

library(lmerTest)
model2 <- lmer(
  attendance ~(sin12 + cos12+sin24 + cos24)+
    (1 | day_in_season),
  data = df_rs2,
  REML = T
)
summary(model2)

# Model diagnostics
library(performance)
check_autocorrelation(model2)  # passed
result <- check_collinearity(model2)
check_heteroscedasticity(model2) # test for heteroscedasticity
r2(model2)

# Posterior simulation of model parameters for confidence intervals 
nsim    <- 5000 # number of simulated draws
sim_mod2 <- arm::sim(model2, n.sims = nsim) # joint sampling of fixed and random effects, generating 5000 sets of possible parameter values

# Extract simulated fixed-effect coefficients (rows = draws, columns = coefficients)
fixefs2 <- sim_mod2@fixef

# Construct prediction data (0–24 hours)
newdat <- expand.grid(
  hour = seq(0, 24, by = 1)
)
newdat <- newdat %>%
  mutate(
    sin12 = sin(2*pi*hour/12),
    cos12 = cos(2*pi*hour/12),
    sin24 = sin(2*pi*hour/24),
    cos24 = cos(2*pi*hour/24)
  )

# Design matrix and generate simulated predictions 
# Only use fixed effects (population-level prediction, equivalent to re.form = NA)
X_new <- model.matrix(~ (sin12 + cos12+sin24 + cos24), data = newdat)   # n_new × p

# Align column names (prevent mismatch caused by factor/contrast order)
common <- intersect(colnames(fixefs2), colnames(X_new))
fixefs2  <- fixefs2[, common, drop = FALSE]
X_new   <- X_new[,  common, drop = FALSE]

# Compute nsim prediction curves: matrix multiplication (nsim × n_new)
fits_mat2 <- fixefs2 %*% t(X_new)

# Summarize median and 95% CI 
pred_df2 <- newdat %>%
  mutate(
    fits   = apply(fits_mat2, 2, median),
    lower = apply(fits_mat2, 2, quantile, probs = 0.025),
    upper = apply(fits_mat2, 2, quantile, probs = 0.975)
  )

# Extract fixed effects and store
fixefs_all <-  sim_mod2@fixef

# Extract random-effect variance and SD
var_corr <- as.data.frame(VarCorr(model2))
# Extract variance (Intercept) for day_in_season random effect
ranefs_all <- var_corr$sdcor[1]

# Compute summary statistics for fixed effects (mean, SD, 95% CI)
fixefs_stats <- data.frame(
  mean = apply(fixefs_all, 2, mean, na.rm = TRUE),
  sd = apply(fixefs_all, 2, sd, na.rm = TRUE),
  lower_95ci = apply(fixefs_all, 2, function(x) quantile(x, 0.025, na.rm = TRUE)),
  upper_95ci = apply(fixefs_all, 2, function(x) quantile(x, 0.975, na.rm = TRUE))
)

write.csv(fixefs_stats, "Same-nest polygyny_fixefs_stats2.csv", row.names = TRUE)
write.csv(ranefs_all, "Same-nest polygyny_ranefs_stats2.csv", row.names = TRUE)

## Combine plots----
# 1) Standardize group labels and merge
plot_df3 <- bind_rows(
  pred_df  %>% mutate(sex = tolower(sex)),        # "male"/"female"/"female2"
  pred_df2 %>% mutate(sex = "Total")              # "total"
) %>%
  mutate(
    group = recode(sex, male = "Male", female = "α_Female", female2 = "β_Female", overall = "Total"),
    group = factor(group, levels = c("Male","α_Female","β_Female","Total"))
  )

# 2) Color palette
cols <- c(Male = "#4682B4", α_Female = "#99322E", β_Female = "#F5DEB3", Total = "#D3D3D3")

# 3) Plot three curves
library(scales)

fig2_2<- ggplot(plot_df3, aes(x = hour, y = fits, color = group, fill = group)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.4, color = NA) +
  geom_line(size = 1) +
  scale_color_manual(values = cols, name = "Sex") +
  scale_fill_manual(values = cols, name = "Sex") +
  scale_x_continuous(limits = c(0, 24), breaks = seq(0, 24, 4)) +
  scale_y_continuous(breaks = seq(0, 100, 25),
                     limits = c(0, 100),
                     oob = scales::squish) +   # key
  labs(x = "Time of day (h)", y = "Hourly nest attendance (%)") +
  ggtitle("Same-nest polygyny") +
  theme_classic() +
  theme(
    legend.position = "bottom",
    text = element_text(size = 9),
    axis.text.x = element_text(margin = margin(t = 0)),
    axis.text.y = element_text(margin = margin(r = 0)),
    plot.title   = element_text(hjust = 0.5, size = 8)
  )

fig2_2



##3. Split-nest polygyny_α-nest----

keep <- c("fig2_2", "fig2_1","make_df_rs","make_df")
rm(list = setdiff(ls(envir=.GlobalEnv, all.names=TRUE), keep),
   envir=.GlobalEnv); gc()# free memory

##3.1 sex difference in nest attendance ----

### model----

load("Organized data/summary_incu_hours.Rdata")

df <- make_df(summary_incu_hours, label_value = "Split-nest polygyny_α-nest")

library(dplyr)
library(lme4)
library(lmerTest)
library(arm)
library(ggplot2)

### Parameters
B     <- 200   # Number of bootstrap replicates (bagging iterations)
nsim  <- 5000  # Number of parameter draws per sim() run
seed0 <- 2024  # Global seed for reproducibility
sex_levels <- c("male","female")  # Individual labels

df$sex <- factor(df$sex, levels = sex_levels)
df$day_in_season <- factor(df$day_in_season)

# Unified prediction grid (0–24h × 2 individuals)
newdat <- expand.grid(
  hour = seq(0, 24, by = 1),
  sex  = sex_levels,
  KEEP.OUT.ATTRS = FALSE
) %>%
  mutate(
    sin12 = sin(2*pi*hour/12),  cos12 = cos(2*pi*hour/12),
    sin24 = sin(2*pi*hour/24),  cos24 = cos(2*pi*hour/24),
    sex   = factor(sex, levels = sex_levels)
  )

# Design matrix consistent with model fixed effects
X_new <- model.matrix(~ sex * (sin12 + cos12 + sin24 + cos24), data = newdat)

# Container for all replicate predictions
all_preds <- vector("list", B)
used <- 0L
# Store all model results
#all_models <- vector("list", B)
fixefs_all <- vector("list", B)  # Store fixed-effect coefficients from each iteration
ranefs_all <- vector("list", B)  # Store random-effect results from each iteration

for (b in seq_len(B)) {
  set.seed(seed0 + b)
  
  # Randomly keep one record per NEST × date × hour
  df_rs_b <- df %>%
    group_by(NEST, date, hour) %>%
    slice_sample(n = 1) %>%
    ungroup() %>%
    mutate(
      sin12 = sin(2*pi*hour/12),  cos12 = cos(2*pi*hour/12),
      sin24 = sin(2*pi*hour/24),  cos24 = cos(2*pi*hour/24),
      sex   = factor(sex, levels = sex_levels),
      day_in_season = factor(day_in_season)
    )
  
  # Skip iteration if any individual is completely missing; otherwise, fixed effects cannot be identified
  if (nlevels(droplevels(df_rs_b$sex)) < length(sex_levels)) next
  
  # Fit model
  mod_b <- try(
    lmer(attendance ~ sex * (sin12 + cos12 + sin24 + cos24) + (1 | day_in_season),
         data = df_rs_b, REML = TRUE),
    silent = TRUE
  )
  if (inherits(mod_b, "try-error")) next
  
  # Parameter simulation
  sim_b <- try(arm::sim(mod_b, n.sims = nsim), silent = TRUE)
  if (inherits(sim_b, "try-error")) next
  
  fixefs <- sim_b@fixef  # nsim × p
  
  # Align column names (to avoid mismatch in order or contrast coding)
  common <- intersect(colnames(fixefs), colnames(X_new))
  if (length(common) == 0L) next
  fits   <- fixefs[, common, drop = FALSE] %*% t(X_new[, common, drop = FALSE])  # nsim × n_new
  
  # Reshape to long format: each row = one “simulation × prediction point”
  fits_vec <- as.vector(t(fits))  # length = nsim * nrow(newdat)
  pred_b <- newdat[rep(seq_len(nrow(newdat)), times = nsim), ]
  pred_b$draw      <- rep(seq_len(nsim), each = nrow(newdat))
  pred_b$replicate <- b
  pred_b$fit       <- fits_vec
  
  used <- used + 1L
  all_preds[[b]] <- pred_b
  # Save fixed effects
  fixefs_all[[b]] <-  sim_b@fixef
  
  # Save random-effect variance and SD
  var_corr <- as.data.frame(VarCorr(mod_b))
  # Extract variance (Intercept) for day_in_season random effect
  ranefs_all[[b]] <- var_corr$sdcor[1]
}



### Summarize model results----

# Combine fixed-effect coefficients (row bind)
fixefs_combined <- do.call(rbind, fixefs_all)

# Combine random-effect results (row bind)
ranefs_combined <- do.call(rbind, ranefs_all)

# Compute summary statistics for fixed effects (mean, SD, 95% CI)
fixefs_stats <- data.frame(
  mean = apply(fixefs_combined, 2, mean, na.rm = TRUE),
  sd = apply(fixefs_combined, 2, sd, na.rm = TRUE),
  lower_95ci = apply(fixefs_combined, 2, function(x) quantile(x, 0.025, na.rm = TRUE)),
  upper_95ci = apply(fixefs_combined, 2, function(x) quantile(x, 0.975, na.rm = TRUE))
)

# Compute summary statistics for random effects
ranefs_stats <- data.frame(
  mean = apply(ranefs_combined, 2, mean, na.rm = TRUE),
  sd = apply(ranefs_combined, 2, sd, na.rm = TRUE),
  lower_95ci = apply(ranefs_combined, 2, function(x) quantile(x, 0.025, na.rm = TRUE)),
  upper_95ci = apply(ranefs_combined, 2, function(x) quantile(x, 0.975, na.rm = TRUE))
)

write.csv(fixefs_stats, "Split-nest polygyny_a_fixefs_stats.csv", row.names = TRUE)
write.csv(ranefs_stats, "Split-nest polygyny_a_ranefs_stats.csv", row.names = TRUE)

### Prepare data for plotting----

preds_all <- dplyr::bind_rows(all_preds)

message("Effective replicates: ", used, " / ", B)

# Summarize all predictions across bootstrap × parameter simulations
pred_summary <- pred_b %>%
  group_by(sex, hour) %>%
  summarise(
    fits   = median(.data$fit, na.rm = TRUE),
    lower = quantile(.data$fit, 0.025, na.rm = TRUE, names = FALSE),
    upper = quantile(.data$fit, 0.975, na.rm = TRUE, names = FALSE),
    .groups = "drop"
  )

pred_df<-pred_summary

## 3.2 total----
load("Organized data/summary_incu_hours.Rdata")
df_rs2 <- make_df_rs(summary_incu_hours, label_value = "Split-nest polygyny_α-nest")

### model----

library(lmerTest)
model2 <- lmer(
  attendance ~(sin12 + cos12+sin24 + cos24)+
    (1 | day_in_season),
  data = df_rs2,
  REML = T
)
summary(model2)

# Model diagnostics
library(performance)
check_autocorrelation(model2)  # passed
result <- check_collinearity(model2)
check_heteroscedasticity(model2) # test for heteroscedasticity
r2(model2)

# Posterior simulation of model parameters for confidence intervals 
nsim    <- 5000 # number of draws
sim_mod2 <- arm::sim(model2, n.sims = nsim) # joint sampling based on fitted model coefficients and covariance matrix, yielding 5000 possible parameter sets

# Extract simulated fixed-effect coefficients (rows = draws, columns = coefficients)
fixefs2 <- sim_mod2@fixef   # equivalent to slot(sim_mod, "fixef")

# Construct prediction data (0–24 hours)
newdat <- expand.grid(
  hour = seq(0, 24, by = 1)
)
newdat <- newdat %>%
  mutate(
    sin12 = sin(2*pi*hour/12),
    cos12 = cos(2*pi*hour/12),
    sin24 = sin(2*pi*hour/24),
    cos24 = cos(2*pi*hour/24)
  )

# Design matrix and generate simulated predictions 
# Only use fixed effects (population-level prediction, equivalent to re.form = NA)
X_new <- model.matrix(~ (sin12 + cos12+sin24 + cos24), data = newdat)   # n_new × p

# Align column names (to prevent mismatch due to factor/contrast encoding)
common <- intersect(colnames(fixefs2), colnames(X_new))
fixefs2  <- fixefs2[, common, drop = FALSE]
X_new   <- X_new[,  common, drop = FALSE]

# Compute nsim prediction curves: matrix multiplication (nsim × n_new)
fits_mat2 <- fixefs2 %*% t(X_new)

# Summarize median and 95% CI 
pred_df2 <- newdat %>%
  mutate(
    fits   = apply(fits_mat2, 2, median),
    lower = apply(fits_mat2, 2, quantile, probs = 0.025),
    upper = apply(fits_mat2, 2, quantile, probs = 0.975)
  )

# Extract fixed effects
fixefs_all <-  sim_mod2@fixef

# Extract random-effect variance and SD
var_corr <- as.data.frame(VarCorr(model2))
# Extract variance (Intercept) for day_in_season random effect
ranefs_all <- var_corr$sdcor[1]

# Compute summary statistics for fixed effects (mean, SD, 95% CI)
fixefs_stats <- data.frame(
  mean = apply(fixefs_all, 2, mean, na.rm = TRUE),
  sd = apply(fixefs_all, 2, sd, na.rm = TRUE),
  lower_95ci = apply(fixefs_all, 2, function(x) quantile(x, 0.025, na.rm = TRUE)),
  upper_95ci = apply(fixefs_all, 2, function(x) quantile(x, 0.975, na.rm = TRUE))
)

write.csv(fixefs_stats, "Split-nest polygyny_a_fixefs_stats2.csv", row.names = TRUE)
write.csv(ranefs_all, "Split-nest polygyny_a_ranefs_stats2.csv", row.names = TRUE)

## Combine plots----
# 1) Standardize group column and merge
plot_df3 <- bind_rows(
  pred_df  %>% mutate(sex = tolower(sex)),        # "male"/"female"
  pred_df2 %>% mutate(sex = "Total")              # "total"
) %>%
  mutate(
    group = recode(sex, male = "Male", female = "α_Female", overall = "Total"),
    group = factor(group, levels = c("Male","α_Female","Total"))
  )

# 2) Color palette
cols <- c(Male = "#4682B4", α_Female = "#99322E", Overall = "#D3D3D3")

# 3) Plot three curves
fig2_3<- ggplot(plot_df3, aes(x = hour, y = fits, color = group, fill = group)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.4, color = NA) +
  geom_line(size = 1) +
  scale_color_manual(values = cols, name = "Sex") +
  scale_fill_manual(values = cols,  name = "Sex") +
  scale_x_continuous(limits = c(0, 24), breaks = seq(0, 24, 4)) +
  scale_y_continuous(breaks = seq(0, 100, 25),
                     limits = c(0, 100),
                     oob = scales::squish) +   # key
  labs(x = "Time of day (h)", y = "Hourly nest attendance (%)") +
  ggtitle("Split-nest polygyny_α-nest") +
  theme_classic() +
  theme(
    text = element_text(size = 9),
    legend.position = "bottom",
    axis.text.x = element_text(margin = margin(t = 0)),
    axis.text.y = element_text(margin = margin(r = 0)),
    plot.title   = element_text(hjust = 0.5, size = 8)
  )
fig2_3


##4. Split-nest polygyny_β-nest----

keep <- c("fig2_3", "fig2_2", "fig2_1","make_df_rs","make_df")
rm(list = setdiff(ls(envir=.GlobalEnv, all.names=TRUE), keep),
   envir=.GlobalEnv); gc()# free memory

##4.1 sex difference in nest attendance----

load("Organized data/summary_incu_hours.Rdata")
df <- make_df(summary_incu_hours, label_value = "Split-nest polygyny_β-nest")

library(dplyr)
library(lme4)
library(lmerTest)
library(arm)
library(ggplot2)

### Parameters
B     <- 200   # Number of bootstrap replicates
nsim  <- 5000  # Number of parameter draws per sim() run
seed0 <- 2024  # Global seed for reproducibility
sex_levels <- c("male","female")  # Individual labels

df$sex <- factor(df$sex, levels = sex_levels)
df$day_in_season <- factor(df$day_in_season)

# Unified prediction grid (0–24h × 2 individuals)
newdat <- expand.grid(
  hour = seq(0, 24, by = 1),
  sex  = sex_levels,
  KEEP.OUT.ATTRS = FALSE
) %>%
  mutate(
    sin12 = sin(2*pi*hour/12),  cos12 = cos(2*pi*hour/12),
    sin24 = sin(2*pi*hour/24),  cos24 = cos(2*pi*hour/24),
    sex   = factor(sex, levels = sex_levels)
  )

# Design matrix consistent with model fixed effects
X_new <- model.matrix(~ sex * (sin12 + cos12 + sin24 + cos24), data = newdat)

# Container for all replicate predictions
all_preds <- vector("list", B)
used <- 0L
# Store all model results
#all_models <- vector("list", B)
fixefs_all <- vector("list", B)  # Store fixed-effect coefficients for each iteration
ranefs_all <- vector("list", B)  # Store random-effect results for each iteration

for (b in seq_len(B)) {
  set.seed(seed0 + b)
  
  # Randomly keep one record per NEST × date × hour
  df_rs_b <- df %>%
    group_by(NEST, date, hour) %>%
    slice_sample(n = 1) %>%
    ungroup() %>%
    mutate(
      sin12 = sin(2*pi*hour/12),  cos12 = cos(2*pi*hour/12),
      sin24 = sin(2*pi*hour/24),  cos24 = cos(2*pi*hour/24),
      sex   = factor(sex, levels = sex_levels),
      day_in_season = factor(day_in_season)
    )
  
  # Skip iteration if any individual is completely missing; otherwise, fixed effects cannot be identified
  if (nlevels(droplevels(df_rs_b$sex)) < length(sex_levels)) next
  
  # Fit model
  mod_b <- try(
    lmer(attendance ~ sex * (sin12 + cos12 + sin24 + cos24) + (1 | day_in_season),
         data = df_rs_b, REML = TRUE),
    silent = TRUE
  )
  if (inherits(mod_b, "try-error")) next
  
  # Parameter simulation
  sim_b <- try(arm::sim(mod_b, n.sims = nsim), silent = TRUE)
  if (inherits(sim_b, "try-error")) next
  
  fixefs <- sim_b@fixef  # nsim × p
  
  # Align column names (to prevent mismatch in order or contrast coding)
  common <- intersect(colnames(fixefs), colnames(X_new))
  if (length(common) == 0L) next
  fits   <- fixefs[, common, drop = FALSE] %*% t(X_new[, common, drop = FALSE])  # nsim × n_new
  
  # Reshape to long format: each row = one “simulation × prediction point”
  fits_vec <- as.vector(t(fits))  # length = nsim * nrow(newdat)
  pred_b <- newdat[rep(seq_len(nrow(newdat)), times = nsim), ]
  pred_b$draw      <- rep(seq_len(nsim), each = nrow(newdat))
  pred_b$replicate <- b
  pred_b$fit       <- fits_vec
  
  used <- used + 1L
  all_preds[[b]] <- pred_b
  # Save fixed effects
  fixefs_all[[b]] <-  sim_b@fixef
  
  # Save random-effect variance and SD
  var_corr <- as.data.frame(VarCorr(mod_b))
  # Extract variance (Intercept) for day_in_season random effect
  ranefs_all[[b]] <- var_corr$sdcor[1]
}

### Summarize model results----

# Combine fixed-effect coefficients (row bind)
fixefs_combined <- do.call(rbind, fixefs_all)

# Combine random-effect results (row bind)
ranefs_combined <- do.call(rbind, ranefs_all)

# Compute summary statistics for fixed effects (mean, SD, 95% CI)
fixefs_stats <- data.frame(
  mean = apply(fixefs_combined, 2, mean, na.rm = TRUE),
  sd = apply(fixefs_combined, 2, sd, na.rm = TRUE),
  lower_95ci = apply(fixefs_combined, 2, function(x) quantile(x, 0.025, na.rm = TRUE)),
  upper_95ci = apply(fixefs_combined, 2, function(x) quantile(x, 0.975, na.rm = TRUE))
)

# Compute summary statistics for random effects
ranefs_stats <- data.frame(
  mean = apply(ranefs_combined, 2, mean, na.rm = TRUE),
  sd = apply(ranefs_combined, 2, sd, na.rm = TRUE),
  lower_95ci = apply(ranefs_combined, 2, function(x) quantile(x, 0.025, na.rm = TRUE)),
  upper_95ci = apply(ranefs_combined, 2, function(x) quantile(x, 0.975, na.rm = TRUE))
)

write.csv(fixefs_stats, "Split-nest polygyny_b_fixefs_stats.csv", row.names = TRUE)
write.csv(ranefs_stats, "Split-nest polygyny_b_ranefs_stats.csv", row.names = TRUE)

### Prepare data for plotting----

preds_all <- dplyr::bind_rows(all_preds)

message("Effective replicates: ", used, " / ", B)

# Summarize all predictions across bootstrap × parameter simulations
pred_summary <- pred_b %>%
  group_by(sex, hour) %>%
  summarise(
    fits   = median(.data$fit, na.rm = TRUE),
    lower = quantile(.data$fit, 0.025, na.rm = TRUE, names = FALSE),
    upper = quantile(.data$fit, 0.975, na.rm = TRUE, names = FALSE),
    .groups = "drop"
  )

pred_df<-pred_summary

## 4.2 total----
load("Organized data/summary_incu_hours.Rdata")
df_rs2 <- make_df_rs(summary_incu_hours, label_value = "Split-nest polygyny_β-nest")

### model----

library(lmerTest)
model2 <- lmer(
  attendance ~(sin12 + cos12+sin24 + cos24)+
    (1 | day_in_season),
  data = df_rs2,
  REML = T
)
summary(model2)

# Model diagnostics
library(performance)

# Posterior simulation of model parameters for confidence intervals 
nsim    <- 5000 # number of draws
sim_mod2 <- arm::sim(model2, n.sims = nsim) # joint sampling based on fitted model coefficients and covariance matrix, yielding 5000 sets of possible parameter values

# Extract simulated fixed-effect coefficients (rows = draws, columns = coefficients)
fixefs2 <- sim_mod2@fixef   # equivalent to: slot(sim_mod, "fixef")

# Construct prediction data (0–24 hours)
newdat <- expand.grid(
  hour = seq(0, 24, by = 1)
)
newdat <- newdat %>%
  mutate(
    sin12 = sin(2*pi*hour/12),
    cos12 = cos(2*pi*hour/12),
    sin24 = sin(2*pi*hour/24),
    cos24 = cos(2*pi*hour/24)
  )

# Design matrix and generate simulated predictions 
# Use only fixed effects (population-level prediction, equivalent to re.form = NA)
X_new <- model.matrix(~ (sin12 + cos12+sin24 + cos24), data = newdat)   # n_new × p

# Align column names (prevent mismatch caused by factor/contrast coding)
common <- intersect(colnames(fixefs2), colnames(X_new))
fixefs2  <- fixefs2[, common, drop = FALSE]
X_new   <- X_new[,  common, drop = FALSE]

# Compute nsim prediction curves: matrix multiplication (nsim × n_new)
fits_mat2 <- fixefs2 %*% t(X_new)

# Summarize median and 95% CI 
pred_df2 <- newdat %>%
  mutate(
    fits   = apply(fits_mat2, 2, median),
    lower = apply(fits_mat2, 2, quantile, probs = 0.025),
    upper = apply(fits_mat2, 2, quantile, probs = 0.975)
  )

# Extract fixed effects
fixefs_all <-  sim_mod2@fixef

# Extract random-effect variance and SD
var_corr <- as.data.frame(VarCorr(model2))
# Extract variance (Intercept) for day_in_season random effect
ranefs_all <- var_corr$sdcor[1]

# Compute summary statistics for fixed effects (mean, SD, 95% CI)
fixefs_stats <- data.frame(
  mean = apply(fixefs_all, 2, mean, na.rm = TRUE),
  sd = apply(fixefs_all, 2, sd, na.rm = TRUE),
  lower_95ci = apply(fixefs_all, 2, function(x) quantile(x, 0.025, na.rm = TRUE)),
  upper_95ci = apply(fixefs_all, 2, function(x) quantile(x, 0.975, na.rm = TRUE))
)

write.csv(fixefs_stats, "Split-nest polygyny_b_fixefs_stats2.csv", row.names = TRUE)
write.csv(ranefs_all, "Split-nest polygyny_b_ranefs_stats2.csv", row.names = TRUE)

## Combine plots----
# 1) Standardize group column and merge
plot_df3 <- bind_rows(
  pred_df  %>% mutate(sex = tolower(sex)),        # "male"/"female"
  pred_df2 %>% mutate(sex = "Total")              # "total"
) %>%
  mutate(
    group = recode(sex, male = "Male", female = "β_Female", overall = "Total"),
    group = factor(group, levels = c("Male","β_Female","Total"))
  )

# 2) Color palette
cols <- c(Male = "#4682B4", β_Female = "#F5DEB3", Overall = "#D3D3D3")

# 3) Plot three curves
fig2_4<- ggplot(plot_df3, aes(x = hour, y = fits, color = group, fill = group)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.4, color = NA) +
  geom_line(size = 1) +
  scale_color_manual(values = cols, name = "Sex") +
  scale_fill_manual(values = cols,  name = "Sex") +
  scale_x_continuous(limits = c(0, 24), breaks = seq(0, 24, 4)) +
  scale_y_continuous(breaks = seq(0, 100, 25),
                     limits = c(0, 100),
                     oob = scales::squish) +   # key
  labs(x = "Time of day (h)", y = "Hourly nest attendance (%)") +
  ggtitle("Split-nest polygyny_β-nest") +
  theme_classic() +
  theme(
    text = element_text(size = 9),
    legend.position = "bottom",
    axis.text.x = element_text(margin = margin(t = 0)),
    axis.text.y = element_text(margin = margin(r = 0)),
    plot.title   = element_text(hjust = 0.5, size = 8)
  )
fig2_4

## Combine all subplots into Figure 2----

fig2_1_nl <- fig2_1 + theme(legend.position = "none") + labs(x = "") 
fig2_2_nl  <- fig2_2 + theme(legend.position = "bottom")+ labs(x = "",y = "") 
fig2_3_nl  <- fig2_3 + theme(legend.position = "none")+ labs(x = "") 
fig2_4_nl  <- fig2_4 + theme(legend.position = "none")+ labs(x = "",y = "") 

p4 <- ggpubr::ggarrange(
  fig2_1_nl, fig2_2_nl, fig2_3_nl, fig2_4_nl,
  nrow = 2, ncol = 2, labels = c("a","b","c","d"),
  common.legend = T, legend = "bottom"  # key lines
)

p4

library(ggpubr)
final_plot <- annotate_figure(p4,
                              bottom = text_grob("Time of the day [h]", 
                                                 size = 8, vjust = -1.5))

ggsave("paper plot/Figure2.png", plot = final_plot, 
       width = 17, 
       height = 11, 
       dpi = 600,  
       units = "cm")  

ggsave("paper plot/Figure2.pdf", plot = final_plot, 
       width = 17, 
       height = 11, 
       dpi = 600,  
       units = "cm")  



# *Figure3*----

rm(list=ls())

library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)

process_nest <- function(path_i) {
  load(path_i)            # data_all
  df <- data_all %>%
    mutate(date_minute = ymd_hms(date_minute)) %>%
    # generate a complete minute sequence
    summarise(start = min(date_minute), end = max(date_minute)) %>%
    mutate(time_seq = list(seq(start, end, by = "1 min"))) %>%
    unnest(time_seq) %>%
    rename(date_minute = time_seq) %>%
    left_join(data_all %>% mutate(date_minute = ymd_hms(date_minute)),
              by = "date_minute") %>%
    mutate(
      Date = as_date(date_minute),
      #Day  = as.numeric(Date - min(Date)) + 1,
      Minute = hour(date_minute)*60 + minute(date_minute),
      birds_behavior = replace_na(birds_behavior,4),
      Status = factor(birds_behavior,
                      levels = c(0,1,2,3,4),
                      labels = c("No bird","Male_α","Female_α","Female_β","Invisible")),
      NEST = data_all$NEST[1]  # add nest identifier
    )
  return(df)
}

# path vector
path <- list.files("nest-1", full.names=TRUE)


## Figure3b----

# α-nest is the 6th, β-nest is the 5th file
df_alpha <- process_nest(path[6])
df_beta  <- process_nest(path[5])

# merge
df_all <- bind_rows(df_alpha, df_beta)

df_all$Day  <- as.numeric(df_all$Date - min(df_all$Date)) + 1

df_all$Status2<-df_all$Status
df_all$Status2[which(df_all$Status2=="Female_α" & df_all$NEST=="E050")]<-"Female_β"

library(ggplot2)

date_labels <- df_all %>%
  distinct(Day, Date) %>%
  arrange(Day)

df_all$NEST <- factor(df_all$NEST,
                      levels = c("E051","E050"),
                      labels = c("Same-nest polygyny α-nest","Same-nest polygyny β-nest"))


# highlight end of incubation period
date_labels

# day0 = 3 that day
day_high <- c(21, 26)         # days to be highlighted
time_start <- c(24*60, 24*60)   # start times (08:00, 12:00)
time_end   <- time_start + 30  # each segment lasts 10 minutes

hl_multi <- data.frame(
  NEST = factor(c("Same-nest polygyny α-nest","Same-nest polygyny β-nest")),
  xmin = time_start,
  xmax = time_end,
  ymin = day_high - 0.5,
  ymax = day_high + 0.5,
  Highlight = "Highlighted period" 
)


# highlighted days used for analysis
load("C:/Users/LX/Desktop/Organized data/summary_incu_days.Rdata")
day_high_α<-date_labels$Day[date_labels$Date %in% nestdaydata$date[nestdaydata$lable=="Split-nest polygyny_α-nest"]]
day_high_β<-date_labels$Day[date_labels$Date %in% nestdaydata$date[nestdaydata$lable=="Split-nest polygyny_β-nest"]]

time_start <- rep((0*60-30),length(day_high_α))   # e.g., 08:00, 12:00
time_end   <- rep((0*60),length(day_high_α))      # each segment lasts 10 min

hl_multi2 <- data.frame(
  NEST = factor(rep("Same-nest polygyny α-nest",length(day_high_α))),
  xmin = time_start,
  xmax = time_end,
  ymin = day_high_α - 0.5,
  ymax = day_high_α + 0.5,
  Highlight = "Highlighted period2" 
)

time_start <- rep((0*60-30),length(day_high_β))
time_end   <- rep((0*60),length(day_high_β))

hl_multi3 <- data.frame(
  NEST = factor(rep("Same-nest polygyny β-nest",length(day_high_β))),
  xmin = time_start,
  xmax = time_end,
  ymin = day_high_β - 0.5,
  ymax = day_high_β + 0.5,
  Highlight = "Highlighted period2" 
)

## 1) mark dates
# Orange: from hl_multi (Highlighted period)
markers_orange <- hl_multi %>%
  transmute(
    NEST,
    Day    = as.numeric((ymin + ymax) / 2),  # recover original Day value
    Minute = -30,                            # place markers at -30 minutes
    Label  = "Highlighted period"            # for color mapping
  ) %>% distinct(NEST, Day, .keep_all = TRUE)

# Blue: from hl_multi2 + hl_multi3 (Highlighted period2)
markers_blue <- bind_rows(hl_multi2, hl_multi3) %>%
  transmute(
    NEST,
    Day    = as.numeric((ymin + ymax) / 2),
    Minute = -20,                            # offset to avoid overlap
    Label  = "Highlighted period2"
  ) %>% distinct(NEST, Day, .keep_all = TRUE)

markers <- bind_rows(markers_orange, markers_blue)


# create rectangles for each day
rects <- df_all %>%
  dplyr::distinct(Day) %>%       # one per day
  dplyr::mutate(
    xmin = 0,                          # minute range from 0 to 1440
    xmax = 1440,
    ymin = Day - 0.5,                  # add 0.5 margin above/below each line
    ymax = Day + 0.5
  )

# plot aligned by Day
p_align_b <- ggplot(df_all, aes(x = Minute, y = Day, fill = Status2)) +
  
  # left-side highlight dots
  geom_point(
    data = markers,
    aes(x = Minute, y = Day, color = Label),
    inherit.aes = FALSE,
    shape = 16, size = 1.5, alpha = 1
  ) +
  # set color manually for markers
  scale_color_manual(
    values = c(
      "Highlighted period"  = "orange",
      "Highlighted period2" = "#56B4E9"
    ),
    guide = "none"
  ) +
  # leave left margin to display negative Minute and prevent clipping
  scale_x_continuous(
    limits = c(-30, 1440),
    breaks = seq(0, 1440, by = 120),
    labels = sprintf("%02d", seq(0, 24, by = 2))
  ) +
  geom_tile() +
  scale_fill_manual(
    values = c(
      "No bird"            = "#F0F0F0",
      "Male_α"             = "#4682B4",
      "Female_α"           = "#99322E",
      "Female_β"           = "#F5DEB3",
      "Invisible"          = "#4A4A4A",
      "Highlighted period" = "orange",
      "Highlighted period2" = "#56B4E9"
    ),
    labels = c(
      "No bird", "Male / α-Male", "Female / α-Female", "β-Female", "Invisible",
      "End of incubation period / Hatch","Date of analysis"
    )
  ) +
  scale_y_continuous(
    trans       = "reverse",
    breaks      = date_labels$Day,
    labels      = date_labels$Date,
    minor_breaks = seq(min(date_labels$Day) - 0.5,
                       max(date_labels$Day) + 0.5,
                       by = 1)     
  ) +
  # side-by-side nests, single row, free x-axis, fixed y-axis
  facet_wrap(~ NEST, nrow = 1, scales = "free_x") +
  labs(x = "Time (h)", y = "Date", fill = "Sex") +
  theme_minimal() +
  guides(
    fill = guide_legend(
      nrow       = 2,
      byrow      = TRUE,
      keywidth   = unit(0.3, "cm"),
      keyheight  = unit(0.3, "cm"),
      default.unit = "cm"
    )
  ) +
  geom_rect(
    data = rects,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE,
    fill  = NA,
    color = "grey80",
    linewidth = 0.4
  ) +
  theme(
    legend.position      = "bottom",
    legend.direction     = "horizontal",
    legend.key.size      = unit(0.4, "cm"),
    legend.text          = element_text(size = 7),
    legend.title         = element_text(size = 8),
    legend.spacing.x     = unit(0.2, "cm"),
    legend.spacing.y     = unit(0.1, "cm"),
    axis.text.x          = element_text(size = 6),
    axis.text.y          = element_text(size = 6),
    strip.text           = element_text(size = 8),
    panel.grid           = element_blank(),
    plot.margin          = margin(0, 0, 0, 2)
  )

print(p_align_b)

## Figure3a----

df_1  <- process_nest(path[1])
df_2 <- process_nest(path[2])

# merge
df_all <- bind_rows(df_1,df_2)

df_all$Day  <- as.numeric(df_all$Date - min(df_all$Date)) + 1

df_all$NEST <- factor(df_all$NEST,
                      levels = c("E015","E009"),
                      labels = c("Biparental monogamy example nest","Same-nest polygyny"))

library(ggplot2)

date_labels <- df_all %>%
  distinct(Day, Date) %>%
  arrange(Day)

# highlight end of incubation period
date_labels

# day0 = 3 that day
day_high <- c(11, 5)
time_start <- c(24*60, 20*60)
time_end   <- time_start + 30

hl_multi <- data.frame(
  NEST = factor(c("Same-nest polygyny","Biparental monogamy example nest")),
  xmin = time_start,
  xmax = time_end,
  ymin = day_high - 0.5,
  ymax = day_high + 0.5,
  Highlight = "Highlighted period" 
)

# highlighted days used for analysis
load("C:/Users/LX/Desktop/Organized data/summary_incu_days.Rdata")
day_high_1<-date_labels$Day[date_labels$Date %in% nestdaydata$date[nestdaydata$lable=="Same-nest polygyny"]]
day_high_2<-date_labels$Day[date_labels$Date %in% nestdaydata$date[nestdaydata$NEST=="E015"]]

time_start <- rep((0*60-30),length(day_high_1))
time_end   <- rep((0*60),length(day_high_1))

hl_multi2 <- data.frame(
  NEST = factor(rep("Same-nest polygyny",length(day_high_1))),
  xmin = time_start,
  xmax = time_end,
  ymin = day_high_1 - 0.5,
  ymax = day_high_1 + 0.5,
  Highlight = "Highlighted period2" 
)

time_start <- rep((0*60-30),length(day_high_2))
time_end   <- rep((0*60),length(day_high_2))

hl_multi3 <- data.frame(
  NEST = factor(rep("Biparental monogamy example nest",length(day_high_2))),
  xmin = time_start,
  xmax = time_end,
  ymin = day_high_2 - 0.5,
  ymax = day_high_2 + 0.5,
  Highlight = "Highlighted period2" 
)



# *Figure4*----

## Figure4a----

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr) # add significance tests
library(ggpubr) # statistical significance testing
library(lubridate)

load("Organized data/summary_incu_days.Rdata") # updated dataset including 2023

plot<-nestdaydata
plot$attendance<-(plot$count_total_incu/plot$total_count)*100


## start plotting
n_data <- plot %>%
  group_by(lable) %>%
  summarise(n = n(), .groups = "drop") %>%
  # assign y-position for each group, slightly above the plot top (around y=95)
  mutate(y = 30)

p2a <- ggplot(data=plot,aes(x=factor(lable, levels = c("Biparental monogamy", "Biparental monogamy_2023-α-pair", "Same-nest polygyny", "Split-nest polygyny_α-nest","Split-nest polygyny_β-nest")), y=attendance)) +
  geom_boxplot(width = 0.5,outlier.shape = NA,alpha = 0.8) +
  stat_summary(fun="mean", geom="point", shape=20, size=3, color="red", fill="red",alpha=0.7) +
  stat_summary(
    fun     = "mean", 
    geom    = "text",
    aes(label = sprintf("%.1f", ..y..)),
    color   = "#333333",     # dark grey text
    size    = 2.5,           # font size
    fontface= "bold",     
    position= position_dodge(width = 0.6),
    vjust   = 4,
    hjust   = -0.3           # left align, place text to the right of the point
  )+
  geom_point( aes(y = attendance),position = position_jitter(width = 0.2),alpha = 0.4,size=1)+ 
  # stat_compare_means(comparisons = list(c("Biparental monogamy", "Biparental monogamy_2023-α-pair"),c("Biparental monogamy", "Same-nest polygyny"), c("Split-nest polygyny_α-nest", "Biparental monogamy"), c("Split-nest polygyny_β-nest", "Biparental monogamy"), c("Same-nest polygyny", "Biparental monogamy_2023-α-pair")), method = "t.test") +
  guides(fill = FALSE,color = FALSE)+  # remove legends
  theme_classic()+
  scale_x_discrete(labels = c("Biparental monogamy" = "Biparental monogamy","Biparental monogamy_2023-α-pair" = "Biparental monogamy\n2023-α-pair", "Same-nest polygyny" = "Same-nest polygyny", "Split-nest polygyny_α-nest" = "Split-nest polygyny\nα-nest", "Split-nest polygyny_β-nest" = "Split-nest polygyny\nβ-nest")) +
  ylab("Total daily nest attendance %") +
  xlab("Breeding types") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(size=10))+ 
  theme(text = element_text(color = "black"))+
  geom_text(
    data = n_data,
    aes(
      x     = factor(lable,
                     levels = c("Biparental monogamy", 
                                "Biparental monogamy_2023-α-pair", 
                                "Same-nest polygyny", 
                                "Split-nest polygyny_α-nest",
                                "Split-nest polygyny_β-nest")),
      y     = y,
      label = paste0("n=", n)
    ),
    size  = 2.5,
    vjust = 0   
  )+
  theme(
    # tick labels
    axis.text.x = element_text(,size = 7),   # x-axis labels
    axis.text.y = element_text(size = 8),    # y-axis labels
    
    # axis titles
    axis.title.x = element_text(size = 7),   # x-axis title
    axis.title.y = element_text(size = 8),   # y-axis title
    strip.text   = element_text(size = 7),
    legend.position = "none"
  ) +theme(
    axis.title.x = element_blank()
  ) 

p2a


## Figure4b----
library(tidyverse)
library(ggpubr)
# 1. Load and combine data for all labels (load adjust file once)
load("Organized data/summary_incu_days.Rdata")  # creates nestdaydata

all_labels <- c(
  "Biparental monogamy",
  "Biparental monogamy_2023-α-pair",
  "Same-nest polygyny",
  "Split-nest polygyny_α-nest",
  "Split-nest polygyny_β-nest"
)

df_list <- map(all_labels, function(lab) {
  nd <- nestdaydata %>% filter(lable == lab)
  # remove unnecessary count columns
  nd <- nd %>% select(-count_0, -count_4)
  
  nd %>%
    pivot_longer(
      cols = starts_with("count_"),
      names_to = "type", values_to = "count"
    ) %>%
    mutate(
      attendance = count / total_count * 100,
      Status = case_when(
        type == "count_1" ~ ifelse(     
          str_detect(lab, "polygyny") | str_detect(lab, "2023"), # check if label string contains "polygyny"
          "α-male", # if yes (polygynous type), label as "α-male"
          "Male"),
        type == "count_2" ~ ifelse(
          str_detect(lab, "β-nest"), 
          "β-female",
          ifelse(str_detect(lab, "polygyny")| str_detect(lab, "2023"), 
                 "α-female", 
                 "Female")),
        type == "count_3" ~ "β-female",
        TRUE ~ "Total" # default case if none of the above match
      ),
      label = lab
    ) %>%
    filter(!(type == "count_3" & label != "Same-nest polygyny"))%>%
    filter(Status != "Total")
})

df <- bind_rows(df_list) # merge all dataframes by rows

# 2. Combine both Split-nest polygyny nests and create an overall panel for α+β nest
E_combined <- df %>%
  # keep only E050/E051
  filter(NEST %in% c("E050","E051")) %>%
  # keep only dates appearing in both nests
  group_by(date) %>%
  filter(all(c("E050","E051") %in% NEST)) %>% 
  # sum count_1, reset NEST
  summarise(
    attendance = sum(attendance[type == "count_1"], na.rm = TRUE),
    NEST    = "E050+E051",
    Status  =  "α-male",
    label   =  "Split-nest polygyny_α+β-nest",
    .groups = "drop"
  )
df2 <- bind_rows(df, E_combined)

# 4. Set factor levels
all_labels <- c(
  "Biparental monogamy",
  "Biparental monogamy_2023-α-pair",
  "Same-nest polygyny",
  "Split-nest polygyny_α-nest",
  "Split-nest polygyny_β-nest",
  "Split-nest polygyny_α+β-nest"
)
df2$label <- factor(df2$label, levels = all_labels)
df2$Status <- factor(df2$Status,
                     levels = c("Male","Female","α-male","α-female","β-female","Total-Female"))

my_labels <- c(
  "Biparental monogamy"             = "Biparental monogamy",
  "Biparental monogamy_2023-α-pair" = "Biparental monogamy\n2023-α-pair",
  "Cooperative polygyny"            = "Cooperative polygyny",
  "Same-nest polygyny"              = "Same-nest polygyny",
  "Split-nest polygyny_α-nest"      = "Split-nest polygyny\nα-nest",
  "Split-nest polygyny_β-nest"      = "Split-nest polygyny\nβ-nest",
  "Split-nest polygyny_α+β-nest"    = "Split-nest polygyny\nα+β-nest"
)

# 5. Plotting: one-row facet + boxplot + mean + scatter + dashed line + significance
library(ggh4x)

p2b <- ggplot(df2, aes(x = Status, y = attendance, fill = Status)) +
  geom_boxplot(width = 0.6, outlier.shape = NA, alpha = 0.8) +
  stat_summary(fun = "mean", geom = "point", shape = 20, size = 2, color = "red") +
  stat_summary(
    fun     = "mean", 
    geom    = "text",
    aes(label = sprintf("%.1f", ..y..)),
    color   = "#333333",     # dark grey text
    size    = 2.5,           # larger font
    fontface= "bold",     
    position= position_dodge(width = 0.6),
    vjust   = 5,
    hjust   = -0.3           # left align, place to the right of the point
  )+
  geom_jitter(aes(color = Status), width = 0.15, size = 0.7, alpha = 0.4) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "#525252",size=1) +
  scale_fill_manual(values = c(
    "Male"          = "#4682B4",
    "Female"        = "#99322E",
    "α-male"        = "#4682B4",
    "α-female"      = "#99322E",
    "β-female"      = "#e9b383",
    "Overall female"= "grey50"
  )) +
  scale_color_manual(values = c(
    "Male"          = "#465a8b",
    "Female"        = "#99322E",
    "α-male"        = "#465a8b",
    "α-female"      = "#99322E",
    "β-female"      = "#a05d1a",
    "Overall female"= "grey50"
  )) +
  ggh4x::facet_grid2(
    . ~ label,           
    scales = "free_x",   
    space  = "fixed",    
    labeller = as_labeller(my_labels)
  )+
  ggh4x::force_panelsizes(
    cols = c(1,1, 1.4, 1, 1, 0.5),   
    respect = FALSE                
  )+
  theme_classic() +
  ylab("Individual daily nest attendance %")  +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1,size = 7),   # x-axis ticks
    axis.text.y = element_text(size = 8),   # y-axis ticks
    axis.title.x = element_text(size = 7),  # x-axis title
    axis.title.y = element_text(size = 8),  # y-axis title
    strip.text.x = element_text(size = 6, margin = margin(t = 1, r = 3, b = 1, l = 4)),
    strip.background.x = element_rect(fill = "grey98", colour = NA),
    legend.position = "none"
  ) +theme(
    axis.title.x = element_blank()
  ) 

print(p2b)

p2<-ggarrange(p2a, p2b, nrow = 2, heights = c(0.8,1.6),labels = c("a", "b"),
              label.x = 0,
              label.y = 1.05)

p2

ggsave("paper plot/Figure4.pdf", plot = p2, 
       width = 18,
       height = 8,
       dpi = 600,   
       units = "cm")  
ggsave("paper plot/Figure4.png", plot = p2, 
       width = 18,
       height = 8,
       dpi = 600,   
       units = "cm") 


# Table S1 - Correlation between sexes ----

## Biparental monogamy ----
load("Organized data/summary_incu_hours.Rdata")
df <- make_df(summary_incu_hours, label_value = "Biparental monogamy")
library(Hmisc)
wide <- df %>%
  dplyr::select(NEST, date, hour, sex, attendance) %>%
  tidyr::pivot_wider(names_from = sex, values_from = attendance)

# Correlation coefficient and p-value
mat <- wide %>% dplyr::select(-NEST, -date, -hour)
res <- rcorr(as.matrix(mat), type = "spearman")
res$r   # correlation matrix


## Same-nest polygyny ----
load("Organized data/summary_incu_hours.Rdata")
df <- make_df(summary_incu_hours, label_value = "Same-nest polygyny")
library(Hmisc)
wide <- df %>%
  dplyr::select(NEST, date, hour, sex, attendance) %>%
  tidyr::pivot_wider(names_from = sex, values_from = attendance)

# Correlation coefficient and p-value
mat <- wide %>% dplyr::select(-NEST, -date, -hour)
res <- rcorr(as.matrix(mat), type = "spearman")
res$r


## Split-nest polygyny_α-nest ----
load("Organized data/summary_incu_hours.Rdata")
df <- make_df(summary_incu_hours, label_value = "Split-nest polygyny_α-nest")

library(Hmisc)
wide <- df %>%
  dplyr::select(NEST, date, hour, sex, attendance) %>%
  tidyr::pivot_wider(names_from = sex, values_from = attendance)

# Correlation coefficient and p-value
mat <- wide %>% dplyr::select(-NEST, -date, -hour)
res <- rcorr(as.matrix(mat), type = "spearman")
res$r


## Split-nest polygyny_β-nest ----
load("Organized data/summary_incu_hours.Rdata")
df <- make_df(summary_incu_hours, label_value = "Split-nest polygyny_β-nest")

library(Hmisc)
wide <- df %>%
  dplyr::select(NEST, date, hour, sex, attendance) %>%
  tidyr::pivot_wider(names_from = sex, values_from = attendance)

# Correlation coefficient and p-value
mat <- wide %>% dplyr::select(-NEST, -date, -hour)
res <- rcorr(as.matrix(mat), type = "spearman")
res$r


# Compare exchange gaps ----

data.all.dir6<-paste0("Organized data/","summary_bout_data",".Rdata")
load(data.all.dir6)

summary(summary_bout_data$gap_length[summary_bout_data$NEST=="E009"])

summary_bout_data2<-summary_bout_data[!summary_bout_data$NEST %in% c("E009","E050","E051","E095"),]
summary(summary_bout_data2$gap_length)

t.test(summary_bout_data$gap_length[summary_bout_data$NEST=="E009"], summary_bout_data2$gap_length)

summary(summary_bout_data$gap_length[summary_bout_data$NEST=="E051"])
summary(summary_bout_data$gap_length[summary_bout_data$NEST=="E050"])

t.test(summary_bout_data$gap_length[summary_bout_data$NEST=="E051"], summary_bout_data2$gap_length)
t.test(summary_bout_data$gap_length[summary_bout_data$NEST=="E050"], summary_bout_data2$gap_length)

