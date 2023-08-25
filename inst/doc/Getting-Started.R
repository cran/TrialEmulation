## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
old <- options("digits" = 3)
data.table::setDTthreads(2)

## ----data---------------------------------------------------------------------
library(TrialEmulation)
# Prepare the example data
data("trial_example")
# Set columns to factors as necessary
trial_example$catvarA <- as.factor(trial_example$catvarA)
trial_example$catvarB <- as.factor(trial_example$catvarB)

head(trial_example)

## ----initiators---------------------------------------------------------------
result <- initiators(
  data = trial_example,
  id = "id",
  period = "period",
  eligible = "eligible",
  treatment = "treatment",
  outcome = "outcome",
  model_var = "assigned_treatment",
  outcome_cov = c("catvarA", "catvarB", "nvarA", "nvarB", "nvarC"),
  use_censor = FALSE,
  use_weight = FALSE
)

## ----initiators_summary-------------------------------------------------------
summary(result)

## ----init_summary-------------------------------------------------------------
summary(result$model)

## ----init_robust_summary------------------------------------------------------
print(result$robust$summary)

## ----init_robust_matrix-------------------------------------------------------
# only print the first columns
head(result$robust$matrix, c(17, 4))

## ----temp_dir-----------------------------------------------------------------
# for the purposes of the vignette, we use a temporary directory, however it may be useful to use a permanent
# location in order to inspect the outputs later
working_dir <- file.path(tempdir(TRUE), "trial_emu")
if (!dir.exists(working_dir)) dir.create(working_dir)

## ----data_preparation---------------------------------------------------------
prep_data <- data_preparation(
  data = trial_example,
  id = "id",
  period = "period",
  eligible = "eligible",
  treatment = "treatment",
  outcome = "outcome",
  model_var = "assigned_treatment",
  outcome_cov = ~ catvarA + catvarB + nvarA + nvarB + nvarC,
  data_dir = working_dir,
  save_weight_models = TRUE,
  use_censor = TRUE,
  use_weight = TRUE,
  chunk_size = 500,
  separate_files = TRUE,
  switch_n_cov = ~ nvarA + nvarB,
  quiet = TRUE
)

## -----------------------------------------------------------------------------
summary(prep_data)

## ----weight_summaries---------------------------------------------------------
prep_data$switch_models$switch_n0

## ----weight_files-------------------------------------------------------------
list.files(working_dir, "*.rds")

# The path is stored in the saved object
switch_n0 <- readRDS(prep_data$switch_models$switch_n0$path)
summary(switch_n0)
hist(switch_n0$fitted.values, main = "Histogram of weights from model switch_n0")

## ----trial_files--------------------------------------------------------------
head(prep_data$data)

## ----sample-------------------------------------------------------------------
sampled_data <- case_control_sampling_trials(prep_data, p_control = 0.1)
str(sampled_data)

## ----modelling----------------------------------------------------------------
model_result <- trial_msm(
  data = sampled_data,
  outcome_cov = c("catvarA", "catvarB", "nvarA", "nvarB", "nvarC"),
  model_var = "assigned_treatment",
  use_weight = TRUE,
  use_censor = FALSE,
  glm_function = "glm",
  use_sample_weights = TRUE
)

## ----modelling_result---------------------------------------------------------
summary(model_result)

## ----glm_summary--------------------------------------------------------------
summary(model_result$model)

## ----predict------------------------------------------------------------------
new_data <- data.table::fread(file.path(working_dir, "trial_1.csv"))
new_data <- rbind(data.table::as.data.table(prep_data$data_template), new_data)
model_preds <- predict(model_result, predict_times = c(0:40), newdata = new_data, type = "cum_inc")

## -----------------------------------------------------------------------------
plot(
  model_preds$difference$followup_time,
  model_preds$difference$cum_inc_diff,
  ty = "l", ylab = "Cumulative Incidence Difference",
  xlab = "Follow-up Time",
  ylim = c(-0.15, 0.05)
)
lines(model_preds$difference$followup_time, model_preds$difference$`2.5%`, lty = 2)
lines(model_preds$difference$followup_time, model_preds$difference$`97.5%`, lty = 2)

## ----cleanup, echo=FALSE------------------------------------------------------
# clean up
unlink(working_dir, recursive = TRUE)

## ---- include=FALSE-----------------------------------------------------------
options(old)
data.table::setDTthreads(NULL)

