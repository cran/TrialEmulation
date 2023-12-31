#' Prepare Sequence of Trial Data
#'
#' This function takes the one row per time period per patient data and constructs
#' a dataset with the records for each period in each trial. This considerably expands
#' the size of the data. It takes into account the eligibility for each trial and
#' calculates the weight models and the weights for each time period in the expanded data.
#'
#' @inheritParams initiators
#' @param chunk_size Number of patients to process in one chunk when `separate_files = TRUE`
#' @param separate_files Save expanded data in separate CSV files for each trial.
#' @param data_dir Directory to model objects when `save_weight_models=TRUE` and
#'   expanded data as `trial_i.csv`s if `separate_files = TRUE`.
#'   If the specified directory does not exist it will be created. If the directory
#'   already contains trial files an error will occur, other files may be overwritten.
#' @export
#'
#' @details
#' The arguments `chunk_size` and `separate_files` allow for processing of large datasets that
#' would not fit in memory once expanded. When `separate_files = TRUE`, the input data are processed
#' in chunks of patients and saved into separate files for each trial starting period. These separate
#' files can be sampled to create the dataset for the modelling.
#'
#' @returns An object of class `TE_data_prep`, which can either be sampled from ([case_control_sampling_trials])
#' or directly used in a model ([trial_msm]).
#' It contains the elements
#' \describe{
#'   \item{data}{the expanded trial dataset for all trial periods. If `separate=FALSE` a `data.table`, if
#'   `separate=TRUE` a character vector with the file path of the expanded data as csv.}
#'   \item{min_period}{the first trial period in the expanded data}
#'   \item{max_period}{the last trial period in the expanded data}
#'   \item{N}{the total number of observations in the expanded data}
#'   \item{data_template}{a zero-row `data.frame` in the with the columns and attributes of the expanded data}
#'   \item{switch_models}{a list of summaries of the models fitted for probability of switching treatment,
#'   if `use_weight=TRUE`}
#'   \item{censor_models}{a list of summaries of the models fitted for probability of censoring treatment,
#'   if `use_weight=TRUE`}
#'   }
#'
data_preparation <- function(data,
                             id = "id",
                             period = "period",
                             treatment = "treatment",
                             outcome = "outcome",
                             eligible = "eligible",
                             outcome_cov = ~1,
                             model_var = NULL,
                             switch_n_cov = ~1,
                             switch_d_cov = ~1,
                             first_period = NA,
                             last_period = NA,
                             use_weight = FALSE,
                             use_censor = FALSE,
                             cense = NA,
                             pool_cense = FALSE,
                             cense_d_cov = ~1,
                             cense_n_cov = ~1,
                             eligible_wts_0 = NA,
                             eligible_wts_1 = NA,
                             where_var = NULL,
                             data_dir,
                             save_weight_models = FALSE,
                             glm_function = "glm",
                             chunk_size = 500,
                             separate_files = FALSE,
                             quiet = FALSE,
                             ...) {
  arg_checks <- makeAssertCollection()
  assert_flag(use_weight, add = arg_checks)
  assert_flag(use_censor, add = arg_checks)
  assert_flag(pool_cense, add = arg_checks)
  assert_flag(save_weight_models, add = arg_checks)
  assert_flag(separate_files, add = arg_checks)
  assert_flag(quiet, add = arg_checks)
  assert_multi_class(outcome_cov, classes = c("formula", "character"), add = arg_checks)
  assert_multi_class(model_var, classes = c("formula", "character"), null.ok = TRUE, add = arg_checks)
  assert_multi_class(switch_n_cov, classes = c("formula", "character"), add = arg_checks)
  assert_multi_class(switch_d_cov, classes = c("formula", "character"), add = arg_checks)
  assert_multi_class(cense_d_cov, classes = c("formula", "character"), add = arg_checks)
  assert_multi_class(cense_n_cov, classes = c("formula", "character"), add = arg_checks)
  assert_integerish(first_period, lower = 0, all.missing = TRUE, len = 1, add = arg_checks)
  assert_integerish(last_period, lower = 0, all.missing = TRUE, len = 1, add = arg_checks)
  reportAssertions(arg_checks)

  if (isTRUE(separate_files)) check_data_dir(data_dir)

  outcome_cov <- as_formula(outcome_cov)
  switch_n_cov <- as_formula(switch_n_cov)
  switch_d_cov <- as_formula(switch_d_cov)
  cense_d_cov <- as_formula(cense_d_cov)
  cense_n_cov <- as_formula(cense_n_cov)

  model_var <- if (!is.null(model_var)) {
    as_formula(model_var)
  } else if (isFALSE(use_censor) && isTRUE(use_weight)) {
    ~dose
  } else {
    ~assigned_treatment
  }

  data <- select_data_cols(
    data,
    id = id,
    period = period,
    treatment = treatment,
    outcome = outcome,
    eligible = eligible,
    eligible_wts_0 = eligible_wts_0,
    eligible_wts_1 = eligible_wts_1,
    formula_vars = unlist(lapply(list(outcome_cov, switch_n_cov, switch_d_cov, cense_n_cov, cense_n_cov), all.vars)),
    cense = cense,
    where_var = where_var
  )

  quiet_msg(quiet, "Starting data manipulation")
  data <- data_manipulation(data, use_censor = use_censor)

  if (isTRUE(use_weight)) {
    weight_result <- weight_func(
      sw_data = data,
      switch_n_cov = switch_n_cov,
      switch_d_cov = switch_d_cov,
      eligible_wts_0 = eligible_wts_0,
      eligible_wts_1 = eligible_wts_1,
      cense = cense,
      pool_cense = pool_cense,
      cense_d_cov = cense_d_cov,
      cense_n_cov = cense_n_cov,
      save_weight_models = save_weight_models,
      save_dir = data_dir,
      quiet = quiet,
      glm_function = glm_function,
      ...
    )
    data <- weight_result$data
  } else if (isFALSE(use_weight)) {
    set(data, j = "wt", value = 1)
  }

  keeplist <- c(
    "id", "trial_period", "followup_time", "outcome", "weight", "treatment",
    where_var, all.vars(outcome_cov), all.vars(model_var)
  )

  quiet_msg(quiet, "Starting data extension")
  result <- data_extension(
    data = data,
    keeplist = keeplist,
    outcomeCov_var = all.vars(outcome_cov),
    first_period = first_period,
    last_period = last_period,
    use_censor = use_censor,
    where_var = where_var,
    separate_files = separate_files,
    data_dir = data_dir,
    chunk_size = chunk_size
  )

  quiet_msg(quiet, "Summary of extended data:")
  quiet_msg(quiet, paste0("Number of observations: ", result$N))
  quiet_line(quiet)

  result$switch_models <- if (use_weight) weight_result$switch_models else NULL
  result$censor_models <- if (use_weight) weight_result$censor_models else NULL

  class(result) <- c(ifelse(separate_files, "TE_data_prep_sep", "TE_data_prep_dt"), "TE_data_prep")
  return(result)
}


#' Check for valid data_dir and create if necessary
#'
#' @param data_dir Directory to check
#' @noRd
check_data_dir <- function(data_dir) {
  if (test_directory_exists(data_dir)) {
    if (length(list.files(data_dir, pattern = "trial_.*csv"))) {
      stop("trial_*.csv files already exist in ", data_dir, ". Remove them or specify a different `data_dir`.")
    }
    if (length(list.files(data_dir, ".*model.*rds"))) {
      warning(data_dir, " contains model rds files. These may be overwritten.")
    }
  } else {
    if (!dir.create(data_dir)) {
      stop(data_dir, " could not be created.")
    }
  }
}
