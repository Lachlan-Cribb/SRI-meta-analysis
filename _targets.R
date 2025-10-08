library(targets)
library(tarchetypes)
library(crew)
library(dotenv)
library(qs2)
library(tidyr)

# Load environment variables from the .env file
dotenv::load_dot_env()
data_dir <- Sys.getenv("DATA_DIR")
cache_dir <- Sys.getenv("CACHE_DIR")
ncpus <- future::availableCores() - 1

# set system environment variables

Sys.setenv(R_DATATABLE_NUM_THREADS = 1)
Sys.setenv(OMP_NUM_THREADS = 1)
Sys.setenv(MKL_NUM_THREADS = 1)
Sys.setenv(OPENBLAS_NUM_THREADS = 1)

# set target configs
tar_config_set(store = cache_dir)
unlink("./logs/*", recursive = FALSE)

controller <- crew_controller_local(
  options_local = crew_options_local(log_directory = "./logs"),
  workers = ncpus
)

tar_option_set(
  packages = c(
    "data.table",
    "survival",
    "forcats",
    "lubridate",
    "lme4",
    "mice",
    "ranger",
    "rms",
    "survival",
    "broom",
    "mitools"
  ),
  format = "qs",
  controller = controller,
  workspace_on_error = TRUE,
  seed = 5678
)

tar_source()

# set paths to files
core_file <- file.path(data_dir, Sys.getenv("CORE_FILE"))
demdeath_file <- file.path(data_dir, Sys.getenv("DEMDEATH_FILE"))
snp_file <- file.path(data_dir, Sys.getenv("SNP_FILE"))
diet_file <- file.path(data_dir, Sys.getenv("DIET_FILE"))
accel_file <- file.path(data_dir, Sys.getenv("ACCEL_FILE"))
is_file <- file.path(data_dir, Sys.getenv("IS_FILE"))
sleep_file <- file.path(data_dir, Sys.getenv("SLEEP_FILE"))
disease_file <- file.path(data_dir, Sys.getenv("DISEASE_FILE"))
sri_file <- file.path(data_dir, Sys.getenv("SRI_FILE"))
sleepreg_file <- file.path(data_dir, Sys.getenv("SLEEPREG_FILE"))

### ANALYSIS PIPELINE

list(
  ## CREATE DATASEt
  tar_target(
    merged_data,
    create_dataset(
      core_file,
      demdeath_file,
      snp_file,
      diet_file,
      accel_file,
      is_file,
      sri_file,
      sleep_file,
      disease_file
    )
  ),
  tar_target(selection_process, merged_data$selection_process),
  tar_target(df, add_sleepreg_sri(merged_data$df, sleepreg_file)),

  ## AGE + SEX STRATIFIED DATASETs
  tar_target(
    strata,
    c("all", "<65 males", "<65 females", "65 to 75 males", "65 to 75 females")
  ),
  tar_target(df_stratified, stratify_data(df, strata), pattern = map(strata)),

  ## IMPUTATION
  tar_target(imp, impute_data(df, 1, 10)),
  tar_target(
    mult_imp,
    impute_data(df_stratified, 10, 10),
    pattern = map(df_stratified)
  ),

  ## ASSUMPTION CHECKS
  tar_map(
    values = expand_grid(
      exposure = c("sri", "rri"),
      model_formula = rlang::syms(c("model1", "model2"))
    ),
    tar_target(assumption_check, check_model(imp, exposure, model_formula))
  ),

  ## ANALYSIS PARAMETERS
  tar_target(model_formula, c("model1", "model2")),
  tar_target(exposure, c("sri", "rri")),

  ## MAIN ANALYSIS
  tar_target(
    pooled_estimates,
    fit_coxph(mult_imp, exposure, model_formula),
    pattern = cross(mult_imp, exposure, model_formula)
  )
)
