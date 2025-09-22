library(targets)
library(tarchetypes)
library(crew)
library(dotenv)
library(qs2)

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
  options_metrics = crew_options_metrics(
    path = "/dev/stdout",
    seconds_interval = 10
  ),
  workers = ncpus
)

tar_option_set(
  packages = c(
    "data.table",
    "survival",
    "forcats",
    "lubridate",
    "lme4"
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
sleep_file <- file.path(data_dir, Sys.getenv("SLEEP_FILE"))
disease_file <- file.path(data_dir, Sys.getenv("DISEASE_FILE"))
sri_file <- file.path(data_dir, Sys.getenv("SRI_FILE"))
sleepreg_file <- file.path(data_dir, Sys.getenv("SLEEPREG_FILE"))

# analysis pipeline
# Have not added interdaily stability (IS) - need to find in GGIR output
list(
  tar_target(
    merged_data,
    create_dataset(
      core_file,
      demdeath_file,
      snp_file,
      diet_file,
      accel_file,
      sri_file,
      sleep_file,
      disease_file
    )
  ),
  tar_target(selection_process, merged_data$selection_process),
  tar_target(df, add_sleepreg_sri(merged_data$df, sleepreg_file)),
  tar_target(
    unstratified_analysis_params,
    CJ(
      exposures = c("sri", "rri", "is"),
      models = c("model1", "model2", "model3")
    )
  )
)
