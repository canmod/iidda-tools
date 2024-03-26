# ==============================================
# Instructions
# ------------
#
# ==============================================

# ==============================================
# Minimal Package Requirements
# ----------------------------
library(iidda)
library(readr)
# ==============================================

# ==============================================
# List Available Data
# -------------------
# possible values for variable `source` in "User Input" section
list_sources()

# possible values for variable `dataset` in "User Input" section,
# organized by `source`
list_datasets_by_source()
# ==============================================

# ==============================================
# User Input
# ----------
# what source and dataset are you interested in?
source = "cdi_ca_1964-67_wk_prov"
dataset = "cdi_ca_1964_wk_prov_mcgill"

# user input: force csv update
# (if force_csv_update = FALSE the csv will only be updated if it does not exist)
force_csv_update = FALSE
# ==============================================

# ==============================================
# Paths to Resources
# ------------------
script = get_main_script(source, dataset)
csv = get_dataset_path(source, dataset)
# ==============================================

# ==============================================
# Create / Load Data
# ------------------
run_script = (!file.exists(csv)) | force_csv_update
if (run_script) system2("Rscript", script)
data = readr::read_csv(csv)
# ==============================================

# ==============================================
# Below this line please write whatever you want.
# ==============================================
