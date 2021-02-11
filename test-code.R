library(polypharmacy)
DT <- readRDS("E:/Github/INESSS-QC_inesss1/data-fake/prescriptions.rds")

t1 <- Sys.time()
dt_process <- data_process(
  Rx_deliv = DT, Rx_id = "ID", Rx_drug_code = "Code", Rx_drug_deliv = "Date", Rx_deliv_dur = "Duree",
  Cohort = NULL, Cohort_id = NULL,
  Hosp_stays = NULL, Hosp_id = NULL, Hosp_admis = NULL, Hosp_discharge = NULL,
  study_start = "2020-01-01", study_end = "2020-01-31",
  grace_fctr = 0.5, grace_cst = 0, max_reserve = NULL,
  cores = parallel::detectCores(logical = FALSE)
)
t2 <- Sys.time(); difftime(t2, t1)

dt_simult <- ind_simult(
  processed_tab = dt_process,
  individual_stats = c("mean", "min", "median", "max"),
  stats = c("mean", "sd", "min", "p5", "p10", "q1", "median", "q3", "p90", "p95", "max"),
  calendar = TRUE,
  cores = parallel::detectCores(logical = FALSE)
)
