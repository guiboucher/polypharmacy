library(polypharmacy)
DT <- readRDS("E:/Github/INESSS-QC_inesss1/data-fake/prescriptions.rds")
dt_process <- data_process(
  Rx_deliv = DT, Rx_id = "ID", Rx_drug_code = "Code", Rx_drug_deliv = "Date",
  Rx_deliv_dur = "Duree",
  Cohort = NULL, Cohort_id = NULL,
  Hosp_stays = NULL, Hosp_id = NULL, Hosp_admis = NULL, Hosp_discharge = NULL,
  study_start = "2020-01-01", study_end = "2020-01-01",
  grace_fctr = 0.5, grace_cst = 0, max_reserve = NULL,
  cores = parallel::detectCores(logical = FALSE)
)


# Multi Indic -------------------------------------------------------------
indic <- indicators(
  processed_tab = dt_process,
  stats = c('mean', 'sd', 'min', 'p5', 'p10', 'p25', 'median', 'p75', 'p90', 'p95', 'max'),
  method = c('ind_simult', 'ind_stdcumul', 'ind_wcumul', 'ind_stdcontinuous', 'ind_ucontinuous'),
  stdconti_pdays = 90,
  simult_ind_stats = c('mean', 'min', 'median', 'max'),
  simult_calendar = TRUE,
  stdcumul_nPeriod = 3,
  cores = parallel::detectCores(logical = FALSE)
)
t3 <- Sys.time(); difftime(t3, t2)


# Single indic ------------------------------------------------------------
dt_simult <- ind_simult(
  processed_tab = dt_process,
  individual_stats = c("mean", "min", "median", "max"),
  stats = c("mean", "sd", "min", "p5", "p10", "q1", "median", "q3", "p90", "p95", "max"),
  calendar = TRUE,
  cores = parallel::detectCores(logical = FALSE)
)

dt_cumul <- ind_stdcumul(processed_tab = dt_process, nPeriod = 3)
dt_wcumul <- ind_wcumul(dt_process)
dt_conti <- ind_stdcontinuous(dt_process, pdays = 5)
dt_uconti <- ind_ucontinuous(dt_process)
