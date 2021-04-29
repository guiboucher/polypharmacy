### Just to be sure that R CMD CHECK has no NOTE messages
### -> problem with data.table package

globalVariables(sort(unique(c(

  ".",
  "by_hosp", "by_hospit",
  "diff_grace", "drug_code", "drug_duration", "duration_ajust",
  "grace_per",
  "hosp", "hosp_date",
  "i_D_s_", "id", "ids", "is_present",
  "ndays", "nRx",
  "P1", "P1_date", "P2", "P2_date", "per", "per1",
  "quantile",
  "ratios", "rx_date",
  "tx_end", "tx_end_ajust", "tx_end_grace", "tx_start"

))))
