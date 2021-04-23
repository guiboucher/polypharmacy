### Just to be sure that R CMD CHECK has no NOTE messages
### -> problem with data.table package

globalVariables(sort(unique(c(

  ### PACKAGES
  # data.table package & variables often used
  ".", ":=",
  "..cols", "..colorder",  # columns selection

  ### FUNCTIONS - after devtools::check()
  ### Mainly because of data.table use without with=FALSE argument
  "by_hosp", "by_hospit",
  "diff_grace", "drug_code", "drug_duration", "duration_ajust",
  "grace_per",
  "hosp",
  "id", "ids", "i_D_s_",
  "ndays", "nRx",
  "P1", "P1_date", "P2", "P2_date", "per", "per1",
  "quantile",
  "ratios",
  "tx_end", "tx_end_ajust", "tx_end_grace", "tx_start"

))))
