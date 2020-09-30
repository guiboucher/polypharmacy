### Just to be sure that R CMD CHECK has no NOTE messages
### -> problem with data.table package use

globalVariables(sort(unique(c(

  ## PACKAGES

  # data.table package & variables often used
  ".", ":=",
  "..cols", "..colorder",  # columns selection


  ## FUNCTIONS

  # data_process
  "by_hosp", "by_hospit",
  "diff_grace", "drug_code", "drug_duration", "duration", "duration_ajust",
  "grace_per",
  "hosp",
  "id",
  "per",
  "start",
  "tx_end", "tx_end_ajust", "tx_end_grace", "tx_start"

))))
