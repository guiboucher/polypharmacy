library(usethis)
library(data.table)
library(readxl)


# drug_bkdn1 --------------------------------------------------------------

drug_bkdn1 <- as.data.table(read_xlsx(
  "inst/extdata/Tableaux_de_references_criteres_selection_CS 2018-10-08.xlsx")
)
drug_bkdn1 <- drug_bkdn1[
  , .(Combn_drug_code = as.integer(COD_DENOM),
      Combn_act_code = as.integer(COD_DENOM_SEP))
]
setkey(drug_bkdn1, Combn_drug_code)



# cst_deliv_duration1 -----------------------------------------------------

cst_deliv_duration1 <- data.table(
  Cst_drug_code = as.integer(c(
    10530, 33634, 34180, 43124, 46288, 47914, 47916,
    13, 10218, 47653,
    44164, 44489, 45531, 47206, 47424, 47536, 47586, 47615, 47749
  )),
  Cst_duration = as.integer(c(
    rep(30, 7),
    rep(60, 3),
    rep(90, 9)
  ))
)
setkey(cst_deliv_duration1, Cst_drug_code)




use_data(drug_bkdn1,
         cst_deliv_duration1,

         internal = TRUE,
         overwrite = TRUE)
