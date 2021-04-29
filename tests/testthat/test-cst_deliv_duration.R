library(testthat)
library(polypharmacy)
library(data.table)

test_that("cst_deliv_duration.std", {
  Rx <- data.frame(id = c(1, 1, 2, 2, 2),
                   code = c("A", "B", "B", "C", "D"),
                   duration = as.integer(c(30, 15, 15, 7, 90)))
  Cst <- data.frame(CODES = c("B", "D"),
                    DURATION = as.integer(c(45, 60)))
  expect_equal(
    cst_deliv_duration(Rx_deliv = Rx, Rx_drug_code = "code", Rx_deliv_dur = "duration",
                       Cst_deliv_dur = Cst, Cst_drug_code = "CODES", Cst_duration = "DURATION"),
    data.table(id = c(1, 1, 2, 2, 2),
               code = c("A", "B", "B", "C", "D"),
               duration = as.integer(c(30, 45, 45, 7, 60)))
  )
  expect_error(cst_deliv_duration(Rx_deliv = "NotADataFrame",
                                  Rx_drug_code = "code", Rx_deliv_dur = "duration",
                                  Cst_deliv_dur = Cst,
                                  Cst_drug_code = "CODES", Cst_duration = "DURATION"))
  expect_error(cst_deliv_duration(Rx_deliv = Rx,
                                  Rx_drug_code = "WrongName", Rx_deliv_dur = "duration",
                                  Cst_deliv_dur = Cst,
                                  Cst_drug_code = "CODES", Cst_duration = "DURATION"))
  expect_error(cst_deliv_duration(Rx_deliv = Rx,
                                  Rx_drug_code = "code", Rx_deliv_dur = "WrongName",
                                  Cst_deliv_dur = Cst,
                                  Cst_drug_code = "CODES", Cst_duration = "DURATION"))
  expect_error(cst_deliv_duration(Rx_deliv = Rx,
                                  Rx_drug_code = "code", Rx_deliv_dur = "duration",
                                  Cst_deliv_dur = "NotADataFrame",
                                  Cst_drug_code = "CODES", Cst_duration = "DURATION"))
  expect_error(cst_deliv_duration(Rx_deliv = Rx,
                                  Rx_drug_code = "code", Rx_deliv_dur = "duration",
                                  Cst_deliv_dur = Cst,
                                  Cst_drug_code = "WrongName", Cst_duration = "DURATION"))
  expect_error(cst_deliv_duration(Rx_deliv = Rx,
                                  Rx_drug_code = "code", Rx_deliv_dur = "duration",
                                  Cst_deliv_dur = Cst,
                                  Cst_drug_code = "CODES", Cst_duration = "WrongName"))
})

test_that("cst_deliv_duration.no_match", {
  Rx <- data.frame(id = c(1, 1, 2, 2, 2),
                   code = c("A", "B", "B", "C", "D"),
                   duration = as.integer(c(30, 15, 15, 7, 90)))
  Cst <- data.frame(CODES = c("E", "F"),
                    DURATION = as.integer(c(45, 60)))
  Obj <- cst_deliv_duration(Rx_deliv = Rx, Rx_drug_code = "code", Rx_deliv_dur = "duration",
                            Cst_deliv_dur = Cst, Cst_drug_code = "CODES", Cst_duration = "DURATION")
  Exp <- data.table(id = c(1, 1, 2, 2, 2),
                    code = c("A", "B", "B", "C", "D"),
                    duration = as.integer(c(30, 15, 15, 7, 90)))
  expect_equal(Obj, Exp)
})
