library(testthat)
library(polypharmacy)
library(data.table)

# Rx ----------------------------------------------------------------------

test_that("data_process.Rx_df", {
  # Rx character
  expect_error(data_process(
    Rx_deliv = "SALUT", Rx_id = "id", Rx_drug_code = "code",
    Rx_drug_deliv = "date", Rx_deliv_dur = "duration"
  ))
  # Rx integer
  expect_error(data_process(
    Rx_deliv = 123456789, Rx_id = "id", Rx_drug_code = "code",
    Rx_drug_deliv = "date", Rx_deliv_dur = "duration"
  ))
  # Rx vector
  expect_error(data_process(
    Rx_deliv = mtcars$mpg, Rx_id = "id", Rx_drug_code = "code",
    Rx_drug_deliv = "date", Rx_deliv_dur = "duration"
  ))
})

test_that("data_process.Rx_cols", {
  Rx <- data.frame(
    id = c(1L, 3:8),
    code = LETTERS[c(1, 3:8)],
    date = as.Date(c("2001-01-15", "2003-03-15", "2004-04-15", "2005-05-15",
                     "2006-06-15", "2007-07-15", "2008-08-15")),
    duration = 10L
  )
  expect_error(data_process(
    Rx_deliv = Rx, Rx_id = "IDs", Rx_drug_code = "code",
    Rx_drug_deliv = "date", Rx_deliv_dur = "duration"
  ))
  expect_error(data_process(
    Rx_deliv = Rx, Rx_id = "id", Rx_drug_code = "CODA",
    Rx_drug_deliv = "date", Rx_deliv_dur = "duration"
  ))
  expect_error(data_process(
    Rx_deliv = Rx, Rx_id = "id", Rx_drug_code = "code",
    Rx_drug_deliv = "dates", Rx_deliv_dur = "duration"
  ))
  expect_error(data_process(
    Rx_deliv = Rx, Rx_id = "id", Rx_drug_code = "code",
    Rx_drug_deliv = "date", Rx_deliv_dur = "durations"
  ))
})

# Hosp --------------------------------------------------------------------

test_that("data_process.hosp_stays", {
  Rx <- data.frame(
    id = c(1L, 3:8),
    code = LETTERS[c(1, 3:8)],
    date = as.Date(c("2001-01-15", "2003-03-15", "2004-04-15", "2005-05-15",
                   "2006-06-15", "2007-07-15", "2008-08-15")),
    duration = 10L
  )
  Hosp <- data.frame(
    ID = 3:8,
    ADM = as.Date(c("2003-03-10", "2004-04-25", "2005-05-12",
                    "2006-06-20", "2007-07-26", "2008-08-01")),
    DEP = as.Date(c("2003-03-14", "2004-04-30", "2005-05-17",
                    "2006-06-30", "2007-07-30", "2008-08-13"))
  )
  Obj <- data_process(
    Rx_deliv = Rx, Rx_id = "id", Rx_drug_code = "code",
    Rx_drug_deliv = "date", Rx_deliv_dur = "duration",
    Cohort = NULL, Cohort_id = NULL,
    Hosp_stays = Hosp, Hosp_id = "ID", Hosp_admis = "ADM", Hosp_discharge = "DEP",
    study_start = "2001-01-01", study_end = "2008-12-31",
    grace_fctr = 0.5, grace_cst = 0, max_reserve = NULL,
    cores = 1L
  )
  Exp <- data.table(
    id = c(1L, 3:8),
    code = LETTERS[c(1, 3:8)],
    tx_start = as.Date(c("2001-01-15", "2003-03-10", "2004-04-15", "2005-05-12",
                         "2006-06-15", "2007-07-15", "2008-08-15")),
    tx_end = as.Date(c("2001-01-24", "2003-03-24", "2004-04-30", "2005-05-27",
                       "2006-07-05", "2007-07-24", "2008-08-24"))
  )
  expect_equal(Obj, Exp, ignore_attr = TRUE)
})

test_that("data_process.hosp_stays_many", {
  Rx <- data.frame(
    id = 1L,
    code = c(111L, 222L, 222L, 333L, 444L),
    date = as.Date(c("2001-01-15", "2002-02-15", "2002-03-01", "2004-04-07", "2004-05-05")),
    duration = as.integer(c(10, 10, 10, 30, 10))
  )
  Hosp <- data.frame(
    id = 1L,
    adm = as.Date(c("2000-01-01", "2000-01-15", "2001-01-01", "2002-02-23", "2004-04-15")),
    dep = as.Date(c("2000-01-31", "2000-01-31", "2001-01-10", "2002-02-28", "2004-05-15"))
  )
  Obj <- data_process(
    Rx_deliv = Rx, Rx_id = "id", Rx_drug_code = "code",
    Rx_drug_deliv = "date", Rx_deliv_dur = "duration",
    Cohort = NULL, Cohort_id = NULL,
    Hosp_stays = Hosp, Hosp_id = "id", Hosp_admis = "adm", Hosp_discharge = "dep",
    study_start = "2001-01-01", study_end = "2008-12-31",
    grace_fctr = 0.5, grace_cst = 0, max_reserve = NULL,
    cores = 1L
  )
  Exp <- data.table(
    id = 1L,
    code = c(111L, 222L, 333L, 444L),
    tx_start = as.Date(c("2001-01-15", "2002-02-15", "2004-04-07", "2004-04-15")),
    tx_end = as.Date(c("2001-01-24", "2002-03-12", "2004-06-06", "2004-05-25"))
  )
  expect_equal(Obj, Exp, ignore_attr = TRUE)
})

# study_start -------------------------------------------------------------

test_that("data_process.study_dates", {
  Rx <- data.frame(id = 1:3,
                   code = "A",
                   date = as.Date(c("2020-01-01", "2020-06-06", "2020-12-22")),
                   duration = 10L)

  expect_equal(  # start + end = NULL
    data_process(Rx_deliv = Rx, Rx_id = "id", Rx_drug_code = "code",
                 Rx_drug_deliv = "date", Rx_deliv_dur = "duration",
                 study_start = NULL, study_end = NULL,
                 cores = 1),
    data.table(id = 1:3,
               code = "A",
               tx_start = as.Date(c("2020-01-01", "2020-06-06", "2020-12-22")),
               tx_end = as.Date(c("2020-01-10", "2020-06-15", "2020-12-31"))),
    ignore_attr = TRUE
  )
  expect_equal(  # start not null, end null
    data_process(Rx_deliv = Rx, Rx_id = "id", Rx_drug_code = "code",
                 Rx_drug_deliv = "date", Rx_deliv_dur = "duration",
                 study_start = "2020-06-10", study_end = NULL,
                 cores = 1),
    data.table(id = 2:3, code = "A",
               tx_start = as.Date(c("2020-06-10", "2020-12-22")),
               tx_end = as.Date(c("2020-06-15", "2020-12-31"))),
    ignore_attr = TRUE
  )
  expect_equal(  # start null, end not null
    data_process(Rx_deliv = Rx, Rx_id = "id", Rx_drug_code = "code",
                 Rx_drug_deliv = "date", Rx_deliv_dur = "duration",
                 study_start = NULL, study_end = "2020-06-10",
                 cores = 1),
    data.table(id = 1:2, code = "A",
               tx_start = as.Date(c("2020-01-01", "2020-06-06")),
               tx_end = as.Date(c("2020-01-10", "2020-06-10"))),
    ignore_attr = TRUE
  )
  expect_equal(  # no nulls
    data_process(Rx_deliv = Rx, Rx_id = "id", Rx_drug_code = "code",
                 Rx_drug_deliv = "date", Rx_deliv_dur = "duration",
                 study_start = "2020-01-05", study_end = "2020-12-25",
                 cores = 1),
    data.table(id = 1:3, code = "A",
               tx_start = as.Date(c("2020-01-05", "2020-06-06", "2020-12-22")),
               tx_end = as.Date(c("2020-01-10", "2020-06-15", "2020-12-25"))),
    ignore_attr = TRUE
  )
})

# grace_fctr --------------------------------------------------------------

test_that("data_process.grace_factr", {
  Rx <- data.frame(id = c(rep(1, 3), rep(2, 3)),
                   code = "A",
                   date = as.Date(c("2000-01-01", "2000-01-17", "2000-01-31",
                                    "2000-06-01", "2000-06-23", "2000-07-16")),
                   duration = as.integer(c(10, 10, 10, 15, 15, 15)))

  expect_equal(  # duration even and odd
    data_process(Rx_deliv = Rx, Rx_id = "id", Rx_drug_code = "code",
                 Rx_drug_deliv = "date", Rx_deliv_dur = "duration",
                 grace_fctr = 0.5,
                 cores = 1),
    data.table(id = as.integer(c(1, 1, 2, 2)),
               code = "A",
               tx_start = as.Date(c("2000-01-01", "2000-01-17",
                                    "2000-06-01", "2000-07-16")),
               tx_end = as.Date(c("2000-01-10", "2000-02-09",
                                  "2000-07-07", "2000-07-30"))),
    ignore_attr = TRUE
  )
  expect_equal(  # fctr = 0
    data_process(Rx_deliv = Rx, Rx_id = "id", Rx_drug_code = "code",
                 Rx_drug_deliv = "date", Rx_deliv_dur = "duration",
                 grace_fctr = 0,
                 cores = 1),
    data.table(id = as.integer(c(1, 1, 1, 2, 2, 2)),
               code = "A",
               tx_start = as.Date(c("2000-01-01", "2000-01-17", "2000-01-31",
                                    "2000-06-01", "2000-06-23", "2000-07-16")),
               tx_end = as.Date(c("2000-01-10", "2000-01-26", "2000-02-09",
                                  "2000-06-15", "2000-07-07", "2000-07-30"))),
    ignore_attr = TRUE
  )
  expect_error(  # grace_fctr < 0
    data_process(Rx_deliv = Rx, Rx_id = "id", Rx_drug_code = "code",
                 Rx_drug_deliv = "date", Rx_deliv_dur = "duration",
                 grace_fctr = -1)
  )
})

# grace_cst ---------------------------------------------------------------

test_that("data_process.grace_cst", {
  Rx <- data.frame(id = 1,
                   code = "A",
                   date = as.Date(c("2000-01-01", "2000-01-14", "2000-01-25")),
                   duration = as.integer(c(10, 10, 6)))

  expect_equal(  # grace_fct = 2
    data_process(Rx_deliv = Rx, Rx_id = "id", Rx_drug_code = "code",
                 Rx_drug_deliv = "date", Rx_deliv_dur = "duration",
                 grace_fctr = 0, grace_cst = 2,
                 cores = 1),
    data.table(id = 1, code = "A",
               tx_start = as.Date(c("2000-01-01", "2000-01-14")),
               tx_end = as.Date(c("2000-01-10", "2000-01-30"))),
    ignore_attr = TRUE
  )
  expect_equal(  # grace_fct = 3
    data_process(Rx_deliv = Rx, Rx_id = "id", Rx_drug_code = "code",
                 Rx_drug_deliv = "date", Rx_deliv_dur = "duration",
                 grace_fctr = 0, grace_cst = 3,
                 cores = 1),
    data.table(id = 1, code = "A",
               tx_start = as.Date(c("2000-01-01")),
               tx_end = as.Date(c("2000-01-30"))),
    ignore_attr = TRUE
  )
  expect_error(data_process(Rx_deliv = Rx, Rx_id = "id", Rx_drug_code = "code",
                            Rx_drug_deliv = "date", Rx_deliv_dur = "duration",
                            grace_fctr = 0, grace_cst = -1))

})

# grace_fctr + grace_cst --------------------------------------------------

test_that("data_process.grace_fctr_cst", {
  Rx <- data.frame(id = 1L, code = "A",
                   date = as.Date(c("2000-01-01", "2000-01-19", "2000-01-26")),
                   duration = 10L)

  expect_equal(
    data_process(Rx_deliv = Rx, Rx_id = "id", Rx_drug_code = "code",
                 Rx_drug_deliv = "date", Rx_deliv_dur = "duration",
                 grace_fctr = 0.5, grace_cst = 2,
                 cores = 1),
    data.table(id = 1, code = "A",
               tx_start = as.Date(c("2000-01-01", "2000-01-19")),
               tx_end = as.Date(c("2000-01-10", "2000-02-04"))),
    ignore_attr = TRUE
  )
  expect_equal(
    data_process(Rx_deliv = Rx, Rx_id = "id", Rx_drug_code = "code",
                 Rx_drug_deliv = "date", Rx_deliv_dur = "duration",
                 grace_fctr = 0.5, grace_cst = 3,
                 cores = 1),
    data.table(id = 1, code = "A",
               tx_start = as.Date(c("2000-01-01")),
               tx_end = as.Date(c("2000-02-04"))),
    ignore_attr = TRUE
  )
})

# max_reserve -------------------------------------------------------------

test_that("data_process.max_reserve", {
  Rx <- data.frame(id = as.integer(c(1, 1, 3, 3, 3, 5, 5)),
                   code = "A",
                   date = as.Date(c("2000-01-01", "2000-01-31",
                                    "2000-03-03", "2000-03-15", "2000-03-30",
                                    "2000-05-05", "2000-05-05")),
                   duration = as.integer(c(30, 30,
                                           30, 30, 30,
                                           90, 90)))

  expect_error(  # < 0
    data_process(Rx_deliv = Rx, Rx_id = "id", Rx_drug_code = "code",
                 Rx_drug_deliv = "date", Rx_deliv_dur = "duration",
                 grace_fctr = 0, grace_cst = 0,
                 max_reserve = -1,
                 cores = 1)
  )
  expect_equal(  # = 0
    data_process(Rx_deliv = Rx, Rx_id = "id", Rx_drug_code = "code",
                 Rx_drug_deliv = "date", Rx_deliv_dur = "duration",
                 study_start = NULL, study_end = "2000-12-31",
                 grace_fctr = 0, grace_cst = 0,
                 max_reserve = 0,
                 cores = 1),
    data.table(id = c(1L, 3L, 5L),
               code = "A",
               tx_start = as.Date(c("2000-01-01", "2000-03-03", "2000-05-05")),
               tx_end = as.Date(c("2000-02-29", "2000-04-28", "2000-08-02"))),
    ignore_attr = TRUE
  )
  expect_equal(  # = 60
    data_process(Rx_deliv = Rx, Rx_id = "id", Rx_drug_code = "code",
                 Rx_drug_deliv = "date", Rx_deliv_dur = "duration",
                 study_start = NULL, study_end = "2000-12-31",
                 grace_fctr = 0, grace_cst = 0,
                 max_reserve = 60,
                 cores = 1),
    data.table(id = c(1L, 3L, 5L),
               code = "A",
               tx_start = as.Date(c("2000-01-01", "2000-03-03", "2000-05-05")),
               tx_end = as.Date(c("2000-02-29", "2000-05-31", "2000-10-01"))),
    ignore_attr = TRUE
  )
  expect_equal(  # NULL = Inf = no limit
    data_process(Rx_deliv = Rx, Rx_id = "id", Rx_drug_code = "code",
                 Rx_drug_deliv = "date", Rx_deliv_dur = "duration",
                 study_start = NULL, study_end = "2000-12-31",
                 grace_fctr = 0, grace_cst = 0,
                 max_reserve = NULL,
                 cores = 1),
    data.table(id = c(1L, 3L, 5L),
               code = "A",
               tx_start = as.Date(c("2000-01-01", "2000-03-03", "2000-05-05")),
               tx_end = as.Date(c("2000-02-29", "2000-05-31", "2000-10-31"))),
    ignore_attr = TRUE
  )
})

# cores -------------------------------------------------------------------


