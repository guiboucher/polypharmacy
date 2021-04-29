library(testthat)
library(polypharmacy)
library(data.table)

test_that("drug_bkdn.matches", {
  rx1 <- data.frame(id = c(1L, 1L, 2L, 2L, 2L),
                    code = c(159L, 753L, 123L, 456L, 789L))
  split1 <- data.frame(code = c(159L, 159L, 456L, 456L, 456L),
                       splitcode = c(1591L, 1592L, 4567L, 4568L, 4569L))
  expect_equal(
    drug_bkdn(Rx_deliv = rx1, Rx_drug_code = "code",
              Combn_drugs = split1, Combn_drug_code = "code", Combn_act_code = "splitcode"),
    data.table(id = as.integer(c(1, 1, 1, 2, 2, 2, 2, 2)),
               code = as.integer(c(1591, 1592, 753, 123, 4567, 4568, 4569, 789))),
    ignore_attr = TRUE
  )
})

test_that("drug_bkdn.no_matches", {
  rx2 <- data.frame(id = c(1L, 1L, 2L, 2L, 2L),
                    code = c(159L, 753L, 123L, 456L, 789L))
  split2 <- data.frame(CODE = c(147L, 147L, 963L, 963L, 963L),
                       SPLITCODE = c(1471L, 1472L, 9637L, 9638L, 9639L))
  expect_equal(
    drug_bkdn(Rx_deliv = rx2, Rx_drug_code = "code",
              Combn_drugs = split2, Combn_drug_code = "CODE", Combn_act_code = "SPLITCODE"),
    data.table(id = c(1L, 1L, 2L, 2L, 2L),
               code = c(159L, 753L, 123L, 456L, 789L)),
    ignore_attr = TRUE
  )
})
