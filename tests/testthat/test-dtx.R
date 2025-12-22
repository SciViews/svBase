test_that("dtf(), dtt() and (dtbl() construct the correct object type", {
  dtf1 <- dtf(x = 1:3, y = 4:6)
  expect_s3_class(dtf1, "data.frame")
  dtt1 <- dtt(x = 1:3, y = 4:6)
  expect_s3_class(dtt1, "data.table")
  dtbl1 <- dtbl(x = 1:3, y = 4:6)
  expect_s3_class(dtbl1, "tbl_df")
})
