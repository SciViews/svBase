context("data frame")

describe("Creation of a data.frame", {
  it("must be a tibble tbl_df object", {
    dtbl1 <- dtbl(x = 1:3, y = 4:6)
    expect_is(dtbl1, "tbl_df")
  })

  it("must be a data.frame object", {
    dtf1 <- dtf(x = 1:3, y = 4:6)
    expect_is(dtf1, "data.frame")
  })

  it("must be a data.table object", {
    dtt1 <- dtt(x = 1:3, y = 4:6)
    expect_is(dtt1, "data.table")
  })
})
