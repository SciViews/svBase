context("data frame")

describe("Creation of a data.frame", {
  it("must be a data.frame object", {
    dtf <- data.frame(x = 1:3, y = 4:6)
    expect_is(dtf, "data.frame")
  })
})
