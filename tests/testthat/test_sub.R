
test_that("regex_sub works in basic mode",
 {
  expect_equal( regex_sub(c("ABC", "AXYZ", "DEFA"),"A", "_"),
      c("_BC", "_XYZ", "DEF_") )
  }
)

test_that("regex_sub works with vector replacement argument",
  {
    expect_equal( regex_sub(c("ABC", "AXYZ", "DEFA"),"A", c(1,2,3)),
      c("1BC", "2XYZ", "DEF3") )
  }
)

test_that("Regex single sub works",
  {
    expect_equal( regex_sub(c("AABC", "AXYA", "A"), "A", "_", sub_all = FALSE),
                  c("_ABC", "_XYA", "_"))
  }
)
