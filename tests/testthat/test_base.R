library("thlcol")

context("Base tests")

test_that("color numbers match",{
  expect_equal(thlcolbynum(1),"#7CD0D8FF")
})

test_that("color schemes match",{
  expect_equal(thlpal("grey1"),
               c("#25A5A2FF","#B2B2B2FF","#8C8C8CFF","#666666FF","#3F3F3FFF","#191919FF"))
})
