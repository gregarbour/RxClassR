context("Approximate term")

r <- httr::GET("https://rxnav.nlm.nih.gov/REST/approximateTerm.json?term=zocor%202010%2020mm&maxEntries=20&option=0")
df <- rx_approximate_term(term = "zocor 2010 20mm")
list <- rx_approximate_term(term = "zocor 2010 20mm", trunc = F)

test_that("Verify that API call is successful",{
  expect_equal(r$status_code, 200)
})

test_that("Dataframe is returned",{
  expect_is(df, "data.frame")
  })

test_that("List is returned",{
  expect_is(list, "list")
})

test_that("Error is return from garbage input",{
  expect_equal(rx_approximate_term(term = "sdhjskdfwer"), "No drugs identified for given search term")
})

test_that("Correct names for dataframe columns",{
  expect_equal(names(df), c("rxcui", "score", "rank"))
})

test_that("Unformatted list is equal to raw API call",{
  expect_equal(list, content(r))
})



