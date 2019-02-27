context("Class by Name")

r <- httr::GET("http://rxnav.nlm.nih.gov/REST/rxclass/class/byName.json?className=radiopharmaceuticals")
df <- class_by_name(className = "radiopharmaceuticals")
list <- class_by_name(className = "radiopharmaceuticals", trunc = F)

test_that("Verify that API call is successful",{
  expect_equal(r$status_code, 200)
})

test_that("Dataframe is returned",{
  expect_is(df, "data.frame")
})

test_that("List is returned",{
  expect_is(list, "list")
})

test_that("Null is return from garbage input",{
  expect_equal(class_by_name(className = "sdhjskdfwer"), NULL)
})

test_that("Correct names for dataframe columns",{
  expect_equal(names(df), c("classId", "className", "classType"))
})

test_that("Unformatted list is equal to raw API call",{
  expect_equal(list, content(r))
})
