# Test cases for get_association function

library(testthat)

# Test 1: Test get_association function with a valid URL
test_that("get_association returns data for valid URL", {
  # This is a test URL, it might not return actual data
  # In a real test, you would use a mock API or a test server
  url <- "https://www.ebi.ac.uk/gwas/rest/api/v2/associations/169106883"
  
  # Test with caching enabled
  result_with_cache <- get_association(url, cached = TRUE)
  expect_is(result_with_cache, "list")
  
  # Test with caching disabled
  result_without_cache <- get_association(url, cached = FALSE)
  expect_is(result_without_cache, "list")
})

# Test 2: Test get_association function with a non-existent URL
test_that("get_association returns empty list for 404 URL", {
  # This URL should return 404
  url <- "https://www.ebi.ac.uk/gwas/api/v2/associations/non_existent_id"
  
  result <- get_association(url)
  expect_is(result, "list")
  expect_length(result, 0)
})

