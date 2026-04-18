# Test cases for Association class

library(testthat)

# Test 1: Test Association constructor with NULL data
test_that("Association constructor with NULL data", {
  assoc <- Association(NULL, "Fat")
  expect_s4_class(assoc, "Association")
  expect_equal(length(assoc), 0)
  expect_equal(nrow(assoc@associations), 0)
})

# Test 2: Test Association constructor with Thin mode
test_that("Association constructor with Thin mode", {
  data <- list(
    list(association_id = "12345", pvalue_mantissa = 1.5, pvalue_exponent = -8)
  )
  assoc <- Association(data, "Thin")
  expect_s4_class(assoc, "Association")
  expect_equal(assoc@mode, "Thin")
  expect_equal(length(assoc), 1)
})

# Test 3: Test Association constructor with Fat mode
test_that("Association constructor with Fat mode", {
  data <- list(
    list(
      association_id = "12345",
      pvalue_mantissa = 1.5,
      pvalue_exponent = -8,
      efo_traits = list(list(efo_id = "EFO_0001", efo_trait = "Trait1"))
    )
  )
  assoc <- Association(data, "Fat")
  expect_s4_class(assoc, "Association")
  expect_equal(assoc@mode, "Fat")
  expect_equal(nrow(assoc@associations), 1)
  expect_equal(nrow(assoc@efo_traits), 1)
})

# Test 4: Test invalid mode
test_that("Association constructor with invalid mode throws error", {
  expect_error(Association(NULL, "Invalid"))
})

# Test 5: Test length method
test_that("length method returns correct count", {
  data <- list(
    list(association_id = "12345"),
    list(association_id = "67890")
  )
  assoc <- Association(data, "Fat")
  expect_equal(length(assoc), 2)
})

# Test 6: Test indexing by numeric
test_that("indexing by numeric returns correct subset", {
  data <- list(
    list(association_id = "12345"),
    list(association_id = "67890"),
    list(association_id = "11111")
  )
  assoc <- Association(data, "Fat")
  sub_assoc <- assoc[1]
  expect_equal(length(sub_assoc), 1)
  expect_equal(sub_assoc@raw_data[[1]]$association_id, "12345")
})

# Test 7: Test indexing by character
test_that("indexing by character returns correct subset", {
  data <- list(
    list(association_id = "12345"),
    list(association_id = "67890")
  )
  assoc <- Association(data, "Fat")
  sub_assoc <- assoc["12345"]
  expect_equal(length(sub_assoc), 1)
  expect_equal(sub_assoc@raw_data[[1]]$association_id, "12345")
})

# Test 8: Test addition operation
test_that("addition combines two associations", {
  data1 <- list(list(association_id = "12345"))
  data2 <- list(list(association_id = "67890"))
  assoc1 <- Association(data1, "Fat")
  assoc2 <- Association(data2, "Fat")
  combined <- assoc1 + assoc2
  expect_equal(length(combined), 2)
})

# Test 9: Test subtraction operation
test_that("subtraction removes matching associations", {
  data1 <- list(list(association_id = "12345"), list(association_id = "67890"))
  data2 <- list(list(association_id = "12345"))
  assoc1 <- Association(data1, "Fat")
  assoc2 <- Association(data2, "Fat")
  result <- assoc1 - assoc2
  expect_equal(length(result), 1)
  expect_equal(result@raw_data[[1]]$association_id, "67890")
})

# Test 10: Test intersection operation
test_that("intersection returns common associations", {
  data1 <- list(list(association_id = "12345"), list(association_id = "67890"))
  data2 <- list(list(association_id = "67890"), list(association_id = "11111"))
  assoc1 <- Association(data1, "Fat")
  assoc2 <- Association(data2, "Fat")
  result <- assoc1 & assoc2
  expect_equal(length(result), 1)
  expect_equal(result@raw_data[[1]]$association_id, "67890")
})

# Test 11: Test union operation
test_that("union combines all unique associations", {
  data1 <- list(list(association_id = "12345"))
  data2 <- list(list(association_id = "67890"))
  assoc1 <- Association(data1, "Fat")
  assoc2 <- Association(data2, "Fat")
  result <- assoc1 | assoc2
  expect_equal(length(result), 2)
})

# Test 12: Test XOR operation
test_that("XOR returns symmetric difference", {
  data1 <- list(list(association_id = "12345"), list(association_id = "67890"))
  data2 <- list(list(association_id = "67890"), list(association_id = "11111"))
  assoc1 <- Association(data1, "Fat")
  assoc2 <- Association(data2, "Fat")
  result <- assoc1 ^ assoc2
  expect_equal(length(result), 2)
})

# Test 13: Test equality operation
test_that("equality returns true for identical associations", {
  data <- list(list(association_id = "12345"))
  assoc1 <- Association(data, "Fat")
  assoc2 <- Association(data, "Fat")
  expect_true(assoc1 == assoc2)
})

# Test 14: Test equality with different data
test_that("equality returns false for different associations", {
  data1 <- list(list(association_id = "12345"))
  data2 <- list(list(association_id = "67890"))
  assoc1 <- Association(data1, "Fat")
  assoc2 <- Association(data2, "Fat")
  expect_false(assoc1 == assoc2)
})

# Test 15: Test mode mismatch error for addition
test_that("addition throws error for different modes", {
  data1 <- list(list(association_id = "12345"))
  data2 <- list(list(association_id = "67890"))
  assoc1 <- Association(data1, "Fat")
  assoc2 <- Association(data2, "Thin")
  expect_error(assoc1 + assoc2)
})

# Test 16: Test mode mismatch error for subtraction
test_that("subtraction throws error for different modes", {
  data1 <- list(list(association_id = "12345"))
  data2 <- list(list(association_id = "67890"))
  assoc1 <- Association(data1, "Fat")
  assoc2 <- Association(data2, "Thin")
  expect_error(assoc1 - assoc2)
})

# Test 17: Test mode mismatch error for intersection
test_that("intersection throws error for different modes", {
  data1 <- list(list(association_id = "12345"))
  data2 <- list(list(association_id = "67890"))
  assoc1 <- Association(data1, "Fat")
  assoc2 <- Association(data2, "Thin")
  expect_error(assoc1 & assoc2)
})

# Test 18: Test mode mismatch error for union
test_that("union throws error for different modes", {
  data1 <- list(list(association_id = "12345"))
  data2 <- list(list(association_id = "67890"))
  assoc1 <- Association(data1, "Fat")
  assoc2 <- Association(data2, "Thin")
  expect_error(assoc1 | assoc2)
})

# Test 19: Test mode mismatch error for XOR
test_that("XOR throws error for different modes", {
  data1 <- list(list(association_id = "12345"))
  data2 <- list(list(association_id = "67890"))
  assoc1 <- Association(data1, "Fat")
  assoc2 <- Association(data2, "Thin")
  expect_error(assoc1 ^ assoc2)
})

# Test 20: Test show method
test_that("show method displays correct information", {
  data <- list(
    list(association_id = "12345", pvalue_mantissa = 1.5, pvalue_exponent = -8)
  )
  assoc <- Association(data, "Fat")
  expect_output(show(assoc), "fat mode")
})

# Test 21: Test Thin mode show method
test_that("show method displays Thin mode information", {
  data <- list(
    list(association_id = "12345", pvalue_mantissa = 1.5, pvalue_exponent = -8)
  )
  assoc <- Association(data, "Thin")
  expect_output(show(assoc), "thin mode")
})

# Test 22: Test summary method
test_that("summary method displays correct information", {
  data <- list(
    list(association_id = "12345", pvalue_mantissa = 1.5, pvalue_exponent = -8)
  )
  assoc <- Association(data, "Fat")
  expect_output(summary(assoc), "Association Object Summary")
})

# Test 23: Test dim method
test_that("dim method returns correct dimensions", {
  data <- list(
    list(association_id = "12345"),
    list(association_id = "67890")
  )
  assoc <- Association(data, "Fat")
  dims <- dim(assoc)
  expect_equal(dims[1], 2)
  expect_equal(dims[2], 2)
})

# Test 24: Test names method
test_that("names method returns slot names", {
  data <- list(list(association_id = "12345"))
  assoc <- Association(data, "Fat")
  expect_equal(names(assoc), c("raw_data", "mode", "associations", "efo_traits", "snp_allele",
                               "bg_efo_traits", "reported_traits", "locations", "mapped_genes",
                               "snp_effect_alleles"))
})

# Test 25: Test empty efo_traits handling
test_that("empty efo_traits is handled correctly", {
  data <- list(
    list(association_id = "12345", efo_traits = NULL)
  )
  assoc <- Association(data, "Fat")
  expect_equal(nrow(assoc@efo_traits), 0)
})

# Test 26: Test multiple efo_traits handling
test_that("multiple efo_traits are handled correctly", {
  data <- list(
    list(
      association_id = "12345",
      efo_traits = list(
        list(efo_id = "EFO_0001", efo_trait = "Trait1"),
        list(efo_id = "EFO_0002", efo_trait = "Trait2")
      )
    )
  )
  assoc <- Association(data, "Fat")
  expect_equal(nrow(assoc@efo_traits), 2)
})

# Test 27: Test indexing with range
test_that("indexing with range returns correct subset", {
  data <- list(
    list(association_id = "12345"),
    list(association_id = "67890"),
    list(association_id = "11111"),
    list(association_id = "22222")
  )
  assoc <- Association(data, "Fat")
  sub_assoc <- assoc[1:2]
  expect_equal(length(sub_assoc), 2)
})

# Test 28: Test indexing with list
test_that("indexing with list returns correct subset", {
  data <- list(
    list(association_id = "12345"),
    list(association_id = "67890"),
    list(association_id = "11111")
  )
  assoc <- Association(data, "Fat")
  sub_assoc <- assoc[c(1, 3)]
  expect_equal(length(sub_assoc), 2)
})

# Test 29: Test indexing by character vector
test_that("indexing by character vector returns correct subset", {
  data <- list(
    list(association_id = "12345"),
    list(association_id = "67890"),
    list(association_id = "11111")
  )
  assoc <- Association(data, "Fat")
  sub_assoc <- assoc[c("12345", "11111")]
  expect_equal(length(sub_assoc), 2)
})

# Test 30: Test subtraction with no matching associations
test_that("subtraction with no matching returns original", {
  data1 <- list(list(association_id = "12345"))
  data2 <- list(list(association_id = "99999"))
  assoc1 <- Association(data1, "Fat")
  assoc2 <- Association(data2, "Fat")
  result <- assoc1 - assoc2
  expect_equal(length(result), 1)
  expect_equal(result@raw_data[[1]]$association_id, "12345")
})
