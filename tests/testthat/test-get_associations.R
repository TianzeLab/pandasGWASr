# Test cases for get_associations function

library(testthat)

# Test 1: Test get_associations with association_id
test_that("get_associations returns data for association_id", {
  skip_if_offline()

  assoc <- get_associations(association_id = "169106883", mode = "Thin")
  expect_is(assoc, "Association")
  expect_equal(length(assoc), 1)
  expect_equal(nrow(assoc@associations), 1)
})

# Test 2: Test get_associations with association_id in Fat mode
test_that("get_associations returns data for association_id in Fat mode", {
  skip_if_offline()

  assoc <- get_associations(association_id = "169106883")
  expect_is(assoc, "Association")
  expect_equal(length(assoc), 1)
  expect_equal(nrow(assoc@associations), 1)
})

# Test 3: Test get_associations with pubmed_id
test_that("get_associations returns data for pubmed_id", {
  skip_if_offline()

  assoc <- get_associations(pubmed_id = "35241825")
  expect_is(assoc, "Association")
  expect_true(length(assoc) >= 1)
})

# Test 4: Test get_associations with rs_id
test_that("get_associations returns data for rs_id", {
  skip_if_offline()

  assoc <- get_associations(rs_id = "rs3093017")
  expect_is(assoc, "Association")
  expect_true(length(assoc) >= 1)
})

# Test 5: Test get_associations with accession_id
test_that("get_associations returns data for accession_id", {
  skip_if_offline()

  assoc <- get_associations(accession_id = "GCST000854")
  expect_is(assoc, "Association")
  expect_true(length(assoc) >= 1)
})

# Test 6: Test get_associations with efo_trait
test_that("get_associations returns data for efo_trait", {
  skip_if_offline()

  assoc <- get_associations(efo_trait = "celiac disease")
  expect_is(assoc, "Association")
  expect_true(length(assoc) >= 1)
})

# Test 7: Test get_associations with efo_id
test_that("get_associations returns data for efo_id", {
  skip_if_offline()

  assoc <- get_associations(efo_id = "EFO_0001060")
  expect_is(assoc, "Association")
  expect_true(length(assoc) >= 1)
})

# Test 8: Test get_associations with mapped_gene
test_that("get_associations returns data for mapped_gene", {
  skip_if_offline()

  assoc <- get_associations(mapped_gene = "CCR6,HBS1L")
  expect_is(assoc, "Association")
  expect_true(length(assoc) >= 1)
})

# Test 9: Test get_associations with full_pvalue_set
test_that("get_associations handles full_pvalue_set parameter", {
  skip_if_offline()

  assoc <- get_associations(accession_id = "GCST000854", full_pvalue_set = TRUE)
  expect_is(assoc, "Association")
  expect_true(length(assoc) >= 0)
})

# Test 10: Test get_associations with show_child_trait
test_that("get_associations handles show_child_trait parameter", {
  skip_if_offline()

  assoc <- get_associations(efo_id = "EFO_0001060", show_child_trait = TRUE)
  expect_is(assoc, "Association")
  expect_true(length(assoc) >= 1)
})

# Test 11: Test get_associations with extended_geneset
test_that("get_associations handles extended_geneset parameter", {
  skip_if_offline()

  assoc <- get_associations(mapped_gene = "CCR6,HBS1L", extended_geneset = TRUE)
  expect_is(assoc, "Association")
  expect_true(length(assoc) >= 1)
})

# Test 12: Test get_associations indexing by numeric
test_that("get_associations supports numeric indexing", {
  skip_if_offline()

  assoc_a <- get_associations(association_id = "169106883")
  assoc_b <- get_associations(accession_id = "GCST000854")

  expect_equal(assoc_a[0], assoc_a)
})

# Test 13: Test get_associations indexing by character
test_that("get_associations supports character indexing", {
  skip_if_offline()

  assoc_a <- get_associations(association_id = "169106883")
  indexed_assoc <- assoc_a["169106883"]
  expect_equal(indexed_assoc, assoc_a)
})

# Test 14: Test get_associations range indexing
test_that("get_associations supports range indexing", {
  skip_if_offline()

  assoc_b <- get_associations(accession_id = "GCST000854")
  expect_equal(length(assoc_b[0:2]), 2)
})

# Test 15: Test get_associations list indexing
test_that("get_associations supports list indexing", {
  skip_if_offline()

  assoc_b <- get_associations(accession_id = "GCST000854")
  expect_equal(length(assoc_b[seq(0, 1)]), 2)
})

# Test 16: Test get_associations addition
test_that("get_associations supports addition operation", {
  skip_if_offline()

  assoc_a <- get_associations(association_id = "169106883")
  assoc_b <- get_associations(accession_id = "GCST000854")

  combined <- assoc_a + assoc_b[0]
  expect_true(length(combined) >= 1)
})

# Test 17: Test get_associations subtraction
test_that("get_associations supports subtraction operation", {
  skip_if_offline()

  assoc_b <- get_associations(accession_id = "GCST000854")
  result <- assoc_b - assoc_b[0]
  expect_equal(length(result), length(assoc_b) - 1)
})

# Test 18: Test get_associations intersection
test_that("get_associations supports intersection operation", {
  skip_if_offline()

  assoc_b <- get_associations(accession_id = "GCST000854")
  result <- assoc_b & assoc_b[0]
  expect_equal(length(result), 1)
})

# Test 19: Test get_associations union
test_that("get_associations supports union operation", {
  skip_if_offline()

  assoc_b <- get_associations(accession_id = "GCST000854")
  result <- assoc_b | assoc_b[0]
  expect_equal(length(result), length(assoc_b))
})

# Test 20: Test get_associations XOR
test_that("get_associations supports XOR operation", {
  skip_if_offline()

  assoc_b <- get_associations(accession_id = "GCST000854")
  result <- assoc_b ^ assoc_b[0]
  expect_equal(length(result), length(assoc_b) - 1)
})

# Test 21: Test get_associations equality
test_that("get_associations supports equality comparison", {
  skip_if_offline()

  assoc_c <- get_associations(association_id = "169106883")
  assoc_a <- get_associations(association_id = "169106883")

  expect_true(assoc_c == assoc_a)
})

# Test 22: Test get_associations with cached parameter FALSE
test_that("get_associations handles cached parameter", {
  skip_if_offline()

  assoc_cached <- get_associations(association_id = "169106883", cached = TRUE)
  assoc_not_cached <- get_associations(association_id = "169106883", cached = FALSE)

  expect_is(assoc_cached, "Association")
  expect_is(assoc_not_cached, "Association")
  expect_equal(length(assoc_cached), length(assoc_not_cached))
})

# Test 23: Test get_associations with mode parameter Thin
test_that("get_associations handles Thin mode", {
  skip_if_offline()

  assoc <- get_associations(association_id = "169106883", mode = "Thin")
  expect_is(assoc, "Association")
  expect_equal(assoc@mode, "Thin")
})

# Test 24: Test get_associations with mode parameter Fat
test_that("get_associations handles Fat mode", {
  skip_if_offline()

  assoc <- get_associations(association_id = "169106883", mode = "Fat")
  expect_is(assoc, "Association")
  expect_equal(assoc@mode, "Fat")
})
