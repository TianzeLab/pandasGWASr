#' Get associations from GWAS API
#'
#' This function retrieves association data from the GWAS API based on various search criteria.
#'
#' @param association_id The association ID to search for
#' @param pubmed_id The PubMed ID to search for
#' @param rs_id The SNP RS ID to search for
#' @param full_pvalue_set Whether to include full p-value set
#' @param accession_id The study accession ID to search for
#' @param efo_trait The EFO trait to search for
#' @param efo_id The EFO ID to search for
#' @param show_child_trait Whether to show child traits
#' @param mapped_gene The mapped gene to search for
#' @param extended_geneset Whether to use extended geneset
#' @param cached Logical value indicating whether to use cached responses
#' @param mode The mode of the Association object ("Fat" or "Thin")
#'
#' @return An Association object containing the retrieved data
#'
#' @export
#'
get_associations <- function(
    association_id = NULL,
    pubmed_id = NULL,
    rs_id = NULL,
    full_pvalue_set = NULL,
    accession_id = NULL,
    efo_trait = NULL,
    efo_id = NULL,
    show_child_trait = NULL,
    mapped_gene = NULL,
    extended_geneset = NULL,
    cached = TRUE,
    mode = "Fat"
) {
  BASE_URL <- "https://www.ebi.ac.uk/gwas/rest/api/v2/associations"

  params <- list()

  if (!is.null(pubmed_id)) {
    params$pubmed_id <- pubmed_id
  }
  if (!is.null(rs_id)) {
    params$rs_id <- rs_id
  }
  if (!is.null(full_pvalue_set)) {
    params$full_pvalue_set <- full_pvalue_set
  }
  if (!is.null(accession_id)) {
    params$accession_id <- accession_id
  }
  if (!is.null(efo_trait)) {
    params$efo_trait <- efo_trait
  }
  if (!is.null(efo_id)) {
    params$efo_id <- efo_id
  }
  if (!is.null(show_child_trait)) {
    params$show_child_trait <- show_child_trait
  }
  if (!is.null(mapped_gene)) {
    params$mapped_gene <- mapped_gene
  }
  if (!is.null(extended_geneset)) {
    params$extended_geneset <- extended_geneset
  }

  has_id <- !is.null(association_id)
  has_query_filters <- length(params) > 0

  result_by_id <- NULL
  result_by_query <- NULL

  if (has_id) {
    id_url <- paste0(BASE_URL, "/", URLencode(association_id, reserved = TRUE))
    result_by_id <- Association(
      data = get_association(id_url, cached = cached),
      mode = mode
    )
  }

  if (!has_id || has_query_filters) {
    if (has_query_filters) {
      encoded_params <- lapply(params, function(value) {
        URLencode(as.character(value), reserved = TRUE)
      })
      query_string <- paste(names(encoded_params), encoded_params, sep = "=", collapse = "&")
      query_url <- paste0(BASE_URL, "?", query_string)
    } else {
      query_url <- BASE_URL
    }

    result_by_query <- Association(
      data = get_association(query_url, cached = cached),
      mode = mode
    )
  }

  if (has_id && has_query_filters) {
    return(result_by_id & result_by_query)
  }
  if (has_id) {
    return(result_by_id)
  }
  return(result_by_query)
}
