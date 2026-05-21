# Association class implementation using S4

#' Association class
#'
#' This class represents association data from the GWAS API.
#'
#' @export
#'
setClass(
  "Association",
  slots = c(
    raw_data = "list",
    mode = "character",
    associations = "data.frame",
    efo_traits = "data.frame",
    snp_allele = "data.frame",
    bg_efo_traits = "data.frame",
    reported_traits = "data.frame",
    locations = "data.frame",
    mapped_genes = "data.frame",
    snp_effect_alleles = "data.frame"
  )
)

#' Association constructor
#'
#' This function creates an Association object.
#'
#' @param data A list of association data
#' @param mode The mode of the association ("Fat" or "Thin")
#'
#' @return An Association object
#'
#' @export
#'
Association <- function(data = NULL, mode = "Fat") {
  if (!mode %in% c("Fat", "Thin")) {
    stop("Mode must be 'Fat' or 'Thin'")
  }

  if (is.null(data)) {
    data <- list()
  }

  normalized_data <- list()
  for (item in data) {
    if (is.list(item)) {
      rec <- item
      if (!is.null(rec$association_id)) {
        rec$association_id <- as.character(rec$association_id)
      }
      normalized_data <- c(normalized_data, list(rec))
    }
  }

  ASSOCIATION_COLUMNS <- c(
    'association_id',
    'risk_frequency',
    'pvalue_description',
    'pvalue_mantissa',
    'pvalue_exponent',
    'multi_snp_haplotype',
    'snp_interaction',
    'snp_type',
    'range',
    'description',
    'or_per_copy_num',
    'or_value',
    'beta_num',
    'beta_unit',
    'beta_direction',
    'beta',
    'last_mapping_date',
    'last_update_date',
    'p_value',
    'pubmed_id',
    'first_author',
    'ci_lower',
    'ci_upper'
  )

  EFO_TRAITS_COLUMNS <- c('association_id', 'efo_id', 'efo_trait')
  SNP_ALLELE_COLUMNS <- c('association_id', 'rs_id', 'effect_allele')
  BG_EFO_TRAITS_COLUMNS <- c('association_id', 'efo_id', 'efo_trait')
  REPORTED_TRAITS_COLUMNS <- c('association_id', 'reported_trait')
  LOCATIONS_COLUMNS <- c('association_id', 'location')
  MAPPED_GENES_COLUMNS <- c('association_id', 'mapped_gene')
  SNP_EFFECT_ALLELES_COLUMNS <- c('association_id', 'snp_effect_allele')

  NESTED_FIELDS <- c(
    'efo_traits',
    'snp_alleles',
    'bg_efo_traits',
    'reported_traits',
    'locations',
    'mapped_genes',
    'snp_effect_alleles'
  )

  obj <- new("Association")
  obj@raw_data <- normalized_data
  obj@mode <- mode

  obj@associations <- data.frame(matrix(ncol = length(ASSOCIATION_COLUMNS), nrow = 0))
  colnames(obj@associations) <- ASSOCIATION_COLUMNS

  obj@efo_traits <- data.frame(matrix(ncol = length(EFO_TRAITS_COLUMNS), nrow = 0))
  colnames(obj@efo_traits) <- EFO_TRAITS_COLUMNS

  obj@snp_allele <- data.frame(matrix(ncol = length(SNP_ALLELE_COLUMNS), nrow = 0))
  colnames(obj@snp_allele) <- SNP_ALLELE_COLUMNS

  obj@bg_efo_traits <- data.frame(matrix(ncol = length(BG_EFO_TRAITS_COLUMNS), nrow = 0))
  colnames(obj@bg_efo_traits) <- BG_EFO_TRAITS_COLUMNS

  obj@reported_traits <- data.frame(matrix(ncol = length(REPORTED_TRAITS_COLUMNS), nrow = 0))
  colnames(obj@reported_traits) <- REPORTED_TRAITS_COLUMNS

  obj@locations <- data.frame(matrix(ncol = length(LOCATIONS_COLUMNS), nrow = 0))
  colnames(obj@locations) <- LOCATIONS_COLUMNS

  obj@mapped_genes <- data.frame(matrix(ncol = length(MAPPED_GENES_COLUMNS), nrow = 0))
  colnames(obj@mapped_genes) <- MAPPED_GENES_COLUMNS

  obj@snp_effect_alleles <- data.frame(matrix(ncol = length(SNP_EFFECT_ALLELES_COLUMNS), nrow = 0))
  colnames(obj@snp_effect_alleles) <- SNP_EFFECT_ALLELES_COLUMNS

  if (mode == "Fat") {
    standardized_data <- list()
    for (item in normalized_data) {
      if (is.list(item)) {
        rec <- item

        standardized <- list()
        for (col in ASSOCIATION_COLUMNS) {
          value <- rec[[col]]
          if (col == 'range') {
            if (is.null(value)) {
              value <- NA
            } else if (is.list(value)) {
              if (length(value) == 0) {
                value <- NA
              } else if (length(value) == 1) {
                value <- if (is.null(value[[1]])) NA else as.character(value[[1]])
              } else {
                value <- paste(sapply(value, function(v) if (is.null(v)) '' else as.character(v)), collapse = '; ')
              }
            } else {
              value <- as.character(value)
            }
          }
          standardized[[col]] <- value
        }

        for (field in NESTED_FIELDS) {
          standardized[[field]] <- rec[[field]]
        }

        standardized_data <- c(standardized_data, list(standardized))
      }
    }

    obj@raw_data <- standardized_data

    if (length(standardized_data) == 0) {
      return(obj)
    }

    association_rows <- list()
    efo_traits_rows <- list()
    snp_allele_rows <- list()
    bg_efo_traits_rows <- list()
    reported_traits_rows <- list()
    locations_rows <- list()
    mapped_genes_rows <- list()
    snp_effect_alleles_rows <- list()

    for (record in standardized_data) {
      association_id <- record$association_id

      association_row <- list()
      for (col in ASSOCIATION_COLUMNS) {
        association_row[[col]] <- record[[col]]
      }
      association_rows <- c(association_rows, list(association_row))

      raw_efo_traits <- record$efo_traits
      if (is.null(raw_efo_traits)) {
        iterable_efo_traits <- list()
      } else if (is.list(raw_efo_traits)) {
        iterable_efo_traits <- raw_efo_traits
      } else {
        iterable_efo_traits <- list(raw_efo_traits)
      }
      for (item in iterable_efo_traits) {
        if (is.list(item)) {
          efo_traits_rows <- c(efo_traits_rows, list(list(
            association_id = association_id,
            efo_id = item$efo_id,
            efo_trait = item$efo_trait
          )))
        } else {
          efo_traits_rows <- c(efo_traits_rows, list(list(
            association_id = association_id,
            efo_id = NA,
            efo_trait = item
          )))
        }
      }

      raw_snp_allele <- record$snp_allele
      if (is.null(raw_snp_allele)) {
        iterable_snp_allele <- list()
      } else if (is.list(raw_snp_allele)) {
        iterable_snp_allele <- raw_snp_allele
      } else {
        iterable_snp_allele <- list(raw_snp_allele)
      }
      for (item in iterable_snp_allele) {
        if (is.list(item)) {
          snp_allele_rows <- c(snp_allele_rows, list(list(
            association_id = association_id,
            rs_id = item$rs_id,
            effect_allele = item$effect_allele
          )))
        } else {
          snp_allele_rows <- c(snp_allele_rows, list(list(
            association_id = association_id,
            rs_id = item,
            effect_allele = NA
          )))
        }
      }

      raw_bg_efo_traits <- record$bg_efo_traits
      if (is.null(raw_bg_efo_traits)) {
        iterable_bg_efo_traits <- list()
      } else if (is.list(raw_bg_efo_traits)) {
        iterable_bg_efo_traits <- raw_bg_efo_traits
      } else {
        iterable_bg_efo_traits <- list(raw_bg_efo_traits)
      }
      for (item in iterable_bg_efo_traits) {
        if (is.list(item)) {
          bg_efo_traits_rows <- c(bg_efo_traits_rows, list(list(
            association_id = association_id,
            efo_id = item$efo_id,
            efo_trait = item$efo_trait
          )))
        } else {
          bg_efo_traits_rows <- c(bg_efo_traits_rows, list(list(
            association_id = association_id,
            efo_id = NA,
            efo_trait = item
          )))
        }
      }

      raw_reported_trait <- record$reported_trait
      if (is.null(raw_reported_trait)) {
        iterable_reported_trait <- list()
      } else if (is.list(raw_reported_trait)) {
        iterable_reported_trait <- raw_reported_trait
      } else {
        iterable_reported_trait <- list(raw_reported_trait)
      }
      for (item in iterable_reported_trait) {
        reported_traits_rows <- c(reported_traits_rows, list(list(
          association_id = association_id,
          reported_trait = item
        )))
      }

      raw_locations <- record$locations
      if (is.null(raw_locations)) {
        iterable_locations <- list()
      } else if (is.list(raw_locations)) {
        iterable_locations <- raw_locations
      } else {
        iterable_locations <- list(raw_locations)
      }
      for (item in iterable_locations) {
        locations_rows <- c(locations_rows, list(list(
          association_id = association_id,
          location = item
        )))
      }

      raw_mapped_genes <- record$mapped_genes
      if (is.null(raw_mapped_genes)) {
        iterable_mapped_genes <- list()
      } else if (is.list(raw_mapped_genes)) {
        iterable_mapped_genes <- raw_mapped_genes
      } else {
        iterable_mapped_genes <- list(raw_mapped_genes)
      }
      for (item in iterable_mapped_genes) {
        mapped_genes_rows <- c(mapped_genes_rows, list(list(
          association_id = association_id,
          mapped_gene = item
        )))
      }

      raw_snp_effect_allele <- record$snp_effect_allele
      if (is.null(raw_snp_effect_allele)) {
        iterable_snp_effect_allele <- list()
      } else if (is.list(raw_snp_effect_allele)) {
        iterable_snp_effect_allele <- raw_snp_effect_allele
      } else {
        iterable_snp_effect_allele <- list(raw_snp_effect_allele)
      }
      for (item in iterable_snp_effect_allele) {
        snp_effect_alleles_rows <- c(snp_effect_alleles_rows, list(list(
          association_id = association_id,
          snp_effect_allele = item
        )))
      }
    }

    if (length(association_rows) > 0) {
      obj@associations <- do.call(rbind, lapply(association_rows, data.frame, stringsAsFactors = FALSE))
      for (col in ASSOCIATION_COLUMNS) {
        if (!col %in% colnames(obj@associations)) {
          obj@associations[[col]] <- NA
        }
      }
      obj@associations <- obj@associations[, ASSOCIATION_COLUMNS, drop = FALSE]
    }

    if (length(efo_traits_rows) > 0) {
      obj@efo_traits <- do.call(rbind, lapply(efo_traits_rows, data.frame, stringsAsFactors = FALSE))
      for (col in EFO_TRAITS_COLUMNS) {
        if (!col %in% colnames(obj@efo_traits)) {
          obj@efo_traits[[col]] <- NA
        }
      }
      obj@efo_traits <- obj@efo_traits[, EFO_TRAITS_COLUMNS, drop = FALSE]
    }

    if (length(snp_allele_rows) > 0) {
      obj@snp_allele <- do.call(rbind, lapply(snp_allele_rows, data.frame, stringsAsFactors = FALSE))
      for (col in SNP_ALLELE_COLUMNS) {
        if (!col %in% colnames(obj@snp_allele)) {
          obj@snp_allele[[col]] <- NA
        }
      }
      obj@snp_allele <- obj@snp_allele[, SNP_ALLELE_COLUMNS, drop = FALSE]
    }

    if (length(bg_efo_traits_rows) > 0) {
      obj@bg_efo_traits <- do.call(rbind, lapply(bg_efo_traits_rows, data.frame, stringsAsFactors = FALSE))
      for (col in BG_EFO_TRAITS_COLUMNS) {
        if (!col %in% colnames(obj@bg_efo_traits)) {
          obj@bg_efo_traits[[col]] <- NA
        }
      }
      obj@bg_efo_traits <- obj@bg_efo_traits[, BG_EFO_TRAITS_COLUMNS, drop = FALSE]
    }

    if (length(reported_traits_rows) > 0) {
      obj@reported_traits <- do.call(rbind, lapply(reported_traits_rows, data.frame, stringsAsFactors = FALSE))
      for (col in REPORTED_TRAITS_COLUMNS) {
        if (!col %in% colnames(obj@reported_traits)) {
          obj@reported_traits[[col]] <- NA
        }
      }
      obj@reported_traits <- obj@reported_traits[, REPORTED_TRAITS_COLUMNS, drop = FALSE]
    }

    if (length(locations_rows) > 0) {
      obj@locations <- do.call(rbind, lapply(locations_rows, data.frame, stringsAsFactors = FALSE))
      for (col in LOCATIONS_COLUMNS) {
        if (!col %in% colnames(obj@locations)) {
          obj@locations[[col]] <- NA
        }
      }
      obj@locations <- obj@locations[, LOCATIONS_COLUMNS, drop = FALSE]
    }

    if (length(mapped_genes_rows) > 0) {
      obj@mapped_genes <- do.call(rbind, lapply(mapped_genes_rows, data.frame, stringsAsFactors = FALSE))
      for (col in MAPPED_GENES_COLUMNS) {
        if (!col %in% colnames(obj@mapped_genes)) {
          obj@mapped_genes[[col]] <- NA
        }
      }
      obj@mapped_genes <- obj@mapped_genes[, MAPPED_GENES_COLUMNS, drop = FALSE]
    }

    if (length(snp_effect_alleles_rows) > 0) {
      obj@snp_effect_alleles <- do.call(rbind, lapply(snp_effect_alleles_rows, data.frame, stringsAsFactors = FALSE))
      for (col in SNP_EFFECT_ALLELES_COLUMNS) {
        if (!col %in% colnames(obj@snp_effect_alleles)) {
          obj@snp_effect_alleles[[col]] <- NA
        }
      }
      obj@snp_effect_alleles <- obj@snp_effect_alleles[, SNP_EFFECT_ALLELES_COLUMNS, drop = FALSE]
    }
  }

  return(obj)
}

#' @export
setMethod(
  "show",
  signature = "Association",
  function(object) {
    if (object@mode == 'Fat') {
      cat(
        "Association is running in fat mode. It has 8 DataFrames with hierarchical dependencies.\n",
        "associations: ", nrow(object), " rows\n",
        "|\n -efo_traits: ", nrow(object@efo_traits), " rows\n",
        "|\n -snp_allele: ", nrow(object@snp_allele), " rows\n",
        "|\n -bg_efo_traits: ", nrow(object@bg_efo_traits), " rows\n",
        "|\n -reported_traits: ", nrow(object@reported_traits), " rows\n",
        "|\n -locations: ", nrow(object@locations), " rows\n",
        "|\n -mapped_genes: ", nrow(object@mapped_genes), " rows\n",
        "|\n -snp_effect_alleles: ", nrow(object@snp_effect_alleles), " rows\n",
        sep = ""
      )
    } else if (object@mode == 'Thin') {
      cat(
        'Association is running in thin mode. It has 1 list that contains the raw data.\n',
        'raw_data: a list of size ', length(object@raw_data), '.\n',
        sep = ""
      )
    }
  }
)

#' @export
setMethod(
  "length",
  signature = "Association",
  function(x) {
    return(length(x@raw_data))
  }
)

#' @export
setMethod(
  "dim",
  signature = "Association",
  function(x) {
    return(c(nrow(x@associations), length(x@raw_data)))
  }
)

#' @export
setMethod(
  "nrow",
  signature = "Association",
  function(x) {
    return(nrow(x@associations))
  }
)

#' @export
setMethod(
  "names",
  signature = "Association",
  function(x) {
    return(c(
      "raw_data",
      "mode",
      "associations",
      "efo_traits",
      "snp_allele",
      "bg_efo_traits",
      "reported_traits",
      "locations",
      "mapped_genes",
      "snp_effect_alleles"
    ))
  }
)

#' @export
setMethod(
  "[",
  signature = "Association",
  function(x, i) {
    if (missing(i)) {
      return(x)
    }

    if (is.character(i)) {
      arr <- i
    } else if (is.numeric(i)) {
      arr <- i
    } else if (is.list(i)) {
      arr <- unlist(i)
    } else if (class(i) == 'integer64') {
      arr <- as.numeric(i)
    } else {
      stop('Invalid argument type: ', class(i))
    }

    raw_data <- x@raw_data
    raw_data_dict <- list()
    for (j in seq_along(raw_data)) {
      if (!is.null(raw_data[[j]]$association_id)) {
        raw_data_dict[[raw_data[[j]]$association_id]] <- raw_data[[j]]
      }
    }

    sub_set <- list()
    for (i_val in arr) {
      if (is.character(i_val)) {
        if (i_val %in% names(raw_data_dict)) {
          sub_set <- c(sub_set, list(raw_data_dict[[i_val]]))
        }
      } else if (is.numeric(i_val)) {
        if (i_val >= 1 && i_val <= length(raw_data)) {
          sub_set <- c(sub_set, list(raw_data[[i_val]]))
        }
      } else {
        stop('Invalid item type: ', class(i_val))
      }
    }

    return(Association(sub_set, x@mode))
  }
)

#' @export
setMethod(
  "+",
  signature = c("Association", "Association"),
  function(e1, e2) {
    if (e1@mode == e2@mode) {
      return(Association(c(e1@raw_data, e2@raw_data), e1@mode))
    } else {
      stop("Please input the same mode")
    }
  }
)

#' @export
setMethod(
  "-",
  signature = c("Association", "Association"),
  function(e1, e2) {
    if (e1@mode == e2@mode) {
      self_key_set <- character()
      self_dict <- list()
      other_key_set <- character()

      for (i in e1@raw_data) {
        if (!is.null(i$association_id)) {
          self_key_set <- unique(c(self_key_set, i$association_id))
          self_dict[[i$association_id]] <- i
        }
      }

      for (j in e2@raw_data) {
        if (!is.null(j$association_id)) {
          other_key_set <- unique(c(other_key_set, j$association_id))
        }
      }

      sub_key <- self_key_set[!self_key_set %in% other_key_set]
      data <- list()
      for (k in sub_key) {
        data <- c(data, list(self_dict[[k]]))
      }

      return(Association(data, e1@mode))
    } else {
      stop("Please input the same mode")
    }
  }
)

#' @export
setMethod(
  "&",
  signature = c("Association", "Association"),
  function(e1, e2) {
    if (e1@mode == e2@mode) {
      self_key_set <- character()
      self_dict <- list()
      other_key_set <- character()

      for (i in e1@raw_data) {
        if (!is.null(i$association_id)) {
          self_key_set <- unique(c(self_key_set, i$association_id))
          self_dict[[i$association_id]] <- i
        }
      }

      for (j in e2@raw_data) {
        if (!is.null(j$association_id)) {
          other_key_set <- unique(c(other_key_set, j$association_id))
        }
      }

      sub_key <- self_key_set[self_key_set %in% other_key_set]
      data <- list()
      for (k in sub_key) {
        data <- c(data, list(self_dict[[k]]))
      }

      return(Association(data, e1@mode))
    } else {
      stop("Please input the same mode")
    }
  }
)

#' @export
setMethod(
  "|",
  signature = c("Association", "Association"),
  function(e1, e2) {
    if (e1@mode == e2@mode) {
      and_dict <- list()

      for (i in e1@raw_data) {
        if (!is.null(i$association_id)) {
          and_dict[[i$association_id]] <- i
        }
      }

      for (j in e2@raw_data) {
        if (!is.null(j$association_id)) {
          and_dict[[j$association_id]] <- j
        }
      }

      data <- unname(and_dict)
      return(Association(data, e1@mode))
    } else {
      stop("Please input the same mode")
    }
  }
)

#' @export
setMethod(
  "^",
  signature = c("Association", "Association"),
  function(e1, e2) {
    if (e1@mode == e2@mode) {
      self_key_set <- character()
      and_dict <- list()
      other_key_set <- character()

      for (i in e1@raw_data) {
        if (!is.null(i$association_id)) {
          self_key_set <- unique(c(self_key_set, i$association_id))
          and_dict[[i$association_id]] <- i
        }
      }

      for (j in e2@raw_data) {
        if (!is.null(j$association_id)) {
          other_key_set <- unique(c(other_key_set, j$association_id))
          and_dict[[j$association_id]] <- j
        }
      }

      all_keys <- unique(c(self_key_set, other_key_set))
      common_keys <- self_key_set[self_key_set %in% other_key_set]
      sub_key <- all_keys[!all_keys %in% common_keys]
      data <- list()
      for (k in sub_key) {
        data <- c(data, list(and_dict[[k]]))
      }

      return(Association(data, e1@mode))
    } else {
      stop("Please input the same mode")
    }
  }
)

#' @export
setMethod(
  "==",
  signature = c("Association", "Association"),
  function(e1, e2) {
    if (is.null(e1) || is.null(e2)) {
      return(is.null(e1) && is.null(e2))
    }
    return(identical(e1@raw_data, e2@raw_data) && e1@mode == e2@mode)
  }
)

#' @export
setMethod(
  "summary",
  signature = "Association",
  function(object) {
    cat("Association Object Summary\n")
    cat("==========================\n")
    cat("Mode:", object@mode, "\n")
    cat("Number of raw records:", length(object@raw_data), "\n")
    cat("Number of associations:", nrow(object), "\n")
    if (object@mode == "Fat") {
      cat("\nDataFrames:\n")
      cat("  associations:", nrow(object@associations), "rows\n")
      cat("  efo_traits:", nrow(object@efo_traits), "rows\n")
      cat("  snp_allele:", nrow(object@snp_allele), "rows\n")
      cat("  bg_efo_traits:", nrow(object@bg_efo_traits), "rows\n")
      cat("  reported_traits:", nrow(object@reported_traits), "rows\n")
      cat("  locations:", nrow(object@locations), "rows\n")
      cat("  mapped_genes:", nrow(object@mapped_genes), "rows\n")
      cat("  snp_effect_alleles:", nrow(object@snp_effect_alleles), "rows\n")
    }
  }
)
