



# 缓存变量（在zzz.R中定义）

ask_yes_no_question <- function(question) {
  yes_no_answer <- ""
  while (toupper(yes_no_answer) != "YES" &&
         toupper(yes_no_answer) != "NO") {
    yes_no_answer <- readline(question)
  }
  return(toupper(yes_no_answer))
}


#' Get data from GWAS API
#'
#' This function retrieves data from the GWAS API, handling caching and pagination.
#'
#' @param url The URL to request data from
#' @param cache_impl A cachem cache object used for caching responses
#' @param cached Logical value indicating whether to use cached responses
#' @param type The type of data to retrieve (e.g., "associations", "studies")
#'
#' @return A list of data objects retrieved from the API
#'
#' @importFrom httr2 request req_headers req_perform resp_status
#' @importFrom httr2 resp_body_string req_cookie_preserve
#' @importFrom jsonlite fromJSON
#' @importFrom progress progress_bar
#' @importFrom cachem cache_mem
#'
#' @noRd
#'
get_data <- function(url,
                     cache_impl = NULL,
                     cached = TRUE,
                     type = NULL) {
  cookie_path <- tempfile()
  # 检查缓存
  if (cache_impl$exists(clean_string(url)) && cached) {
    r <- cache_impl$get(clean_string(url))
  } else {
    # 发送HTTP请求
    r <- httr2::request(url) |> req_cookie_preserve(cookie_path) |>
      httr2::req_headers(Accept = "application/json") |>
      httr2::req_perform() # nolint
  }
  # 处理响应
  if (httr2::resp_status(r) == 200) {
    content <- httr2::resp_body_string(r)
    if (nchar(content) == 0) {
      return(list())
    }
    cache_impl$set(clean_string(url), r)
    # 解析JSON
    parsed_data <- jsonlite::fromJSON(content, simplifyVector = FALSE)
    # 处理嵌入式数据
    if (!is.null(parsed_data[["_embedded"]])) {
      results_list <- parsed_data[["_embedded"]][[type]]
      # 处理分页
      if (!is.null(parsed_data[["_links"]])) {
        next_link <- parsed_data[["_links"]][["next"]]
        if (!is.null(next_link)) {
          # 显示进度条
          total_elements <- parsed_data[["page"]][["totalElements"]]
          progress <- length(results_list)
          pb <- progress::progress_bar$new(
            format = "  downloading [:bar] :current/:total eta: :eta",
            total = total_elements,
            clear = FALSE,
            width = 60
          )
          pb$update(progress / total_elements)

          # 循环获取下一页
          while (!is.null(next_link)) {
            next_url <- next_link[["href"]]

            # 检查缓存
            if (cache_impl$exists(clean_string(next_url)) && cached) {
              r <- cache_impl$get(clean_string(next_url))
            } else {
              # 发送HTTP请求
              r <- httr2::request(next_url) |> req_cookie_preserve(cookie_path) |>
                httr2::req_headers(Accept = "application/json") |>
                httr2::req_perform()
            }
            if (httr2::resp_status(r) == 200) {
              # 解析下一页数据
              content_next <- httr2::resp_body_string(r)
              parsed_data_next <- jsonlite::fromJSON(content_next, simplifyVector = FALSE)
              # 扩展结果列表
              page_results <- parsed_data_next[["_embedded"]][[type]]
              results_list <- c(results_list, page_results)
              # 缓存下一页结果
              cache_impl$set(clean_string(next_url), r)
            } else if (httr2::resp_status(r) == 404) {
              return(list())
            } else {
              stop(
                paste(
                  "The request for",
                  next_url,
                  "failed: response code was",
                  httr2::resp_status(r),
                  "content was\n",
                  httr2::resp_body_string(r)
                )
              )
            }
            # 更新进度条
            progress <- length(results_list)
            pb$update(progress / total_elements)

            # 获取下一页链接
            next_link <- parsed_data_next[["_links"]][["next"]]
          }
        }
      }
      for (result in results_list) {
        result[["_links"]] <- NULL
      }
      return(results_list)
    }
    else {
      parsed_data[["_links"]] <- NULL
      return(list(parsed_data))
    }

  } else if (httr2::resp_status(r) == 404) {
    return(list())
  } else {
    stop(
      paste(
        "The request for",
        url,
        "failed: response code was",
        httr2::resp_status(r),
        "content was\n",
        httr2::resp_body_string(r)
      )
    )
  }
}
#' Get association data
#'
#' This function retrieves association data from the GWAS API.
#'
#' @param url The URL to request data from
#' @param cached Logical value indicating whether to use cached responses
#'
#' @return A list of association objects retrieved from the API
#'
#' @export
#'
get_association <- function(url, cached = TRUE) {
  return(
    get_data(
      url,
      cache_impl = .pandasGWASr$association_cache,
      cached = cached,
      type = "associations"
    )
  )
}

#' Get study data
#'
#' This function retrieves study data from the GWAS API.
#'
#' @param url The URL to request data from
#' @param cached Logical value indicating whether to use cached responses
#'
#' @return A list of study objects retrieved from the API
#'
#' @export
#'
get_study <- function(url, cached = TRUE) {
  return(
    get_data(
      url,
      cache_impl = .pandasGWASr$study_cache,
      cached = cached,
      type = "studies"
    )
  )
}

#' Get trait data
#'
#' This function retrieves trait data from the GWAS API.
#'
#' @param url The URL to request data from
#' @param cached Logical value indicating whether to use cached responses
#'
#' @return A list of trait objects retrieved from the API
#'
#' @export
#'
get_trait <- function(url, cached = TRUE) {
  return(
    get_data(
      url,
      cache_impl = .pandasGWASr$trait_cache,
      cached = cached,
      type = "efo_traits"
    )
  )
}

#' Get SNP data
#'
#' This function retrieves SNP data from the GWAS API.
#'
#' @param url The URL to request data from
#' @param cached Logical value indicating whether to use cached responses
#'
#' @return A list of SNP objects retrieved from the API
#'
#' @export
#'
get_snp <- function(url, cached = TRUE) {
  return(
    get_data(
      url,
      cache_impl = .pandasGWASr$snp_cache,
      cached = cached,
      type = "snps"
    )
  )
}

#' Get ancestry data
#'
#' This function retrieves ancestry data from the GWAS API.
#'
#' @param url The URL to request data from
#' @param cached Logical value indicating whether to use cached responses
#'
#' @return A list of ancestry objects retrieved from the API
#'
#' @export
#'
get_ancestry <- function(url, cached = TRUE) {
  return(
    get_data(
      url,
      cache_impl = .pandasGWASr$ancestry_cache,
      cached = cached,
      type = "ancestries"
    )
  )
}

#' Get publication data
#'
#' This function retrieves publication data from the GWAS API.
#'
#' @param url The URL to request data from
#' @param cached Logical value indicating whether to use cached responses
#'
#' @return A list of publication objects retrieved from the API
#'
#' @export
#'
get_publication <- function(url, cached = TRUE) {
  return(
    get_data(
      url,
      cache_impl = .pandasGWASr$publication_cache,
      cached = cached,
      type = "publications"
    )
  )
}

#' Get genomic context data
#'
#' This function retrieves genomic context data from the GWAS API.
#'
#' @param url The URL to request data from
#' @param cached Logical value indicating whether to use cached responses
#'
#' @return A list of genomic context objects retrieved from the API
#'
#' @export
#'
get_genomicContext <- function(url, cached = TRUE) {
  return(
    get_data(
      url,
      cache_impl = .pandasGWASr$genomicContext_cache,
      cached = cached,
      type = "genomic_contexts"
    )
  )
}

#' Get gene data
#'
#' This function retrieves gene data from the GWAS API.
#'
#' @param url The URL to request data from
#' @param cached Logical value indicating whether to use cached responses
#'
#' @return A list of gene objects retrieved from the API
#'
#' @export
#'
get_gene <- function(url, cached = TRUE) {
  return(
    get_data(
      url,
      cache_impl = .pandasGWASr$gene_cache,
      cached = cached,
      type = "genes"
    )
  )
}

#' Get locus data
#'
#' This function retrieves locus data from the GWAS API.
#'
#' @param url The URL to request data from
#' @param cached Logical value indicating whether to use cached responses
#'
#' @return A list of locus objects retrieved from the API
#'
#' @export
#'
get_locus <- function(url, cached = TRUE) {
  return(
    get_data(
      url,
      cache_impl = .pandasGWASr$locus_cache,
      cached = cached,
      type = "loci"
    )
  )
}

#' Reinitialize cache
#'
#' This function reinitializes the cache for the specified data type or all caches.
#'
#' @param field The field to reinitialize the cache for, or "All" for all caches
#' @param maxsize The maximum size of the cache
#' @param ttl The time to live for the cache in seconds
#'
#' @export
#'
reinit_cache <- function(field = "All", maxsize = 2 * 1024, ttl = 60 * 60 * 24) {
  fields <- c("Association", "Study", "Trait", "Snp", "Ancestry", "Publication", "GenomicContext", "Gene", "Locus")
  fields_and_all <- c("All", "Association", "Study", "Trait", "Snp", "Ancestry", "Publication", "GenomicContext", "Gene", "Locus")
  
  if (!field %in% fields_and_all) {
    stop(paste("The field must be one of", paste(fields_and_all, collapse = ", ")))
  }
  
  if (field == "Association") {
    .pandasGWASr$association_cache <- cache_mem(max_age = ttl, max_size = maxsize)
  } else if (field == "Study") {
    .pandasGWASr$study_cache <- cache_mem(max_age = ttl, max_size = maxsize)
  } else if (field == "Trait") {
    .pandasGWASr$trait_cache <- cache_mem(max_age = ttl, max_size = maxsize)
  } else if (field == "Snp") {
    .pandasGWASr$snp_cache <- cache_mem(max_age = ttl, max_size = maxsize)
  } else if (field == "Ancestry") {
    .pandasGWASr$ancestry_cache <- cache_mem(max_age = ttl, max_size = maxsize)
  } else if (field == "Publication") {
    .pandasGWASr$publication_cache <- cache_mem(max_age = ttl, max_size = maxsize)
  } else if (field == "GenomicContext") {
    .pandasGWASr$genomicContext_cache <- cache_mem(max_age = ttl, max_size = maxsize)
  } else if (field == "Gene") {
    .pandasGWASr$gene_cache <- cache_mem(max_age = ttl, max_size = maxsize)
  } else if (field == "Locus") {
    .pandasGWASr$locus_cache <- cache_mem(max_age = ttl, max_size = maxsize)
  } else { # All
    .pandasGWASr$association_cache <- cache_mem(max_age = ttl, max_size = maxsize)
    .pandasGWASr$study_cache <- cache_mem(max_age = ttl, max_size = maxsize)
    .pandasGWASr$trait_cache <- cache_mem(max_age = ttl, max_size = maxsize)
    .pandasGWASr$snp_cache <- cache_mem(max_age = ttl, max_size = maxsize)
    .pandasGWASr$ancestry_cache <- cache_mem(max_age = ttl, max_size = maxsize)
    .pandasGWASr$publication_cache <- cache_mem(max_age = ttl, max_size = maxsize)
    .pandasGWASr$genomicContext_cache <- cache_mem(max_age = ttl, max_size = maxsize)
    .pandasGWASr$gene_cache <- cache_mem(max_age = ttl, max_size = maxsize)
    .pandasGWASr$locus_cache <- cache_mem(max_age = ttl, max_size = maxsize)
  }
}

#' 清理字符串：仅保留字母数字 + 转小写
#'
#' @param x 输入字符串（字符型）
#' @return 清理后的字符串（仅字母数字，全小写）
#' @noRd
clean_string <- function(x) {
  # 1. 检查输入
  if (!is.character(x)) {
    stop("输入必须是字符型字符串")
  }
  
  # 2. 仅保留 字母 + 数字，移除所有其他符号
  x_clean <- gsub("[^a-zA-Z0-9]", "", x)
  
  # 3. 大写转小写
  x_clean <- tolower(x_clean)
  
  # 返回结果
  return(x_clean)
}