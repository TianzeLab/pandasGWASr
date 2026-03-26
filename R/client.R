



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
      return(results_list)
    }
    else {
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