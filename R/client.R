
# 缓存变量
association_cache <- list()
study_cache <- list()
trait_cache <- list()
snp_cache <- list()
ancestry_cache <- list()
publication_cache <- list()
genomicContext_cache <- list()
gene_cache <- list()
locus_cache <- list()

ask_yes_no_question <- function(question) {
  yes_no_answer <- ""
  while (toupper(yes_no_answer) != "YES" && toupper(yes_no_answer) != "NO") {
    yes_no_answer <- readline(question)
  }
  return(toupper(yes_no_answer))
}

#' Get data from GWAS API
#'
#' This function retrieves data from the GWAS API, handling caching and pagination.
#'
#' @param url The URL to request data from
#' @param cache_impl A named list used for caching responses
#' @param cached Logical value indicating whether to use cached responses
#' @param type The type of data to retrieve (e.g., "associations", "studies")
#'
#' @return A list of data objects retrieved from the API
#'
#' @importFrom httr GET status_code content add_headers config
#' @importFrom jsonlite fromJSON
#' @importFrom utils txtProgressBar setTxtProgressBar
#'
#' @keywords internal
#'
get_data <- function(url, cache_impl = NULL, cached = TRUE, type = NULL) {

  # 检查缓存
  if (!is.null(cache_impl) && cached && url %in% names(cache_impl)) {
    r <- cache_impl[[url]]
  } else {
    # 发送HTTP请求，设置最大重试次数为5
    r <- httr::GET(url, httr::config(connecttimeout = 30), httr::add_headers(Accept = "application/json"))
  }
  
  # 处理响应
  if (httr::status_code(r) == 200) {
    content <- httr::content(r, as = "text")
    if (nchar(content) == 0) {
      return(list())
    }
    
    # 缓存响应
    if (!is.null(cache_impl)) {
      cache_impl[[url]] <- r
    }
    
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
          utils::txtProgressBar(min = 0, max = total_elements, style = 3)
          utils::setTxtProgressBar(utils::txtProgressBar(min = 0, max = total_elements, style = 3), progress)
          
          # 循环获取下一页
          while (!is.null(next_link)) {
            next_url <- next_link[["href"]]
            
            # 检查缓存
            if (!is.null(cache_impl) && cached && next_url %in% names(cache_impl)) {
              r_next <- cache_impl[[next_url]]
            } else {
              r_next <- httr::GET(next_url, httr::config(connecttimeout = 30), httr::add_headers(Accept = "application/json"))
              
              if (httr::status_code(r_next) == 200) {
                if (!is.null(cache_impl)) {
                  cache_impl[[next_url]] <- r_next
                }
              } else if (httr::status_code(r_next) == 404) {
                return(list())
              } else {
                stop(paste("The request for", next_url, "failed: response code was", httr::status_code(r_next), 
                          "content was\n", httr::content(r_next, as = "text")))
              }
            }
            
            # 解析下一页数据
            content_next <- httr::content(r_next, as = "text")
            parsed_data_next <- jsonlite::fromJSON(content_next, simplifyVector = FALSE)
            
            # 扩展结果列表
            results_list <- c(results_list, parsed_data_next[["_embedded"]][[type]])
            
            # 更新进度条
            progress <- length(results_list)
            utils::setTxtProgressBar(utils::txtProgressBar(min = 0, max = total_elements, style = 3), progress)
            
            # 获取下一页链接
            next_link <- parsed_data_next[["_links"]][["next"]]
          }
          
          # 关闭进度条
          close(utils::txtProgressBar(min = 0, max = total_elements, style = 3))
        }
      }
      
      return(results_list)
    } else {
      return(list(parsed_data))
    }
  } else if (httr::status_code(r) == 404) {
    return(list())
  } else {
    stop(paste("The request for", url, "failed: response code was", httr::status_code(r), 
              "content was\n", httr::content(r, as = "text")))
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
  return(get_data(url, cache_impl = association_cache, cached = cached, type = "associations"))
}
