# 包的初始化文件

# 定义包全局环境
.pandasGWASr <- new.env(parent = emptyenv())

#' @importFrom cachem cache_mem
#' @noRd
.onLoad <- function(libname, pkgname) {
  # 设置缓存过期时间为24小时（单位：秒）
  ttl <- 60 * 60 * 24
  
  # 创建包全局内存TTL缓存
  .pandasGWASr$association_cache <- cache_mem(max_age = ttl)
  .pandasGWASr$study_cache <- cache_mem(max_age = ttl)
  .pandasGWASr$trait_cache <- cache_mem(max_age = ttl)
  .pandasGWASr$snp_cache <- cache_mem(max_age = ttl)
  .pandasGWASr$ancestry_cache <- cache_mem(max_age = ttl)
  .pandasGWASr$publication_cache <- cache_mem(max_age = ttl)
  .pandasGWASr$genomicContext_cache <- cache_mem(max_age = ttl)
  .pandasGWASr$gene_cache <- cache_mem(max_age = ttl)
  .pandasGWASr$locus_cache <- cache_mem(max_age = ttl)
}

#' Clear cache
#'
#' This function clears the cache for the specified data type or all caches.
#'
#' @param cache_name The name of the cache to clear, or "all" to clear all caches
#'
#' @export
clear_cache <- function(cache_name = "all") {
  if (cache_name == "all") {
    # 清除所有缓存
    cache_names <- c("association_cache", "study_cache", "trait_cache", 
                    "snp_cache", "ancestry_cache", "publication_cache", 
                    "genomicContext_cache", "gene_cache", "locus_cache")
    for (name in cache_names) {
      if (exists(name, envir = .pandasGWASr)) {
        cache <- get(name, envir = .pandasGWASr)
        cache$reset()
      }
    }
  } else {
    # 清除指定缓存
    if (exists(cache_name, envir = .pandasGWASr)) {
      cache <- get(cache_name, envir = .pandasGWASr)
      cache$reset()
    }
  }
}
