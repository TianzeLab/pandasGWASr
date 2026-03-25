# 包的初始化文件

# 定义包全局环境
.pandasGWASr <- new.env(parent = emptyenv())

# 初始化缓存变量
.onLoad <- function(libname, pkgname) {
  # 加载必要的包
  require(cachem)
  
  # 设置缓存过期时间为24小时（单位：秒）
  ttl <- 60 * 60 * 24
  
  # 创建包全局内存TTL缓存
  .pandasGWASr$association_cache <- cachem::cache_mem(max_age = ttl)
  .pandasGWASr$study_cache <- cachem::cache_mem(max_age = ttl)
  .pandasGWASr$trait_cache <- cachem::cache_mem(max_age = ttl)
  .pandasGWASr$snp_cache <- cachem::cache_mem(max_age = ttl)
  .pandasGWASr$ancestry_cache <- cachem::cache_mem(max_age = ttl)
  .pandasGWASr$publication_cache <- cachem::cache_mem(max_age = ttl)
  .pandasGWASr$genomicContext_cache <- cachem::cache_mem(max_age = ttl)
  .pandasGWASr$gene_cache <- cachem::cache_mem(max_age = ttl)
  .pandasGWASr$locus_cache <- cachem::cache_mem(max_age = ttl)
}

# 定义清除缓存的函数
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
