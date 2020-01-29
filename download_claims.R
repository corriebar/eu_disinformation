library(httr)
library(jsonlite)
library(glue)

euvsdis <- "https://api.veedoo.io/"

url  <- "https://api.veedoo.io"


claims <- list()
for(i in 1:241) {
  path1 <- paste0("claims?page=", i)
  path_full <- paste0(path1, "&perPage=30")
  raw.result <- GET(url = url, path = path_full)
  names(raw.result)
  this.raw.content <- rawToChar(raw.result$content)
  this.content <- fromJSON(this.raw.content)
  claims[[i]] <- this.content$`hydra:member`
  if (i %% 5 == 0) {
    print(glue("We're at page {i}"))
  }
}

all_claims <- do.call(rbind, claims)
