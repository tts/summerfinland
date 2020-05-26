library(httr)
library(jsonlite)
library(tidyverse)

baseurl <- "https://core.ac.uk:443/api-v2/repositories/scrolling-search/*?"

request <- GET(url = baseurl, 
               query = list(
                 pageSize = 100,
                 apiKey = key,
                 scrolldId = NULL)
)

response <- content(request, as = "text", encoding = "UTF-8")

df <- fromJSON(response, flatten = TRUE) %>% 
  data.frame(stringsAsFactors = FALSE)

# There are also journals but maybe I'll filter them in the end
#
# df_repos <- df %>% 
#  filter(data.type == 'repository')

pages <- ceiling(df[1, "totalHits"] / 100)
nextpage <- df[1, "scrollId"]

# See how to iterate over pages
# https://github.com/tts/aaltoced4pureconf/blob/master/getdois.R

request2 <- GET(url = baseurl, 
               query = list(
                 pageSize = 100,
                 apiKey = key,
                 scrollId = nextpage)
)

response2 <- content(request2, as = "text", encoding = "UTF-8")

df3 <- fromJSON(response2, flatten = TRUE) %>% 
  data.frame()

nextpage <- df2[1, "scrollId"]

# Alright, this above works in a manual fashion so next: functions

size <- 100

get_items <- function(size, key, nextpage = NULL) {
  
  GET(url = baseurl, 
      query = list(
        pageSize = size,
        apiKey = key,
        scrollId = nextpage)
      ) -> res
  
  return(res)
}


result <- get_items(size, key)
response <- content(result, as = "text", encoding = "UTF-8")

first_page <- fromJSON(response, flatten = TRUE) %>% 
  data.frame(stringsAsFactors = FALSE)

nextpage <- first_page[1, "scrollId"]
pages <- ceiling(df[1, "totalHits"] / 100)

#nextpage <- df[1, "scrollId"]


resultset <- sapply(pages, function(x) { 
  r <- get_items(size, x, after, before) 
  r_p <- httr::content(r, "parsed")
  r_p$items
}) 


