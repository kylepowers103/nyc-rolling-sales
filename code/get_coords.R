# Get_coordinates

# This file geocodes the addresses of the dataset using Google Maps's API. The limit is 2500 requests per user per day, so I had to manually run different chunks each day. 
# When I have the full geocoded dataset I'll upload it to Kaggle.

if(!require(RCurl)) install.packages("RCurl", repos="http://cran.itam.mx/")
if(!require(RJSONIO)) install.packages("RJSONIO", repos="http://cran.itam.mx/")
if(!require(dplyr)) install.packages("dplyr", repos="http://cran.itam.mx/")
if(!require(tidyr)) install.packages("tidyr", repos="http://cran.itam.mx/")
if(!require(purrr)) install.packages("purrr", repos="http://cran.itam.mx/")
if(!require(readr)) install.packages("readr", repos="http://cran.itam.mx/")

require(RCurl)
require(RJSONIO)
require(dplyr)
require(tidyr)
require(purrr)
require(readr)

# Two functions I use to make pretty column names

make_names_vec <- function (vec) 
{
  vec <- make.names(vec, unique = T)
  vec <- iconv(vec, from = "utf-8", to = "ASCII//TRANSLIT")
  vec <- stringi::stri_replace_all(regex = "[^a-zA-Z0-9\\_\\.]", 
                                   replacement = "", str = vec)
  vec <- stringi::stri_replace_all(fixed = ".", replacement = "_", 
                                   str = vec)
  vec <- stringi::stri_replace_all(regex = "_+", replacement = "_", 
                                   str = vec)
  return(vec)
}

make_names <- function (df) 
{
  df2 <- df %>% setNames(make_names_vec(names(.)))
  return(df2)
}

# Functions that build the geocoder. I found them online a couple of years ago.

construct.geocode.url <- function(address, return.call = "json", sensor = "false") {
  root <- "http://maps.google.com/maps/api/geocode/"
  u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
  return(URLencode(u))
}

gGeoCode <- function(address,verbose=FALSE) {
  if(verbose) cat(address,"\n")
  u <- construct.geocode.url(address)
  doc <- getURL(u)
  x <- fromJSON(doc,simplify = FALSE)
  if(x$status=="OK") {
    lat <- x$results[[1]]$geometry$location$lat
    lng <- x$results[[1]]$geometry$location$lng
    loc_type <- x$results[[1]]$geometry$location_type
    ## En el zip code hay que modificarlo para otros casos porque no siempre son el mismo número de elementos en la lista
    returned_zip_code <- try(x$results[[1]]$address_components[[8]]$long_name, silent = T)
    out <- c(lat, lng, "OK", loc_type, returned_zip_code)
  } else {
    out <- c(NA,NA, x$status, NA, NA)
  }
  if(verbose) cat("\t", out, "\n\n")
  return(out)
}

getGeoCode <- function(address, verbose = FALSE){
  tryCatch(gGeoCode(address, verbose), 
           error = function(err){
             # error handler picks up where error was generated
             print(paste("MY_ERROR:  ",err))
             f <- "error"
             return(f)
           })}

# Tests if the sales data file exists in the same working directory; and if it doesn't, it downloads it from my github.

files <- list.files()
file_name <- "../data/nyc-rolling-sales.csv"
file_exists <- (sum(files == file_name) == 1)
if(!file_exists) {
  cat("FILE NOT FOUND. \nDownloading from github...")
  download.file("https://raw.githubusercontent.com/mariobecerra/Data/master/nyc-rolling-sales.csv",
                file_name)
  cat("File Downloaded.\n\n")
}

# Cleans the data, filtering empty sales or too low sale prices; also keeps registers that have Zip Code and size info.

nyc_data <- read_csv(file_name,
                     na = c("", "NA", "-")) %>% 
  make_names() %>% 
  mutate(id = paste(X1, BOROUGH, sep = "_")) %>% 
  filter(SALE_PRICE > 7000, 
         !is.na(SALE_PRICE)) %>% 
  filter(!is.na(LAND_SQUARE_FEET),
         LAND_SQUARE_FEET > 0,
         !is.na(GROSS_SQUARE_FEET),
         GROSS_SQUARE_FEET > 0,
         ZIP_CODE != 0) %>% 
  mutate(BOROUGH_NAME = 
           ifelse(BOROUGH == 1, "Manhattan", ifelse(
             BOROUGH == 2, "Bronx", ifelse(
               BOROUGH == 3, "Brooklyn", ifelse(
                 BOROUGH == 4, "Queens", "Staten Island")))
           ),
         Complete_address = paste0(
           ADDRESS, 
           ", ", 
           NEIGHBORHOOD,
           ", ",
           BOROUGH_NAME, 
           ", New York, ", 
           ZIP_CODE, 
           ", USA"))



# Partitions the data in different chunks to be run in different days or different machines. This has to be updated manueally.


n_requests <- 2400
n_chunks <- ceiling(nrow(nyc_data)/n_requests)

rep_chunks <- lapply(1:n_chunks, function(i){
  df_out <- tibble(x1 = rep(i, n_requests))
  return(df_out)
}) %>% 
  bind_rows() %>% 
  slice(1:nrow(nyc_data)) %>% 
  pull(x1)

data_chunks <- tibble(
  row = 1:nrow(nyc_data),
  id = nyc_data$id,
  chunk = rep_chunks
)

chunk_id <- 7

address_data <- nyc_data %>% 
  select(Complete_address, ZIP_CODE, id) %>% 
  inner_join(data_chunks) %>% 
  filter(chunk == chunk_id)

# Calls the geocoding functions


# Chunk 12: HP
# Chunk 11: compu ITAM Mario
# Chunk 10: compu Saúl ITAM rstudio@148.205.36.106 123456
# Chunk 9: Compu ITAM Tania
# Chunk 8: compu Farid ITAM elmario@148.205.36.110 mario1234
# Chunk 7: HP
# Chunk 6: Compu ITAM Mario
# Chunk 5: Compu Farid # Murió
# Chunk 4: Compu Saúl
# Chunk 3: Compu Tania
# Chunk 2:
# Chunk 1:

i = 1
coords <- apply(address_data, 1, function(x) {
  print(i)
  i <<- i + 1
  Sys.sleep(0.025)
  coord <- getGeoCode(x["Complete_address"], verbose = T)
  df_out <- tibble(
    id = x["id"],
    Complete_address = x["Complete_address"],
    ZIP_CODE = x["ZIP_CODE"],
    lat = coord[1],
    long = coord[2],
    google_status = coord[3],
    loc_type = coord[4],
    returned_zip_code = coord[5]
  )
  return(df_out)
})

# Saves the data

saveRDS(coords, 
        file = paste0(
          "coords_", chunk_id, ".rds" 
        ))

