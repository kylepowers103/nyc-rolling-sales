# check_coords

library(rgdal)
library(maptools)
library(gpclib)
library(tidyverse)
gpclibPermit() 

nyc_zip_shape <- readOGR("../data/ZIP_CODE_040114/", layer = "ZIP_CODE_040114") %>% 
  spTransform(CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

nyc_zip_shape@data$id <- as.character(1:nrow(nyc_zip_shape@data))

nyc_zip_tibble <- fortify(nyc_zip_shape, region = "id") %>% 
  as_tibble() %>% 
  left_join(nyc_zip_shape@data)

nyc_sales <- read_rds("../out/nyc_sales.rds") %>% 
  rename(id_sale = id)


nyc_zip_tibble %>% 
  ggplot()+
  geom_path(aes(x = long, y = lat, group = group),
            color = 'black', size = 0.1) +
  coord_fixed() +
  theme_bw()

# There's a problem with addresses in zip code boundaries: the geocoder gives the rooftop location, not the street location, which may be in different zip codes
iter = 0
lista <- lapply(unique(nyc_zip_tibble$id), function(i){
  iter <<- iter + 1
  idx <- nyc_zip_tibble$id == i
  df_temp <- nyc_zip_tibble[idx,]
  zip_code <- as.character(unique(df_temp$ZIPCODE))
  cat("iter:", iter, "\t ZIP Code:", zip_code, "\n")
  vec1 <- sp::point.in.polygon(nyc_sales$long, 
                               nyc_sales$lat, 
                               df_temp$long,
                               df_temp$lat)
  df_out <- tibble(
    is_in = vec1, 
    ZIPCODE_checked = zip_code,
    id_sale = nyc_sales$id_sale) %>% 
    filter(is_in == 1) %>% 
    select(-is_in)
  return(df_out)
})


# Example showing the zip code boundary problem corresponding to sale id 7941_5 and address
# 54 NORTH AVENUE, WESTERLEIGH, Staten Island, New York, 10302, USA
nyc_zip_tibble %>% filter(ZIPCODE == 10302) %>% 
  ggplot() +
  geom_polygon(aes(long, lat, group = group), fill = 'white', color = 'black') +
  geom_point(data = tibble(long = -74.13627,
                           lat = 40.62313),
             aes(long, lat))


nyc_sales_2 <- lista %>% 
  bind_rows() %>% 
  right_join(nyc_sales)

nyc_sales_2 %>% 
  filter(ZIPCODE_checked != ZIP_CODE)


nyc_sales_2 %>% 
  filter(ZIPCODE_checked != ZIP_CODE,
         loc_type == "RANGE_INTERPOLATED")

  
  
  
  