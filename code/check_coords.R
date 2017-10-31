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

nyc_coords_api <- read_csv("../out/coods_nyc_api.csv")

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




nyc_zip_tibble %>% 
  ggplot()+
  geom_polygon(aes(x = long, y = lat, group = group),
               fill = 'white', color = 'black', size = 0.1) +
  geom_point(data = nyc_coords_api,
             aes(x = long, y = lat),
             size = 0.1,
             alpha = 0.5) +
  coord_fixed() +
  theme_bw()



geodesic_distance <- function(long1, lat1, long2, lat2) {
  # geodesic_distance(-99.113231, 19.296341, -99.110213, 19.295771)
  # geodesic_distance(-99.110712, 19.295860, -99.113153, 19.296328)
  # geodesic_distance(c(-99.113231,-99.110712),
  #                   c(19.296341, 19.295860),
  #                   c(-99.110213, -99.113153),
  #                   c(19.295771, 19.296328))
  n1 <- length(long1)
  n2 <- length(long2)
  n3 <- length(lat1)
  n4 <- length(lat2)
  bool <- n1 != n2 | n1 != n3 | n1 != n4 | n2 != n3 | n2 != n4 | n3 != n4
  if(bool) return("Error: las entradas no tienen la misma longitud")
  else{
    long1 <- long1*pi/180
    lat1 <- lat1*pi/180
    long2 <- long2*pi/180
    lat2 <- lat2*pi/180
    delta.long <- (long2 - long1)
    delta.lat <- (lat2 - lat1)
    a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2
    #c <- 2 * asin(min(1,sqrt(a)))
    #c <- 2 * asin(sapply(a, function(x) min(1,sqrt(x))))
    c <- 2 * asin(min_vec(sqrt(a), rep(1, n1)))
    d = 6371 * c # 6371 is Earth mean radius [km]
    return(d) # Distance in km
  }
}

Rcpp::cppFunction("
  NumericVector min_vec(NumericVector vec1, NumericVector vec2) {
    int n = vec1.size();
    if(n != vec2.size()) return 0;
    else {
      NumericVector out(n);
      for(int i = 0; i < n; i++) {
        out[i] = std::min(vec1[i], vec2[i]);
      }
      return out;
    }
  }
")


nyc_joined <- nyc_sales %>% 
  mutate(long_google = as.numeric(long),
         lat_google = as.numeric(lat),
         returned_zip_code_google = returned_zip_code) %>% 
  select(-long, -lat, -returned_zip_code) %>% 
  inner_join(nyc_coords_api) %>% 
  mutate(coord_dif = sqrt((long - long_google)^2 + (lat - lat_google)^2),
         dist_meters = 1000*geodesic_distance(long_google, lat_google, long, lat))


qplot(nyc_joined$coord_dif)

# Biggest difference in coordinates was 13 Km
qplot(nyc_joined$dist_meters)

nyc_joined %>% 
  filter(dist_meters > 100) %>% 
  View


nyc_zip_tibble %>% 
  ggplot()+
  geom_polygon(aes(x = long, y = lat, group = group),
               fill = 'white', color = 'black', size = 0.1) +  
  geom_point(data = nyc_joined %>% 
               filter(coord_dif > 0.02) %>% 
               select(lat_google, long_google, lat, long) %>% 
               gather(lat1, lat2, lat_google, lat) %>% 
               gather(long1, long2, long_google, long),
             aes(long2, lat2, color = lat1))


