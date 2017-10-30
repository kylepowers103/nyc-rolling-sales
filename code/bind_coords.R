library(dplyr)


nyc_data_orig <- read_csv("../data/nyc-rolling-sales.csv",
                     na = c("", "NA", "-")) %>% 
  make_names() %>% 
  mutate(id = paste(X1, BOROUGH, sep = "_"),
         ZIP_CODE = as.character(ZIP_CODE)) %>% 
  filter(SALE_PRICE > 7000, 
         !is.na(SALE_PRICE)) %>% 
  filter(!is.na(LAND_SQUARE_FEET),
         LAND_SQUARE_FEET > 0,
         !is.na(GROSS_SQUARE_FEET),
         GROSS_SQUARE_FEET > 0,
         ZIP_CODE != 0)
  # mutate(BOROUGH_NAME = 
  #          ifelse(BOROUGH == 1, "Manhattan", ifelse(
  #            BOROUGH == 2, "Bronx", ifelse(
  #              BOROUGH == 3, "Brooklyn", ifelse(
  #                BOROUGH == 4, "Queens", "Staten Island")))
  #          ),
  #        Complete_address = paste0(
  #          ADDRESS, 
  #          ", ", 
  #          NEIGHBORHOOD,
  #          ", ",
  #          BOROUGH_NAME, 
  #          ", New York, ", 
  #          ZIP_CODE, 
  #          ", USA"))


coords_12 <- readRDS("../out/coords_12.rds")
coords_11 <- readRDS("../out/coords_11.rds")
coords_10 <- readRDS("../out/coords_10.rds")
coords_9 <- readRDS("../out/coords_9.rds")
coords_8 <- readRDS("../out/coords_8.rds")
coords_7 <- readRDS("../out/coords_7.rds")
coords_6 <- readRDS("../out/coords_6.rds")
coords_5 <- readRDS("../out/coords_5.rds")
coords_4 <- readRDS("../out/coords_4.rds")
coords_3 <- readRDS("../out/coords_3.rds")
coords_2 <- readRDS("../out/coords_2.rds")
coords_1 <- readRDS("../out/coords_1.rds")


nyc_sales <- 	coords_1 %>% 
  bind_rows(coords_2) %>%  
  bind_rows(coords_3) %>%
  bind_rows(coords_4) %>% 
  bind_rows(coords_5) %>% 
  bind_rows(coords_6) %>% 
  bind_rows(coords_7) %>% 
  bind_rows(coords_8) %>% 
  bind_rows(coords_9) %>% 
  bind_rows(coords_10) %>%
  bind_rows(coords_11) %>%
  bind_rows(coords_12) %>% 
  inner_join(nyc_data_orig)

saveRDS(nyc_sales, "../out/nyc_sales.rds")

nyc_sales %>% 
  mutate(Street = stringi::stri_split(ADDRESS, regex = "^[A-Za-z0-9\\-]+ ", simplify = T)[,2],
         Street_Number = stringi::stri_extract_first(ADDRESS, regex = "^[A-Za-z0-9\\-]+", simplyfy = T),
         BOROUGH_NAME = 
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
           ", USA")) %>% 
  write_csv(., "../out/nyc_sales.csv")

