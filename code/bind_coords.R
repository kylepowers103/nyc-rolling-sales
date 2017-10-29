library(dplyr)

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


nyc_sales <- coords_12 %>% 
  bind_rows(coords_11) %>%
  bind_rows(coords_10) %>%
  bind_rows(coords_9) %>% 
  bind_rows(coords_8) %>% 
  bind_rows(coords_7) %>% 
  bind_rows(coords_6) %>% 
  bind_rows(coords_5) %>% 
  bind_rows(coords_4) %>% 
  bind_rows(coords_3)

saveRDS(nyc_sales, "../out/nyc_sales.rds")
