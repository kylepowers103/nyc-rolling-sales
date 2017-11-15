# Clean neighborhood info
# Information from https://www.health.ny.gov/statistics/cancer/registry/appendix/neighborhoods.htm

library(tidyverse)

dat <- read_csv2("../data/nyc_zip_neighborhoods.csv")

zip_codes_list <- strsplit(dat$ZIP_Codes, ",")
names(zip_codes_list) <- dat$Neighborhood

lapply(zip_codes_list, function(x){
  tibble(Neigh = )
})


zip_codes_tibble <- lapply(seq_along(zip_codes_list), function(i){
  neighborhood = names(zip_codes_list)[[i]]
  out <- tibble(zip_code = zip_codes_list[[i]]) %>% 
    mutate(Neighborhood = neighborhood)
  return(out)
}) %>% 
  bind_rows()

zip_code_neighborhood_borough <- dat %>% 
  select(Borough, Neighborhood) %>% 
  inner_join(zip_codes_tibble)

saveRDS(zip_code_neighborhood_borough, "../out/zip_code_neighborhood_borough.rds")
