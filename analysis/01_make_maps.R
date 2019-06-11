#? This script ...

# Set working directory
setwd('/home/jrl/doct/events/19_uppsala')

# Load packages
library('dplyr'); library('sf')


# World ----------
temp <- tempfile()
download.file('https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip', 
              dest = temp)
unzip(temp, exdir = tempdir())
worldSf <- st_read(paste0(tempdir(), '/ne_50m_admin_0_countries.shp'))


# NUTS2 2013 ----------
temp <- tempfile()
download.file('https://ec.europa.eu/eurostat/cache/GISCO/distribution/v2/nuts/download/ref-nuts-2013-20m.shp.zip', 
              dest = temp)
unzip(temp, exdir = tempdir())
unzip(paste0(tempdir(), '/NUTS_RG_20M_2013_3035_LEVL_2.shp.zip'), exdir = tempdir())
nuts2Sf <- st_read(paste0(tempdir(), '/NUTS_RG_20M_2013_3035_LEVL_2.shp'), stringsAsFactors = F)


# Bind maps ----------

nuts2Sf %<>% 
  subset(CNTR_CODE %in% regLs$nuts28, select = NUTS_ID)
names(nuts2Sf)[names(nuts2Sf) == 'NUTS_ID'] <- 'geo'

worldSf %<>% 
  subset(subset = !(WB_A2 %in% regLs$iso28) , select = WB_A2) %>% 
  dplyr::rename(geo = WB_A2) %>% 
  st_transform(crs = 3035)

mapSf <- rbind(nuts2Sf, worldSf) %>% 
  st_transform(crs = 3035)


# Save ----------
saveRDS(mapSf, 'analysis/maps.Rds')
