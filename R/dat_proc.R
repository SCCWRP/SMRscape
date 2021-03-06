library(tidyverse)
library(sf)
library(lubridate)
library(rgdal)

prstr <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 

######
# processing data files for use with classify app

## 
# whole state data

# flw_pth <- 'ignore/SMR_NHDPlus.shp'
# shd_pth <- 'ignore/calwater_SWAMP3Code.shp'
flw_pth <- 'M:/Data/RaphaelMazor/Santa Margarita Case Study/SMR_NHDPlus.shp'
shd_pth <- 'M:/Data/RaphaelMazor/Santa Margarita Case Study/SMR_Sheds.shp'
scr_pth <- 'ignore/csci_061917.csv'
exp_pth <- 'ignore/comid_prd.Rdata'

# stream expectations, named comid
load(file = exp_pth)

# csci scores
scrs <- read.csv(scr_pth, header = T, stringsAsFactors = F) %>% 
  dplyr::select(SampleID, StationCode, New_Lat, New_Long, COMID, SampleDate, CSCI) %>% 
  rename(
    csci = CSCI, 
    lat = New_Lat, 
    long = New_Long
  ) %>% 
  mutate(
    SampleDate = dmy(SampleDate), 
    COMID = as.character(COMID)
  )

# watersheds
shed <- readOGR(shd_pth) %>% 
  spTransform(CRS(prstr)) %>% 
  st_as_sf %>% 
  mutate(
    SMR_Region  = as.character(SMR_Region),
    SMR_Region = ifelse(SMR_Region %in% "MIDDLE", "LOWER", SMR_Region)
    ) %>% 
  group_by(SMR_Region) %>% 
  summarise()

# flowlines
spat <- readOGR(flw_pth) %>% 
  spTransform(CRS(prstr)) %>% 
  st_as_sf %>% 
  dplyr::select(COMID)  


##
# process separate spatial and score files for each watershed

# sheds to process, appended to file names
shds <- shed$SMR_Region

# process and save files for each shed
for(shd in shds){
  
  # counter
  cat(shd, which(shds %in% shd), 'of', length(shds), '\n')
  
  # filter watershed for intersect
  shd_tmp <- shed %>% 
    filter(SMR_Region %in% shd)
  
  # create spatial polyines from shed intersect, left_join with csci scrs
  spat_tmp <- spat %>%
    st_intersection(shd_tmp) %>% 
    left_join(comid_prd, by = 'COMID') %>% 
    select(COMID, matches('^full0'))
  
  # csci scores
  scrs_tmp <- scrs %>% 
    filter(COMID %in% spat_tmp$COMID)
  
  # assign unique names to scrs and spat
  scrs_shd <- paste0('scrs_', shd)
  spat_shd <- paste0('spat_', shd)
  assign(scrs_shd, scrs_tmp)
  assign(spat_shd, spat_tmp)
  
  # save unique scrs, spat
  save(list = scrs_shd, file = paste0('data/', scrs_shd, '.RData'))
  save(list = spat_shd, file = paste0('data/', spat_shd, '.RData'))
  
}