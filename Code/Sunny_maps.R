#Make exploratory maps

#Load libraries
library(tidyverse)
library(sf)
library(ggmap)
library(mapproj)
library(maps)
library(rgdal)

#Load data
Data <- read.csv("Data/Initial_sunny_data.csv")
Boundaries <- st_read("Data/California_Drinking_Water_System_Area_Boundaries/")
Boundaries <- Boundaries[, c(1,2,3,4,11,17,18,33,34,35)]
Boundaries$BOUNDARY_T <- as.factor(Boundaries$BOUNDARY_T)
Boundaries <- Boundaries %>% filter(ACTIVITY_S == "A") %>% filter(BOUNDARY_T=="Water Service Area")
Data <- left_join(Data, Boundaries, by = c("PWSID" = "SABL_PWSID"))
Data <- Data %>% filter(STATE_CLAS == "COMMUNITY")
Data <- Data[!is.na(Data$Shape__Are),]
Data$Population_Served <- as.numeric(Data$Population_Served)
Data$Logpop <- log(Data$Population_Served) #make a log of population served for mapping

Data$Centroid <- NA
Data$Centroid <- st_centroid(Data$geometry) #Get centroids to service area boundaries


Data_SWonly <- Data %>% filter(Uses_SW == "YES") #Filter to only those using SW

Data_SWonly$SW_Type <- NA
Data_SWonly$SW_Type <- ifelse(Data_SWonly$Uses_SW_Purchased == "YES" & Data_SWonly$Uses_SW_Produced == "YES", "Both", NA)
Data_SWonly$SW_Type <- ifelse(Data_SWonly$Uses_SW_Purchased == "YES" & Data_SWonly$Uses_SW_Produced == "MAYBE", "Maybe both", Data_SWonly$SW_Type)
Data_SWonly$SW_Type <- ifelse(Data_SWonly$Uses_SW_Purchased == "MAYBE" & Data_SWonly$Uses_SW_Produced == "YES", "Maybe both", Data_SWonly$SW_Type)
Data_SWonly$SW_Type <- ifelse(Data_SWonly$Uses_SW_Purchased == "YES" & Data_SWonly$Uses_SW_Produced == "NO", "Purchased", Data_SWonly$SW_Type)
Data_SWonly$SW_Type <- ifelse(Data_SWonly$Uses_SW_Purchased == "NO" & Data_SWonly$Uses_SW_Produced == "YES", "Produced", Data_SWonly$SW_Type)
Data_SWonly$SW_Type <- as.factor(Data_SWonly$SW_Type)

Data_SWonly <- st_as_sf(Data_SWonly)

#Basemap
caCountiesTmp <- tigris::counties(state = 06) %>%
  st_as_sf()

#maps
SWCWS_Type <- ggplot() +
  geom_sf(data = caCountiesTmp) +
  geom_sf(mapping = aes(colour = SW_Type, geometry = Centroid, size=Population_Served), data = Data_SWonly, inherit.aes = FALSE) +
  theme_void() +
  theme(legend.position = "bottom", 
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(10,10,10,10))


#Map exclusively sw systems
Data_noGW <- Data_SWonly %>% filter(Uses_GW == "NO")
st_crs(Data_noGW) 
Data_noGW <- st_as_sf(Data_noGW)


CWS_SW_only <- ggplot() +
  geom_sf(data = caCountiesTmp) +
  geom_sf(mapping = aes(colour = SW_Type, geometry = Centroid, size=Population_Served), data = Data_noGW, inherit.aes = FALSE) +
  theme_void() +
  theme(legend.position = "bottom", 
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(10,10,10,10))


