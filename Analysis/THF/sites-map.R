##############################################
################### SETUP ####################
##############################################

################## Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr,stringr,sp,ggplot2,plyr,gmodels,Rmisc,DescTools,
               data.table,Hmisc,tibble,rgdal,leaflet,rgeos,raster,plotly,
               pbapply,pbmcapply,skimr,ROCR,pROC,margins,jtools)

################## Clean up the global environment
rm(list = ls())

################## Set directory
setwd(str_replace_all(path.expand("~"), "Documents", ""))
setwd("/Users/sgpeytrignet/Dropbox (Personal)/THF/Branding/")

################## Projection codes
ukgrid = "+init=epsg:27700"
latlong="+init=epsg:4326"

################## Locations

site.coordinates <- fread("site-coordinates.csv", header=TRUE, sep=",", check.names=T)

sites_shp <- SpatialPointsDataFrame(cbind(site.coordinates$Long,site.coordinates$Lat),
                                               data = site.coordinates,
                                               proj4string = CRS(latlong))

content <- paste(paste0("<b>",sites_shp$Name,"</b>"),sites_shp$Site.Name,sep="<br/>")

content <- paste0("<b>",sites_shp$Name,"</b>")

leaflet(sites_shp,options = leafletOptions(zoomControl = FALSE)) %>%
  addProviderTiles(providers$Wikimedia) %>%
  addCircleMarkers(data=sites_shp,fillColor = "blue",radius=10,
                   fillOpacity = 0.75,stroke=F,col="red",weight = 2)

%>%
  addPopups(sites_shp$popup.long, sites_shp$popup.lat,content,
            options = popupOptions(closeButton = TRUE)
  )

help(addPopups)