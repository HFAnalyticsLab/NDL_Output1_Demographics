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
gitdir <- dirname(rstudioapi::getSourceEditorContext()$path)

################## Projection codes
ukgrid = "+init=epsg:27700"
latlong="+init=epsg:4326"

################## Locations
site.coordinates <- fread(paste0(gitdir,"/site-coordinates.csv"), header=TRUE, sep=",", check.names=T)

sites_shp <- SpatialPointsDataFrame(cbind(site.coordinates$Long,site.coordinates$Lat),
                                               data = site.coordinates,
                                               proj4string = CRS(latlong))

content <- paste(paste0("<b>",sites_shp$Name,"</b>"),sites_shp$Site.Name,sep="<br/>")

content <- paste0("<b>",sites_shp$Name,"</b>")

leaflet(sites_shp,options = leafletOptions(zoomControl = FALSE)) %>%
  addTiles() %>%
  addCircleMarkers(data=sites_shp,fillColor = "blue",radius=12,
                   fillOpacity = 0.75,stroke=F,col="red",weight = 2)

# %>%
#   addPopups(sites_shp$popup.long, sites_shp$popup.lat,content,
#             options = popupOptions(closeButton = TRUE)
#   )

writeOGR(sites_shp, ".", "filename", driver="ESRI Shapefile") #also you were missing the driver argument
writeOGR(sites_shp, dsn="sites_shp.GeoJSON", layer="sites_shp", driver="GeoJSON")
