setwd("C:/Users/londonCommuter")
library(ggplot2)
library("rgdal")
library("rgeos")
library(sp)
library("tmap")
library(automap)
library(grid)



ODdata = read.csv("londonMSOAbycicle.csv",header = TRUE)
Di = apply(ODdata, 2, sum)
centerRank = rank(-Di)
idlist = names(ODdata)
idlist = substring(idlist, 1,9) 
slist = 0
glist = 0
for(i in 1:nrow(ODdata)){
  maxDest = which.max(ODdata[i,])
  if(centerRank[i]>centerRank[maxDest]){
    slist = c(slist,i)
    glist = c(glist,maxDest)
  }
}
slist = slist[-1]
glist = glist[-1]
boundary =readOGR("shapefile","MSOA_2011_London_gen_MHW")


centroid = gCentroid(boundary, byid = TRUE)
proj4string(centroid) = CRS("+init=EPSG:27700")
centroid = SpatialPointsDataFrame(centroid@coords,boundary@data)

lines <-list()
for(i in 1:length(slist)){
  sid = idlist[slist[i]]
  gid = idlist[glist[i]]
  sx = centroid@coords[centroid@data$MSOA11CD==sid,1]
  sy = centroid@coords[centroid@data$MSOA11CD==sid,2]
  gx = centroid@coords[centroid@data$MSOA11CD==gid,1]
  gy = centroid@coords[centroid@data$MSOA11CD==gid,2]
  lines = c(lines,Line(cbind(c(sx,gx),c(sy,gy))))
}
splines = SpatialLines(list(Lines(lines,ID="a")),proj4string = CRS("+init=EPSG:27700"))

library(leaflet)
# turns view map on
tmap_mode("view")
tm_shape(boundary)+
  tm_borders(alpha = 0.5)+tm_fill(col = "POPDEN",title = "Population Density")+
  tm_shape(splines) + tm_lines(col = "red",alpha = 0.5)+
  tm_view(alpha = 0.5,basemaps = "OpenStreetMap.BlackAndWhite",
          legend.position = c("left", "bottom"))
