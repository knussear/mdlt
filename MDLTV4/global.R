#library(rgdal)
library(terra)
library(shiny)
#library(shinydashboard)
library(dplyr, warn.conflicts = FALSE)
library(sf)
sf::sf_use_s2(FALSE)
library(rasterVis)
library(stringr)
library(ggplot2)
library(tidyterra)
library(patchwork)
library(purrr)
library(readr)
library(shinyBS)
library(leaflet)
library(leaflet.extras)
library(DT)
library(data.table)
library(gridExtra)
#library(ggpubr)
library(cowplot)
library(htmltools)
library(zip)####MDLT V3
library(bslib)
library(rlist)
library(scales)
# Rescale function to set raster to 0-1------------------------
tr.rescale <- function(x,new.min = 0, new.max = 255) {
  x.min =  global(x, fun='min', na.rm=T)$min
  x.max =  global(x, fun='max', na.rm=T)$max
  if(x.max != 0){
  new.min + (x - x.min) * ((new.max - new.min) / (x.max - x.min))
  } else {
  x
    }
}

#Working directory----------------
datatab<-read.csv("Filez/RasterDataLayersKN.csv")
datatab$class <- rep(3, dim(datatab)[1])
#datatab <- as.data.table(datatab[,c('id', 'Layer.name','Filename','category','class','type','source')])
#datatab$trial <- c(rep(1,6), rep(0, dim(datatab)[1] -6))
datatab <- datatab[datatab$type =='c',]
datatab$id <- 1:dim(datatab)[1]

pchoice<-read.csv("Filez/Parcel shapefile.csv")
#whichdir<-getwd()
#rasters<-list.files('whichdir', pattern = '.csv')
#View(rasters)
# Define CSS styles for the plot panels


#Buffer Map
mymap<-read_sf("Filez/Buffer/Buffer_final.shp")
map<-st_transform(mymap, 'epsg:4326')
rasterpath <- 'Filez/rasterz/'

#Continuous Files-----------------

 # TDI.r<-rast("Filez/rasterz/TDI.tif")
# # TotalDevelopmentIndex.r<-rast("C:/Users/cbutt/Documents/R shiny/completed rasters/TDI.tif")

#Parcel file------------------------------
  parcels.shp<-vect("Filez/Parcels/ParcelsStudyAreaUTMKN.gpkg")
#Courtney's
#parcels.shp<-st_read("C:/Users/Public/Documents/MDLT/Parcels/ParcelsStudyArea.shp")
  miniparcels <- vect('Filez/miniparcels.gpkg')
  miniparcels.utm <- project(miniparcels, crs('epsg:26911'))


#Graph parameters & Parcel coloration code--------------------     
  sdf  <- data.frame(l = 1:7, s1 = c(5,5:1,1), s2 = c(1,1:5,5)) ## dataframe for slider value settings
   
zat <- seq(from = 0, to = 1, by = 0.05)
twocol <- c('red','blue')

parcels.svtmp <- rnorm(dim(parcels.shp)[1], mean = 20,sd = 5)

#x <- seq(0, 1, length = 20)

plotseq <- seq(from = 0, to = 1, by = 0.01) ## x values for the beta plots

posplotsize <- '600px'
negplotsize <- '600px'

globaltmprsts <- reactiveVal(NULL)
#Environmental Layers List----------------------------
# env.layers  <- ls(pattern = '\\.r$')
# env.layers.names  <- env.layers
# 
# env.layers.list <- list()
# for(i in 1:length(env.layers)){
#   env.layers.list[[i]] <- list(layer = env.layers[i], name = env.layers.names[i])
# }
# 
# parcel.layers<- ls(pattern= '\\.shp$')
# parcel.layers.names <- parcel.layers

#Function that changes graph & raster on INPUT page based on user input into a temporary raster--------------
# terra.pbeta <- function(r,s1,s2,w){
#   tpb <- pbeta(values(r)[,1], s1, s2)*w
#   tmp.r <- r
#   values(tmp.r) <- tpb
#   return(tmp.r)
# }


