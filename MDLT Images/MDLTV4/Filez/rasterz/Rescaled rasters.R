
library(terra)


tr.rescale <- function(x,new.min = 0, new.max = 255) {
  x.min =  global(x, fun='min', na.rm=T)$min
  x.max =  global(x, fun='max', na.rm=T)$max
  new.min + (x - x.min) * ((new.max - new.min) / (x.max - x.min))
}

TDI.r<-rast("Z:/Documents/Masters Data/Finished data/DT_TDI/DT_TDI_1km_250m_final.tif")
waterbodies.r<-rast("Z:/Documents/Masters Data/Finished data/NHD Waterbodies/waterbodies.tif")
plot(waterbodies.r)
noPlaya.r<-rast("Z:/Documents/Masters Data/Finished data/NHD Waterbodies no Playa/noPlaya.tif")
rivcreek.r<-rast("Z:/Documents/Masters Data/Finished data/NHD Rivers and Creeks/rivcreek.tif")
aquifers.r<-rast("Z:/Documents/Masters Data/Finished data/Aquifers/aquifers.tif")
resilencerank.r<-rast("Z:/Documents/Masters Data/Finished data/Terrestrial Climate Resilence/Ter_Clim_Chnge_Resil.tif")
usfwscrithab.r<-rast("Z:/Documents/Masters Data/Finished data/USFWS Critical Habitat/USFWSCritHab.tif")
consintactcmbnd.r<-rast("Z:/Documents/Masters Data/Finished data/ConservAndIntactDRECP.tif")
mojcolconservalues.r<-rast("Z:/Documents/Masters Data/Finished data/Mojave_Colorado_Conservation_Value.tif")
tnatsprich.r<-rast("Z:/Documents/Masters Data/Finished data/Terrestrial Native Species Richness/TerrestrialNatSpeciesRich.tif")
tclimvulsp.r<-rast("Z:/Documents/Masters Data/Finished data/Terrestrial Climate Vulnerable Species/TerrClimVulSpCount.tif")
tnatgamesp.r<-rast("Z:/Documents/Masters Data/Finished data/Terrestrial Native Game Species/TerrNativeGameSp.tif")
traresprich.r<-rast("Z:/Documents/Masters Data/Finished data/Terrestrial Rare Species Richness/TerrRareSpRich.tif")
tirrepl.r<-rast("Z:/Documents/Masters Data/Finished data/Terrestrial Irreplaceability/TerrIrreplaceabilityRank.tif")
tbiodiv.r<-rast("Z:/Documents/Masters Data/Finished data/Terrestrial Biodiversity/TerrestrialBiodiveristy.tif")
aqnatsprich.r<-rast("Z:/Documents/Masters Data/Finished data/Aquatic Native Species Richness/AqNatSpRich.tif")
aqraresprich.r<-rast("Z:/Documents/Masters Data/Finished data/Aquatic Rare Species Richness/AqRareSpRichness.tif")
aqirrepl.r<-rast("Z:/Documents/Masters Data/Finished data/Aquatic Irreplaceability/AqIrrepl.tif")
aqbiodiv.r<-rast("Z:/Documents/Masters Data/Finished data/Aquatic Biodiversity/AqBiodiv.tif")
spbiodiv.r<-rast("Z:/Documents/Masters Data/Finished data/Species Biodiversity/SpBiodiv.tif")
tconnectrank.r<-rast("Z:/Documents/Masters Data/Finished data/Terrestrial Connectivity/TerrConnectivity.tif")
DThablink.r<-rast("Z:/Documents/Masters Data/Finished data/DTHabitatLinkages.tif")
railroads.r<-rast("Z:/Documents/Masters Data/Finished data/Railroads/Railroads.tif")
roads.r<-rast("Z:/Documents/Masters Data/Finished data/Roads/Roads.tif")
ohv.r<-rast("Z:/Documents/Masters Data/Finished data/OHVRoutes.tif")
#finalDFAs.r<-rast("Z:/Documents/Masters Data/Finished data/FinalDFAs.tif")
#proposedDFAs.r<-rast("Z:/Documents/Masters Data/Finished data/ConservIntactnessProposedDFAs.tif")
#transmissionDFAs.r<-rast("Z:/Documents/Masters Data/Finished data/TransmissonLines4DFAs.tif")
landuse.r<-rast("Z:/Documents/Masters Data/Finished data/Land Use/landuse.tif")
DRECPbaselayers.r<-rast("Z:/Documents/Masters Data/Finished data/DRECPBaseLayers.tif")
natrails.r<-rast("Z:/Documents/Masters Data/Finished data/NationalTrails.tif")
dist2water.r<- dist2water.r<-rast("Z:/Documents/Masters Data/Finished data/Distance to water/Dist_water.tif")
carbonpriority.r<-rast("Z:/Documents/Masters Data/Finished data/Rank Forest Carbon Priority/Rank_Forest_Carbon_Priority_final.tif")
forestNEP.r<-rast("Z:/Documents/Masters Data/Finished data/Rank Forest Potential NEP/Rank_Forest_Potential_NEP_final.tif")
plot(forestNEP.r)
probfire.r<-rast("Z:/Documents/Masters Data/Finished data/Probability_Fire_Occurrence_final.tif")
mojseqdivergence.r<-rast("Z:/Documents/Masters Data/Finished data/Sequence Divergence/Seq_Divergence_final.tif")
mojseqdiversity.r<-rast("Z:/Documents/Masters Data/Finished data/Sequence Diversity/Seq_Diversity_final.tif")
mojgenediversity.r<-rast("Z:/Documents/Masters Data/Finished data/Gene Diversity/Gene_Diversity_final.tif")
dist2roads.r<-rast("Z:/Documents/Masters Data/Finished data/Distance to roads/Dist_to_Roads.tif")

#newly added files----------
transmissionlines.v<-vect("Z:/Documents/Masters Data/Finished data/TransmissionLines_DRECP.shp")
plot(transmissionlines.v)
transmissionlines.r<-rasterize(transmissionlines.v, TDI.r, field="Unique")
plot(transmissionlines.r, col="black")
renewableenergy.v<-vect("Z:/Documents/Masters Data/Finished data/BLM_LUPA_RenewableEnergyDesignations.shp")
plot(renewableenergy.v)
renewableenergy.r<-rasterize(renewableenergy.v, TDI.r, field="Integrat_1")
plot(renewableenergy.r)
conservationdesignations.v<-vect("Z:/Documents/Masters Data/Finished data/BLM_LUPA_Conservation Designations.shp")
plot(conservationdesignations.v)
conservationdesignations.r<-rasterize(conservationdesignations.v, TDI.r, field="Type")
plot(conservationdesignations.r)
desertnatconservlands.v<-vect("Z:/Documents/Masters Data/Finished data/BLM_LUPA_CaDesertNationalConservLands.shp")
plot(desertnatconservlands.v)
desertnatconservlands.r<-rasterize(desertnatconservlands.v, TDI.r, field="NLCS")
plot(desertnatconservlands.r)
recreationdesignations.v<-vect("Z:/Documents/Masters Data/Finished data/BLM_LUPA_RecreationDesignations_final.shp")
plot(recreationdesignations.v)
recreationdesignations.r<-rasterize(recreationdesignations.v, TDI.r, field="Type")
plot(recreationdesignations.r)
generalpubiclands.v<-vect("Z:/Documents/Masters Data/Finished data/BLM_LUPA_GeneralPublicLands.shp")
plot(generalpubiclands.v)
generalpubliclands.r<-rasterize(generalpubiclands.v, TDI.r, field="LandType")
plot(generalpubliclands.r, col="black")

ext(TDI.r)
summary(TDI.r)
ext(renewableenergy.r)
##Rescaling rasters & write Raster into Completed Rasters------------------------

writeRaster(renewableenergy.r,"Z:/Documents/Masters Project/R shiny/completed rasters/renewable_energy.tif", overwrite=TRUE)
writeRaster(transmissionlines.r, "Z:/Documents/Masters Project/R shiny/completed rasters/transmissionlines.tif", overwrite=TRUE)
writeRaster(conservationdesignations.r, "Z:/Documents/Masters Project/R shiny/completed rasters/conservationdesignations.tif")
writeRaster(desertnatconservlands.r, "Z:/Documents/Masters Project/R shiny/completed rasters/desertnatconservlands.tif")
writeRaster(recreationdesignations.r, "Z:/Documents/Masters Project/R shiny/completed rasters/recreationdesignations.tif")
writeRaster(generalpubliclands.r, "Z:/Documents/Masters Project/R shiny/completed rasters/generalpubliclands.tif")

#Rescaling coninuous rasters------------------
mojgenediversity.resamp.r<-resample(mojgenediversity.r,TDI.r)
ext(mojgenediversity.resamp.r)<-ext(TDI.r)
mojgenediversity.rescale.r<-tr.rescale(mojgenediversity.resamp.r,0,1)
writeRaster(mojgenediversity.rescale.r,"Z:/Documents/Masters Project/R shiny/completed rasters/mojgenediversity.tif" )
plot(mojgenediversity.rescale.r)

dist2water.resamp.r<-resample(dist2water.r,TDI.r)
ext(dist2water.resamp.r)<-ext(TDI.r)
dist2water.rescale.r<-tr.rescale(dist2water.resamp.r,0,1)
writeRaster(dist2water.rescale.r, "Z:/Documents/Masters Project/R shiny/completed rasters/dist2water.tif")
plot(dist2water.rescale.r)

TDI.rescale.r<-tr.rescale(TDI.r,0,1)
writeRaster(TDI.rescale.r, "Z:/Documents/Masters Project/R shiny/completed rasters/TDI.tif",overwrite=TRUE)
TDI.r<-rast("Z:/Documents/Masters Project/R shiny/completed rasters/TDI.tif")
plot(TDI.r)

dist2roads.resamp.r<-resample(dist2roads.r, TDI.r)
ext(dist2roads.resamp.r)<-ext(TDI.r)
dist2roads.rescale.r<-tr.rescale(dist2roads.resamp.r,0,1)
writeRaster(dist2roads.rescale.r,"Z:/Documents/Masters Project/R shiny/completed rasters/dist2roads.tif")
plot(dist2roads.rescale.r)  

mojseqdiversity.resamp.r<-resample(mojseqdiversity.r, TDI.r)
ext(mojseqdiversity.resamp.r)<-ext(TDI.r)
mojseqdiversity.rescale.r<-tr.rescale(mojseqdiversity.resamp.r,0,1)
writeRaster(mojseqdiversity.rescale.r,"Z:/Documents/Masters Project/R shiny/completed rasters/mojseqdiversity.tif", overwrite=TRUE)
plot(mojseqdiversity.rescale.r)

mojseqdivergence.resample.r<-resample(mojseqdivergence.r, TDI.r)
ext(mojseqdivergence.resample.r)<-ext(TDI.r)
mojseqdivergence.rescale.r<-tr.rescale(mojseqdivergence.resample.r,0,1)
writeRaster(mojseqdivergence.rescale.r,"Z:/Documents/Masters Project/R shiny/completed rasters/mojseqdivergence.tif")
plot(mojseqdivergence.rescale.r)

probfire.resample.r<-resample(probfire.r,TDI.r)
ext(probfire.resample.r)<-ext(TDI.r)
probfire.rescale.r<-tr.rescale(probfire.resample.r,0,1)
writeRaster(probfire.rescale.r,"Z:/Documents/Masters Project/R shiny/completed rasters/probfire.tif", overwrite=TRUE)
plot(probfire.rescale.r)

forestNEP.resample.r<-resample(forestNEP.r,TDI.r)
ext(forestNEP.resample.r)<-ext(TDI.r)
forestNEP.rescale.r<-tr.rescale(forestNEP.resample.r,0,1)
writeRaster(forestNEP.rescale.r,"Z:/Documents/Masters Project/R shiny/completed rasters/forestNEP.tif", overwrite=TRUE)
plot(forestNEP.rescale.r)
#lab4<-data.frame(id=1:3, cover=c("Low Potential NEP", "Medium Potential NEP", "High Potential NEP"))
#levels(forestNEP.rescale.r)<-lab4 
#plot(forestNEP.rescale.r)
#text(forestNEP.rescale.r, digits=3, cex=.75, halo=TRUE)

carbonpriority.resample.r<-resample(carbonpriority.r,TDI.r)
ext(carbonpriority.resample.r)<-ext(TDI.r)
carbonpriority.rescale.r<-tr.rescale(carbonpriority.resample.r,0,1)
writeRaster(carbonpriority.rescale.r,"Z:/Documents/Masters Project/R shiny/completed rasters/carbonpriority.tif")
plot(carbonpriority.rescale.r)


writeRaster(natrails.r,"Z:/Documents/Masters Project/R shiny/completed rasters/natrails.tif", overwrite=TRUE)

writeRaster(DRECPbaselayers.r,"Z:/Documents/Masters Project/R shiny/completed rasters/DRECPbaselayers.tif", overwrite=TRUE)

writeRaster(landuse.r,"Z:/Documents/Masters Project/R shiny/completed rasters/landuse.tif",overwrite=TRUE)

#writeRaster(transmissionDFAs.r,"Z:/Documents/Masters Project/R shiny/completed rasters/transmissionDFAs.tif",overwrite=TRUE)

#writeRaster(proposedDFAs.r,"Z:/Documents/Masters Project/R shiny/completed rasters/proposedDFAs.tif",overwrite=TRUE)

writeRaster(finalDFAs.r,"Z:/Documents/Masters Project/R shiny/completed rasters/finalDFAs.tif",overwrite=TRUE)

writeRaster(ohv.r,"Z:/Documents/Masters Project/R shiny/completed rasters/ohv.tif",overwrite=TRUE)

writeRaster(roads.r, "Z:/Documents/Masters Project/R shiny/completed rasters/roads.tif",overwrite=TRUE)

writeRaster(railroads.r,"Z:/Documents/Masters Project/R shiny/completed rasters/railroads.tif",overwrite=TRUE)

writeRaster(DThablink.r,"Z:/Documents/Masters Project/R shiny/completed rasters/DThablink.tif",overwrite=TRUE)

writeRaster(tconnectrank.r,"Z:/Documents/Masters Project/R shiny/completed rasters/tconnectrank.tif",overwrite=TRUE)

spbiodiv.resample.r<-resample(spbiodiv.r,TDI.r)
ext(spbiodiv.resample.r)<-ext(TDI.r)
spbiodiv.rescale.r<-tr.rescale(spbiodiv.resample.r,0,1)
writeRaster(spbiodiv.rescale.r,"Z:/Documents/Masters Project/R shiny/completed rasters/spbiodiv.tif")
plot(spbiodiv.rescale.r)

aqbiodiv.resample.r<-resample(aqbiodiv.r,TDI.r)
ext(aqbiodiv.resample.r)<-ext(TDI.r)
aqbiodiv.rescale.r<-tr.rescale(aqbiodiv.resample.r,0,1)
writeRaster(aqbiodiv.rescale.r,"Z:/Documents/Masters Project/R shiny/completed rasters/aqbiodiv.tif")
plot(aqbiodiv.rescale.r)

aqirrepl.resample.r<-resample(aqirrepl.r,TDI.r)
ext(aqirrepl.resample.r)<-ext(TDI.r)
aqirrepl.rescale.r<-tr.rescale(aqirrepl.resample.r,0,1)
writeRaster(aqirrepl.rescale.r,"Z:/Documents/Masters Project/R shiny/completed rasters/aqirrepl.tif")
plot(aqirrepl.rescale.r)

aqraresprich.resample.r<-resample(aqraresprich.r,TDI.r)
ext(aqraresprich.resample.r)<-ext(TDI.r)
aqraresprich.rescale.r<-tr.rescale(aqraresprich.resample.r,0,1)
writeRaster(aqraresprich.rescale.r,"Z:/Documents/Masters Project/R shiny/completed rasters/aqraresprich.tif")
plot(aqraresprich.rescale.r)

aqnatsprich.resample.r<-resample(aqnatsprich.r,TDI.r)
ext(aqnatsprich.resample.r)<-ext(TDI.r)
aqnatsprich.rescale.r<-tr.rescale(aqnatsprich.resample.r,0,1)
writeRaster(aqnatsprich.rescale.r,"Z:/Documents/Masters Project/R shiny/completed rasters/aqnatsprich.tif")
plot(aqnatsprich.rescale.r)
plot(aqnatsprich.r)

writeRaster(tbiodiv.r,"Z:/Documents/Masters Project/R shiny/completed rasters/tbiodiv.tif",overwrite=TRUE)

tirrepl.resample.r<-resample(tirrepl.r,TDI.r)
ext(tirrepl.resample.r)<-ext(TDI.r)
tirrepl.rescale.r<-tr.rescale(tirrepl.resample.r,0,1)
writeRaster(tirrepl.rescale.r,"Z:/Documents/Masters Project/R shiny/completed rasters/tirrepl.tif")
plot(tirrepl.rescale.r)
plot(tirrepl.r)

traresprich.resample.r<-resample(traresprich.r,TDI.r)
ext(traresprich.resample.r)<-ext(TDI.r)
traresprich.rescale.r<-tr.rescale(traresprich.resample.r,0,1)
writeRaster(traresprich.rescale.r,"Z:/Documents/Masters Project/R shiny/completed rasters/traresprich.tif")
traresprich2.r<-rast("Z:/Documents/Masters Project/R shiny/completed rasters/traresprich.tif")
plot(traresprich2.r)
plot(traresprich.r)

tnatgamesp.resample.r<-resample(tnatgamesp.r,TDI.r)
ext(tnatgamesp.resample.r)<-ext(TDI.r)
tnatgamesp.rescale.r<-tr.rescale(tnatgamesp.resample.r,0,1)
writeRaster(tnatgamesp.rescale.r,"Z:/Documents/Masters Project/R shiny/completed rasters/tnatgamesp.tif")
plot(tnatgamesp.rescale.r)
plot(tnatgamesp.r)

tclimvulsp.resample.r<-resample(tclimvulsp.r,TDI.r)
ext(tclimvulsp.resample.r)<-ext(TDI.r)
tclimvulsp.rescale.r<-tr.rescale(tclimvulsp.resample.r,0,1)
writeRaster(tclimvulsp.rescale.r,"Z:/Documents/Masters Project/R shiny/completed rasters/tclimvulsp.tif")
plot(tclimvulsp.rescale.r)
plot(tclimvulsp.r)

writeRaster(tnatsprich.r,"Z:/Documents/Masters Project/R shiny/completed rasters/tnatsprich.tif",overwrite=TRUE)

writeRaster(mojcolconservalues.r,"Z:/Documents/Masters Project/R shiny/completed rasters/mojcolconservalues.tif",overwrite=TRUE)

writeRaster(consintactcmbnd.r,"Z:/Documents/Masters Project/R shiny/completed rasters/consintactcmbnd.tif",overwrite=TRUE)

writeRaster(usfwscrithab.r,"Z:/Documents/Masters Project/R shiny/completed rasters/usfwscrithab.tif",overwrite=TRUE)

resilencerank.resample.r<-resample(resilencerank.r,TDI.r)
ext(resilencerank.resample.r)<-ext(TDI.r)
resilencerank.rescale.r<-tr.rescale(resilencerank.resample.r,0,1)
writeRaster(resilencerank.rescale.r,"Z:/Documents/Masters Project/R shiny/completed rasters/resilencerank.tif")
plot(resilencerank.rescale.r)
plot(resilencerank.r)

writeRaster(aquifers.r,"Z:/Documents/Masters Project/R shiny/completed rasters/aquifers.tif",overwrite=TRUE)

writeRaster(rivcreek.r,"Z:/Documents/Masters Project/R shiny/completed rasters/rivcreek.tif",overwrite=TRUE)

writeRaster(noPlaya.r,"Z:/Documents/Masters Project/R shiny/completed rasters/noPlaya.tif",overwrite=TRUE)

writeRaster(waterbodies.r,"Z:/Documents/Masters Project/R shiny/completed rasters/waterbodies.tif",overwrite=TRUE)

