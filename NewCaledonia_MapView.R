library(sf)
library(mapview)
library(readr)
library(dplyr)
library(magrittr)
library(RColorBrewer)
library(tidyr)
library(ggtree)
library(ape)
library(stringr)
# this post is very useful for understanding sf objects
# https://www.jessesadler.com/post/simple-feature-objects/#:~:text=The%20most%20likely%20way%20to,an%20sf%20object%20from%20scratch.


######################################################################################
#  Following an online very basic example to get an idea how to set up a map object  #
######################################################################################
la_sfg <- st_point(c(-118.2615805, 34.1168926))
amsterdam_sfg <- st_point(c(4.8979755, 52.3745403))

points_sfc <- st_sfc(la_sfg, amsterdam_sfg, crs = 4326)
data <- data.frame(name = c("Los Angeles", "Amsterdam"),
                   language = c("English", "Dutch"),
                   weather = c("sunny", "rainy/cold"))
all<-st_sf(cbind(data, points_sfc))
mapview(all)
mapview(all, zcol = c("weather", "language"))

######################################################################################
#  Now trying with Diospyros: I got the latitude longitude from the spreadsheet      #
######################################################################################

# read in a modified version of the online spreadsheet; I removed some columns and renamed some of the soil types to match
# e.g. Seprpentines and Serpetine both become Serpentine
diospyros_localities<-read_tsv("/Users/katieemelianova/Desktop/Diospyros/diospyros_plots/Diospyros_Localities.tsv")

# Remove fields which dont have a location and dont have soiltype info
diospyros_localities %<>% filter(Latitude != 0 & !(is.na(Soil)))

# use the st_as_sf function which does what the test example functions did but over a data frame of objects
# crs stands for coordinate reference system and this needs to be correct for lat/long to be mapped correctly
# I got the NCcal crs from here after trying a few of them out:
# https://epsg.io/?q=New+Caledonia
diospyros_sfc<-st_as_sf(x = diospyros_localities, coords = c("Longitude", "Latitude"), crs=4642)

# pick some colours to match the number of soiltypes we have (7)
cols<-brewer.pal(7, "Set1")
# some nice colors that got picked which happened to look nice
#cols<-c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628")

# map the locations, colouring by soiltype
mapview(diospyros_sfc, zcol="Soil", col.regions = cols)


######################################################################################
#     Got the Newick tree fron Teerna's tree and trying to plot it with ggtree       #
######################################################################################

# read in original tree
nwk<-ape::read.tree("/Users/katieemelianova/Desktop/Diospyros/diospyros_plots/RadiatingSpeciesDiospyros_ingrp-inds.nwk")

# plot the tree in cirlular
ggtree(nwk, layout="circular")

# split the tip name column by BT to get the population names
# I dont understand the format of this function and I dont like it at all
# but it works so whatevs
species<-sapply(strsplit(nwk$tip.label,"BT"), `[`, 1)  

# set the species in the newick object
nwk$species <- species

# make a list where item name is species name and objects within are the tip labels belonging to that species
groupInfo<-split(nwk$tip.label, nwk$species)

# use groupOTU to group the tips by species
nwk_grouped<-groupOTU(nwk, groupInfo, group_name = "species")
ggtree(nwk_grouped, aes(color=species), layout='circular') + geom_tiplab(size=2.5) + theme(legend.position = "None")
dev.off()
  

# this is a  handy bit of code to make up a random tree and plot it in different ways
#tr <- rtree(10)
#tr$tip.label = gsub('^', 'A long label: ', tr$tip.label)
#ggtree(tr, branch.length = 'none') + geom_tiplab()




