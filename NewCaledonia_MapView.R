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
library(phytools)
# this post is very useful for understanding sf objects
# https://www.jessesadler.com/post/simple-feature-objects/#:~:text=The%20most%20likely%20way%20to,an%20sf%20object%20from%20scratch.



la_sfg <- st_point(c(-118.2615805, 34.1168926))
amsterdam_sfg <- st_point(c(4.8979755, 52.3745403))

points_sfc <- st_sfc(la_sfg, amsterdam_sfg, crs = 4326)
data <- data.frame(name = c("Los Angeles", "Amsterdam"),
                   language = c("English", "Dutch"),
                   weather = c("sunny", "rainy/cold"))

all<-st_sf(cbind(data, points_sfc))
mapview(all)
mapview(all, zcol = c("weather", "language"))


diospyros_localities<-read_tsv("/Users/katieemelianova/Desktop/Diospyros/diospyros_plots/Diospyros_Localities.tsv")
diospyros_localities %<>% filter(Latitude != 0 & !(is.na(Soil)))


test<-st_as_sf(x = diospyros_localities, coords = c("Longitude", "Latitude"), crs=4642)

cols<-brewer.pal(7, "Set1")
# some nice coloirs that got picked which happened to look nice
#cols<-c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628")

mapview(test, zcol="Soil", col.regions = cols)


crossing(c("max ice", "min ice", "no ice"), c("ziploc", "open"), c("tube", "tissue"))


# read in original tree
nwk<-ape::read.tree("/Users/katieemelianova/Desktop/Diospyros/diospyros_plots/RadiatingSpeciesDiospyros_ingrp-inds.nwk")

# plot the tree in cirlular
ggtree(nwk, layout="circular")

# split the tip name column by BT to get the population names
# I dont understand the format of this function and I dont like it at all
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




