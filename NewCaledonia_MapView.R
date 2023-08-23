library(sf)
library(mapview)
library(readr)
library(dplyr)
library(magrittr)
library(RColorBrewer)
library(tidyr)
library(ggtree)
#library(ape)
library(stringr)
library(reshape2)
library(ggnewscale)
library(ggtreeExtra)
library(ggplot2)
library(cowplot)
library(gridExtra)
library(grid)
library(ggpubr)

# I had some issues installing terra, one of the things was that the timeout limit was being reached
# When I extended this time limit it completed fine
#options(timeout=100)
#install.packages("terra")


packageurl <- "https://cran.r-project.org/src/contrib/Archive/terra/terra_1.7-3.tar.gz"

install.packages(packageurl, repos=NULL, type="source")

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
# update 22.08.23: we realised that a lot of the soil labels were incorrect and were taken from field notes. Correct mappings of soil to sample 
# are in the Paun et al. 2016 supplementary figure 5, though in a coloir coded tree form
# I took the designations from supp fig 5 and re-annotated the soil column based on this
# where there is no population data, soiltype is simply transposed. Where there is population data but all individuals in the supp figure 
# are the same, I transpose that soil type of all samples in the table. Where there are a mix of soiltypes per species
# and not all individuals are available in the tree for the table, not-included individuals in the table are left blank for soiltype
diospyros_localities<-read_tsv("/Users/katieemelianova/Desktop/Diospyros/diospyros_plots/Diospyros_Localities_corrected2016.tsv")

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

# split the tip name column by BT to get the population names
# I dont understand the format of this function and I dont like it at all
# but it works so whatevs
# also do some data cleaning
species<-sapply(strsplit(nwk$tip.label,"BT"), `[`, 1)
nwk$tip.label[nwk$tip.label =="impolitaBt103"] = "impolitaBT103"
sample_id<-sapply(strsplit(nwk$tip.label,"BT"), `[`, 2)
sample_id<-paste("BT", sample_id, sep = "")
sample_id[sample_id == "BTNA"] = nwk$tip.label[sample_id =="BTNA"]
sample_id<-sapply(strsplit(sample_id, "_"), `[`, 1)
sample_id<-sapply(strsplit(sample_id, "-"), `[`, 1)

# set the species in the newick object
nwk$species <- species
nwk$sample_id<-sample_id



# take the original data frame from the map part and take the rad and BT sample name columns only
mapping_loc<-diospyros_localities %>% 
  dplyr::select(`RAD localities`, `sequenced samples`, Soil) %>% 
  data.frame()

# this is a really dumb way to do it but whatever it works
# apply over each row, split the BTXXX column by ", ", and then add it to a data frame, where the other column is the RAD number
# this gives you a list of data frames, which are bound into one using bind_rows
bt_soil_mapping<-apply(mapping_loc, 1, function(x) strsplit(x[2], ", ") %>% data.frame(rad=x[3])) %>% bind_rows()

# remove the weird rownames the apply function gives it
rownames(bt_soil_mapping) <-NULL
colnames(bt_soil_mapping)<-c("sample_id", "soil")

# remove duplicate samples
bt_soil_mapping_noduplicates <- bt_soil_mapping %>% filter(sample_id != "BT147" & sample_id != "BT296")

# join tip names and soil types
bt_soil_mapping_joined<-left_join(data.frame(sample_id=nwk$sample_id), bt_soil_mapping_noduplicates) %>% dplyr::select(soil)

#set rownames to corresponding tip labels
rownames(bt_soil_mapping_joined) <- nwk$tip.label


# make a list where item name is species name and objects within are the tip labels belonging to that species
groupInfo<-split(nwk$tip.label, nwk$species)

# use groupOTU to group the tips by species and plot
nwk_grouped<-groupOTU(nwk, groupInfo, group_name = "species")

# take the same tree as above but make the tip labels the species name only (no population info)
nwk_grouped_speciesonly<-nwk_grouped
nwk_grouped_speciesonly$tip.label <- nwk_grouped_speciesonly$species

# first make one tree which is grouped and coloured by species
circ<-ggtree(nwk_grouped, aes(color=species), layout='circular') + geom_tiplab(size=5) 

circ_soil<-gheatmap(circ, bt_soil_mapping_joined, offset=.012, width=.05, colnames=FALSE) + 
  scale_fill_viridis_d(option = "C", name = "Clade", na.value = "gray94") + theme(legend.position="none")

# make the legend for the soiltypes
circ_nolegend<-ggtree(nwk)
soiltype_values<-bt_soil_mapping_joined %>% dplyr::select(soil)


# repeat this for soiltype
soiltype_legend<-gheatmap(circ_nolegend, soiltype_values, offset = .01, width = 0.04,
                          colnames = FALSE,
                          colnames_offset_y = 1) +
  scale_fill_viridis_d(option = "C", na.value = "gray94", name = "soiltype"
                       #scale_fill_viridis_d(option = "C", na.value = "gray94", name = "Soiltype", labels = c("Ironcrust", "Kalcarious", "Serpentine", "Ultramafic", "Volcano-Sedimentary", "Unknown")
                       #scale_fill_manual(values=mycolors, name="Soiltype"
  ) + theme(legend.text=element_text(size=25), legend.title= element_blank())

# get the soiltype legend
soiltype_legend <- get_legend(soiltype_legend)

#pdf("tree_colour3.pdf", width=25, height=25)
ggarrange(circ_soil, soiltype_legend, widths = c(8,2))
#dev.off()


