# SCRIPT TO LOAD, PLOT, AND EXPORT CURRENT VS. CRITICAL N SURPLUS IN GLOBAL SURFACE WATER MAP IN R
#
# LOAD libraries & P/runoff data
#
# Set working directory & load libraries
setwd("C:/Users/David/Earth Commission ESBs/N surplus map") #change to appropriate path
library(raster)
library(ncdf4)
library(RNetCDF)
library(ggplot2)
library(ggthemes)
library(tidyverse)
#
# Load current vs. critical N surplus data
N_surplusvscrit_raster <- raster("exc_nsur_crit_mi_all_ph.asc") #data file in swetwd path above
N_surplusvscrit_raster_df <- as.data.frame(rasterToPoints(N_surplusvscrit_raster))
colnames(N_surplusvscrit_raster_df) <- c("x","y","Nsurplus")
world <- map_data("world")
#
# PLOT current vs. critical N surplus data
#
N_relsurplus_plot <- ggplot() +
  geom_map(data = world, map = world,aes(long, lat, map_id = region),color = NA, fill = "lightgrey", size = 0.1) +
  geom_raster(data=N_surplusvscrit_raster_df,aes(x=x,y=y,fill=cut(Nsurplus,breaks=c(-300,-100,-50,0,50,100,300)))) + 
  coord_quickmap() + #comment out this line & uncomment next for proper (but slow) map projection
  #coord_map('rectangular',lat(0=30) + 
  scale_fill_manual(name=bquote("Current minus \nCritical N Surplus (kgN"~ha^-1~y^-1~")"),values=c('midnightblue','blue','light blue','lightsalmon','red','dark red'),labels=c('< -100','-100~-50','-50~0','0~50','50~100','>100')) +
  theme_map() +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme(legend.title=element_text(colour="black",size=15,face="bold")) + 
  theme(legend.text=element_text(colour="black",size=15)) + 
  theme(legend.position=c(0,0)) + 
  theme(legend.key.width=unit(3,"cm"))
#
# EXPORT R plot
#
save(N_relsurplus_plot,file="N_relsurplus_plot.RData")