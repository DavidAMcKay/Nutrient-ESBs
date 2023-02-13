# SCRIPT TO LOAD, CALCULATE, AND PLOT P CONCENTRATION IN GLOBAL SURFACE WATER MAP IN R
#
# LOAD libraries & P/runoff data
#
# Set working directory & load libraries
setwd("C:/Users/David/Earth Commission ESBs/P concentration map") #change to appropriate
library(raster)
library(ncdf4)
library(RNetCDF)
library(ggplot2)
library(ggthemes)
library(tidyverse)
#
# Load and plot P data
P <- stack("p_load_kgha") #data file in setwd path above
plot(P,zlim=c(0,5),main="P load")
#
# Load WBM runoff data
Runoff_raster <- raster("MekonnenHoekstra/Mekonnen_Maps/WBM_TerraClimate_RO-MM_DIST_aLTM_2000-2020.tif") #load runoff data
Runoff_raster[Runoff_raster < 5] <- NA # remove low runoff values to mask anomalous values in low-flow regions
plot(Runoff_raster,main="WBM runoff") #plot to check
#
# Resample P resolution to runoff
res(P) #check P load resolution
res(Runoff_raster) #check runoff resolution
P.resample <- raster::resample(P,Runoff_raster) #resample P raster to resolution of runoff
res(P.resample) #check new P resolution
plot(P.resample,zlim=c(0,5),main="P load resampled") #plot new P resolution
#
# Calculate area of each 0.5x0.5 degree pixel
r <- raster(xmn=xmin(P.resample),xmx=xmax(P.resample),ymn=ymin(P.resample),
            ymx=ymax(P.resample),res=(res(P.resample))) #create raster with new P resolution
a <- area(r) #calculate area of raster pixels in km^2
area <- a[,1] #extract area of each cell from raster
#
# CALCULATE P concentration from P load and discharge
#
# Calculate discharge per cell from runoff
discharge_percell <- (Runoff_raster/1000)*(area*1000000) #runoff mm/yr, cell area km2 => discharge m^3/cell/yr
plot(discharge_percell,zlim=c(0,1000000),main="Discharge") #plot to check
#
# Calculate P load per cell from cell area
P_percell <- (P.resample/10000)*(area*1000000) #load kgP/ha/yr, cell area km^2 => load kgP/yr in cell
plot(P_percell,zlim=c(0,10000),main="P load per cell") #plot to check
#
# Calculate P concentration from P load and discharge per cel
P_concn <- (P_percell/discharge_percell)*1000000 #load kgP/yr, discharge m3/yr => concn mgP/m3
plot(P_concn,zlim=c(0,1000),main="P concentration") #plot to check
#
# PLOT P concentration in runoff
#
# Pre-plot data cleanup
P_concn_df <- as.data.frame(P_concn, xy = TRUE) #create P concn dataframe
colnames(P_concn_df) <- c("x","y","P") #label P concn df columns
P_concn_df[P_concn_df[,3] < 0 & !is.na(P_concn_df[,3]), 3] <- NA #replace any negative values with NA
P_concn_df[is.infinite(P_concn_df[,3]), 3] <- NA #replace any infinite values with NA
P_concn_df[(P_concn_df[,3])>10000 & !is.na(P_concn_df[,3]), 3] <- NA #exclude very high P concs as likely anomalies (e.g. where v. low discharge)
world <- map_data("world") #load world map for plotting
#
# ggplot
P_concn_plot <- ggplot() +
  geom_map(data = world, map = world,aes(long, lat, map_id = region),color = NA, fill = "lightgrey", size = 0.1) +
  geom_raster(data=P_concn_df,aes(x=x,y=y,fill=cut(P,breaks=c(0,50,100,200,500,10000000)))) + 
  coord_quickmap() + #comment out this line & uncomment next for proper (but slow) map projection
  #coord_map('rectangular',lat(0=30) +
  scale_fill_manual(name=bquote("P concentration (mgP"~m^-3~")"),values=c('limegreen','orange','red','purple','black'),labels=c('0-50','50-100','100-200','200-500','>500')) +
  theme_map() +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme(legend.title=element_text(colour="black",size=15,face="bold")) + 
  theme(legend.text=element_text(colour="black",size=15)) + 
  theme(legend.position=c(0,0)) + 
  theme(legend.key.width=unit(3,"cm"))
#
# EXPORT data
#
setwd("C:/Users/David/Dropbox/EarthComm/NP Quants/P concn map")
save(P_concn_plot,file="P_concn_plot.RData")
P_concn[P_concn > 10000] <- NA #exclude v high values as likely anomalies
save(P_concn, file = "P_concn.RData")
writeRaster(P_concn, "MH2018_Pconcn_runoffcutoff5_concngt10k.nc", overwrite=TRUE, format="CDF",
            varname="P concentration", varunit="mgP/m3", 
            longname="Anthropogenic Phosphorus concentration in surface water 2002-2010",
            xname="Longitude", yname="Latitude")
writeRaster(P_concn, "MH2018_Pconcn_runoffcutoff5_concngt10k.tif", format = "GTiff")
#