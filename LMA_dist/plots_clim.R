rm(list = ls())

library(geodata)
library(terra)
library(foreign)
library(dplyr)

setwd("C:/Users/micho/OneDrive/Documentos/PHD/PhD")

fgm_calc <- function(fgd){
  fgm <- round((12*(fgd/365)) %% 12)
  fgm <- ifelse(is.na(fgm), 0, fgm)
  
  return(fgm)
}

gsl_m_calc <- function(gsl){
  gsl_m <- round(gsl/30.41)
  return(gsl_m)
}


# read LMA data rasters
dbf_NEON <- read.dbf("./NEON/NEON_trait_plot_locations.dbf") ## dbf file with NEON plot IDs and coords 
all_data <- read.csv("./allData.csv") ## LMA data for all plots
names(all_data)[2] <- "plotID" ## Change plotID col name to match dbf files

# join LMA data with 


neon <- vect("./NEON/NEON_trait_plot_locations.shp")
#global <- vect("./global/global_forest_plots_betterLocations.shp")

# Using Treelim growing season estimates to choose relevant months
#First day of growing season (0-365)
fgd_rast <- rast("D:/CHELSEA/CHELSA_fgd_1981-2010_V.2.1.tif") 

#Growing season length (1-365)
gsl_rast <- rast("D:/CHELSEA/CHELSA_gsl_1981-2010_V.2.1.tif")

# Monthly precipitation rasters
#prec_rast <- rast("D:/CHELSEA/CHELSA_gsp_1981-2010_V.2.1.tif")

# Accumulated growing season precipitation raster
gsp_rast <- rast("D:/CHELSEA/CHELSA_gsp_1981-2010_V.2.1.tif")

# Monthly RH rasters
hurs1_rast <- rast("D:/CHELSEA/CHELSA_hurs_01_2018_V.2.1.tif")
hurs2_rast <- rast("D:/CHELSEA/CHELSA_hurs_02_2018_V.2.1.tif")
hurs3_rast <- rast("D:/CHELSEA/CHELSA_hurs_03_2018_V.2.1.tif")
hurs4_rast <- rast("D:/CHELSEA/CHELSA_hurs_04_2018_V.2.1.tif")
hurs5_rast <- rast("D:/CHELSEA/CHELSA_hurs_05_2018_V.2.1.tif")
hurs6_rast <- rast("D:/CHELSEA/CHELSA_hurs_06_2018_V.2.1.tif")
hurs7_rast <- rast("D:/CHELSEA/CHELSA_hurs_07_2018_V.2.1.tif")
hurs8_rast <- rast("D:/CHELSEA/CHELSA_hurs_08_2018_V.2.1.tif")
hurs9_rast <- rast("D:/CHELSEA/CHELSA_hurs_09_2018_V.2.1.tif")
hurs10_rast <- rast("D:/CHELSEA/CHELSA_hurs_10_2018_V.2.1.tif")
hurs11_rast <- rast("D:/CHELSEA/CHELSA_hurs_11_2018_V.2.1.tif")
hurs12_rast <- rast("D:/CHELSEA/CHELSA_hurs_12_2018_V.2.1.tif")

# Mean temperature of growing season

gst_rast <- rast("D:/CHELSEA/CHELSA_gst_1981-2010_V.2.1.tif")


# Extract clim
NEON_clim <- extract(gsl_rast, neon, bind=TRUE)
NEON_clim <- extract(fgd_rast, NEON_clim, bind=TRUE)
NEON_clim <- extract(gst_rast, NEON_clim, bind=TRUE)
NEON_clim <- extract(gsp_rast, NEON_clim, bind=TRUE)

clim_df <- as.data.frame(NEON_clim)
clim_df['fgm'] <- sapply(clim_df$CHELSA_fgd_1981.2010_V.2.1, fgm_calc)
clim_df['gsl_m'] <- sapply(clim_df$CHELSA_gsl_1981.2010_V.2.1, gsl_m_calc)

# Monthly avg RH rasters
NEON_hurs <- extract(hurs1_rast, neon, bind=TRUE)
NEON_hurs <- extract(hurs2_rast, NEON_hurs, bind=TRUE)
NEON_hurs <- extract(hurs3_rast, NEON_hurs, bind=TRUE)
NEON_hurs <- extract(hurs4_rast, NEON_hurs, bind=TRUE)
NEON_hurs <- extract(hurs5_rast, NEON_hurs, bind=TRUE)
NEON_hurs <- extract(hurs6_rast, NEON_hurs, bind=TRUE)
NEON_hurs <- extract(hurs7_rast, NEON_hurs, bind=TRUE)
NEON_hurs <- extract(hurs8_rast, NEON_hurs, bind=TRUE)
NEON_hurs <- extract(hurs9_rast, NEON_hurs, bind=TRUE)
NEON_hurs <- extract(hurs10_rast, NEON_hurs, bind=TRUE)
NEON_hurs <- extract(hurs11_rast, NEON_hurs, bind=TRUE)
NEON_hurs <- extract(hurs12_rast, NEON_hurs, bind=TRUE)

hurs_df <- as.data.frame(NEON_hurs)

clim_df['gshurs'] <- rep(0, nrow(clim_df))

for(r in 1:nrow(clim_df)){
  fm <- clim_df[r,]$fgm
  lm <- clim_df[r,]$gsl_m + fm
  gslm <- clim_df[r,]$gsl_m
  months <- c((fm):(lm-1))%%12 + 4
  temp <- rowSums(hurs_df[r, months])/gslm
  clim_df[r, 'gshurs'] <- temp
}

sfcWind1_rast <- rast("D:/CHELSEA/CHELSA_sfcWind_01_1981-2010_V.2.1.tif")
sfcWind2_rast <- rast("D:/CHELSEA/CHELSA_sfcWind_01_1981-2010_V.2.1.tif")
sfcWind3_rast <- rast("D:/CHELSEA/CHELSA_sfcWind_01_1981-2010_V.2.1.tif")
sfcWind4_rast <- rast("D:/CHELSEA/CHELSA_sfcWind_01_1981-2010_V.2.1.tif")
sfcWind5_rast <- rast("D:/CHELSEA/CHELSA_sfcWind_01_1981-2010_V.2.1.tif")
sfcWind6_rast <- rast("D:/CHELSEA/CHELSA_sfcWind_01_1981-2010_V.2.1.tif")
sfcWind7_rast <- rast("D:/CHELSEA/CHELSA_sfcWind_01_1981-2010_V.2.1.tif")
sfcWind8_rast <- rast("D:/CHELSEA/CHELSA_sfcWind_01_1981-2010_V.2.1.tif")
sfcWind9_rast <- rast("D:/CHELSEA/CHELSA_sfcWind_01_1981-2010_V.2.1.tif")
sfcWind10_rast <- rast("D:/CHELSEA/CHELSA_sfcWind_01_1981-2010_V.2.1.tif")
sfcWind11_rast <- rast("D:/CHELSEA/CHELSA_sfcWind_01_1981-2010_V.2.1.tif")
sfcWind12_rast <- rast("D:/CHELSEA/CHELSA_sfcWind_01_1981-2010_V.2.1.tif")


NEON_sfcWind <- extract(sfcWind1_rast, neon, bind=TRUE)
NEON_sfcWind <- extract(sfcWind2_rast, NEON_sfcWind, bind=TRUE)
NEON_sfcWind <- extract(sfcWind3_rast, NEON_sfcWind, bind=TRUE)
NEON_sfcWind <- extract(sfcWind4_rast, NEON_sfcWind, bind=TRUE)
NEON_sfcWind <- extract(sfcWind5_rast, NEON_sfcWind, bind=TRUE)
NEON_sfcWind <- extract(sfcWind6_rast, NEON_sfcWind, bind=TRUE)
NEON_sfcWind <- extract(sfcWind7_rast, NEON_sfcWind, bind=TRUE)
NEON_sfcWind <- extract(sfcWind8_rast, NEON_sfcWind, bind=TRUE)
NEON_sfcWind <- extract(sfcWind9_rast, NEON_sfcWind, bind=TRUE)
NEON_sfcWind <- extract(sfcWind10_rast, NEON_sfcWind, bind=TRUE)
NEON_sfcWind <- extract(sfcWind11_rast, NEON_sfcWind, bind=TRUE)
NEON_sfcWind <- extract(sfcWind12_rast, NEON_sfcWind, bind=TRUE)

sfcWind_df <- as.data.frame(NEON_sfcWind)

clim_df['gs_sfcWind'] <- rep(0, nrow(clim_df))

for(r in 1:nrow(clim_df)){
  fm <- clim_df[r,]$fgm
  lm <- clim_df[r,]$gsl_m + fm
  gslm <- clim_df[r,]$gsl_m
  months <- c((fm):(lm-1))%%12 + 4
  temp <- rowSums(sfcWind_df[r, months])/gslm
  clim_df[r, 'gs_sfcWind'] <- temp
}

GEDI_rast <- sfcWind12_rast <- rast("D:/GEDI/gedi_rh90_1000m.tif")

plot(GEDI_rast)

NEON_height <- extract(GEDI_rast, neon, bind=TRUE)

height_df <- as.data.frame(NEON_height)

NEON_data <- inner_join(clim_df, height_df)



# joining climate and trait data
NEON_full <- left_join(NEON_data, all_data, by=join_by(plotID)) # join plot coords with LMA data
NEON_full_shp <- vect(NEON_full, geom=c("longitude", "latitude"), crs="epsg:4326") 

write.csv(NEON_full, "./NEON_full.csv")

