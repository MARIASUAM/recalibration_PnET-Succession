# Sensitivity Analysis of PnET-Succession - hydrological parameters of ecoregions

# Settings
# w_dir <- "/Users/maria.suarez.munoz/Library/CloudStorage/GoogleDrive-msuamun@go.ugr.es/My Drive/proj_LANDIS/calibration/recalibration_PnET/single_cohort_simulations/"# From Mac
w_dir <- "Z:/Google Drive/proj_LANDIS/calibration/recalibration_PnET/single_cohort_simulations/" # From Windows
setwd(w_dir)
library(dplyr)

# Define parameters space
PrecLossFrac <- data.frame(PrecLossFrac = c(0.25, 0.5, 0.75)) # 
LeakageFrac <- data.frame(LeakageFrac = c(0.25, 0.5, 0.75)) # 
PrecIntConst <- data.frame(PrecIntConst = c(0.1))
RootingDepth <- data.frame(RootingDepth = c(125))
Soiltype <- data.frame(Soiltype = c("SALO"))
Precipitation <- data.frame(Precipitation = c("../0_climate_inputs/high_prec_station_5506_year_2002.txt", "../0_climate_inputs/intermedia_prec_station_5506_year_2006.txt", "../0_climate_inputs/low_prec_station_5506_year_1994.txt"))
                              # 

params_space <- full_join(PrecLossFrac, LeakageFrac, by = character()) %>%
  full_join(PrecIntConst, by = character()) %>%
  full_join(RootingDepth, by = character()) %>%
  full_join(Soiltype, by = character()) %>%
  full_join(Precipitation, by = character())

# Loop over parameters space and run simulations
for(i in 1:length(params_space[,1])){
  ecoreg_txt <- c('LandisData	EcoregionParameters', # first line
                  'EcoregionParameters	SoilType	Latitude	RootingDepth	PrecLossFrac	LeakageFrac	SnowSublimFrac	ClimateFileName', # second line
                  paste("eco1", params_space$Soiltype[i],	37.2, params_space$RootingDepth[i], params_space$PrecLossFrac[i], params_space$LeakageFrac[i],	0.15,	params_space$Precipitation[i], sep = " ")) # third line
  writeLines(ecoreg_txt, paste0(w_dir, "12_ecoreg_parameters.txt"))
  
  source("Z:/Google Drive/proj_LANDIS/calibration/scripts_desfutur/single_cohort_sims.R")
}
