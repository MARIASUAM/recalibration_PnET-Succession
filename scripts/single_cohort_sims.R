# Execute LANDIS-II simulation, document conditions and organise outputs

PnET_version <- "5.1.0.0"

# Settings
library(dplyr)
w_dir <- "/Volumes/GoogleDrive/My Drive/proj_LANDIS/calibration/recalibration_PnET/single_cohort_simulations/"# From Mac
w_dir <- "Z:/Google Drive/proj_LANDIS/calibration/recalibration_PnET/single_cohort_simulations/" # From Windows
setwd(w_dir)

# Run LANDIS-II 
shell('Run.bat')
print("Simulation finished")

# Assign simulation name and rename output folder
simulations <- list.files(pattern = "sim_")
simulation_nr <- length(simulations) + 1
simulation_name <- paste0("sim_", simulation_nr)
file.rename("output", simulation_name)

# Read simulation input files

scenario <- readtext::readtext(paste(w_dir, "1_scenario.txt", sep = ""))

initial_com <- readtext::readtext(paste(w_dir, "8_initial_communities.txt", sep = ""))
IC_spp <- strsplit(initial_com$text, split = "\t")[[1]][6]

pnet_succesion <- readtext::readtext(paste(w_dir, "5_PnET_succession.txt", sep = ""))

species_parameters <- read.table(paste(w_dir, "2_species.txt", sep = ""), skip = 5)
colnames(species_parameters) <- c("Name", "Longevity", "Sex_Mat", "Shade_Tol", "Fire_Tol", "Seed_Displ_Dist_Effective", "Seed_Displ_Dist_Max", "Vegetative_Reprod_Prob", "Sprout_Age_Min", "Sprout_Age_Max", "Post_Fire_Regen")
IC_sp_parameters <- species_parameters %>%
  filter(Name == IC_spp)

pnet_species_parameters <- read.table(paste(w_dir, "11_PnET_Species_Parameters.txt", sep = ""), skip = 3)
colnames(pnet_species_parameters) <- c("PnETSpeciesParameters", "FolN", "FracFol", "SLWmax", "FracBelowG", "TOfol", "HalfSat", "H4", "H3", "PsnTMin", "LeafOnMinT", "PsnTOpt", "PsnTMax", "SLWDel", "AmaxA", "AmaxB", "k", "EstRad")
IC_sp_PnET_parameters <- pnet_species_parameters %>%
  filter(PnETSpeciesParameters == IC_spp)

ecoregions_parameters <- read.table(paste(w_dir, "12_ecoreg_parameters.txt", sep = ""), skip = 2, header = FALSE)
colnames(ecoregions_parameters) <- c("EcoregionParameters", "SoilType", "Latitude", "RootingDepth", "PrecLossFrac", "LeakageFrac", "SnowSublimFrac", "ClimateFileName")

generic_parameters <- read.table(paste(w_dir, "10_PnET_Generic_Species_Parameters.txt", sep = ""), skip = 3)
colnames(generic_parameters) <- c("Parameter", "Value")

# Create table with simulation inputs
if(!is.na(IC_spp)){
  my_df <- data.frame(Simulation_name = simulation_name,
                      Nr_active_cells = 1,
                      Nr_ecoregions = 1,
                      Nr_initial_cohorts = 1,
                      PnET_version = PnET_version,
                      StartYear = strsplit(pnet_succesion$text, split = " ")[[1]][7],
                      Duration = strsplit(scenario$text, split = " ")[[1]][7],
                      IC_species = IC_spp,
                      IC_age = strsplit(initial_com$text, split = "\t")[[1]][7],
                      sp_FolN = IC_sp_PnET_parameters$FolN,
                      sp_FracFol = IC_sp_PnET_parameters$FracFol,
                      sp_SLWmax = IC_sp_PnET_parameters$SLWmax,
                      sp_FracBelowG = IC_sp_PnET_parameters$FracBelowG,
                      sp_Tofol = IC_sp_PnET_parameters$TOfol,
                      sp_HalfSat = IC_sp_PnET_parameters$HalfSat,
                      sp_H4 = IC_sp_PnET_parameters$H4,
                      sp_H3 = IC_sp_PnET_parameters$H3,
                      sp_PsnTMin = IC_sp_PnET_parameters$PsnTMin,
                      sp_LeafOnMinT = IC_sp_PnET_parameters$LeafOnMinT,
                      sp_PsnTOpt = IC_sp_PnET_parameters$PsnTOpt,
                      sp_PsnMax = IC_sp_PnET_parameters$PsnTMax,
                      sp_SLWDel = IC_sp_PnET_parameters$SLWDel,
                      sp_AmaxA = IC_sp_PnET_parameters$AmaxA,
                      sp_AmaxB = IC_sp_PnET_parameters$AmaxB,
                      sp_k = IC_sp_PnET_parameters$k,
                      sp_EstRad = IC_sp_PnET_parameters$EstRad,
                      sp_Longevity = IC_sp_parameters$Longevity,
                      sp_Sex_Mat = IC_sp_parameters$Sex_Mat,
                      sp_Shade_Tol = IC_sp_parameters$Shade_Tol,
                      sp_Fire_Tol = IC_sp_parameters$Fire_Tol,
                      sp_Seed_Displ_Effective = IC_sp_parameters$Seed_Displ_Dist_Effective,
                      sp_Dist_Max = IC_sp_parameters$Seed_Displ_Dist_Max,
                      sp_VegetativeReprodProb = IC_sp_parameters$Vegetative_Reprod_Prob,
                      sp_Sprout_Age_Min = IC_sp_parameters$Sprout_Age_Min,
                      sp_Sprout_Age_Max = IC_sp_parameters$Sprout_Age_Max,
                      sp_Post_Fire_Regen = IC_sp_parameters$Post_Fire_Regen,
                      ecoreg_SoilType = ecoregions_parameters$SoilType,
                      ecoreg_Latitude = ecoregions_parameters$Latitude,
                      ecoreg_RootingDepth = ecoregions_parameters$RootingDepth,
                      ecoreg_PrecLossFrac = ecoregions_parameters$PrecLossFrac,
                      ecoreg_LeakageFrac = ecoregions_parameters$LeakageFrac,
                      ecoreg_SnowSublimFrac = ecoregions_parameters$SnowSublimFrac,
                      ecoreg_ClimateFileName = ecoregions_parameters$ClimateFileName,
                      CellLength = strsplit(scenario$text, split = " ")[[1]][22],
                      DisturbanceRandomOrder = strsplit(scenario$text, split = " ")[[1]][71],
                      RandomNumberSeed = strsplit(scenario$text, split = " ")[[1]][102])
  }else 
    my_df <- data.frame(Simulation_name = simulation_name,
                        Nr_active_cells = 1,
                        Nr_ecoregions = 1,
                        Nr_initial_cohorts = 1,
                        PnET_version = PnET_version,
                        StartYear = strsplit(pnet_succesion$text, split = " ")[[1]][7],
                        Duration = strsplit(scenario$text, split = " ")[[1]][7],
                        IC_species = NA,
                        IC_age = NA,
                        sp_FolN = NA,
                        sp_FracFol = NA,
                        sp_SLWmax = NA,
                        sp_FracBelowG = NA,
                        sp_Tofol = NA,
                        sp_HalfSat = NA,
                        sp_H4 = NA,
                        sp_H3 = NA,
                        sp_PsnTMin = NA,
                        sp_LeafOnMinT = NA,
                        sp_PsnTOpt = NA,
                        sp_PsnMax = NA,
                        sp_SLWDel = NA,
                        sp_AmaxA = NA,
                        sp_AmaxB = NA,
                        sp_k = NA,
                        sp_EstRad = NA,
                        sp_Longevity = NA,
                        sp_Sex_Mat = NA,
                        sp_Shade_Tol = NA,
                        sp_Fire_Tol = NA,
                        sp_Seed_Displ_Effective = NA,
                        sp_Dist_Max = NA,
                        sp_VegetativeReprodProb = NA,
                        sp_Sprout_Age_Min = NA,
                        sp_Sprout_Age_Max = NA,
                        sp_Post_Fire_Regen = NA,
                        ecoreg_SoilType = ecoregions_parameters$SoilType,
                        ecoreg_Latitude = ecoregions_parameters$Latitude,
                        ecoreg_RootingDepth = ecoregions_parameters$RootingDepth,
                        ecoreg_PrecLossFrac = ecoregions_parameters$PrecLossFrac,
                        ecoreg_LeakageFrac = ecoregions_parameters$LeakageFrac,
                        ecoreg_SnowSublimFrac = ecoregions_parameters$SnowSublimFrac,
                        ecoreg_ClimateFileName = ecoregions_parameters$ClimateFileName,
                        CellLength = strsplit(scenario$text, split = " ")[[1]][22],
                        DisturbanceRandomOrder = strsplit(scenario$text, split = " ")[[1]][71],
                        RandomNumberSeed = strsplit(scenario$text, split = " ")[[1]][102])

# Add inputs into table
write.table(my_df, file="././registro.csv", append = T, sep=';', row.names=F, col.names=F)
