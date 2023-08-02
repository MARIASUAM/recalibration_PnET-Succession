# Analyse sensitivity analysis results

# Simulations to compare: baselines and all other sims
all_sims <- c("sim_8", "sim_9", "sim_10", "sim_11", "sim_12", "sim_13",
              "sim_14", "sim_15", "sim_16", "sim_17", "sim_18", "sim_19",
              "sim_20", "sim_21", "sim_22", "sim_23", "sim_24", "sim_25",
              "sim_26", "sim_27", "sim_28", "sim_29", "sim_30", "sim_31",
              "sim_32", "sim_33", "sim_34")  # baseline sims are excluded

baselines <- c("sim_20", "sim_21", "sim_22") # Baseline simulations with ref value of parameters and each climate file

sims <- c("sim_11", "sim_12", "sim_13",
          "sim_17", "sim_18", "sim_19",
          "sim_23", "sim_24", "sim_25",
          "sim_29", "sim_30", "sim_31")  # baseline sims are excluded

# General settings
library(dplyr)
library(ggplot2)
library(reshape2)
library(tidyr)
w_dir <- "/Users/maria.suarez.munoz/Library/CloudStorage/GoogleDrive-msuamun@go.ugr.es/My Drive/proj_LANDIS/calibration/" 

sim_params <- read.table(paste0(w_dir, "recalibration_PnET-Succession/single_cohort_simulations/registro.csv"), sep = ";", header = TRUE)
registro_simulaciones <- data.frame()
for(i in 1:length(all_sims)){
  registro_sim <- sim_params %>%
    filter(Simulation_name == all_sims[i])
  
  registro_simulaciones <- rbind(registro_simulaciones, registro_sim)
}

climates <- data.frame(ecoreg_ClimateFileName = c("../0_climate_inputs/high_prec_station_5506_year_2002.txt",
                                                  "../0_climate_inputs/intermedia_prec_station_5506_year_2006.txt",
                                                  "../0_climate_inputs/low_prec_station_5506_year_1994.txt"),
                       Precipitation = c("high", "intermediate", "low"))
# Extract simulation parameters
Check that simulations differ only in desired parameters

dif_params_values <- registro_simulaciones %>%
  select(Simulation_name,
         ecoreg_PrecLossFrac,
         ecoreg_LeakageFrac,
         ecoreg_ClimateFileName)

simulated_spp <- registro_simulaciones$IC_species[registro_simulaciones$Simulation_name == sims[1]]
StartYear <- registro_simulaciones$StartYear[registro_simulaciones$Simulation_name == sims[1]]
Duration <- registro_simulaciones$Duration[registro_simulaciones$Simulation_name == sims[1]]
FinalYear <- StartYear + Duration - 1

# Load site data
site_data <- data.frame()
for(i in 1:length(sims)){
  site_data <- rbind(site_data,
                     read.table(paste0(w_dir, "recalibration_PnET-Succession/single_cohort_simulations/", sims[i], "/Site1/Site.csv"), sep = ",", header = TRUE) %>% 
                     mutate(Simulation = sims[i]))
}

site_data_baselines <- data.frame()
for(i in 1:length(baselines)){
  site_data_baselines <- rbind(site_data_baselines,
                               read.table(paste0(w_dir, "recalibration_PnET-Succession/single_cohort_simulations/", baselines[i], "/Site1/Site.csv"), sep = ",", header = TRUE) %>%
                                 mutate(Simulation = baselines[i]))
}

# Calculate annual available water and fetch
avail_water <- site_data %>%
  select(Simulation, Year, Available.Water..mm.) %>%
  group_by(Simulation, Year) %>%
  summarise(aWater = sum(Available.Water..mm.)) %>%
  left_join(dif_params_values, by = c("Simulation" = "Simulation_name"))

avail_water_baselines <- site_data_baselines %>%
  rename(Simulation_baseline = Simulation) %>%
  select(Simulation_baseline, Year, Available.Water..mm.) %>%
  group_by(Simulation_baseline, Year) %>%
  summarise(aWater_baseline = sum(Available.Water..mm.)) %>%
  left_join(dif_params_values, by = c("Simulation_baseline" = "Simulation_name")) %>%
  rename(ecoreg_PrecLossFrac_baseline = ecoreg_PrecLossFrac,
         ecoreg_LeakageFrac_baseline = ecoreg_LeakageFrac)

# Calculate Sensitivity Index values (following Millares et al., 2019)
SI_PrecLossFrac <- avail_water %>%
  filter(Simulation == "sim_11" |
         Simulation == "sim_12" |
         Simulation == "sim_13" |
         Simulation == "sim_29" |
         Simulation == "sim_30" |
         Simulation == "sim_31") %>%
  left_join(avail_water_baselines) %>%
  mutate(SI = ((aWater - aWater_baseline)/aWater_baseline)/
           ((ecoreg_PrecLossFrac - ecoreg_PrecLossFrac_baseline) / ecoreg_PrecLossFrac_baseline)) %>%
  # filter(Year == FinalYear) %>%
  select(Simulation, Simulation_baseline, 
         SI, Year, aWater, aWater_baseline,
         ecoreg_LeakageFrac, ecoreg_LeakageFrac_baseline,
         ecoreg_PrecLossFrac, ecoreg_PrecLossFrac_baseline, 
         ecoreg_ClimateFileName) %>%
  mutate(tested_par = "PrecLossFrac")
           
SI_LeakageFrac <- avail_water %>%
  filter(Simulation == "sim_17" |
           Simulation == "sim_18" |
           Simulation == "sim_19" |
           Simulation == "sim_23" |
           Simulation == "sim_24" |
           Simulation == "sim_25") %>%
  left_join(avail_water_baselines) %>%
  mutate(SI = ((aWater - aWater_baseline)/aWater_baseline)/
           ((ecoreg_LeakageFrac - ecoreg_LeakageFrac_baseline) / ecoreg_LeakageFrac_baseline)) %>%
  # filter(Year == FinalYear) %>%
  select(Simulation, Simulation_baseline, 
         SI, Year, aWater, aWater_baseline,
         ecoreg_LeakageFrac, ecoreg_LeakageFrac_baseline,
         ecoreg_PrecLossFrac, ecoreg_PrecLossFrac_baseline, 
         ecoreg_ClimateFileName) %>%
  mutate(tested_par = "LeakageFrac")

SI <- rbind(SI_PrecLossFrac, SI_LeakageFrac) %>%
  full_join(climates)

ggplot(SI, aes(x = tested_par, y = SI, fill = Precipitation)) + 
  geom_boxplot() +
  theme_classic() +
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.1) +
  theme(legend.position = "bottom") +
  scale_x_discrete("Parameter") +
  scale_y_continuous("Sensitivity Index (available water final year)")

