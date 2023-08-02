# Calibrate species parameters

# Simulations to compare
sims <- c("sim_15", "sim_16")

# General settings
library(dplyr)
library(ggplot2)
library(reshape2)
w_dir <- "/Users/maria.suarez.munoz/Library/CloudStorage/GoogleDrive-msuamun@go.ugr.es/My Drive/proj_LANDIS/calibration/" 

tabla_especies <- data.frame(Especie = c("Pinus pinaster", "Quercus ilex", "Quercus faginea", "Pinus sylvestris", "Pinus halepensis", "Pinus nigra", "Quercus pyrenaica"),
                             landis_spp = c("ppinaster", "qilex", "qfaginea", "psylvestris", "phalepensis", "pnigra", "qpyrenaica"))

sim_params <- read.table(paste0(w_dir, "recalibration_PnET/single_cohort_simulations/registro.csv"), sep = ";", header = TRUE)
registro_simulaciones <- data.frame()
for(i in 1:length(sims)){
  registro_sim <- sim_params %>%
    filter(Simulation_name == sims[i])
  
  registro_simulaciones <- rbind(registro_simulaciones, registro_sim)
}

# Checks: must be TRUE to continue
# registro_simulaciones$IC_species[registro_simulaciones$Simulation_name == sim_X] == registro_simulaciones$IC_species[registro_simulaciones$Simulation_name == sim_Y] # Simulations to compare consider the same species
# registro_simulaciones$IC_species[registro_simulaciones$Simulation_name == sim_X] == registro_simulaciones$IC_species[registro_simulaciones$Simulation_name == sim_Z]
# 
# registro_simulaciones$StartYear[registro_simulaciones$Simulation_name == sim_X] == registro_simulaciones$StartYear[registro_simulaciones$Simulation_name == sim_Y] # Simulations to compare consider the same start year
# registro_simulaciones$StartYear[registro_simulaciones$Simulation_name == sim_X] == registro_simulaciones$StartYear[registro_simulaciones$Simulation_name == sim_Z]

establishment <- data.frame()
for(i in 1:length(sims)){
  establishment <- rbind(establishment, 
                         read.table(paste0(w_dir, "recalibration_PnET/single_cohort_simulations/", sims[i], "/Site1/Establishment.csv"), sep = ",", header = TRUE) %>% 
                           mutate(Simulation = sims[i]))
}
unique(establishment$Est) == "False" # No establishment allowed in calibration runs

# Extract simulation parameters
dif_params <- row.names(as.data.frame(t(as.matrix(registro_simulaciones[,-1]))) %>%
                          mutate(Different = (V1 == V2)) %>%
                          filter(Different == FALSE))

dif_params_values <- registro_simulaciones %>%
  select(Simulation_name, all_of(dif_params))

simulated_spp <- registro_simulaciones$IC_species[registro_simulaciones$Simulation_name == sims[1]]
StartYear <- registro_simulaciones$StartYear[registro_simulaciones$Simulation_name == sims[1]]

# Load calibration data
calib_data <- read.table(paste0(w_dir, "calibration_data/yield_tables_biomass.csv"), sep = ";", header = TRUE) %>%
  filter(Especie == tabla_especies$Especie[tabla_especies$landis_spp == simulated_spp]) %>%
  select(Edad, Calidad, Leyenda, BT_tn_ha_calc_report) %>%
  rename(Age = Edad,
         Yield_curve = Leyenda,
         AGBiomass_tnha = BT_tn_ha_calc_report)
  
# Load cohort data
year_in_file_name <- StartYear - registro_simulaciones$IC_age[1] + 1

cohort_data <- data.frame()
for(i in 1:length(sims)){
  cohort_data <- rbind(cohort_data,
                       read.table(paste0(w_dir, "recalibration_PnET/single_cohort_simulations/", sims[i], "/Site1/Cohort_", simulated_spp, "_", year_in_file_name, ".csv"), sep = ",", header = TRUE) %>% 
                         mutate(Simulation = sims[i]))
}

# Calculate AGBiomass in simulations
# Biomass data are reported in Cohort and Site tables, with no clear definition (see questions on PnET). 
# For now Fol.gDW., Wood.gDW. in cohort table will be used.
sim_biomass <- cohort_data %>%
  mutate(AGBiomass_tnha = (Fol.gDW. + Wood.gDW.) / 100) %>% # g/m2 /100 = tn/ha 
  rename(Age = Age.yr.) %>%
  select(Simulation, Age, AGBiomass_tnha) %>%
  left_join(dif_params_values, by = c("Simulation" = "Simulation_name"))

# Compare AGBiomass in simulations and yield tables
ggplot() +
  geom_point(data = sim_biomass, aes(x = Age, y = AGBiomass_tnha, color = as.factor(sim_biomass[,4]))) +
  geom_line(data = calib_data, aes(x = Age, y = AGBiomass_tnha, color = Yield_curve)) +
  theme_classic() +
  labs(color = colnames(sim_biomass)[4]) +
  theme(legend.position = "bottom") +
  ggtitle(tabla_especies$Especie[tabla_especies$landis_spp == simulated_spp])

# Compare LAI in simulations
ggplot(data = cohort_data, aes(x = Time, y = LAI.m2., color = as.factor(sim_biomass[,4]))) +
  geom_point() +
  geom_line() +
  theme_classic() +
  labs(color = colnames(sim_biomass)[4]) +
  theme(legend.position = "bottom") +
  ggtitle(tabla_especies$Especie[tabla_especies$landis_spp == simulated_spp])

# Load site data
site_data <- data.frame()
for(i in 1:length(sims)){
  site_data <- rbind(site_data,
                     read.table(paste0(w_dir, "recalibration_PnET/single_cohort_simulations/", sims[i], "/Site1/Site.csv"), sep = ",", header = TRUE) %>% mutate(Simulation = sims[i]))
}

# Compare water variables
water <- site_data %>%
  select(Simulation, Time, Year, Month,
         Available.Water..mm., 
         RunOff.mm.mo., Leakage.mm., Interception.mm., PrecLoss.mm.mo., LeakageFrac..., Surface.Water..mm.,
         Potential.Evapotranspiration.mm., Potential.Evaporation.mm., Evaporation.mm., PotentialTranspiration.mm., Transpiration.mm., 
         Water.m.m., PressureHead.mm.,
         SnowPack.mm.) %>%
  melt(id.vars = c("Simulation", "Time", "Year", "Month")) %>%
  left_join(dif_params_values, by = c("Simulation" = "Simulation_name"))

# jpeg(file = paste0(w_dir, "", , ".jpeg"), 
     # width = 12, height = 8, units = "in", res = 300)
subset <- water %>%
  filter(
    variable == "Water.m.m.",
    Year == 1880)

ggplot(subset, aes(x = Time, y = value, color = as.factor(subset[,7]))) +
  geom_point() +
  geom_line() +
  facet_wrap(variable ~ ., ncol = 3, scales = "free") +
  theme_classic() +
  # scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12)) +
  labs(color = colnames(subset)[7])
# dev.off()  

