#Joshua G. Smith
#January 12, 2023

rm(list=ls())

require(tidyverse)
require(patchwork)

#set directories
datadir <- "/Users/Joshua/Box Sync/Data"
gisdir <- file.path(datadir, "gis_data/processed")
figdir <- "/Users/Joshua/Box Sync/hotspot_analyses/Analyses/figures"
tabdir <- "/Users/Joshua/Box Sync/hotspot_analyses/Analyses/tables"

#read spatial data
#sites <- readRDS(file.path(gisdir, "site_locations.Rds"))
state_waters_poly <- readRDS(file.path(gisdir, "CA_state_waters_polygons.Rds"))
state_waters_line <- readRDS(file.path(gisdir, "CA_state_waters_polyline.Rds"))
mpas_orig <- readRDS(file.path(gisdir, "CA_MPA_polygons.Rds"))

#read mlpa data
mlpa_swath <- read.csv(file.path(datadir, "monitoring_processed/mlpa_swath.csv"))%>%
  rename("Lat" = latitude,
         "Long"=longitude) %>%
  mutate(site = paste(affiliated_mpa, mpa_designation),
         region = ifelse(Lat > 39.5,"Humboldt",
                         ifelse(Lat < 39.5 &
                                  Lat > 39.2, "Mendocino",
                                ifelse(Lat < 39 &
                                         Lat > 38.8, "Point Arena",
                                       ifelse(Lat < 38.8 &
                                                Lat > 38.5, "Sonoma",Lat))))) %>%
  select(year, site, region, Lat, Long, transect, species, count, size)

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")


################################################################################
#process urchin density data

#calculate mean site density
site_density <- mlpa_swath %>%
  #select years of interest
  filter(
    species == "Strongylocentrotus purpuratus"|
      species == "Mesocentrotus franciscanus") %>%
  mutate(species = recode(species, "Strongylocentrotus purpuratus" = "Purple urchin",
                                    "Mesocentrotus franciscanus" = "Red urchin")) %>%
  #calculate average site density
  group_by(year, region, site, species) %>%
  summarize(u_transect_den = mean(count, na.rm=TRUE),
            Lat = mean(Lat),
            Long = mean(Long))

#calculate site level quantile range
quantile_range <- mlpa_swath %>%
  #select years of interest
  filter(
    species == "Strongylocentrotus purpuratus"|
      species == "Mesocentrotus franciscanus") %>%
  mutate(species = recode(species, "Strongylocentrotus purpuratus" = "Purple urchin",
                          "Mesocentrotus franciscanus" = "Red urchin")) %>%
  group_by(site, year, species) %>%
  #calculate average density per transect
  dplyr::summarize(transect_den = mean(count))%>%
  #regroup to site level
  group_by(site, species)%>%
  dplyr::summarize(lower_den = quantile(transect_den, probs=0.25, na.rm=TRUE),
                   upper_den = quantile(transect_den, probs = 0.75, na.rm=TRUE))%>%
  mutate(q_range = upper_den-lower_den)


#calculate global quantiles
global_quantiles <- mlpa_swath %>%
  #select years of interest
  filter(
    species == "Strongylocentrotus purpuratus"|
      species == "Mesocentrotus franciscanus") %>%
  mutate(species = recode(species, "Strongylocentrotus purpuratus" = "Purple urchin",
                          "Mesocentrotus franciscanus" = "Red urchin")) %>%
  group_by(site, year, species) %>%
  #calculate average density per transect
  dplyr::summarize(transect_den = mean(count, na.rm=TRUE))%>%
  #regroup to site level
  group_by(species)%>%
  #calcualte quantiles scaled to m2
  dplyr::summarize(q1 = quantile(transect_den, probs=0.25, na.rm=TRUE)/60,
                   q2 = quantile(transect_den, probs=0.50, na.rm=TRUE)/60,
                   q3 = quantile(transect_den, probs = 0.75, na.rm=TRUE)/60,
                   q4 = quantile(transect_den, probs = 1, na.rm=TRUE)/60)


urch_den_spatial1 <-  left_join(site_density, quantile_range, by=c("site","species"))%>%
  #calcualte m2 density
  mutate(m2_quantile_range = q_range/60,
         m2_den = u_transect_den/60)



urch_den_spatial2 <-  left_join(urch_den_spatial1, global_quantiles, by=c("species"))%>%
  mutate(qntl = ifelse(m2_den < q1, "q1",
                       ifelse(m2_den >= q1 & m2_den<= q2, "q2",
                              ifelse(m2_den >= q2 & m2_den<= q3, "q3","q4"))),
         survey = "mlpa")%>%
  select(survey, everything())


write.csv(urch_den_spatial2, file.path(datadir, "/monitoring_processed/mlpa_urchin_demographics.csv"),row.names = FALSE)












