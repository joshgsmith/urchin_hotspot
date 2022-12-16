#Joshua G. Smith
#December 7, 2022

rm(list=ls())

require(dplyr)

#set directories
datadir <- "/Users/Joshua/Box Sync/Data"
dataout <- "/Users/Joshua/Box Sync/Data/RC_EM_2010to2019/processed"
figdir <- "/Users/Joshua/Box Sync/hotspot_analyses/Analyses/figures"
tabdir <- "/Users/Joshua/Box Sync/hotspot_analyses/Analyses/tables"


################################################################################
#import data

mlpa_swath <- read.csv(file.path(datadir, "RC_EM_2010to2019/raw/MLPA_kelpforest_swath.4.csv"))%>%
                janitor::clean_names()

rcca_ab <- read.csv(file.path(datadir, "RC_EM_2010to2019/raw/RCCA_abalone_size_data.csv"))%>%
  janitor::clean_names()
rcca_invert_swath <- read.csv(file.path(datadir, "RC_EM_2010to2019/raw/RCCA_invertebrate_swath_data.csv"))%>%
  janitor::clean_names()
rcca_urchin_size <- read.csv(file.path(datadir, "RC_EM_2010to2019/raw/RCCA_urchin_size_data.csv"))%>%
  janitor::clean_names()
rcca_inverts_2020 <- read.csv(file.path(datadir, "RC_EM_2010to2019/raw/RCCA_Inverts_2020-21.csv"))%>%
  janitor::clean_names()
rcca_urchin_size_data_2020 <- read.csv(file.path(datadir, "RC_EM_2010to2019/raw/RCCA_urchin_size_data_2020-21.csv"))%>%
  janitor::clean_names()

mpa_traits <- read.csv(file.path(datadir, "mpa_traits/mpa_attributes_clean.csv"))%>%
  janitor::clean_names()

kelp_taxon <- read.csv(file.path(datadir, "RC_EM_2010to2019/raw/MLPA_kelpforest_taxon_table.4.csv"))%>%
  janitor::clean_names()
kelp_site_table <- read.csv(file.path(datadir, "RC_EM_2010to2019/raw/MLPA_kelpforest_site_table.4.csv"))%>%
  janitor::clean_names()


################################################################################
#process kelp forest

#process sites
kelp_sites <- kelp_site_table %>%
  janitor::clean_names() %>%
  dplyr::select(site, ca_mpa_name_short, mpa_class=site_designation, mpa_designation=site_status,
                latitude, longitude)%>%
  distinct() #remove duplicates

#select kelp dat
kelp_swath <- mlpa_swath %>%
  dplyr::select(year, site, zone, transect, classcode, count, size)

#join species names by class code 
kelp_taxon1 <- kelp_taxon %>%
  janitor::clean_names() %>%
  dplyr::select(classcode, species_definition) %>%
  distinct() #remove duplicates

kelp_swath_counts <-left_join(kelp_swath, kelp_taxon, by="classcode")

kelp_swath_counts1 <- kelp_swath_counts %>%
  dplyr::select(year, site,  zone, transect, species=species_definition, count, size)


#join with affiliated mpa
kelp_swath_counts2 <- left_join(kelp_swath_counts1, kelp_sites, by="site")

kelp_swath_counts3 <- kelp_swath_counts2 %>%
  ungroup() %>%
  dplyr::select(year, affiliated_mpa=ca_mpa_name_short,  latitude, longitude,mpa_class, mpa_designation, zone, transect, species, count, size) %>%
  mutate(mpa_designation = ifelse(mpa_designation=="reference","ref",mpa_class))

#add regions

regions <- mpa_traits %>%
  dplyr::select(name, region3=bioregion, region4 = four_region_north_ci)

kelp_swath_counts3$affiliated_mpa <- tolower(kelp_swath_counts3$affiliated_mpa)

kelp_swath_counts4 <- left_join(kelp_swath_counts3, regions, by=c("affiliated_mpa"="name"))%>%
                      dplyr::select(year, region3, region4, everything())%>%
                      filter(region3 == "north")

View(kelp_swath_counts4)


#export
write.csv(kelp_swath_counts4, file.path(dataout, "kelp_forest_swath.csv"), row.names = FALSE)

################################################################################
#process Reef Check

#invert swath
View(rcca_invert_swath)

#calculate transect density
rcca_swath <- rcca_invert_swath %>%
              mutate(transect_den = round(ifelse(distance < 30,
                                           amount*(30/distance),
                                           amount),2),
                     region = ifelse(latitude >= 37.7749, "North",""))%>%
              filter(region == "North")%>%
              dplyr::select(year, month, day, site,latitude, longitude,
                            transect, species=classcode,
                            counts = transect_den, depth_ft, temp10m)%>%
                mutate(species = tolower(species))

rcca_swath_2020 <- rcca_inverts_2020 %>%
                    dplyr::select(date_end, site=site_name, latitude, longitude,
                                  transect, species, amount, distance)%>%
                    mutate(year = format(as.Date(date_end, format="%m/%d/%Y"),"%Y"),
                           month = format(as.Date(date_end, format="%m/%d/%Y"),"%m"),
                           day = format(as.Date(date_end, format="%m/%d/%Y"),"%d"),
                           transect_den = round(ifelse(distance < 30,
                                                           amount*(30/distance),
                                                           amount),2),
                           depth_ft = NA,
                           temp10m = NA)%>%
                   dplyr::select(year, month, day, site,latitude, longitude,
                     transect, species,
                      counts = transect_den, depth_ft, temp10m) %>%
                  mutate(species = tolower(species))

rcca_swath2 <- rbind(rcca_swath, rcca_swath_2020)

unique(rcca_swath2$species)

write.csv(rcca_swath2, file.path(dataout, "rcca_invert_swath.csv"), row.names = FALSE)
#





  
  