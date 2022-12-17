#Joshua G. Smith
#December 7, 2022

rm(list=ls())

require(dplyr)

#set directories
datadir <- "/Users/Joshua/Box Sync/Data"
dataout <- "/Users/Joshua/Box Sync/Data/RC_EM_2022/processed"
figdir <- "/Users/Joshua/Box Sync/hotspot_analyses/Analyses/figures"
tabdir <- "/Users/Joshua/Box Sync/hotspot_analyses/Analyses/tables"


################################################################################
#import data

input_file <- "RC_EM_2022/raw/Urchin Health and Gonad/raw/Gonad_TNC_fall22.xlsx"
urch_gonad_fall <- readxl::read_excel(file.path(datadir, input_file), sheet=1, skip = 1, na="NA") %>%
                    janitor::clean_names()%>%
                    mutate(season = "fall")

input_file <- "RC_EM_2022/raw/Urchin Health and Gonad/raw/Gonad_TNC_spring22.xlsx"
urch_gonad_spring <- readxl::read_excel(file.path(datadir, input_file), sheet=3, skip = 1, na="NA") %>%
  janitor::clean_names()%>%
  mutate(season = "spring")

input_file <- "RC_EM_2022/raw/Urchin Health and Gonad/raw/Gonad_TNC_summer22.xlsx"
urch_gonad_summer <- readxl::read_excel(file.path(datadir, input_file), sheet=3, skip = 1, na="NA") %>%
  janitor::clean_names()%>%
  mutate(season = "summer")

input_file <- "RC_EM_2022/raw/Urchin Health and Gonad/raw/Gonad_TNC_winter22.xlsx"
urch_gonad_winter <- readxl::read_excel(file.path(datadir, input_file), sheet=3, skip = 1, na="NA") %>%
  janitor::clean_names()%>%
  mutate(season = "winter")

gonad_dockside <- rbind(urch_gonad_fall, urch_gonad_spring, urch_gonad_summer, urch_gonad_winter)

gonad_dockside_dat <- gonad_dockside %>%
                      #drop data
                      dplyr::select(!(c(data_entry_initials, qaqc_initials, 
                                        commercial_harvesters, vessel_id,
                                        diver_notes, processor_name, diver_id,
                                        urchin_metrics_notes)))%>%
                      #reorder
                      dplyr::select(season, everything())%>%
                      #drop content
                      filter(!(effort_type=="context"))%>%
                      filter(species == "strongylocentrotus purpuratus")%>%
                      #check dat type
                      mutate(season = factor(season),
                             site_county = factor(site_county),
                             site_name = factor(site_name),
                             effort_type = factor(effort_type),
                             region = factor(region),
                             service_area_cells_shallows_or_surface_tag_trap_number = 
                               factor(service_area_cells_shallows_or_surface_tag_trap_number),
                             bolt_id = factor(bolt_id),
                             habitat_status = factor(habitat_status),
                             bin_number = factor(bin_number),
                             catch_type = factor(catch_type),
                             species = factor(species),
                             urchin_id = factor(urchin_id),
                             wasting_disease = factor(wasting_disease),
                             blackspot_disease = factor(blackspot_disease),
                             gonad_visible = factor(gonad_visible),
                             jaw_collected = factor(jaw_collected),
                             cannibal = factor(cannibal),
                             algae_any = factor(algae_any),
                             brown_algae = factor(brown_algae),
                             red_algae = factor(red_algae),
                             green_algae = factor(green_algae),
                             sand = factor(sand),
                             rocks = factor(rocks),
                             parasites = factor(parasites),
                             sex = factor(sex))




