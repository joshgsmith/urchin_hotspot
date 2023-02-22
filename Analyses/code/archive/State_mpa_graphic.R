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
sites <- readRDS(file.path(gisdir, "site_locations.Rds"))
state_waters_poly <- readRDS(file.path(gisdir, "CA_state_waters_polygons.Rds"))
state_waters_line <- readRDS(file.path(gisdir, "CA_state_waters_polyline.Rds"))
mpas_orig <- readRDS(file.path(gisdir, "CA_MPA_polygons.Rds")) %>%
  mutate(mpa_simple = ifelse(type == "SMCA","SMCA","SMR"))

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")



region_lats <- c(39.0, 37.18, 34.5)
region_lats_long <- c(42, 39.0, 37.18, 34.5, 32)
region_labels <- tibble(region=paste0(c("North", "North\nCentral", "Central", "South"), "\nCoast"),
                        lat_dd=zoo::rollmean(region_lats_long, k = 2))

# create site number



# Theme
base_theme <-  theme(axis.text=element_text(size=5),
                     axis.title=element_text(size=6),
                     legend.text=element_text(size=7),
                     legend.title=element_text(size=8),
                     plot.tag=element_text(size=4),
                     plot.title = element_text(size=6),
                     # Gridlines
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     # Legend
                     legend.key = element_rect(fill=alpha('blue', 0)),
                     legend.background = element_rect(fill=alpha('blue', 0)))


g1 <- ggplot() +
  # Plot state waters
  #geom_sf(data=state_waters_line, color="grey60", lwd=0.1) +
  # Plot land
  geom_sf(data=foreign, fill="#C2B280", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="#C2B280", color="white", lwd=0.3) +
  # Plot MPAs
  geom_sf(data=mpas_orig, aes(fill=mpas_orig$mpa_simple), color="black") +
  scale_fill_manual(values = c("skyblue","indianred"))+
  #annotate("text",label = "Mendocino County", x= -123.2, y=38.768802+0.3, size=4)+
  #annotate("text",label = "Sonoma County", x= -123.2, y=38.768802-0.08, size=4)+

  labs(x="", y="", tag="",legend.position="none") +
  # Crop
  coord_sf(xlim = c(-124.8, -116), ylim = c(32, 42)) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.title=element_blank(),
        #legend.position = c(0.15, 0.17), 
        #legend.key.size = unit(0.4, "cm"),
        legend.position="none")+
  theme( panel.background = element_rect(fill='transparent'), #transparent panel bg
         plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
         panel.grid.major = element_blank(), #remove major gridlines
         panel.grid.minor = element_blank(), #remove minor gridlines
         legend.background = element_rect(fill='transparent'), #transparent legend bg
         legend.box.background = element_rect(fill='transparent') #transparent legend panel
  )


g1



# Export figure
ggsave(g1, filename=file.path(figdir, "CA_MPAs.png"), 
       width=16, height=36, units="in", dpi=800, bg='transparent')






