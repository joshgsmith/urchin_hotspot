#Joshua G. Smith
#January 2, 2023

rm(list=ls())

require(tidyverse)
require(patchwork)

#set directories
datadir <- "/Users/Joshua/Box Sync/Data"
gisdir <- file.path(datadir, "gis_data/processed")
figdir <- "/Users/Joshua/Box Sync/hotspot_analyses/Analyses/figures"
tabdir <- "/Users/Joshua/Box Sync/hotspot_analyses/Analyses/tables"

#read data
sites <- readRDS(file.path(gisdir, "site_locations.Rds"))
state_waters_poly <- readRDS(file.path(gisdir, "CA_state_waters_polygons.Rds"))
state_waters_line <- readRDS(file.path(gisdir, "CA_state_waters_polyline.Rds"))
mpas_orig <- readRDS(file.path(gisdir, "CA_MPA_polygons.Rds"))

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

################################################################################
#format data

# Habitat stats
hab_stats <- sites %>% 
  count(site, program, method)

# Build habitat key
program <- sort(unique(sites$habitat))

hab_key <- expand.grid(mpa=mpa_names, habitat=habitats) %>% 
  # Add habitat stats
  left_join(hab_stats) %>% 
  mutate(data_yn=!is.na(n)) %>% 
  # Add lat/long
  left_join(mpas_data %>% select(mpa, lat_dd, long_dd), by="mpa") %>% 
  # Order habitats
  mutate(habitat=factor(habitat, c("Rocky intertidal", 
                                   "Kelp forest", "Rocky reef", "Deep reef")),
         habitat_num=as.numeric(habitat)) %>% 
  # Adjust longitude
  mutate(long_dd_adj=long_dd-0.04-habitat_num*0.05) %>% 
  # Adjust latitude
  mutate(lat_dd_adj=case_when(# Elkohorn Slough (move both up)
    mpa=="Elkhorn Slough SMR"~lat_dd+0.035,
    mpa=="Elkhorn Slough SMCA"~lat_dd+0.015,
    # Downtown Monterey
    mpa=="Pacific Grove Marine Gardens SMCA"~lat_dd+0.03,
    mpa=="Asilomar SMR"~lat_dd+0.01,
    mpa=="Lovers Point - Julia Platt SMR"~lat_dd-0.02,
    mpa=="Edward F. Ricketts SMCA"~lat_dd-0.04,
    # Carmel (move both down)
    mpa=="Carmel Pinnacles SMR"~lat_dd-0.01,
    mpa=="Carmel Bay SMCA"~lat_dd-0.03,
    # Point Lobos (move both down)
    mpa=="Point Lobos SMR"~lat_dd-0.02,
    mpa=="Point Lobos SMCA"~lat_dd-0.05,
    # Point Sur
    mpa=="Point Sur SMR"~lat_dd+0.02,
    mpa=="Point Sur SMCA"~lat_dd-0.02,
    # Big Creek
    mpa=="Big Creek SMR"~lat_dd+0.031,
    mpa=="Big Creek SMCA"~lat_dd-0.031,
    # Piedras Blancas
    mpa=="Piedras Blancas SMR"~lat_dd+0.02,
    mpa=="Piedras Blancas SMCA"~lat_dd-0.02,
    # Morro Bay
    mpa=="Morro Bay SMR"~lat_dd+0.03,
    mpa=="Morro Bay SMCA"~lat_dd-0.03,
    # Point Buchon
    mpa=="Point Buchon SMR"~lat_dd+0.01,
    mpa=="Point Buchon SMCA"~lat_dd-0.01,
    T ~ lat_dd)) %>% 
  # Arrange
  arrange(mpa, habitat) %>% 
  select(mpa, habitat, habitat_num, data_yn, n, lat_dd, long_dd, everything()) %>% 
  # Reduce to only available habitats
  filter(data_yn==T) %>% 
  group_by(mpa) %>% 
  mutate(habitat_num2=1:n()) %>% 
  ungroup() %>% 
  mutate(long_dd_adj2=long_dd-0.04-habitat_num2*0.05)







################################################################################
#plot data

restoration_site <- tibble(site=c("Albion", "Caspar"),
                 long_dd=c(-123.770694, -123.818565),
                 lat_dd=c(39.228264, 39.361313),
                 #hjust=c(-0.2, 0.5, 0.5),
                 #vjust=c(0.5, -0.3, -0.3)
                 )

# MPA regions
# CA/OR, Alder Creek, Pigeon Point, Point Conception, CA/MEX 
region_lats <- c(39.0, 37.18, 34.5)
region_lats_long <- c(42, 39.0, 37.18, 34.5, 32)
region_labels <- tibble(region=paste0(c("North", "North\nCentral", "Central", "South"), "\nCoast"),
                        lat_dd=zoo::rollmean(region_lats_long, k = 2))



# Theme
base_theme <-  theme(axis.text=element_text(size=7),
                     axis.title=element_text(size=8),
                     legend.text=element_text(size=7),
                     legend.title=element_text(size=8),
                     plot.tag=element_text(size=8),
                     # Gridlines
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     # Legend
                     legend.key = element_rect(fill=alpha('blue', 0)),
                     legend.background = element_rect(fill=alpha('blue', 0)))

# Build inset
g1_inset <-  ggplotGrob(
  ggplot() +
    # Plot regions
    #geom_hline(mapping=aes(yintercept=region_lats), lwd=0.4) +
    # Plot land
    geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
    geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
    # Plot box
    annotate("rect", xmin=-122.6, xmax=-120.4, ymin=34.4, ymax=37.3, color="indianred1", fill=NA, lwd=0.8) +
    # Label regions
    #geom_text(data=region_labels, mapping=aes(y=lat_dd, label=region), x= -124.4, hjust=0, size=2) +
    # Labels
    labs(x="", y="") +
    # Crop
    coord_sf(xlim = c(-124.5, -117), ylim = c(32.5, 42)) +
    # Theme
    theme_bw() + base_theme +
    theme( plot.margin = unit(rep(0, 4), "null"),
           panel.margin = unit(rep(0, 4), "null"),
           panel.background = element_rect(fill='transparent'), #transparent panel bg
           # plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
           axis.ticks = element_blank(),
           axis.ticks.length = unit(0, "null"),
           axis.ticks.margin = unit(0, "null"),
           axis.text = element_blank(),
           axis.title=element_blank())
)
g1_inset





g1 <- ggplot() +
  # Plot state waters
  geom_sf(data=state_waters_line, color="grey60", lwd=0.1) +
  # Plot regions
  geom_hline(mapping=aes(yintercept=region_lats)) +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Plot MPAs
  geom_sf(data=mpas_orig, fill="indianred1", color="black") +
  # ggrepel::geom_text_repel(data=mpas,
  #           mapping=aes(x=long_dd, y=lat_dd, label=name), direction="y", hjust=-0.1) +
  #plot sites
  geom_point(data=sites, aes(x=longitude, y=latitude, color=method, shape=program)) +
  #geom_point(data=hab_key, aes(x=long_dd_adj2, y=lat_dd_adj, color=habitat, shape=data_yn), pch=16, size=1.8) +
  #Plot habitats
  # geom_point(data=hab_key, aes(x=long_dd_adj, y=lat_dd_adj, color=habitat, shape=data_yn)) +
  #geom_point(data=hab_key, aes(x=long_dd_adj2, y=lat_dd_adj, color=habitat, shape=data_yn), pch=16, size=1.8) +
  # Plot cities
  #geom_point(data=cities, mapping=aes(x=long_dd, y=lat_dd), size=2.2) +
  #geom_text(data=cities, mapping=aes(x=long_dd, y=lat_dd, label=city, hjust=hjust, vjust=vjust),
  #          size=2.6) +
  # Labels
  labs(x="", y="", tag="A") +
  # Legend
  scale_color_manual(name="Habitat", values=c( "lightslategray", "limegreen", "dodgerblue", "purple")) +
  scale_shape_manual(name="Monitoring data?", values=c(1, 16)) +
  guides(color = guide_legend(order = 1), shape = guide_legend(order = 2)) +
  
  # Crop
  coord_sf(xlim = c(-125.5, -122), ylim = c(37.8, 42.1)) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.title=element_blank(),
        legend.position = c(0.2, 0.2), 
        legend.key.size = unit(0.4, "cm")) +
  # Add inset
  annotation_custom(grob = g1_inset, 
                    xmin = -123, 
                    xmax = -122,
                    ymin = 41) 
# Add inset
# patchwork::inset_element(g1_inset, 
#                          right = -120.6,
#                          left=-121.4,
#                          top=37.1, 
#                          bottom=36.0)

g1






#Zoom
g1_inset <-  ggplotGrob(
  ggplot() +
    # Plot regions
    geom_hline(mapping=aes(yintercept=region_lats), lwd=0.4) +
    # Plot land
    geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
    geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
    # Plot box
    annotate("rect", xmin=-122.6, xmax=-120.4, ymin=34.4, ymax=37.3, color="indianred1", fill=NA, lwd=0.8) +
    # Label regions
    geom_text(data=region_labels, mapping=aes(y=lat_dd, label=region), x= -124.4, hjust=0, size=2) +
    # Labels
    labs(x="", y="") +
    # Crop
    coord_sf(xlim = c(-124.5, -117), ylim = c(32.5, 42)) +
    # Theme
    theme_bw() + base_theme +
    theme( plot.margin = unit(rep(0, 4), "null"),
           panel.margin = unit(rep(0, 4), "null"),
           panel.background = element_rect(fill='transparent'), #transparent panel bg
           # plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
           axis.ticks = element_blank(),
           axis.ticks.length = unit(0, "null"),
           axis.ticks.margin = unit(0, "null"),
           axis.text = element_blank(),
           axis.title=element_blank())
)
g1_inset






#Build inset
g2_inset <-  ggplotGrob(
  ggplot() +
    # Plot regions
    #geom_hline(mapping=aes(yintercept=region_lats), lwd=0.4) +
    # Plot land
    geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
    geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
    # Plot box
    annotate("rect", xmin=-123, xmax=-124, ymin=38.2, ymax=39.5, color="indianred1", fill=NA, lwd=0.8) +
    # Label regions
    #geom_text(data=region_labels, mapping=aes(y=lat_dd, label=region), x= -124.4, hjust=0, size=2) +
    # Labels
    labs(x="", y="") +
    # Crop
    coord_sf(xlim = c(-124.5, -117), ylim = c(32.5, 42)) +
    # Theme
    theme_bw() + base_theme +
    theme( plot.margin = unit(rep(0, 4), "null"),
           panel.margin = unit(rep(0, 4), "null"),
           panel.background = element_rect(fill='transparent'), #transparent panel bg
           # plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
           axis.ticks = element_blank(),
           axis.ticks.length = unit(0, "null"),
           axis.ticks.margin = unit(0, "null"),
           axis.text = element_blank(),
           axis.title=element_blank())
)
g2_inset





g2 <- ggplot() +
  # Plot state waters
  geom_sf(data=state_waters_line, color="grey60", lwd=0.1) +
  # Plot regions
  #geom_hline(mapping=aes(yintercept=region_lats)) +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Plot MPAs
  geom_sf(data=mpas_orig, fill="indianred1", color="black") +
  # ggrepel::geom_text_repel(data=mpas,
  #           mapping=aes(x=long_dd, y=lat_dd, label=name), direction="y", hjust=-0.1) +
  #plot sites
  geom_point(data=sites %>% filter(program=="rcca",
                                   method == "invert swath")
             , aes(x=longitude, y=latitude, color=method, shape=program), size = 2) +
  # Plot restoration sites
  geom_point(data=restoration_site, mapping=aes(x=long_dd+0.025, y=lat_dd), size=2.2) +
  geom_text(data=restoration_site, mapping=aes(x=long_dd, y=lat_dd, label=site, hjust=-0.3, vjust=0
                                               ),
            size=4) +
  #add county line
  geom_hline(mapping=aes(yintercept=38.768802), linetype = "dashed")+
  annotate("text",label = "Mendocino County", x= -123.2, y=38.768802+0.3, size=4)+
  annotate("text",label = "Sonoma County", x= -123.2, y=38.768802-0.08, size=4)+
  
  # Labels
  labs(x="", y="", tag="A") +
  # Legend
  scale_color_manual(name="Survey type", values=c( "purple", "limegreen", "dodgerblue")) +
  scale_shape_manual(name="Monitoring program", values=c(16, 17)) +
  guides(color = guide_legend(order = 1), shape = guide_legend(order = 2)) +
  
  # Crop
  coord_sf(xlim = c(-124, -123), ylim = c(38.2, 39.5)) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.title=element_blank(),
        legend.position = c(0.2, 0.2), 
        legend.key.size = unit(0.4, "cm"))+
  # Add inset
  annotation_custom(grob = g2_inset, 
                    xmin = -123.4, 
                    xmax = -122.8,
                    ymin = 39.2) 

g2






