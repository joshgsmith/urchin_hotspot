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
mpas_orig <- readRDS(file.path(gisdir, "CA_MPA_polygons.Rds"))

#read RCCA data
rcca_invert_swath <- read.csv(file.path(datadir, "monitoring_processed/rcca_invert_swath.csv"))
rcca_kelp_swath <- read.csv(file.path(datadir, "monitoring_processed/rcca_kelp_swath.csv"))
rcca_urchin_sizefq <- read.csv(file.path(datadir, "monitoring_processed/rcca_urchin_sizefq.csv"))
rcca_site_table <- read.csv(file.path(datadir, "site_tables/raw/rcca_site_table.csv"))%>%
                            mutate(site = tolower(Name),
                                   region = ifelse(Lat > 39.5,"Humboldt",
                                     ifelse(Lat < 39.5 &
                                                     Lat > 39.2, "Mendocino",
                                                   ifelse(Lat < 39 &
                                                            Lat > 38.8, "Point Arena",
                                                          ifelse(Lat < 38.8 &
                                                                   Lat > 38.5, "Sonoma",Lat))))) %>%
                            select(site, region, Lat, Long)

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")


################################################################################
#process urchin density data


site_density <- rcca_invert_swath %>%
                #select years of interest
                filter(year > 2018,
                       species == "purple urchin"|
                         species == "red urchin") %>%
                #calculate average site density
                group_by(site_new, species) %>%
                summarize(u_transect_den = mean(counts, na.rm=TRUE))

          
#join density with authoritative lat long

urch_den_spatial <- left_join(site_density, rcca_site_table, by=c("site_new"="site"))%>%
                          #calcualte m2 density
                          mutate(m2_den = u_transect_den/60)




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


#Build inset
g1_inset <-  ggplotGrob(
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
g1_inset





g1 <- ggplot() +
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
  #plot density
  geom_point(data=urch_den_spatial %>%
               mutate(Long = ifelse(species == "red urchin",
                                    Long-0.05, Long)),
            aes(x=Long, y=Lat, color=species, size = m2_den), alpha=0.6) +
  # Plot restoration sites
  #geom_point(data=restoration_site, mapping=aes(x=long_dd+0.025, y=lat_dd), size=2.2) +
  geom_text(data=restoration_site, mapping=aes(x=long_dd, y=lat_dd, label=site, hjust=-0.5, vjust=0
  ),
  size=3) +
  #add county line
  geom_hline(mapping=aes(yintercept=38.768802), linetype = "dashed")+
  annotate("text",label = "Mendocino County", x= -123.2, y=38.768802+0.3, size=4)+
  annotate("text",label = "Sonoma County", x= -123.2, y=38.768802-0.08, size=4)+
  
  #add site names
  #ggrepel::geom_text_repel(sites_off %>% filter(method == "size fq"),
  #                        mapping=aes(x=mean_lon, y=mean_lat, label=site), 
  #                       inherit.aes = F, size=2, max.overlaps = 1000, color="black") +
  #geom_text(data=sites_label %>% filter(method == "kelp swath"),
  #         mapping=aes(x=long_adj, y=mean_lat, label=site, hjust=-0.4, vjust=0
  #),
  #size=2)+
  # Labels
  labs(x="", y="", tag="A") +
  # Legend
  scale_color_manual(name="Species", values=c( "purple", "darkred")) +
  scale_shape_manual(name="Monitoring program", values=c(16, 17)) +
  scale_size(range = c(1,8), name="Sea urchin density")+
  guides(color = guide_legend(order = 1), shape = guide_legend(order = 2)) +
  
  # Crop
  coord_sf(xlim = c(-124.2, -123), ylim = c(38.51, 39.5)) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.title=element_blank(),
        legend.position = c(0.15, 0.17), 
        legend.key.size = unit(0.4, "cm"))+
  # Add inset
  annotation_custom(grob = g2_inset, 
                    xmin = -123.4, 
                    xmax = -122.8,
                    ymin = 39.2) 

g1










g1 <- ggplot() +
  # Plot state waters
  geom_sf(data=state_waters_line, color="grey60", lwd=0.1) +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Plot MPAs
  geom_sf(data=mpas_orig, fill="indianred1", color="black") +
  # ggrepel::geom_text_repel(data=mpas,
  #           mapping=aes(x=long_dd, y=lat_dd, label=name), direction="y", hjust=-0.1) +
  #plot density
  geom_point(data=urch_den_spatial %>%
               mutate(Long = ifelse(species == "red urchin",
                                    Long-0.05, Long)),
             aes(x=Long, y=Lat, color=species, size = m2_den), alpha=0.6) +
  # Plot restoration sites
  geom_text(data=restoration_site, mapping=aes(x=long_dd, y=lat_dd, label=site, hjust=-0.5, vjust=0
  ),
  size=3) +
  #add county line
  geom_hline(mapping=aes(yintercept=38.768802), linetype = "dashed")+
  annotate("text",label = "Mendocino County", x= -123.2, y=38.768802+0.3, size=4)+
  annotate("text",label = "Sonoma County", x= -123.2, y=38.768802-0.08, size=4)+
  
  #add site names
  #ggrepel::geom_text_repel(sites_off %>% filter(method == "size fq"),
  #                        mapping=aes(x=mean_lon, y=mean_lat, label=site), 
  #                       inherit.aes = F, size=2, max.overlaps = 1000, color="black") +
  #geom_text(data=sites_label %>% filter(method == "kelp swath"),
  #         mapping=aes(x=long_adj, y=mean_lat, label=site, hjust=-0.4, vjust=0
  #),
  #size=2)+
  # Labels
  labs(x="", y="", tag="B") +
  # Legend
  scale_color_manual(name="Species", values=c( "purple", "darkred")) +
  scale_shape_manual(name="Monitoring program", values=c(16, 17)) +
  scale_size(range = c(1,8), name="Sea urchin density")+
  guides(color = guide_legend(order = 1), shape = guide_legend(order = 2)) +
  
  # Crop
  coord_sf(xlim = c(-124.5, -123.6), ylim = c(39.6, 42)) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.title=element_blank(),
        legend.position = c(0.15, 0.17), 
        legend.key.size = unit(0.4, "cm"))
# Add inset
#annotation_custom(grob = g2_inset, 
#                 xmin = -123.4, 
#                xmax = -122.8,
#               ymin = 39.2) 

g1






g2 <- ggplot() +
  # Plot state waters
  geom_sf(data=state_waters_line, color="grey60", lwd=0.1) +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Plot MPAs
  geom_sf(data=mpas_orig, fill="indianred1", color="black") +
  # ggrepel::geom_text_repel(data=mpas,
  #           mapping=aes(x=long_dd, y=lat_dd, label=name), direction="y", hjust=-0.1) +
  #plot density
  geom_point(data=urch_den_spatial %>%
               mutate(Long = ifelse(species == "red urchin",
                                    Long-0.02, Long)),
             aes(x=Long, y=Lat, color=species, size = m2_den), alpha=0.6) +
  #plot site name
  #geom_text(data=urch_den_spatial %>% filter (species =="red urchin"), 
   #         mapping=aes(x=Long, y=Lat, label=site_new, hjust=-0.5, vjust=0
  #),
  #size=2) +
  # Plot restoration sites
  geom_text(data=restoration_site, mapping=aes(x=long_dd, y=lat_dd, label=site, hjust=-0.5, vjust=0
  ),
  size=3) +
  #add county line
  geom_hline(mapping=aes(yintercept=38.768802), linetype = "dashed")+
  annotate("text",label = "Mendocino County", x= -123.2, y=38.768802+0.3, size=4)+
  annotate("text",label = "Sonoma County", x= -123.2, y=38.768802-0.08, size=4)+
  
  #add site names
  ggrepel::geom_text_repel(urch_den_spatial %>% filter(species == "purple urchin",
                                                       region == "Mendocino") %>%
                             mutate(Long = Long-0.04),
                          mapping=aes(x=Long, y=Lat, label=site_new), 
                       inherit.aes = F, size=3, max.overlaps = 1000, color="black") +
  #geom_text(data=sites_label %>% filter(method == "kelp swath"),
  #         mapping=aes(x=long_adj, y=mean_lat, label=site, hjust=-0.4, vjust=0
  #),
  #size=2)+
  # Labels
  labs(x="", y="", tag="B") +
  # Legend
  scale_color_manual(name="Species", values=c( "purple", "darkred")) +
  scale_shape_manual(name="Monitoring program", values=c(16, 17)) +
  scale_size_binned(breaks = c(2,4,10,32),
                    labels = function(x) as.character(round(x,0)),
                    name = "Sea urchin density \n(no. per m²)")+
  guides(color = guide_legend(order = 1), shape = guide_legend(order = 2)) +
  
  # Crop
  coord_sf(xlim = c(-124, -123.6), ylim = c(39.2, 39.5)) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.title=element_blank(),
        legend.position = c(0.15, 0.17), 
        legend.key.size = unit(0.4, "cm"))
  # Add inset
  #annotation_custom(grob = g2_inset, 
   #                 xmin = -123.4, 
    #                xmax = -122.8,
     #               ymin = 39.2) 

g2








g3 <- ggplot() +
  # Plot state waters
  geom_sf(data=state_waters_line, color="grey60", lwd=0.1) +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Plot MPAs
  geom_sf(data=mpas_orig, fill="indianred1", color="black") +
  # ggrepel::geom_text_repel(data=mpas,
  #           mapping=aes(x=long_dd, y=lat_dd, label=name), direction="y", hjust=-0.1) +
  #plot density
  geom_point(data=urch_den_spatial %>%
               mutate(Long = ifelse(species == "red urchin",
                                    Long-0.02, Long)),
             aes(x=Long, y=Lat, color=species, size = m2_den), alpha=0.6) +
  #plot site name
  #geom_text(data=urch_den_spatial %>% filter (species =="red urchin"), 
  #         mapping=aes(x=Long, y=Lat, label=site_new, hjust=-0.5, vjust=0
  #),
  #size=2) +
  # Plot restoration sites
  geom_text(data=restoration_site, mapping=aes(x=long_dd, y=lat_dd, label=site, hjust=-0.5, vjust=0
  ),
  size=3) +
  #add county line
  geom_hline(mapping=aes(yintercept=38.768802), linetype = "dashed")+
  annotate("text",label = "Mendocino County", x= -123.2, y=38.768802+0.3, size=4)+
  annotate("text",label = "Sonoma County", x= -123.2, y=38.768802-0.08, size=4)+
  
  #add site names
  ggrepel::geom_text_repel(urch_den_spatial %>% filter(species == "purple urchin",
                                                       region == "Point Arena") %>%
                             mutate(Long = Long-0.04),
                           mapping=aes(x=Long, y=Lat, label=site_new), 
                           inherit.aes = F, size=3, max.overlaps = 1000, color="black") +
  #geom_text(data=sites_label %>% filter(method == "kelp swath"),
  #         mapping=aes(x=long_adj, y=mean_lat, label=site, hjust=-0.4, vjust=0
  #),
  #size=2)+
  # Labels
  labs(x="", y="", tag="B") +
  # Legend
  scale_color_manual(name="Species", values=c( "purple", "darkred")) +
  scale_shape_manual(name="Monitoring program", values=c(16, 17)) +
  scale_size_binned(breaks = c(2,4,10,32),
                    labels = function(x) as.character(round(x,0)),
                    name = "Sea urchin density \n(no. per m²)")+
  guides(color = guide_legend(order = 1), shape = guide_legend(order = 2)) +
  
  # Crop
  coord_sf(xlim = c(-123.9, -123.6), ylim = c(38.85, 39)) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.title=element_blank(),
        legend.position = c(0.15, 0.17), 
        legend.key.size = unit(0.4, "cm"))
# Add inset
#annotation_custom(grob = g2_inset, 
#                 xmin = -123.4, 
#                xmax = -122.8,
#               ymin = 39.2) 

g3



g4 <- ggplot() +
  # Plot state waters
  geom_sf(data=state_waters_line, color="grey60", lwd=0.1) +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Plot MPAs
  geom_sf(data=mpas_orig, fill="indianred1", color="black") +
  # ggrepel::geom_text_repel(data=mpas,
  #           mapping=aes(x=long_dd, y=lat_dd, label=name), direction="y", hjust=-0.1) +
  #plot density
  geom_point(data=urch_den_spatial %>%
               mutate(Long = ifelse(species == "red urchin",
                                    Long-0.02, Long)),
             aes(x=Long, y=Lat, color=species, size = m2_den), alpha=0.6) +
  #plot site name
  #geom_text(data=urch_den_spatial %>% filter (species =="red urchin"), 
  #         mapping=aes(x=Long, y=Lat, label=site_new, hjust=-0.5, vjust=0
  #),
  #size=2) +
  # Plot restoration sites
  geom_text(data=restoration_site, mapping=aes(x=long_dd, y=lat_dd, label=site, hjust=-0.5, vjust=0
  ),
  size=3) +
  #add county line
  geom_hline(mapping=aes(yintercept=38.768802), linetype = "dashed")+
  annotate("text",label = "Mendocino County", x= -123.2, y=38.768802+0.3, size=4)+
  annotate("text",label = "Sonoma County", x= -123.2, y=38.768802-0.08, size=4)+
  
  #add site names
  ggrepel::geom_text_repel(urch_den_spatial %>% filter(species == "purple urchin",
                                                       region == "Sonoma") %>%
                             mutate(Long = Long-0.04),
                           mapping=aes(x=Long, y=Lat, label=site_new), 
                           inherit.aes = F, size=3, max.overlaps = 1000, color="black") +
  #geom_text(data=sites_label %>% filter(method == "kelp swath"),
  #         mapping=aes(x=long_adj, y=mean_lat, label=site, hjust=-0.4, vjust=0
  #),
  #size=2)+
  # Labels
  labs(x="", y="", tag="B") +
  # Legend
  scale_color_manual(name="Species", values=c( "purple", "darkred")) +
  scale_shape_manual(name="Monitoring program", values=c(16, 17)) +
  scale_size_binned(breaks = c(2,4,10,32),
                    labels = function(x) as.character(round(x,0)),
                    name = "Sea urchin density \n(no. per m²)")+
  guides(color = guide_legend(order = 1), shape = guide_legend(order = 2)) +
  
  # Crop
  coord_sf(xlim = c(-123.6, -123.1), ylim = c(38.5, 38.7)) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.title=element_blank(),
        legend.position = c(0.15, 0.17), 
        legend.key.size = unit(0.4, "cm"))
# Add inset
#annotation_custom(grob = g2_inset, 
#                 xmin = -123.4, 
#                xmax = -122.8,
#               ymin = 39.2) 

g4

layout_matrix <- matrix(data=c(1, 2,
                               1, 3,
                               1, 4,
                               1, 5), byrow=T, ncol=2)

g <- gridExtra::grid.arrange(g1, g2, g3, g4)
g







# Export figure
ggsave(g1, filename=file.path(figdir, "Rcca_regionwide_density.png"), 
       width=5.8, height=6.5, units="in", dpi=600)


















