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
urch_den_spatial <- read.csv(file.path(datadir, "monitoring_processed/rcca_urchin_demographics.csv"))

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")


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
base_theme <-  theme(axis.text=element_text(size=5),
                     axis.title=element_text(size=6),
                     legend.text=element_text(size=4),
                     legend.title=element_text(size=4),
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


every_nth = function(n) {
  return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
}


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
  #annotate("text",label = "Mendocino County", x= -123.2, y=38.768802+0.3, size=4)+
  #annotate("text",label = "Sonoma County", x= -123.2, y=38.768802-0.08, size=4)+
  
  #add site names
  ggrepel::geom_text_repel(urch_den_spatial %>% filter(species == "purple urchin",
                                                       region == "Humboldt") %>%
                             mutate(Long = Long-0.04),
                           mapping=aes(x=Long, y=Lat, label=site_new), 
                           inherit.aes = F, size=1.5, max.overlaps = 1000, color="black") +
  # Labels
  labs(x="", y="", tag="",legend.position="none") +
  # Legend
  scale_color_manual(name="Species", values=c( "purple", "darkred")) +
  scale_shape_manual(name="Monitoring program", values=c(16, 17)) +
  scale_size(range = c(1,8), name="Sea urchin density")+
  guides(color = guide_legend(order = 1), shape = guide_legend(order = 2)) +
  
  # Crop
  coord_sf(xlim = c(-124.8, -123.3), ylim = c(41, 42)) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.title=element_blank(),
        #legend.position = c(0.15, 0.17), 
        #legend.key.size = unit(0.4, "cm"),
        legend.position="none")+
  ggtitle("Humboldt")
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
  #annotate("text",label = "Mendocino County", x= -123.2, y=38.768802+0.3, size=4)+
  #annotate("text",label = "Sonoma County", x= -123.2, y=38.768802-0.08, size=4)+
  
  #add site names
  ggrepel::geom_text_repel(urch_den_spatial %>% filter(species == "purple urchin",
                                                       region == "Mendocino") %>%
                             mutate(Long = Long-0.04),
                           mapping=aes(x=Long, y=Lat, label=site_new), 
                           inherit.aes = F, size=1.5, max.overlaps = 1000, color="black") +
  #geom_text(data=sites_label %>% filter(method == "kelp swath"),
  #         mapping=aes(x=long_adj, y=mean_lat, label=site, hjust=-0.4, vjust=0
  #),
  #size=2)+
  # Labels
  labs(x="", y="", tag="",legend.position="none") +
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
        #legend.position = c(0.15, 0.17), 
        #legend.key.size = unit(0.4, "cm"),
        legend.position="none")+
  ggtitle("Mendocino")
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
  #annotate("text",label = "Mendocino County", x= -123.2, y=38.768802+0.3, size=4)+
  #annotate("text",label = "Sonoma County", x= -123.2, y=38.768802-0.08, size=4)+
  
  #add site names
  ggrepel::geom_text_repel(urch_den_spatial %>% filter(species == "purple urchin",
                                                       region == "Point Arena") %>%
                             mutate(Long = Long-0.04),
                           mapping=aes(x=Long, y=Lat, label=site_new), 
                           inherit.aes = F, size=1.5, max.overlaps = 1000, color="black") +
  #geom_text(data=sites_label %>% filter(method == "kelp swath"),
  #         mapping=aes(x=long_adj, y=mean_lat, label=site, hjust=-0.4, vjust=0
  #),
  #size=2)+
  # Labels
  labs(x="", y="", tag="",legend.position="none") +
  # Legend
  scale_color_manual(name="Species", values=c( "purple", "darkred")) +
  scale_shape_manual(name="Monitoring program", values=c(16, 17)) +
  scale_size_binned(breaks = c(2,4,10,32),
                    labels = function(x) as.character(round(x,0)),
                    name = "Sea urchin density \n(no. per m²)")+
  guides(color = guide_legend(order = 1), shape = guide_legend(order = 2)) +
  
  # Crop
  coord_sf(xlim = c(-123.9, -123.55), ylim = c(38.85, 39.05)) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.title=element_blank(),
        #legend.position = c(0.15, 0.17), 
        #legend.key.size = unit(0.4, "cm"),
        legend.position="none")+
  ggtitle("Point Arena")
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
  #annotate("text",label = "Mendocino County", x= -123.2, y=38.768802+0.3, size=4)+
  #annotate("text",label = "Sonoma County", x= -123.2, y=38.768802-0.08, size=4)+
  
  #add site names
  ggrepel::geom_text_repel(urch_den_spatial %>% filter(species == "purple urchin",
                                                       region == "Sonoma") %>%
                             mutate(Long = Long-0.04),
                           mapping=aes(x=Long, y=Lat, label=site_new), 
                           inherit.aes = F, size=1.5, max.overlaps = 1000, color="black") +
  #geom_text(data=sites_label %>% filter(method == "kelp swath"),
  #         mapping=aes(x=long_adj, y=mean_lat, label=site, hjust=-0.4, vjust=0
  #),
  #size=2)+
  # Labels
  labs(x="", y="", tag="") +
  # Legend
  scale_color_manual(name="Species", values=c( "purple", "darkred")) +
  scale_shape_manual(name="Monitoring program", values=c(16, 17)) +
  scale_size_binned(breaks = c(2,4,10,32),
                    labels = function(x) as.character(round(x,0)),
                    name = "Sea urchin density \n(no. per m²)")+
  guides(color = guide_legend(order = 1), shape = guide_legend(order = 2)) +
  
  # Crop
  coord_sf(xlim = c(-123.65, -123.2), ylim = c(38.45, 38.75)) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.title=element_blank(),
        #legend.position = c(0.15, 0.17), 
        #legend.key.size = unit(0.4, "cm"),
        legend.position="none"
  )+
  ggtitle("Sonoma")

# Add inset
#annotation_custom(grob = g2_inset, 
#                 xmin = -123.4, 
#                xmax = -122.8,
#               ymin = 39.2) 

g4



combined_plot <- gridExtra::grid.arrange(g1, g2, g3, g4)



get_only_legend <- function(plot) {
  
  # get tabular interpretation of plot
  plot_table <- ggplot_gtable(ggplot_build(plot)) 
  
  #  Mark only legend in plot
  legend_plot <- which(sapply(plot_table$grobs, function(x) x$name) == "guide-box") 
  
  # extract legend
  legend <- plot_table$grobs[[legend_plot]]
  
  # return legend
  return(legend) 
}

g_legend1 <- ggplot() +
  geom_point(data=urch_den_spatial %>%
               mutate(Long = ifelse(species == "red urchin",
                                    Long-0.02, Long)),
             aes(x=Long, y=Lat, color=species), alpha=0.6) +
  # Legend
  scale_color_manual(name="Species", values=c( "purple", "darkred")) +
  scale_shape_manual(name="Monitoring program", values=c(16, 17)) +
  scale_size_binned(breaks = c(2,4,10,32),
                    labels = function(x) as.character(round(x,0)),
                    name = "Sea urchin density \n(no. per m²)")+
  guides(color = guide_legend(order = 1), shape = guide_legend(order = 2)) +
  
  # Crop
  coord_sf(xlim = c(-123.65, -123.2), ylim = c(38.45, 38.75)) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.title=element_blank(),
        #legend.position = c(0.15, 0.17), 
        #legend.key.size = unit(0.4, "cm"),
        legend.position="bottom"
  )

g_legend2 <- ggplot() +
  geom_point(data=urch_den_spatial %>%
               mutate(Long = ifelse(species == "red urchin",
                                    Long-0.02, Long)),
             aes(x=Long, y=Lat, size=m2_den), alpha=0.6) +
  # Legend
  scale_color_manual(name="Species", values=c( "purple", "darkred")) +
  scale_shape_manual(name="Monitoring program", values=c(16, 17)) +
  scale_size_binned(breaks = c(2,4,10,32),
                    labels = function(x) as.character(round(x,0)),
                    name = "Sea urchin density \n(no. per m²)")+
  guides(color = guide_legend(order = 1), shape = guide_legend(order = 2)) +
  
  # Crop
  coord_sf(xlim = c(-123.65, -123.2), ylim = c(38.45, 38.75)) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.title=element_blank(),
        #legend.position = c(0.15, 0.17), 
        #legend.key.size = unit(0.4, "cm"),
        legend.position="bottom"
  )

legend1 <- get_only_legend(g_legend1)
legend2 <- get_only_legend(g_legend2)


g <- gridExtra::grid.arrange(combined_plot, legend1, legend2,
                             ncol=1,
                             heights = c(10,0.5,1))

g



# Export figure
ggsave(g, filename=file.path(figdir, "Rcca_urch_density_4regions.png"), 
       width=7, height=6.5, units="in", dpi=600)
