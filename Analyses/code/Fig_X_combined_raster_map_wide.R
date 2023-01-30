#Joshua G. Smith
#January 23, 2023

rm(list=ls())

require(tidyverse)
require(patchwork)
require(sf)
require(maptools)
require(mapdata)
require(Metrics)
require(terra)
require(tidyterra)
require(gstat)

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

#read RCCA data
urch_den <- read.csv(file.path(datadir, "monitoring_processed/mlpa_rcca_urchin_density_combined.csv"))

# Get land
states <- map_data("state") %>%
  filter(region %in% c("california","oregon"))%>%
  #WGS84
  sf::st_as_sf(coords = c("long","lat"), crs=4326) %>%
  group_by(region) %>% 
  summarize(do_union=FALSE) %>%
  st_cast("POLYGON") %>% 
  ungroup()

usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")
counties <- map_data("county") %>%
  filter(region == "california")%>%
  #WGS84
  sf::st_as_sf(coords = c("long","lat"),  crs = 4236) %>%
  group_by(region, subregion) %>% 
  summarize(do_union=FALSE) %>%
  st_cast("POLYGON") %>% 
  ungroup()

################################################################################
#aggregate data
density_mean <- urch_den %>%
                #calculate mean after MHW
                filter(year > 2016)%>%
                select(survey, region, site_new, species, Lat, Long, m2_den)%>%
                group_by(survey, region, site_new, species) %>%
                summarise_at(vars("Lat","Long","m2_den"), mean)

# create site number

urch_den1 <- urch_den %>%
  filter(species == "purple urchin")%>%
  arrange(desc(Lat))%>%
  mutate(site_number = seq(1:nrow(.))) %>%
  select(site_new, site_number)

urch_den_spatial <- left_join(urch_den, urch_den1, by="site_new") %>%
  mutate(site_name = paste(site_number, site_new))


density_dat_all <- st_as_sf(urch_den_spatial, coords = c("Long","Lat"),
                            crs=4326) 
density_dat_pur <- st_as_sf(urch_den_spatial, coords = c("Long","Lat"),
                            crs=4326) %>%
  filter(species == "purple urchin")

density_dat_red <- st_as_sf(urch_den_spatial, coords = c("Long","Lat"),
                            crs=4326) %>%
  filter(species == "red urchin")

####transoform to Teale Albers
states_T <- st_transform(states, crs = 3310)
counties_T <- st_transform(counties, crs=3310)
density_pur_T <- st_transform(density_dat_pur, crs=3310)
density_red_T <- st_transform(density_dat_red, crs=3310)


#create clipping buffer
CA_buff_1000 <- states_T %>%
  st_buffer(5000)

#check features
ggplot() +
  geom_sf(data = states_T)+
  geom_sf(data = CA_buff_1000)+
  geom_sf(data = counties_T)+
  geom_sf(data = density_pur_T, aes(col = m2_den))+
  coord_sf(xlim = c(-124.8, -123), ylim = c(38, 42.1), crs=4236) 


#CA_buff_inner <- st_crop(st_union(CA_buff_1000), st_union(states))



################################################################################
#create null model of original data
#https://bookdown.org/igisc/EnvDataSci/spatial-interpolation.html

RMSE <- function(observed, predicted) {
  sqrt(mean((predicted - observed)^2, na.rm=TRUE))
}
null <- RMSE(mean(density_pur_T$m2_den), density_pur_T$m2_den)
null

sd(density_dat_pur$m2_den)

n <- length(density_dat_pur$m2_den)
null*sqrt(n/(n-1))

################################################################################
#Veronoi polygons

# proximity (Voronoi/Thiessen) polygons
denV <- vect(density_pur_T)
v <- terra::voronoi(denV)
plot(v)
points(denV)

#crop to buffer
outer_crop <- vect(st_union(CA_buff_1000))
vDen <- terra::crop(v, outer_crop)

plot(vDen,"m2_den")

#crop to land
vDen1 <- crop(vDen, states_T)

plot(vDen1, "m2_den")

#rasterize the polygons
r <- rast(vDen1, res=500)  # Builds a blank raster of given dimensions and resolution  
# We're in Teale metres, so the res is 1000 m
vr <- rasterize(vDen1, r, "m2_den")
plot(vr)


#### plot
#https://dieghernan.github.io/tidyterra/
V <- ggplot(states_T) + 
  geom_spatraster(data=vr) +  
  scale_fill_whitebox_c(
    palette = "muted",
    labels = scales::label_number(#suffix = "Density"
    )
  )+
  #add states
  geom_sf(data = states_T)+
  #add counties
  geom_sf(data = counties_T)+
  #add survey sites
  geom_sf(data = density_pur_T)+
  #crop
  coord_sf(xlim = c(-124.8, -123), ylim = c(38, 42.1), crs=4326) +
  theme_minimal()+
  labs(fill = "Sea urchin \ndensity \n(no. per m²)")



################################################################################
#IDW purple

d <- data.frame(geom(denV)[,c("x", "y")], as.data.frame(denV))

gs <- gstat(formula=m2_den~1, locations=~x+y, data=d, nmax=Inf, set=list(idp=2))
idw <- interpolate(r, gs, debug.level=0)
idwr <- mask(idw, vr)
plot(idwr, 1)


# Theme
my_theme <-  theme(axis.text=element_text(size=5),
                   axis.title=element_text(size=6),
                   #legend
                   legend.text = element_text(size=4),
                   legend.key.size = unit(0.3,'cm'),
                   legend.title=element_text(size=5),
                   plot.tag=element_text(size=4),
                   plot.title = element_text(size=6),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_rect(fill=alpha('blue', 0)),
                   legend.background = element_rect(color=NA))


#### plot
idw_pur <- ggplot(states_T) + 
  geom_spatraster(data=idwr) +  
  scale_fill_whitebox_c(
    palette = "muted",
    labels = scales::label_number(#suffix = "Density",
    )
  )+
  #add states
  geom_sf(data = states_T)+
  #add counties
  geom_sf(data = counties_T)+
  #add survey sites
  geom_sf(data = density_pur_T, aes(shape=survey), size=0.5)+
  #crop
  coord_sf(xlim = c(-124.6, -123.2), ylim = c(38.1, 42.1), crs=4326) +
  theme_minimal()+
  labs(fill = "Density \n(no. per m²)")+
  ggtitle("Purple sea urchin")+
  scale_x_continuous(breaks = c(-124, -123.5))+
  my_theme+
  theme(legend.key = element_blank())


idw_pur

#ggpubr::ggarrange(V, idw)


################################################################################
#IDW red


denV <- vect(density_red_T)
v <- terra::voronoi(denV)
vDen <- terra::crop(v, outer_crop)
vDen1 <- crop(vDen, states_T)
r <- rast(vDen1, res=500)
vr <- rasterize(vDen1, r, "m2_den")


d <- data.frame(geom(denV)[,c("x", "y")], as.data.frame(denV))

gs <- gstat(formula=m2_den~1, locations=~x+y, data=d, nmax=Inf, set=list(idp=2))
idw <- interpolate(r, gs, debug.level=0)
idwr <- mask(idw, vr)
plot(idwr, 1)



#### plot
idw_red <- ggplot(states_T) + 
  geom_spatraster(data=idwr) +  
  scale_fill_whitebox_c(
    palette = "muted",
    labels = scales::label_number(#suffix = "Density"
    )
  )+
  #add states
  geom_sf(data = states_T)+
  #add counties
  geom_sf(data = counties_T)+
  #add sites
  geom_sf(data = density_red_T, aes(shape=survey), size=0.5)+
  #scale_color_manual(values=c("species"="black"))+
  #crop
  coord_sf(xlim = c(-124.6, -123.2), ylim = c(38.1, 42.1), crs=4326) +
  #add survey sites
  theme_minimal()+
  labs(fill = "Density \n(no. per m²)")+
  ggtitle("Red sea urchin") +
  my_theme+
  scale_x_continuous(breaks = c(-124, -123.5))+
  theme(legend.key = element_blank())

idw_red

g <- ggpubr::ggarrange(idw_pur, idw_red)

# Export figure
ggsave(g, filename=file.path(figdir, "combined_urch_density_statewide_raster.png"), 
       width=5, height=7, units="in", dpi=600, bg="white")



####help links

#modify palette 
#https://www.r-bloggers.com/2022/12/hillshade-colors-and-marginal-plots-with-tidyterra-ii/

###fix projections and zoom
#https://datascience.blog.wzb.eu/2019/04/30/zooming-in-on-maps-with-sf-and-ggplot2/



#####maybe need to convert terra to sf?
#https://stackoverflow.com/questions/73825468/converting-spatvector-objects-to-data-frames-for-use-in-ggplot2





