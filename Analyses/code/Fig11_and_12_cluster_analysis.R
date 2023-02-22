#Joshua G. Smith
#February 2, 2023

rm(list=ls())

require(tidyverse)
require(gghighlight)
require(moments)

#set directories
datadir <- "/Users/Joshua/Box Sync/Data"
gisdir <- file.path(datadir, "gis_data/processed")
figdir <- "/Users/Joshua/Box Sync/hotspot_analyses/Analyses/figures"
tabdir <- "/Users/Joshua/Box Sync/hotspot_analyses/Analyses/tables"

#read combined density data
urch_den <- read.csv(file.path(datadir, "monitoring_processed/mlpa_rcca_urchin_density_combined.csv"))%>%
  mutate(site_new = sub("(.)", "\\U\\1", site_new, perl=TRUE)) %>%
  mutate(region = ifelse(site_new == "Saunders reef smca ref","Point Arena",region))%>%
  #examine rcca only
  filter(survey == "rcca")

#read size data
urch_size <- readRDS(file.path(datadir, "/monitoring_processed/rcca_mean_density_2016-22.Rda"))
rcca_size_raw <-  read.csv(file.path(datadir, "monitoring_processed/rcca_urchin_sizefq.csv"))%>%
  mutate(site_new = sub("(.)", "\\U\\1", site_new, perl=TRUE))


#examine data using gghighlight
ggplot(urch_purp, aes(year, m2_den, color=site_new))+
  geom_line(stat="identity")+
  ylab("Purple sea urchin density")+
  gghighlight(mean(m2_den) > 15,
              max_highlight = 4,
              use_direct_label = TRUE)+
  theme_minimal()+
  theme(legend.position = "none")


###############################################################################
#prep data for clustering
#NOTE:: since time series is incomplte, cannot calculate temporal cluster,
#so take post-heatwave average at each site. 

#calculate summary statistics

#calculate skewness

DatS <- vcdExtra::expand.dft(rcca_size_raw, freq="count")

skew1 <- rcca_size_raw %>%
            filter(year > 2016)%>%
            group_by(year, site_new, species)%>%
            summarize(askew = skewness(size))

#caluclate size mean size
site_mean_size <- DatS %>%
                  group_by(year, site_new, species)%>%
                  summarize(mean_size = mean(size))


#join density, size, skewness
rcca_den_size1 <- left_join(urch_den, site_mean_size, by=c("year","site_new", "species"))
rcca_den_size <- left_join(rcca_den_size1, skew1, by=c("year","site_new", "species"))

urch_red <- rcca_den_size %>% filter(species=="red urchin") %>% 
                    mutate(site_new = factor(site_new))
urch_purp <- rcca_den_size %>% filter(species=="purple urchin")%>% 
                     mutate(site_new = factor(site_new))


urch_red_dat <- urch_red %>%
                      filter(year >2016)%>%
                      group_by(site_new)%>%
                      summarize(n = n(),
                                u_den = mean(m2_den, na.rm=TRUE),
                                u_size = mean(mean_size, na.rm=TRUE),
                                u_skew = mean(askew, na.rm=TRUE),
                                sd_den = sd(m2_den, na.rm=TRUE),
                                sd_size = sd(mean_size, na.rm=TRUE),
                                se_den = sd_den / sqrt(n),
                                se_size = sd_size/ sqrt(n)
                                )%>%
                      filter(!(is.na(u_size)))


urch_purp_dat <- urch_purp %>%
  filter(year >2016)%>%
  group_by(site_new)%>%
  summarize(n = n(),
            u_den = mean(m2_den, na.rm=TRUE),
            u_size = mean(mean_size, na.rm=TRUE),
            u_skew = mean(askew, na.rm=TRUE),
            sd_den = sd(m2_den, na.rm=TRUE),
            sd_size = sd(mean_size, na.rm=TRUE),
            se_den = sd_den / sqrt(n),
            se_size = sd_size/ sqrt(n)
  )%>%
  filter(!(is.na(u_size)))



urch_red_clust <- urch_red_dat %>%
  select(u_den, u_size, u_skew)


urch_purp_clust <- urch_purp_dat %>%
  select(u_den, u_size, u_skew)


###############################################################################
#determine optimal clusters

wssplot <- function(data, nc=15, seed=1985){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of groups",
       ylab="Sum of squares within a group")
  }

wssplot(urch_red_clust, nc = 20)
wssplot(urch_purp_clust, nc = 20)


#run k means cluster
set.seed(1985)
k_mean_red <- kmeans(urch_red_clust, centers = 8, nstart = 20)
k_mean_purp <- kmeans(urch_purp_clust, centers = 4, nstart = 20)


#join data 
urch_red_dat$cluster <- as.factor(k_mean_red$cluster)
urch_purp_dat$cluster <- as.factor(k_mean_purp$cluster)




###############################################################################
#plot size and density by cluster for purple urchins

plot_dat1 <- urch_purp_dat %>% pivot_longer(cols=c(u_den, u_size), names_to="variable", values_to="value") %>%
                  mutate(clust_order =  ifelse(cluster == "2","Cluster 1",
                                              ifelse(cluster=="3","Cluster 2",
                                                    ifelse(cluster=="4","Cluster 3","Cluster 4"))))

size_error <- plot_dat1 %>% filter(variable == "u_size") %>% select(site_new, "se" = se_size) %>% mutate(variable = "u_size")
den_error <- plot_dat1 %>% filter(variable == "u_den") %>% select(site_new, "se" = se_den) %>% mutate(variable = "u_den")
error_dat <- rbind(size_error, den_error) 

plot_dat <- left_join(plot_dat1, error_dat, by=c("site_new","variable")) %>%
                      mutate(variable = recode(variable, "u_den" = "Density", 
                                               "u_size" = "Size")) %>%
                      filter(!(site_new == "Ocean cove kelper")) %>%
                      #add restoration sites
                      mutate(restoration_site = ifelse(site_new == "Noyo north" |
                                                         site_new == "Caspar north" |
                                                         site_new == "Caspar south" |
                                                         site_new == "Albion cove","*",""))



my_theme <-  theme(axis.text=element_text(size=8),
                   axis.title=element_text(size=10),
                   #legend
                   legend.text = element_text(size=8),
                   legend.key.size = unit(0.3,'cm'),
                   legend.title=element_text(size=10),
                   plot.tag=element_text(size=8),
                   plot.title = element_text(size=10),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_rect(fill=alpha('blue', 0)),
                   legend.background = element_rect(color=NA))

den_max <- plot_dat %>% filter(variable == "Density")
size_max <- plot_dat %>% filter(variable == "Size")
scaleFactor <- (max(den_max$value) / max(size_max$value))*.75


g1 <- ggplot(plot_dat %>%
              mutate(value = ifelse(variable == "Size",value*scaleFactor, value))
             , aes(site_new, value, fill=variable))+
  geom_bar(stat="identity", position = position_dodge())+
  geom_errorbar(aes(ymin=value-se, ymax=value+se), position=position_dodge(width=0.9), width=0.2)+
  #add restoration site
  geom_text(data=plot_dat %>% filter(variable == "Size"),aes(x = site_new, y=scaleFactor*(value+se), label=restoration_site), size=7)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  scale_y_continuous(  breaks = seq(0,45, by=5),
                     sec.axis = sec_axis( trans=~./scaleFactor, name="Size (cm)")
   )+
  #add minimum harvestable size
  geom_hline(yintercept = scaleFactor*4, color="purple", linetype="dashed", linewidth=0.75)+
  #add threshold density
  geom_hline(yintercept = 8, color="darkgreen", linetype="dashed", linewidth=0.75)+
  scale_fill_brewer(palette = "Accent")+
  labs(x="Site", y="Density (no. per m²)")+
  guides(fill=guide_legend(title="Variable"))+
  facet_wrap(~clust_order, scales="free_x")+
  ggtitle("Purple sea urchin")+
  my_theme

g1


ggsave(g1, filename=file.path(figdir, "Fig12_cluster_purp.png"), 
       width=7, height=7, units="in", dpi=600, bg="white")



###############################################################################
#plot size and density by cluster for red urchins

plot_dat1 <- urch_red_dat %>% pivot_longer(cols=c(u_den, u_size), names_to="variable", values_to="value") %>%
  mutate(clust_order =  ifelse(cluster == "2","Cluster 1",
                               ifelse(cluster=="1","Cluster 3",
                                      ifelse(cluster=="3","Cluster 2",
                                             ifelse(cluster=="5","Cluster 5",
                                                    ifelse(cluster=="6","Cluster 6",
                                                           ifelse(cluster=="7","Cluster 4",
                                      ifelse(cluster=="4","Cluster 7","Cluster 8"))))))))

size_error <- plot_dat1 %>% filter(variable == "u_size") %>% select(site_new, "se" = se_size) %>% mutate(variable = "u_size")
den_error <- plot_dat1 %>% filter(variable == "u_den") %>% select(site_new, "se" = se_den) %>% mutate(variable = "u_den")
error_dat <- rbind(size_error, den_error) 

plot_dat <- left_join(plot_dat1, error_dat, by=c("site_new","variable")) %>%
  mutate(variable = recode(variable, "u_den" = "Density", 
                           "u_size" = "Size")) %>%
  filter(!(site_new == "Ocean cove kelper")) %>%
  #add restoration sites
  mutate(restoration_site = ifelse(site_new == "Noyo north" |
                                     site_new == "Caspar north" |
                                     site_new == "Caspar south" |
                                     site_new == "Albion cove","*",""))



my_theme <-  theme(axis.text=element_text(size=8),
                   axis.title=element_text(size=10),
                   #legend
                   legend.text = element_text(size=8),
                   legend.key.size = unit(0.3,'cm'),
                   legend.title=element_text(size=10),
                   plot.tag=element_text(size=8),
                   plot.title = element_text(size=10),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_rect(fill=alpha('blue', 0)),
                   legend.background = element_rect(color=NA))

den_max <- plot_dat %>% filter(variable == "Density")
size_max <- plot_dat %>% filter(variable == "Size")
scaleFactor <- (max(den_max$value) / max(size_max$value))*.9


g2 <- ggplot(plot_dat %>%
               mutate(value = ifelse(variable == "Size",value*scaleFactor, value))
             , aes(site_new, value, fill=variable))+
  geom_bar(stat="identity", position = position_dodge())+
  geom_errorbar(aes(ymin=value-se, ymax=value+se), position=position_dodge(width=0.9), width=0.2)+
  #add restoration site
  geom_text(data=plot_dat %>% filter(variable == "Size"),aes(x = site_new, y=scaleFactor*(value+se+2), label=restoration_site), size=7)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  scale_y_continuous(  breaks = seq(0,4, by=1),
                       sec.axis = sec_axis( trans=~./scaleFactor, name="Size (cm)")
  )+
  #add minimum harvestable size
  geom_hline(yintercept = scaleFactor*8, color="darkorange", linetype="dashed", linewidth=0.75)+
  #add threshold density
  geom_hline(yintercept = 2, color="darkgreen", linetype="dashed", linewidth=0.75)+
  scale_fill_brewer(palette = "Dark2")+
  labs(x="Site", y="Density (no. per m²)")+
  guides(fill=guide_legend(title="Variable"))+
  facet_wrap(~clust_order, scales="free_x")+
  ggtitle("Red sea urchin")+
  my_theme

g2


ggsave(g2, filename=file.path(figdir, "Fig13_cluster_red.png"), 
       width=7, height=7, units="in", dpi=600, bg="white")










