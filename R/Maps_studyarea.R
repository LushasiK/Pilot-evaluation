
# Study Area Maps
# Arusha City and Kilosa District, Tanzania
# Author: Kennedy Lushasi

rm(list = ls())

# Load packages
library(sf)
library(lwgeom)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(ggspatial)
library(cowplot)
library(ggrepel)
library(scales)
library(here)
library(ragg)
library(cowplot)

theme_set(theme_bw())

# Read datasets

facilities <-read_csv(here("data","facilities_lat_log.csv"),show_col_types = FALSE)
region_shp <- st_read(here("data","GIS","regions_2022_population.shp"),quiet = TRUE)
district_shp <-st_read(here("data","GIS","districts_2022_population.shp"),quiet = TRUE)
ward_shp <-st_read(here("data","GIS","TZ_Ward_2012_pop.shp"),quiet = TRUE)
prot_areas <-st_read(here("data","GIS","Protected_areas.shp"), quiet = TRUE)

names(prot_areas)[names(prot_areas)=="Reg_ID"] <- "Region_Nam"


# Fix invalid geometries
region_shp   <- st_make_valid(region_shp)
district_shp <- st_make_valid(district_shp)
ward_shp     <- st_make_valid(ward_shp)
prot_areas   <- st_make_valid(prot_areas)

# Harmonize district names
# District &  ward shapefile
district_shp <-district_shp %>%
  mutate(District_std = recode(NewDist20, "Kilosa District" = "Kilosa","Arusha City" = "Arusha City"))

ward_shp <-ward_shp %>%
  mutate( District_std = recode(District_N,"Arusha Urban" = "Arusha City", "Kilosa" = "Kilosa" ))

# Facility dataset
facilities <-facilities %>% 
  mutate(District_std = case_when(District_facility %in% c("Arusha","Arusha Urban")
                                  ~ "Arusha City",District_facility=="Kilosa"
                                  ~ "Kilosa",TRUE 
                                  ~ District_facility ))

# Standardize facility types
study_facilities_sf <- facilities%>%
  mutate(FacilityType = case_when(`Facility type` %in%
                                    c("Regional Hospital","District Hospital","Hospital") ~ "Hospital",
                                  `Facility type` %in% 
                                    c("Health Center","Health Centre") ~ "Health Centre", 
                                  `Facility type` == "Dispensary" ~ "Dispensary",
                                  TRUE ~ "Other"))

# Update district datasets
arusha_fac <- study_facilities_sf %>%
  filter(District_std == "Arusha City")

kilosa_fac <- study_facilities_sf %>%
  filter(District_std == "Kilosa")

# Create study datasets
study_regions <- region_shp %>%
  filter(Region_Nam %in% c("Arusha", "Morogoro"))

study_districts <- district_shp %>% 
  filter(District_std %in% c("Arusha City", "Kilosa" ))

study_wards <-ward_shp %>%
  filter( District_std %in% c("Arusha City", "Kilosa"))

study_facilities <-facilities %>%
  filter(District_std %in%c("Arusha City", "Kilosa"))

# Convert facilities to sf
study_facilities_sf <-study_facilities %>%
  filter(!is.na(Longitude),
         !is.na(Latitude)) %>%
  st_as_sf(coords=c("Longitude", "Latitude"), crs=4326,remove=FALSE)


# Separate datasets
arusha_dist <-study_districts %>%
  filter(District_std=="Arusha City")

kilosa_dist <-study_districts %>%
  filter(District_std=="Kilosa")

arusha_wards <-study_wards %>%
  filter(District_std=="Arusha City")

kilosa_wards <-study_wards %>%
  filter(District_std=="Kilosa")

arusha_fac <-study_facilities_sf %>%
  filter(District_std=="Arusha City")

kilosa_fac <-study_facilities_sf %>%
  filter(District_std=="Kilosa")

# Manual label positions
## We avoid centroids because Tanzanian administrative polygons often contain invalid geometries.

label_points <- tibble(District_std = c("Arusha City","Kilosa"),
                       Longitude = c( 35.68,36.00),
                       Latitude = c(-3.36, -6.75),
                       Label = c( "Arusha City", "Kilosa District"))
label_points <-st_as_sf(label_points,coords=c("Longitude", "Latitude"),
                        crs=4326)


#------------------------Part 2--------------------------
# Create Tanzania outline
tanzania_outline <-region_shp %>%
  st_union() %>%
  st_sf(geometry = .)

# Arusha City point
arusha_city_pt <-arusha_dist %>%st_transform(32737) %>%
  st_point_on_surface() %>%
  st_transform(4326)

# Kilosa point
kilosa_pt <-kilosa_dist %>%
  st_transform(32737) %>%
  st_point_on_surface() %>%
  st_transform(4326)

# Tanzania overview map
map_tanzania1 <-ggplot() +
  geom_sf(data=tanzania_outline, fill="grey93",colour="grey55",linewidth=.40)+
  
# Study regions
geom_sf( data=study_regions,aes(fill=Region_Nam),colour="grey40",linewidth=.45)+
geom_sf(data=kilosa_dist,fill="#d95f02",colour="black",linewidth=.60)+ # Kilosa District
geom_sf(data=arusha_city_pt,shape=21,fill="#d73027",colour="black", stroke=.50, size=3)+ # Arusha CC
  
## Leader lines
annotate("curve", x=35.65,y=-3.35,xend=36.72,yend=-3.27, curvature=.20,linewidth=.45)+
annotate("curve",x=36.00, y=-6.70, xend=36.75,yend=-6.75,curvature=-.15,linewidth=.45 )+  

## Labels
annotate("text",x=35.60,y=-3.35,label="Arusha City",fontface="bold",hjust=1,size=3.8)+
annotate("text", x=35.95, y=-6.70,label="Kilosa District", fontface="bold",hjust=1,size=3.8)+  
# Colours
scale_fill_manual(values=c(Arusha="#2AA6A5",Morogoro="#F4A259"),name=NULL)+  
# North arrow
# annotation_north_arrow(location="tr",style=north_arrow_fancy_orienteering,
#                        height=unit(1.1,"cm"),
#                        width=unit(1.1,"cm"))+
# annotation_scale(location="bl", width_hint=.28)+  # Scale bar
# Theme
coord_sf(expand=FALSE,clip = "off")+
  theme_void(base_size=11)+
  theme(legend.position=c(.16,.16),
    legend.background=element_rect(fill=alpha("white",.90),colour=NA),
    legend.text= element_text(size=10),
    plot.margin=margin(2,40, 2,40))  

#--------- Display---------------------
map_tanzania1

#---------------- version option 2-----------------------
# Create Tanzania outline
tanzania_outline <-region_shp %>%
  st_union() %>%
  st_as_sf()

# Create representative points
# Use projected CRS to avoid geometry problems

arusha_city_pt <-arusha_dist %>% 
  st_transform(32737) %>%
  st_point_on_surface() %>%
  st_transform(4326)

kilosa_pt <-
  kilosa_dist %>%
  st_transform(32737) %>%
  st_point_on_surface() %>%
  st_transform(4326)

# Extract coordinates
arusha_xy <- st_coordinates(arusha_city_pt)
kilosa_xy <- st_coordinates(kilosa_pt)

# Label positions
label_df <- tibble(District=c("Arusha City","Kilosa District"),
                   x=c(35.5,35.7),y=c(-3.15,-6.95),
                   xend=c(arusha_xy[1],kilosa_xy[1]),
                   yend=c(arusha_xy[2],kilosa_xy[2]))

# Tanzania map
map_tanzania <-ggplot() + 
  geom_sf(data=tanzania_outline,fill="grey94",colour="grey65",linewidth=.40)+
geom_sf(data=study_regions,aes(fill=Region_Nam),colour="grey35",linewidth=.35)+   # Study Regions
geom_sf(data=kilosa_dist,fill="#D95F02",colour="black",linewidth=.60)+ # Kilosa District
geom_sf(data=arusha_city_pt,shape=21,fill="#D73027",colour="black",stroke=.45,size=3)+ # Arusha City
geom_segment(data=label_df,aes(x=x,y=y,xend=xend,yend=yend),linewidth=.40)+ # Leader lines
geom_label(data=label_df,aes(x=x, y=y,label=District),fontface="bold",size=3.3,label.size=.25,fill="white")+# Labels
scale_fill_manual(values=c(Arusha="#1F9E89",Morogoro="#F4A261"),name=NULL)+ # Colours
  
# North Arrow
# annotation_north_arrow(location="tr",which_north="true",style=north_arrow_fancy_orienteering,height=unit(1.0,"cm"),
#                        width=unit(1.0,"cm"))+
# Scale bar
#annotation_scale(location="bl",width_hint=.28)+
  
# Coordinates
coord_sf(expand=FALSE,clip = "off")+
  
# Theme
theme_void(base_size=11)+
  theme(legend.position=c(.15,.18),
        legend.background=element_rect(fill=alpha("white",.90), colour=NA),
        legend.text=element_text( size=10),
        plot.margin=margin(2,40,2,40))

# Display
map_tanzania






#-----------------PART 3A-------------------------------------
# Arusha City
# Create common population classes
pop_breaks <- c(0, 5000, 10000, 20000, 40000,Inf)
pop_labels <- c("<5,000","5,000–10,000", "10,000–20,000", "20,000–40,000", ">40,000")
#  SHARED COLOR PELLETTE
population_cols <- c("<5,000" = "#fff7bc","5,000–10,000" = "#fee391","10,000–20,000" = "#fec44f",
                     "20,000–40,000" = "#fe9929",">40,000" = "#d95f0e")
# Replace missing ward population(s) with the district median
kilosa_wards <- kilosa_wards %>%
  mutate(pop_2012 = ifelse(is.na(pop_2012),median(pop_2012, na.rm = TRUE),pop_2012))


# APPLY TO ARUSHA
arusha_wards <- arusha_wards %>%
  mutate(PopClass = cut(pop_2012,breaks = pop_breaks,labels = pop_labels,include.lowest = TRUE))

# APPLY TO KILOSA
kilosa_wards <- kilosa_wards %>%
  mutate(PopClass = cut(pop_2012, breaks = pop_breaks,labels = pop_labels,include.lowest = TRUE))



# ----------Arusha City Map---------
study_facilities_sf <- study_facilities_sf %>%
  mutate(FacilityType = case_when(`Facility type` %in%
                                    c("Regional Hospital","District Hospital","Hospital") ~ "Hospital",
                                  `Facility type` %in% 
                                    c("Health Center","Health Centre") ~ "Health Centre", 
                                  `Facility type` == "Dispensary" ~ "Dispensary",
                                  TRUE ~ "Other"))

# Update district datasets
arusha_fac <- study_facilities_sf %>%
  filter(District_std == "Arusha City")

kilosa_fac <- study_facilities_sf %>%
  filter(District_std == "Kilosa")


# Extract coordinates
coords <- st_coordinates(arusha_fac)
arusha_fac_plot <- arusha_fac %>%
  mutate(X = coords[,1],Y = coords[,2]) %>% 
  group_by(X, Y) %>%  
  mutate(n = row_number()) %>%
  ungroup() %>%
  mutate(X = ifelse(n == 2, X + 0.010, X),
         Y = ifelse(n == 2, Y + 0.010, Y)) %>%
  st_drop_geometry() %>%
  st_as_sf(coords = c("X", "Y"),crs = 4326)

# Offset overlapping facilities
coords <- st_coordinates(arusha_fac)
arusha_fac_plot <- arusha_fac %>%
  mutate(X = coords[,1],Y = coords[,2]) %>%
  group_by(X, Y) %>%
  mutate(offset = row_number()) %>%
  ungroup() %>% 
  mutate(X = case_when(offset == 1 ~ X - 0.0015, offset == 2 ~ X + 0.0015, TRUE ~ X),
         Y = case_when(offset == 1 ~ Y - 0.008,offset == 2 ~ Y + 0.008,TRUE ~ Y)) %>%
  st_drop_geometry() %>% 
  st_as_sf(coords = c("X", "Y"),crs = 4326)

# Arusha City map
map_arusha <-ggplot() +
  geom_sf(data = arusha_wards,aes(fill = PopClass),colour = "white",linewidth = 0.25) +   # Wards
  geom_sf(data = arusha_dist,fill = NA,colour = "black",linewidth = 0.8) + # District boundary
  geom_sf(data = arusha_fac_plot,aes(shape = FacilityType),colour = "black",size = 3) +   # Facilities
  scale_fill_manual(values = population_cols,drop = FALSE, name = "Ward population") +   # Population colours
  scale_shape_manual(values = c("Hospital" = 15,"Health Centre" = 17,"Dispensary" = 16),drop = FALSE,name = "Health facility") + # Facility symbols

  # North arrow
  # annotation_north_arrow(location = "tr", style = north_arrow_fancy_orienteering,
  #                        height = unit(0.8, "cm"),width = unit(0.8, "cm")) +
  #  Scale bar
  # annotation_scale(location = "bl",width_hint = 0.30) +
  coord_sf(expand = FALSE) +
  labs(title = "Arusha City") +
  theme_bw(base_size = 11) +
  theme(plot.title = element_text(face = "bold",hjust = 0.5,size = 13),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),# to remove the map frame
        legend.position = "right",
        legend.title = element_text(face = "bold"),legend.text =element_text(size = 11))
map_arusha

# KILOSA
# Standardize facility types
kilosa_fac <- study_facilities_sf %>%
  filter(District_std == "Kilosa")

# MAP KILOSA
map_kilosa <- ggplot() + geom_sf(data = kilosa_wards,aes(fill = PopClass), colour = "white", linewidth = 0.20) +

# District boundary
geom_sf(data = kilosa_dist,fill = NA,colour = "black",linewidth = 0.8) +
geom_sf(data = kilosa_fac,aes(shape = FacilityType),colour = "black",size = 3) + # Health facilities
  
  
  
## Population legend
scale_fill_manual(values = population_cols,breaks = names(population_cols),drop = FALSE,na.translate = FALSE,name = "Ward population")+
  
## Facility legend
scale_shape_manual(values = c("Health Centre" = 17,"Hospital" = 15, "Other" = 4),drop = FALSE, name = "Health facility") +
  
# # North arrow
# annotation_north_arrow(location = "tr",style = north_arrow_fancy_orienteering,height = unit(0.8, "cm"),width  = unit(0.8, "cm")) +
# 
# # Scale bar
# annotation_scale(location = "bl",width_hint = 0.30) +
  coord_sf(expand = FALSE) +
labs(title = "Kilosa District") +
  theme_bw(base_size = 11) +
  theme(plot.title = element_text(face = "bold",hjust = 0.5,size = 13),
axis.title = element_blank(),
axis.text = element_blank(),
axis.ticks = element_blank(),
panel.grid = element_blank(),
panel.border = element_blank(),
legend.position = "right", 
legend.title =element_text(face = "bold"),legend.text =element_text(size = 11))
# Display
map_kilosa


# combine maps  and share legends
legend <- get_legend(
  map_arusha +
    theme(
      legend.position      = "right",
      legend.direction     = "vertical",
      legend.box           = "vertical",
      legend.justification = "center",
      
      # Legend text
      legend.title = element_text(size = 15, face = "bold"),
      legend.text  = element_text(size = 13),
      
      # Legend symbols
      legend.key.size = unit(0.8, "cm"),
      
      # Spacing
      legend.spacing.y = unit(0.35, "cm"),
      legend.margin = margin(0, 0, 0, 0),
      legend.box.margin = margin(0, 0, 0, 0)
    )
)

# Remove legends
map_arusha2 <-map_arusha +annotation_scale(location = "bl", width_hint = 0.3,text_size = 12) +
  theme(legend.position = "none",plot.margin = margin(t = 0, r = -35, b = 0, l = 0, unit = "pt"))
map_kilosa2 <- map_kilosa +annotation_scale(location = "bl", width_hint = 0.4,text_size = 12) +
  theme(legend.position = "none",plot.margin = margin(t = 0, r = 0, b = 0, l = -35, unit = "pt"))


#--combine district maps-------------
map_tanzania2 <- map_tanzania1 +theme(legend.position = "none", plot.margin = margin(0,0,0,0))
bottom_panel <- plot_grid(map_arusha2,map_kilosa2,ncol = 2,rel_widths = c(1,1),align = "h")
figure_base <- plot_grid(map_tanzania2,bottom_panel,ncol = 1,rel_heights = c(0.80, 1)) +
  theme(plot.margin = margin(t = 5,r = 86,b = 5,l = 5))

# Overlay the legend
figure1 <- ggdraw(figure_base) +draw_plot(legend,x =0.82,y = 0.36, width = 0.10,height = 0.30)
figure1

ggsave(here("Figures","map_KilosaArusha.pdf"),figure1,width =8,height = 8,dpi = 600)
ggsave(here("Figures", "map_KilosaArusha.png"),figure1,width = 8,height = 8,units = "in", bg = "white")
