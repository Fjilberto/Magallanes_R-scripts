#
# Description:
#
# Script to plot a map with sampling locations. Include the option to add landform names, colors by country or zone delimitation
# Includes codes lines to add Pie Chart in each sampling locations to show species assignment or any other information.
#

rm(list=ls())
graphics.off()
setwd("Where_you_want_save_your_plot")

library(ggplot2) 
library(rgeos)
library(rgdal)  
library(raster)  
library(ggsn) 
library(rworldmap)
library(rworldxtra)
library(scatterpie)
library(ggrepel)

# Input data

myti_pop <-data.frame(pop=c("your_pop_names"),
                      Lat=c("your latitudes",
                      Lon=c("your longitudes"),
                      #pais=c("if you want to add the country where is the population: add your country names by each pop"),
                      #zone=c("if you want to add zones by each pop")) 

#PieCharts coordinates

move1 <-data.frame(pop=c("your_pop_names"),
                   Lat=c(your latitudes),
                   Lon=c(your longitudes),
                   Group1=c(Individuals asigned by population to Group1),
                   Group2=c(Individuals asigned by population to Group2),
                   Group3=c(Individuals asigned by population to Group3))

# Each group can be a species or hybrids between some species

#### Map

# Get world map
world <- getMap(resolution = "high")

# Preliminary plot
ggplot(myti_pop, mapping = aes(x = Lon, y = Lat)) + 
  geom_point(alpha = 0.5)

# Get map data
#extent
clipper_chile <- as(extent(-74, -57, -56, -51), "SpatialPolygons")
proj4string(clipper_chile) <- CRS(proj4string(world))
world_clip <- raster::intersect(world, clipper_chile)
world_clip_f <- fortify(world_clip)

# Plot Map

ggplot() + 
  geom_polygon(data = world_clip_f, 
               aes(x = long, y = lat, group = group),
               fill = "#E5E5E5", colour = "black") +
  geom_scatterpie(aes(x=Lon, y=Lat, group=pop),data = move1,
                  cols=c("Group1","Group2","Group3"),pie_scale = 1) + coord_equal() +
  scale_fill_manual("Group",values = c("indianred1","plum","gray75")) +
  # Labels for coordinates. "x" is the last row with information for each population.
  geom_label_repel(data = myti_pop[1:x,],aes(label=pop, x = Lon, y = Lat),
                   fill = "white",
                   alpha = 1, size = 4, fontface=2, #alpha manipula la transparencia, 1 es sin 
                   nudge_x = c(-1,1,1.5,-1,0.4,-0.2,-0.6,0.4,1), 
                   nudge_y = c(-0.7,0.5,0,0.3,0.3,0.2,-0.2,0.2,0.2),
  ) +
  # Use only if you added to your input coordinates for geographical accidents and want it plotted in it. "x" is the last row with information for each population. "y" is the last row with geographical accident data
  geom_text_repel(data = myti_pop[x+1:y,],aes(label=pop, x = Lon, y = Lat),
                  alpha = 1, size = 4, fontface=2,segment.color = c(NA,rep("black",9)), #alpha manipula la transparencia, 1 es sin 
                  nudge_x = c(0.5,4,0.4,-2.5,-3,-1,rep(0,2),0.5,1.5), 
                  nudge_y = c(0.7,0.6,-0.3,-1.2,-1,-1,rep(0,2),-0.8,-0.5),
  ) +
  ggtitle("Sampling locations by zone") +
  theme_bw() +
  labs(x="Longitude",y="Latitude",color="Zone") +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(plot.title = element_text(hjust = 0.5,lineheight=.8, face="bold")) +
  coord_quickmap() +
  geom_point(aes(x = Lon, y = Lat, shape = factor(zone)),size=c(rep(3,9),rep(0,2),rep(0.5,4),rep(0,2),0.5,0.5),
            data = myti_pop,color=c(rep("black",9),"white","#E5E5E5",rep("black",4),rep("white",2),"black","black")) + 
  guides(color = guide_legend(override.aes = list(linetype = 0, size=3))) +
  #coord_fixed() +
  theme(legend.position = "none")

### Save map as tiff file

ggsave(path = getwd(), filename = "sampling_locationspie.tiff", width = 16, height = 8.3, device='tiff', dpi=300)
