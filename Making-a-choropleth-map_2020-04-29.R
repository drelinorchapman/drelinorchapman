### Making a choropleth map ###
### Dr. Elinor Chapman 2020-04-28 ###
#Install packages # Like buying a book and putting it on the shelf #
install.packages("tidyverse")
install.packages("sf")
install.packages("mapview")
install.packages("dplyr")
install.packages("ggplot2")
#Open packages using library function # Like getting a book off the shelf #
library(tidyverse)
library(sf)
library(mapview)
library(viridis)
library(dplyr)
library(ggplot2)
###Set a working directory #
setwd("c:/Users/echapman/Desktop/STATS/")
#### REGIONS MAP ####
### REGIONS API Query URL"
# Reading spatial data --------------------------- 
#Downloads from the ONS website#
Regions <- st_read(paste0("https://ons-inspire.esriuk.com/arcgis/rest/services/Administrative_Boundaries/Regions_December_2019_Boundaries_EN_BUC/MapServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json"))
# This can take a long time. # It plots a the Regions uncoloured
plot(st_geometry(Regions))
dev.copy(png,'Regions.png')
dev.off()
##?st_geometry = it's part of Package sf

#Find out what the name of the Regions are?
View(Regions) #note the heading columns with the text in it.
Regions$rgn19nm
#make a new column called region identical to other column.
Regions$region <- Regions$rgn19nm
Regions$region
#[1] North East               North West               Yorkshire and The Humber East Midlands           
#[5] West Midlands            East of England          London                   South East              
#[9] South West              
#9 Levels: East Midlands East of England London North East North West South East ... Yorkshire and The Humber

#### DATA ####
# Import data --------------------------- 
LungCancerRegioMortStats <- read.csv("/Nomis/DATALUNGMORTALITY.csv")
LungCancerRegioMortStats$Mortality.statisticsfactor <-LungCancerRegioMortStats$Mortality.statistics..C33.C34.Malignant.neoplasm.of.trachea..bronchus.and.lung..2018
class(LungCancerRegioMortStats$Mortality.statisticsfactor)
#Make this column a factor (not numeric or integer)
LungCancerRegioMortStats$Mortality.statisticsfactor <- as.factor(LungCancerRegioMortStats$Mortality.statisticsfactor)
#Find out what the name of the Regions are?
LungCancerRegioMortStats$region
#[1] East                     East Midlands            London                   North East              
#[5] North West               South East               South West               West Midlands           
#[9] Yorkshire and The Humber
#9 Levels: East East Midlands London North East North West South East South West ... Yorkshire and The Humber

##Problem "East" in LungCancerRegioMortStats will not merge with "East of England" in Regions !!!
##Rename "East" to "East of England" in the csv file ##

# Joining spatial data --------------------------- 
Lung_Cancer_Region_Mort <- left_join(Regions, LungCancerRegioMortStats, by ="region")

# Creating a choropleth map --------------------------- 
#LungCancerRegioMortStats$Mortality.statistics..C33.C34.Malignant.neoplasm.of.trachea..bronchus.and.lung..2018
# equal intervals # HAS TO BE NUMERIC
graph1 <- ggplot() +
  geom_sf(data = Lung_Cancer_Region_Mort,
          aes(fill = cut_number(Mortality.statistics..C33.C34.Malignant.neoplasm.of.trachea..bronchus.and.lung..2018, 5)),
          alpha = 0.8,
          colour = 'white',
          size = 0.3) +
  scale_fill_brewer(palette = "PuBu",
                    name = "Absolute Number (2018)") +
  labs(x = NULL, y = NULL,
       title = "Mortality Statistics (C33-C34 Malignant neoplasm of trachea, bronchus and lung) 2018",
       subtitle = "Source: Mortality statistics [from ONS, Nomis on 27 September 2019]",
       caption = "Contains ONS Crown © Copyright Reserved (2018)") +
  theme(line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank()) +
  coord_sf(datum = NA)
graph1
ggsave("graph1.png", plot = graph1)

## 9 bins ## because 9 regions
##Change aes(fill=cut_number(Category, 9))
graph1B <- ggplot() +
  geom_sf(data = Lung_Cancer_Region_Mort,
          aes(fill = cut_number(Mortality.statistics..C33.C34.Malignant.neoplasm.of.trachea..bronchus.and.lung..2018, 9)),
          alpha = 0.8,
          colour = 'white',
          size = 0.3) +
  scale_fill_brewer(palette = "PuBu",
                    name = "Absolute Number (2018)") +
  labs(x = NULL, y = NULL,
       title = "Mortality Statistics (C33-C34 Malignant neoplasm of trachea, bronchus and lung) 2018",
       subtitle = "Source: Mortality statistics [from ONS, Nomis on 27 September 2019]",
       caption = "Contains ONS Crown © Copyright Reserved (2018)") +
  theme(line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank()) +
  coord_sf(datum = NA)
graph1B
ggsave("graph1B.png", plot = graph1B)

# Change colour scale - # scale_fill_viridis 
# discrete = F 
# add hozintal colourbar guide

graph2 <- ggplot(Lung_Cancer_Region_Mort, aes(fill = Mortality.statistics..C33.C34.Malignant.neoplasm.of.trachea..bronchus.and.lung..2018)) +
  geom_sf(alpha = 0.8, colour = 'white', size = 0.3) +
  scale_fill_viridis(discrete = F,
                     name = "Absolute Number (2018)", 
                     direction = -1, 
                     guide = guide_colourbar(
                       direction = "horizontal",
                       barheight = unit(2, units = "mm"),
                       barwidth = unit(50, units = "mm"),
                       draw.ulim = F,
                       title.position = 'top',
                       title.hjust = 0.5,
                       label.hjust = 0.5)) +
  labs(x = NULL, y = NULL, 
       title = "Mortality Statistics (C33-C34 Malignant neoplasm of trachea, bronchus and lung) 2018",
       subtitle = "Source: Mortality statistics [from ONS, Nomis on 27 September 2019]",
       caption = "Contains ONS Crown © Copyright Reserved (2018)") +
  coord_sf(datum = NA) +
  theme(line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank(),
        legend.position = c(0.2, 0.09),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8))
graph2
ggsave("graph2.png", plot = graph2)
# Change colour scale 2nd time. # inside option = scale_fill_viridis "A"
graph2A <- ggplot(Lung_Cancer_Region_Mort, aes(fill = Mortality.statistics..C33.C34.Malignant.neoplasm.of.trachea..bronchus.and.lung..2018)) +
  geom_sf(alpha = 0.8, colour = 'white', size = 0.3) +
  scale_fill_viridis(discrete = F,
                     name = "Absolute Number (2018)", 
                     direction = -1, 
                     option = "A", 
                     guide = guide_colourbar(
                       direction = "horizontal",
                       barheight = unit(2, units = "mm"),
                       barwidth = unit(50, units = "mm"),
                       draw.ulim = F,
                       title.position = 'top',
                       title.hjust = 0.5,
                       label.hjust = 0.5)) +
  labs(x = NULL, y = NULL, 
       title = "Mortality Statistics (C33-C34 Malignant neoplasm of trachea, bronchus and lung) 2018",
       subtitle = "Source: Mortality statistics [from ONS, Nomis on 27 September 2019]",
       caption = "Contains ONS Crown © Copyright Reserved (2018)") +
  coord_sf(datum = NA) +
  theme(line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank(),
        legend.position = c(0.2, 0.09),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8))
graph2A
ggsave("graph2A.png", plot = graph2A)
# Change colour scale 3rd time. # inside option = scale_fill_viridis "B"
graph2B <- ggplot(Lung_Cancer_Region_Mort, aes(fill = Mortality.statistics..C33.C34.Malignant.neoplasm.of.trachea..bronchus.and.lung..2018)) +
  geom_sf(alpha = 0.8, colour = 'white', size = 0.3) +
  scale_fill_viridis(discrete = F,
                     name = "Absolute Number (2018)", 
                     direction = -1, 
                     option = "B", 
                     guide = guide_colourbar(
                       direction = "horizontal",
                       barheight = unit(2, units = "mm"),
                       barwidth = unit(50, units = "mm"),
                       draw.ulim = F,
                       title.position = 'top',
                       title.hjust = 0.5,
                       label.hjust = 0.5)) +
  labs(x = NULL, y = NULL, 
       title = "Mortality Statistics (C33-C34 Malignant neoplasm of trachea, bronchus and lung) 2018",
       subtitle = "Source: Mortality statistics [from ONS, Nomis on 27 September 2019]",
       caption = "Contains ONS Crown © Copyright Reserved (2018)") +
  coord_sf(datum = NA) +
  theme(line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank(),
        legend.position = c(0.2, 0.09),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8))
graph2B
ggsave("graph2B.png", plot = graph2B)
# Change colour scale 4th time. # inside option = scale_fill_viridis "C"
graph2C <- ggplot(Lung_Cancer_Region_Mort, aes(fill = Mortality.statistics..C33.C34.Malignant.neoplasm.of.trachea..bronchus.and.lung..2018)) +
  geom_sf(alpha = 0.8, colour = 'white', size = 0.3) +
  scale_fill_viridis(discrete = F,
                     name = "Absolute Number (2018)", 
                     direction = -1, 
                     option = "C", 
                     guide = guide_colourbar(
                       direction = "horizontal",
                       barheight = unit(2, units = "mm"),
                       barwidth = unit(50, units = "mm"),
                       draw.ulim = F,
                       title.position = 'top',
                       title.hjust = 0.5,
                       label.hjust = 0.5)) +
  labs(x = NULL, y = NULL, 
       title = "Mortality Statistics (C33-C34 Malignant neoplasm of trachea, bronchus and lung) 2018",
       subtitle = "Source: Mortality statistics [from ONS, Nomis on 27 September 2019]",
       caption = "Contains ONS Crown © Copyright Reserved (2018)") +
  coord_sf(datum = NA) +
  theme(line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank(),
        legend.position = c(0.2, 0.09),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8))
graph2C
ggsave("graph2C.png", plot = graph2C)

# Change colour scale 5th time. # inside option = scale_fill_viridis "E"
graph2E <- ggplot(Lung_Cancer_Region_Mort, aes(fill = Mortality.statistics..C33.C34.Malignant.neoplasm.of.trachea..bronchus.and.lung..2018)) +
  geom_sf(alpha = 0.8, colour = 'white', size = 0.3) +
  scale_fill_viridis(discrete = F,
                     name = "Absolute Number (2018)", 
                     direction = -1, 
                     option = "E", 
                     guide = guide_colourbar(
                       direction = "horizontal",
                       barheight = unit(2, units = "mm"),
                       barwidth = unit(50, units = "mm"),
                       draw.ulim = F,
                       title.position = 'top',
                       title.hjust = 0.5,
                       label.hjust = 0.5)) +
  labs(x = NULL, y = NULL, 
       title = "Mortality Statistics (C33-C34 Malignant neoplasm of trachea, bronchus and lung) 2018",
       subtitle = "Source: Mortality statistics [from ONS, Nomis on 27 September 2019]",
       caption = "Contains ONS Crown © Copyright Reserved (2018)") +
  coord_sf(datum = NA) +
  theme(line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank(),
        legend.position = c(0.2, 0.09),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8))
graph2E
ggsave("graph2E.png", plot = graph2E)


######### ADDING LABELS #########
### geom_text ###
graph2Elabels <- ggplot(Lung_Cancer_Region_Mort, aes(fill = Mortality.statistics..C33.C34.Malignant.neoplasm.of.trachea..bronchus.and.lung..2018)) +
  geom_sf(alpha = 0.8, colour = 'white', size = 0.3) +
  scale_fill_viridis(discrete = F,
                     name = "Absolute Number (2018)", 
                     direction = -1, 
                     option = "E", 
                     guide = guide_colourbar(
                       direction = "horizontal",
                       barheight = unit(2, units = "mm"),
                       barwidth = unit(50, units = "mm"),
                       draw.ulim = F,
                       title.position = 'top',
                       title.hjust = 0.5,
                       label.hjust = 0.5)) +
  labs(x = NULL, y = NULL, 
       title = "Mortality Statistics (C33-C34 Malignant neoplasm of trachea, bronchus and lung) 2018",
       subtitle = "Source: Mortality statistics [from ONS, Nomis on 27 September 2019]",
       caption = "Contains ONS Crown © Copyright Reserved (2018)") +
  coord_sf(datum = NA) +
  theme(line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank(),
        legend.position = c(0.2, 0.09),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8)) +
  geom_text(data=Lung_Cancer_Region_Mort, aes(long, lat, label = paste(region, Mortality.statistics..C33.C34.Malignant.neoplasm.of.trachea..bronchus.and.lung..2018, sep="\n") ), size=2)
graph2Elabels
ggsave("graph2Elabels.png", plot = graph2Elabels)

######### ADDING LABELS #########
graph2Elabels2 <- ggplot(Lung_Cancer_Region_Mort, aes(fill = Mortality.statistics..C33.C34.Malignant.neoplasm.of.trachea..bronchus.and.lung..2018)) +
  geom_sf(alpha = 0.8, colour = 'white', size = 0.3) +
  scale_fill_viridis(discrete = F,
                     name = "Absolute Number (2018)", 
                     direction = -1, 
                     option = "E", 
                     guide = guide_colourbar(
                       direction = "horizontal",
                       barheight = unit(2, units = "mm"),
                       barwidth = unit(50, units = "mm"),
                       draw.ulim = F,
                       title.position = 'top',
                       title.hjust = 0.5,
                       label.hjust = 0.5)) +
  labs(x = NULL, y = NULL, 
       title = "Mortality Statistics (C33-C34 Malignant neoplasm of trachea, bronchus and lung) 2018",
       subtitle = "Source: Mortality statistics [from ONS, Nomis on 27 September 2019]",
       caption = "Contains ONS Crown © Copyright Reserved (2018)") +
  coord_sf(datum = NA) +
  theme(line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank(),
        legend.position = c(0.2, 0.09),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8)) +
  geom_text(data=Lung_Cancer_Region_Mort, aes(long, lat, label = paste(Mortality.statistics..C33.C34.Malignant.neoplasm.of.trachea..bronchus.and.lung..2018) ), size=3)
graph2Elabels2
ggsave("graph2Elabels2.png", plot = graph2Elabels2)
ggsave("graph2Elabels2.svg", plot = graph2Elabels2)

# natural breaks # MUST BE A FACTOR FOR THIS TO BE THE CASE
#class(Lung_Cancer_Region_Mort$Mortality.statistics..C33.C34.Malignant.neoplasm.of.trachea..bronchus.and.lung..2018)
#Lung_Cancer_Region_Mort$Mortality.statistics..C33.C34.Malignant.neoplasm.of.trachea..bronchus.and.lung..2018 <- as.factor(Lung_Cancer_Region_Mort$Mortality.statistics..C33.C34.Malignant.neoplasm.of.trachea..bronchus.and.lung..2018)
graph3 <- ggplot() +
  geom_sf(data = Lung_Cancer_Region_Mort,
          aes(fill = Mortality.statisticsfactor),
          alpha = 0.8,
          colour = 'white',
          size = 0.3) +
  scale_fill_brewer(palette = "PuBu",
                    name = "Absolute Number (2018)") +
  labs(x = NULL, y = NULL,
       title = "Mortality Statistics (C33-C34 Malignant neoplasm of trachea, bronchus and lung) 2018",
       subtitle = "Source: Mortality statistics [from ONS, Nomis on 27 September 2019]",
       caption = "Contains ONS Crown © Copyright Reserved (2018)") +
  theme(line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank()) +
  coord_sf(datum = NA)
graph3
ggsave("graph3.png", plot = graph3)