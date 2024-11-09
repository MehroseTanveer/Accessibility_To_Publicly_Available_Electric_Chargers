rm(list = ls())

# Libraries 
library(tidyverse)
library(sf)
library(nngeo)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(patchwork)
library(dplyr)
library(stars)

# Setting the path for shape files
setwd("C:/Users/asd/Documents/Data_Sweden")

# Reading the shape files
Kommun <- read_sf("Kommun_Sweref99TM_region.shp")
reg <- read_sf("Lan_Sweref99TM_region.shp")
roads <- read_sf("Dalarna1NVDB_DKVagtrafiknat.shp")
char <- read_sf("ladd_dalarna_SWEREF_99.shp")

#Reading the data file
pop <- read_sf("Totalbefolkning_1km_191231.gpkg")

#Filtering the data frame for visuals
Dal_reg<-filter(reg, LnNamn=="Dalarnas")
muni_dal<-filter(Kommun, KnKod>"2000"&KnKod<"2100")

pop_dal<-pop[Dal_reg,]

char_dal<-subset(char, chargers_7 == "20" & X!=0 & Y!=0)

#Fixing coordinates system to SweRef 99TM
st_crs (Dal_reg) <-"EPSG3006"
st_crs (char)<-"EPSG3006"
st_crs (pop_dal)<-"EPSG3006"
st_crs (muni_dal)<-"EPSG3006"
st_crs (char_dal)<-"EPSG3006"

# Calculating the nearest charging stations and distance
nearest <- st_nn(pop_dal, char_dal, returnDist = TRUE, progress = TRUE)
distance <- unlist(nearest$dist)
neighbors <- unlist(nearest$nn)
tabell <- data.frame(distance, neighbors)
pop_dal1 <- cbind(pop_dal, tabell)

# Multiply the distance by the population
pop_dal1$total <- pop_dal1$distance * pop_dal1$pop

# Performing a spatial join with municipality data
pop_dal1 <- st_join(pop_dal1, muni_dal, st_within)

# Group by municipality code and calculate sums
pop_dal2 <- aggregate(cbind(sum_pop = pop, sum_pop_dist = total) ~ KnKod, data = pop_dal1, FUN = sum)

# Converting to data frame for joining with municipality data
pop3 <- data.frame(pop_dal2)
map <- merge(muni_dal, pop3, by = "KnKod", all.x = TRUE)

# Calculating the mean distance
map$avg_dist <- map$sum_pop_dist / map$sum_pop

# plot  the maps

reds <- colorRampPalette(brewer.pal(9, "Reds"))
blues <- colorRampPalette(brewer.pal(9, "Blues"))
greens <- colorRampPalette(brewer.pal(9, "Greens"))

#A map showing the Population in Dalarna
plot1 <- ggplot()+
  geom_sf(data = map, aes(fill = sum_pop), show.legend = TRUE) +
  geom_sf_label(data = map, aes(label = KnNamn), cex = 2.5) +
  scale_fill_gradientn(name = "Population", colors = greens(7)) +
  xlab("") +
  ylab("") +
  ggtitle("Population in Dalarna") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

#A map showing the charging stations in Dalarna
plot2 <- ggplot()+
  geom_sf(data = Dal_reg[,3]) +
  geom_sf(data = muni_dal[,2],col="white") +
  geom_sf(data = char_dal[,14],cex=3,col="green",pch=17) +
  ggtitle("Location of the charging stations in Dalarna") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

#A map showing Charging Station's mean distance in Dalarna
plot3 <- ggplot() +
  geom_sf(data = map, aes(fill = avg_dist), show.legend = TRUE) +
  geom_sf_label(data = map, aes(label = KnNamn), cex = 2.5) +
  scale_fill_gradientn(name = "Distance", colors = blues(7)) +
  xlab("") +
  ylab("") +
  ggtitle("Charging Station's mean distance") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))


# Combining plots side by side
combined_plot <- plot1 + plot2 + plot3 + plot_layout(ncol = 3)

# Display combined plot
combined_plot


