
rm(list = ls())
setwd("~/UARK/Grower Survey/Analysis")
micro.data = read.csv("microgreendata.csv", header = T, sep = ",", skip = 1, na.strings=c("", "NA"))

## Packages
library(stringr)
library(data.table)
library(ggplot2)
library(ggpubr)
library(mapproj)
library(ggmap)
library(maps)
library(tidyverse)
library(zipcode)

## Regional influences on Documentation (All the US Maps)

### DATA SET UP
#### Make a data table with the columns you want to compare

datatomap <- data.frame(micro.data$doc.count, micro.data$zipcode, micro.data$system.loc, 
                        micro.data$other.crops.yn, micro.data$system.type, micro.data$training.count, 
                        micro.data$Revenue, micro.data$education)

names(datatomap) <- c("doc", "zip", "sys.loc", "crops", "sys.type", "train", "rev", "edu")


datatomap <- na.omit(datatomap)

getOption("ggmap")

register_google(key = "AIzaSyAi0z5LO9WPn2o44mBUwvgmpERqZCg8pdY")

locations <- clean.zipcodes(datatomap$zip) %>%
  geocode()

datatomap$lon <- data.frame(locations$lon)
datatomap$lat <- data.frame(locations$lat)
head(datatomap)

names(datatomap[,9]) <- "lon"
names(datatomap[,10]) <- "lat"

datatomap = na.omit(datatomap)
datatomap2 <- datatomap[datatomap$zip!=96950,]
datatomap3 <- datatomap2[datatomap2$zip!=29753,]
datatomap = datatomap3
datatomap <- na.omit(datatomap)

revcats = c("Prefer Not to Answer", "Less than $5000", "$5000 - $9999", "$10000 - $24999", "$25000 - $49999", "Greater than $50000")
educats = c("Some high school", "High school or GED", "Some college", "Associates degree", 
            "Bachelors degree", "Masters degree", "Professional degree (Law, Medical, etc)", "Doctoral degree")

datatomap$rev <- factor(datatomap$rev, levels = revcats)
datatomap$edu <- factor(datatomap$edu, levels = educats)

### Spatial Plots
# REVENUE
world <- map_data("world", region = c("USA", "Canada", "Mexico"), xlim = c(-150,0), ylim = c(10, 70))

png(filename="maps/revenue.png", width=9, height=5, units="in", res=300)
revenue <- ggplot() +
  geom_polygon(data = world,  aes(long, lat, group = group), fill = "gray") +
  geom_point(data = datatomap, aes(lon, lat, colour = rev), size = 3) +
  ggtitle("Farm Size by Region") + scale_color_brewer(palette="Spectral")
print(revenue)
dev.off()

# EDUCATION
world <- map_data("world", region = c("USA", "Canada", "Mexico"), xlim = c(-150,0), ylim = c(10, 70))

png(filename="maps/education.png", width=9, height=5, units="in", res=300)
education <- ggplot() +
  geom_polygon(data = world,  aes(long, lat, group = group), fill = "gray") +
  geom_point(data = datatomap, aes(lon, lat, colour = edu), size = 3) +
  ggtitle("Grower Education Level by Region") + scale_color_brewer(palette="Spectral")
print(education)
dev.off()

# DOCUMENTATION
world <- map_data("world", region = c("USA", "Canada", "Mexico"), xlim = c(-150,0), ylim = c(10, 70))

png(filename="maps/documentation.png", width=9, height=5, units="in", res=300)
documentation <- ggplot() +
  geom_polygon(data = world,  aes(long, lat, group = group), fill = "gray") +
  geom_point(data = datatomap, aes(lon, lat, colour = doc), size = 3) +
  ggtitle("Number of Farm Procedures Documented, by Region")
print(documentation)
dev.off()

# FOOD SAFETY TRAINING
world <- map_data("world", region = c("USA", "Canada", "Mexico"), xlim = c(-150,0), ylim = c(10, 70))

png(filename="maps/fstraining.png", width=9, height=5, units="in", res=300)
fstraining <- ggplot() +
  geom_polygon(data = world,  aes(long, lat, group = group), fill = "gray") +
  geom_point(data = datatomap, aes(lon, lat, colour = train), size = 3) +
  ggtitle("Number of Food Safety Trainings Attended, by Region")
print(fstraining)
dev.off()

# System Location
world <- map_data("world", region = c("USA", "Canada", "Mexico"), xlim = c(-150,0), ylim = c(10, 70))

png(filename="maps/systemlocation.png", width=9, height=5, units="in", res=300)

systemlocation <- ggplot() +
  geom_polygon(data = world,  aes(long, lat, group = group), fill = "gray") +
  geom_point(data = datatomap, aes(lon, lat, colour = sys.loc), size = 3) +
  ggtitle("Production System Location, by Region")

print(systemlocation)
dev.off()

# System Type
world <- map_data("world", region = c("USA", "Canada", "Mexico"), xlim = c(-150,0), ylim = c(10, 70))

png(filename="maps/systemtype.png", width=9, height=5, units="in", res=300)

systemtype <- ggplot() +
  geom_polygon(data = world,  aes(long, lat, group = group), fill = "gray") +
  geom_point(data = datatomap, aes(lon, lat, colour = crops), size = 3) +
  ggtitle("Crop Diversity of Microgreen Growers, by Region")

print(systemtype)
dev.off()

# Crop Diversity
world <- map_data("world", region = c("USA", "Canada", "Mexico"), xlim = c(-150,0), ylim = c(10, 70))

png(filename="maps/crops.png", width=9, height=5, units="in", res=300)

crops <- ggplot() +
  geom_polygon(data = world,  aes(long, lat, group = group), fill = "gray") +
  geom_point(data = datatomap, aes(lon, lat, colour = sys.type), size = 3) +
  ggtitle("Production System Type, by Region")

print(crops)
dev.off()
