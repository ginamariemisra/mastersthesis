rm(list = ls())
setwd("~/UARK/Listeria Salmonella Work/Chapter 4 - Growing Experiments/Experiment 1c")

library(ggplot2) 
library(scales) 
library(ggpubr)
library(dplyr)
library(forcats)
library(emmeans)
library(broom)

## Data Preparation

### Read in the raw data from both iterations of the experiment
exp1data <- read.csv(file = "sunflower2.csv")

### Convert to dataframe
exp1data <- data.frame(exp1data)

### Removes all the blanks and controls from the dataset
exp1data <- subset(exp1data, sample != "Blank")

### Set as numeric and do Log transformation
class(exp1data$CFU.g)
class(exp1data$log.CFU)
(exp1data$CFU.g <- as.numeric(as.character(exp1data$CFU.g)))
(exp1data$log.CFU <- log10(exp1data$CFU.g+1))

### Salmonella subset
exp1sa <- subset(exp1data, pathogen == "Salmonella")

### Listeria subset
exp1Li <- subset(exp1data, pathogen == "Listeria")


## Make Boxplots

### Set text, fonts, margins, sizes, etc.

plottheme <- theme(plot.title = element_text(face = "bold", size = (22)), 
                 legend.title = element_text(size = (14)), 
                 legend.text = element_text(size = (13)), 
                 axis.title = element_text(size = (18)),
                 axis.text = element_text(size = (18)),
                 axis.text.x = element_text(angle = 50, hjust = 1, size = 13),
                 axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
                 axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))




### --------------- Plot for Salmonella

x = paste(exp1sa$media,exp1sa$time,exp1sa$condition)

x <- ordered(x, levels = c("Biostrate Initial Unplanted", "Biostrate Initial Planted",
                           "Biostrate Harvest Unplanted", "Biostrate Harvest Planted", "Biostrate Harvest Microgreen",
                           "Peat Initial Unplanted", "Peat Initial Planted",
                           "Peat Harvest Unplanted", "Peat Harvest Planted", "Peat Harvest Microgreen"))

titlesal = expression(paste(italic("Salmonella enterica"), " transfer to Sunflower Microgreens"))

sunsal <- ggplot(exp1sa, aes(x = x, y = log.CFU)) + 
  geom_boxplot(aes(x = x, y = log.CFU, fill = x)) +
  scale_colour_manual(values = c("beige","tan","beige","tan","palegreen","beige","tan","beige","tan","palegreen"),
                      aesthetics = "fill") + labs(fill='Media, Time, and Condition') +
  plottheme + 
  ggtitle(titlesal) + 
  scale_y_continuous(breaks = c(1:9), limits = c(1, 9)) +
  xlab("Media, Time, and Condition") + ylab("log CFU/g") + 
  stat_summary(fun.y=mean, colour="black", geom="point", shape=18, size=3, show.legend = FALSE) +
  stat_summary(fun.y=mean, colour="black", geom="text", show.legend = FALSE, 
               vjust=-1, aes( label=round(..y.., digits=2))) + 
  stat_compare_means(label = "p.signif", method = "t.test", ref.group = ".all.", hide.ns = TRUE) + theme(legend.position="none")


### --------------- Plot for Listeria

x2 = paste(exp1Li$media,exp1Li$time,exp1Li$condition)

x2 <- ordered(x, levels = c("Biostrate Initial Unplanted", "Biostrate Initial Planted",
                           "Biostrate Harvest Unplanted", "Biostrate Harvest Planted", "Biostrate Harvest Microgreen",
                           "Peat Initial Unplanted", "Peat Initial Planted",
                           "Peat Harvest Unplanted", "Peat Harvest Planted", "Peat Harvest Microgreen"))

titlelis = expression(paste(italic("Listeria monocytogenes"), " transfer to Sunflower Microgreens"))

sunlis <- ggplot(exp1Li, aes(x = x2, y = log.CFU)) + 
  geom_boxplot(aes(x = x2, y = log.CFU, fill = x)) +
  scale_colour_manual(values = c("beige","tan","beige","tan","green","beige","tan","beige","tan","green"),
                      aesthetics = "fill") + labs(fill='Media, Time, and Condition') + 
  plottheme + 
  ggtitle(titlelis) + 
  scale_y_continuous(breaks = c(1:9), limits = c(1, 9)) +
  xlab("Media, Time, and Condition") + ylab("log CFU/g") + 
  stat_summary(fun.y=mean, colour="black", geom="point", shape=18, size=3) +
  stat_summary(fun.y=mean, colour="black", geom="text", 
               vjust=-1, aes( label=round(..y.., digits=2))) + 
  stat_compare_means(label = "p.signif", method = "t.test", ref.group = ".all.", hide.ns = TRUE) + theme(legend.position="none")


## Save the Plots

print(sunsal)
ggsave("rplots/sunflower-salmonella-noleg.png", width = 15, height = 9, dpi = 300)

print(sunlis)
ggsave("rplots/sunflower-listeria-noleg.png", width = 15, height = 9, dpi = 300)



# _________________________Statistics___________________________

## Summary Stats

## Salmonella (absolute)
meanssa <- aggregate(exp1sa$log.CFU ~ exp1sa$time + exp1sa$media + exp1sa$condition, FUN = mean)
colnames(meanssa) <- c("time", "media", "condition","mean log CFU")

stdevsa <- aggregate(exp1sa$log.CFU ~ exp1sa$time + exp1sa$media + exp1sa$condition, FUN = sd)
colnames(stdevsa) <- c("time", "media", "condition","sd log CFU")

tablesal <- data.frame(meanssa, stdevsa)
tablesal[,5:7] <- NULL



## Listeria
meansli <- aggregate(exp1Li$log.CFU ~ exp1Li$time + exp1Li$media + exp1Li$condition, FUN = mean)
colnames(meansli) <- c("time", "media", "condition","mean log CFU")

stdevli <- aggregate(exp1Li$log.CFU ~ exp1Li$time + exp1Li$media + exp1Li$condition, FUN = sd)
colnames(stdevli) <- c("time", "media", "condition","sd log CFU")

tablelis <- data.frame(meansli, stdevli)
tablelis[,5:7] <- NULL

write.csv(tablesal, file = "rtables/means-salmonella.csv")

write.csv(tablelis, file = "rtables/means-Listeria.csv")



## Differences for both pathogens
## Salmonella (differences)

mean.diff <- aggregate(harvestchange$diff ~ harvestchange$media + harvestchange$condition + harvestchange$pathogen, FUN = mean)
head(mean.diff)
colnames(mean.diff) <- c("media", "condition", "pathogen","mean")

std.diff <- aggregate(harvestchange$diff ~ harvestchange$media + harvestchange$condition + harvestchange$pathogen, FUN = sd)
head(std.diff)
colnames(std.diff) <- c("media", "condition", "pathogen","std")

mean.std.diffs <- data.frame(mean.diff, std.diff)
mean.std.diffs [,5:7] <- NULL

write.csv(mean.std.diffs, file = "rtables/mean-and-sd-for-all-differences.csv")


# ANOVAs

## Make the differences dataset

### Make the Initial dataset
initial <- subset(exp1data, time == "Initial")

#### Add the fake rows for subtracting zero from microgreen levels (Seeds were blank for pathogens)
initial[25:36,] <- NA

initial[25:36,3] <- "Microgreen"
initial[25:36,5] <- "Initial"
initial[25:30,2] <- "Peat"
initial[31:36,2] <- "Biostrate"
initial[25:27,6] <- "Listeria"
initial[28:30,6] <- "Salmonella"
initial[31:33,6] <- "Listeria"
initial[34:36,6] <- "Salmonella"
initial[25:36,4] <- "Contaminated"

initial[25:36,7:8] <- 0

### Make the Harvest dataset
harvest <- subset(exp1data, time == "Harvest")

(harvest <- harvest[order(harvest$pathogen),])
(harvest <- harvest[order(harvest$media),])
(harvest <- harvest[order(harvest$condition),])

(initial <- initial[order(initial$pathogen),])
(initial <- initial[order(initial$media),])
(initial <- initial[order(initial$condition),])

head(harvest)
head(initial)

### Assemble the differences data frame
harvestchange <- data.frame(harvest,initial$log.CFU)
harvestchange$time <- NULL #time will be reflected in the difference calculation
head(harvestchange)

colnames(harvestchange) <- c("id", "media","condition", "sample", "pathogen", "CFU.g", "harv.logCFU", "init.logCFU")

harvestchange$diff <- NA
harvestchange$diff <- harvestchange$harv.logCFU - harvestchange$init.logCFU
head(harvestchange)


## Subset the Data for each response y1 (salmonella change) and y2 (Listeria change)

### Salmonella subset w/o microgreen
diffsa <- subset(harvestchange, pathogen == "Salmonella" & condition != "Microgreen")

### Listeria subset w/o microgreen
diffLi <- subset(harvestchange, pathogen == "Listeria" & condition != "Microgreen")

### Full Dataset w/o microgreen
diffall <- subset(harvestchange, condition != "Microgreen")

## Run the actual ANOVA and generate CLD tables

### Salmonella

fitdiffsa <- aov(diff ~ condition * media, data=diffsa)
summary(fitdiffsa)

salpairs <- tidy(TukeyHSD(fitdiffsa))

emfitdiffsa <- emmeans(fitdiffsa, specs = c("condition", "media"))
cldfitdiffsa <- data.frame(multcomp::cld(emfitdiffsa, Letters = letters))

write.csv(cldfitdiffsa, file = "rtables/Salmonella-diff-CLD-nomicro.csv")
write.csv(salpairs, file = "rtables/Salmonella-diff-PAIRS-nomicro.csv")


### Listeria

fitdiffLi <- aov(diff ~ condition * media, data=diffLi)
summary(fitdiffLi)

lispairs <- tidy(TukeyHSD(fitdiffLi))

emfitdiffLi <- emmeans(fitdiffLi, specs = c("condition", "media"))
cldfitdiffLi <- data.frame(multcomp::cld(emfitdiffLi, Letters = letters))

write.csv(cldfitdiffLi, file = "rtables/Listeria-diff-CLD-nomicro.csv")
write.csv(lispairs, file = "rtables/Listeria-diff-PAIRS-nomicro.csv")


### Pathogen Differences

fitdiff <- aov(diff ~ pathogen * condition * media, data=diffall)
summary(fitdiff)
diffpairs <- tidy(TukeyHSD(fitdiff))

emfitdiff <- emmeans(fitdiff, specs = c("pathogen", "condition", "media"))
cldfitdiff <- data.frame(multcomp::cld(emfitdiff, Letters = letters))

write.csv(cldfitdiff, file = "rtables/anova-pathdiff-all-CLD-nomicro.csv")
write.csv(diffpairs, file = "rtables/anova-pathdiff-all-PAIRS-nomicro.csv")





# Microgreen Analyses, separate from others because different recovery method, not comparable to SFGM levels


## Subset the Data for each response y1 (salmonella change) and y2 (Listeria change)

### Salmonella subset microgreen only
diffsa.m <- subset(harvestchange, pathogen == "Salmonella" & condition == "Microgreen")

### Listeria subset microgreen only
diffLi.m <- subset(harvestchange, pathogen == "Listeria" & condition == "Microgreen")

### Full Dataset microgreen only
diffall.m <- subset(harvestchange, condition == "Microgreen")

## Run the actual ANOVA and generate CLD tables

### Salmonella

fitdiffsa.m <- aov(diff ~ media, data=diffsa.m)
summary(fitdiffsa.m)

salpairs.m <- tidy(TukeyHSD(fitdiffsa.m))

emfitdiffsa.m <- emmeans(fitdiffsa.m, specs = "media")
cldfitdiffsa.m <- data.frame(multcomp::cld(emfitdiffsa.m, Letters = letters))

write.csv(cldfitdiffsa.m, file = "rtables/Salmonella-diff-CLD-mgonly.csv")
write.csv(salpairs.m, file = "rtables/Salmonella-diff-PAIRS-mgonly.csv")


### Listeria

fitdiffLi.m <- aov(diff ~ media, data=diffLi.m)
summary(fitdiffLi.m)

lispairs.m <- tidy(TukeyHSD(fitdiffLi.m))

emfitdiffLi.m <- emmeans(fitdiffLi.m, specs = "media")
cldfitdiffLi.m <- data.frame(multcomp::cld(emfitdiffLi.m, Letters = letters))

write.csv(cldfitdiffLi.m, file = "rtables/Listeria-diff-CLD-mgonly.csv")
write.csv(lispairs.m, file = "rtables/Listeria-diff-PAIRS-mgonly.csv")


### Pathogen Differences

fitdiff.m <- aov(diff ~ pathogen * media, data=diffall.m)
summary(fitdiff.m)
diffpairs.m <- tidy(TukeyHSD(fitdiff.m))

emfitdiff.m <- emmeans(fitdiff.m, specs = c("pathogen", "media"))
cldfitdiff.m <- data.frame(multcomp::cld(emfitdiff.m, Letters = letters))

write.csv(cldfitdiff.m, file = "rtables/anova-pathdiff-all-CLD-mgonly.csv")
write.csv(diffpairs.m, file = "rtables/anova-pathdiff-all-PAIRS-mgonly.csv")
