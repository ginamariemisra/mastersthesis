rm(list = ls())
setwd("~/UARK/Listeria Salmonella Work/Chapter 3 - Survival Curve/Analysis")

###Packages used in this script
##### Un-comment these if you need to reinstall

####install.packages("ggplot2") # the plotting package
####install.packages("scales") # to access break formatting functions
####install.packages("ggpubr")
####install.packages("gdata")
####install.packages("pgirmess")
####install.packages("FSA")
####install.packages("broom")
####install.packages("emmeans")


library(ggplot2)# the plotting package
library(scales) # to access break formatting functions
library(ggpubr) # GGplot publication ready
library(gdata) # forgot why I used this =(
library(emmeans) # makes better tables for pairwise

### ggline plotting help https://rpkgs.datanovia.com/ggpubr/reference/ggline.html
### kruskal wallis help https://rcompanion.org/rcompanion/d_06.html 

### Read in the raw data from both iterations of the experiment
survivaldata <- read.csv(file = "bothexperiments.csv")

### Convert to dataframe
survival <- data.frame(survivaldata)
### Simplify column names

### Removes all the blanks from the dataset because they are all zero
#### Non-Listeria, non-Salmonella background bacteria from blanks will be addressed separately
survival<-survival[!(survival$Material=="PBS Only"),]
survival<-survival[!(survival$Material=="Biostrate Blank"),]
survival<-survival[!(survival$Material=="Hemp Blank"),]
survival<-survival[!(survival$Material=="Soil Blank"),]
survival<-survival[!(survival$Material=="Coco Coir Blank"),]

### -------- Rename the Sampling Day Column to Match Pub Text ----------------------

survival$Day <- as.character(survival$Day)

survival$Day[survival$Day == "Initial"] <- "Day 0"
survival$Day[survival$Day == "One Day"] <- "Day 1"
survival$Day[survival$Day == "Three Days"] <- "Day 3"
survival$Day[survival$Day == "Six Days"] <- "Day 6"
survival$Day[survival$Day == "Ten Days"] <- "Day 10"

#### Change Soil to Peat
survival$Material <- as.character(survival$Material)

survival$Material[survival$Material == "Soil"] <- "Peat"

#--------------------Are the variances equal?-------------------
# The Normal Way

survival1 <- survival[(survival$Experiment=="Experiment 1"),]
survival2 <- survival[(survival$Experiment=="Experiment 2"),]

## Compare over all collection days

var.test(survival1$Listeria.LOG, survival2$Listeria.LOG)
var.test(survival1$Salmonella.LOG, survival2$Salmonella.LOG)

## Compare by each day
Day0.1 <- subset(survival1, survival1$Day == "Initial")
Day0.2 <- subset(survival2, survival2$Day == "Initial")

Day1.1 <- subset(survival1, survival1$Day == "One Day")
Day1.2 <- subset(survival2, survival2$Day == "One Day")

Day3.1 <- subset(survival1, survival1$Day == "Three Days")
Day3.2 <- subset(survival2, survival2$Day == "Three Days")

Day6.1 <- subset(survival1, survival1$Day == "Six Days")
Day6.2 <- subset(survival2, survival2$Day == "Six Days")

var.test(Day0.1$Listeria.LOG, Day0.2$Listeria.LOG)
var.test(Day0.1$Salmonella.LOG, Day0.2$Salmonella.LOG)

var.test(Day1.1$Listeria.LOG, Day1.2$Listeria.LOG)
var.test(Day1.1$Salmonella.LOG, Day1.2$Salmonella.LOG)

var.test(Day3.1$Listeria.LOG, Day3.2$Listeria.LOG)
var.test(Day3.1$Salmonella.LOG, Day3.2$Salmonella.LOG)

var.test(Day6.1$Listeria.LOG, Day6.2$Listeria.LOG)
var.test(Day6.1$Salmonella.LOG, Day6.2$Salmonella.LOG)

# The Means overall

t.test(survival1$Listeria.LOG, survival2$Listeria.LOG)
t.test(survival1$Salmonella.LOG, survival2$Salmonella.LOG)

# The means for each day

t.test(Day0.1$Listeria.LOG, Day0.2$Listeria.LOG)
t.test(Day0.1$Salmonella.LOG, Day0.2$Salmonella.LOG)

t.test(Day1.1$Listeria.LOG, Day1.2$Listeria.LOG)
t.test(Day1.1$Salmonella.LOG, Day1.2$Salmonella.LOG)

t.test(Day3.1$Listeria.LOG, Day3.2$Listeria.LOG)
t.test(Day3.1$Salmonella.LOG, Day3.2$Salmonella.LOG)

t.test(Day6.1$Listeria.LOG, Day6.2$Listeria.LOG)
t.test(Day6.1$Salmonella.LOG, Day6.2$Salmonella.LOG)

## -----------------Making the Plots-----------------------

### Set order of x axis labels
Row_Labels <- c("Day 0", "Day 1", "Day 3", "Day 6", "Day 10")

###To use italics in the title (Salmonella)
my_title <- expression(paste("10-Day Survival of ", italic("Salmonella enterica"), " in Soil-Free Growing Media"))

png(filename="Figures/salmonella_bw.png", width=8, height=5, units="in", res=300)

###The Plot for Salmonella
sal.plot <- ggline(survival, x = "Day", y = "Salmonella.LOG", add = c("mean", "point"), order = Row_Labels,
                   color = "Material", palette = c("#000000", "#696969", "#A9A9A9", "#C0C0C0", "#D3D3D3"), size = 0.5,
                   linetype = "Material",
                   title = my_title, ylim = c(0,8),
                   xlab = "Incubation Time", ylab = "Log CFU/mL") +
  theme_bw() +
  font("title", size = 12, color = "black", face = "bold")+
  font("xlab", size = 12, color = "black")+
  font("ylab", size = 12, color = "black")+
  font("xy.text", size = 12, color = "black") +
  theme(axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)), 
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)))

print(sal.plot)
dev.off()

###The Plot for Listeria

####To use italics in the title (Listeria)
my_title2 <- expression(paste("10-Day Survival of ", italic("Listeria monocytogenes"), " in Soil-Free Growing Media"))

png(filename="Figures/listeria_bw.png", width=8, height=5, units="in", res=300)

lis.plot <- ggline(survival, x = "Day", y = "Listeria.LOG", add = c("mean", "point"), order = Row_Labels,
                   color = "Material", palette = c("#000000", "#696969", "#A9A9A9", "#C0C0C0", "#D3D3D3"), size = 0.5,
                   linetype = "Material",
                   title = my_title2,  ylim = c(0,8),
                   xlab = "Incubation Time", ylab = "Log CFU/mL") +
  theme_bw() +
  font("title", size = 12, color = "black", face = "bold")+
  font("xlab", size = 12, color = "black")+
  font("ylab", size = 12, color = "black")+
  font("xy.text", size = 12, color = "black") +
  theme(axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)), 
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)))

print(lis.plot)
dev.off()



## ----------- Normal Statistical Analysis (ANOVA) -----------------

library(broom) ## This is for cleaning up the output later
library(lsmeans)
library(emmeans)
install.packages("multcompView")
library(multcompView)

### Initial: Pulls subset for initial
Day0 <- subset(survival, survival$Day == "Day 0")
print(Day0)
#### ANOVA for Listeria, Initial, by Material
fit0.L <- aov(Day0$Listeria.LOG ~ Day0$Material, data=Day0)
sum0L <- summary(fit0.L)
tukey0.L <- TukeyHSD(fit0.L)
tidy0.L <- tidy(tukey0.L)
#### ANOVA for Listeria, Initial, by Material
fit0.S <- aov(Day0$Salmonella.LOG ~ Day0$Material, data=Day0)
sum0S <- summary(fit0.S)
tukey0.S <- TukeyHSD(fit0.S)
tidy0.S <- tidy(tukey0.S)

lsmobject0S <- lsmeans(fit0.S, "Material")
cld0S <- data.frame(CLD(lsmobject0S, Letters = c("a", "b", "c", "d")))

lsmobject0L <- lsmeans(fit0.L, "Material")
cld0L <- data.frame(CLD(lsmobject0L, Letters = c("a", "b", "c", "d")))

### Day 1: Pulls subset for day 1
Day1 <- subset(survival, survival$Day == "Day 1")
print(Day1)
#### ANOVA for Listera, Day 1, by Material
fit1.L <- aov(Day1$Listeria.LOG ~ Day1$Material, data=Day1)
sum1L <- summary(fit1.L)
tukey1.L <- TukeyHSD(fit1.L)
tidy1.L <- tidy(tukey1.L)
#### ANOVA for Salmonella, Day 1, by Material
fit1.S <- aov(Day1$Salmonella.LOG ~ Day1$Material, data=Day1)
sum1S <- summary(fit1.S)
tukey1.S <- TukeyHSD(fit1.S)
tidy1.S <- tidy(tukey1.S)

lsmobject1S <- lsmeans(fit1.S, "Material")
cld1S <- data.frame(CLD(lsmobject1S, Letters = c("a", "b", "c", "d")))

lsmobject1L <- lsmeans(fit1.L, "Material")
cld1L <- data.frame(CLD(lsmobject1L, Letters = c("a", "b", "c", "d")))

### Day 3: Pulls subset for day 3
Day3 <- subset(survival, survival$Day == "Day 3")
print(Day3)
#### ANOVA for Listera, Day 3, by Material
fit3.L <- aov(Day3$Listeria.LOG ~ Day3$Material, data=Day3)
sum3L <- summary(fit3.L)
tukey3.L <- TukeyHSD(fit3.L)
tidy3.L <- tidy(tukey3.L)
#### ANOVA for Salmonella, Day 3, by Material
fit3.S <- aov(Day3$Salmonella.LOG ~ Day3$Material, data=Day3)
sum3S <- summary(fit3.S)
tukey3.S <- TukeyHSD(fit3.S)
tidy3.S <- tidy(tukey3.S)

lsmobject3S <- lsmeans(fit3.S, "Material")
cld3S <- data.frame(CLD(lsmobject3S, Letters = c("a", "b", "c", "d")))

lsmobject3L <- lsmeans(fit3.L, "Material")
cld3L <- data.frame(CLD(lsmobject3L, Letters = c("a", "b", "c", "d")))

### Day 6: Pulls subset for day 6
Day6 <- subset(survival, survival$Day == "Day 6")
print(Day6)
#### ANOVA for Listera, Day 6, by Material
fit6.L <- aov(Day6$Listeria.LOG ~ Day6$Material, data=Day6)
sum6L <- summary(fit6.L)
tukey6.L <- TukeyHSD(fit6.L)
tidy6.L <- tidy(tukey6.L)
#### ANOVA for Salmonella, Day 6, by Material
fit6.S <- aov(Day6$Salmonella.LOG ~ Day6$Material, data=Day6)
sum6S <- summary(fit6.S)
tukey6.S <- TukeyHSD(fit6.S)
tidy6.S <- tidy(tukey6.S)

lsmobject6S <- lsmeans(fit6.S, "Material")
cld6S <- data.frame(CLD(lsmobject6S, Letters = c("a", "b", "c", "d")))

lsmobject6L <- lsmeans(fit6.L, "Material")
cld6L <- data.frame(CLD(lsmobject6L, Letters = c("a", "b", "c", "d")))

### Day 10: Pulls subset for Day 10
Day10 <- subset(survival, survival$Day == "Day 10")
print(Day10)
#### ANOVA for Listera, Day 10, by Material
fit10.L <- aov(Day10$Listeria.LOG ~ Day10$Material, data=Day10)
sum10L <- summary(fit10.L)
tukey10.L <- TukeyHSD(fit10.L)
tidy10.L <- tidy(tukey10.L)
#### ANOVA for Salmonella, Day 10, by Material
fit10.S <- aov(Day10$Salmonella.LOG ~ Day10$Material, data=Day10)
sum10S <- summary(fit10.S)
tukey10.S <- TukeyHSD(fit10.S)
tidy10.S <- tidy(tukey10.S)

lsmobject10S <- lsmeans(fit10.S, "Material")
cld10S <- data.frame(CLD(lsmobject10S, Letters = c("a", "b", "c", "d")))

lsmobject10L <- lsmeans(fit10.L, "Material")
cld10L <- data.frame(CLD(lsmobject10L, Letters = c("a", "b", "c", "d")))

## --------------Saving as Table in CSV for LSM Means Package

cld.table.L <- data.frame(cld0L, cld1L, cld3L, cld6L, cld10L)
write.csv(cld.table.L, file = "cld.Listeria.csv")

cld.table.S <- data.frame(cld0S, cld1S, cld3S, cld6S, cld10S)
write.csv(cld.table.S, file = "cld.Salmonella.csv")

anovas.L <- c(sum0L, sum1L, sum3L, sum6L, sum10L)
anovas.S <- c(sum0S, sum1S, sum3S, sum6S, sum10S)





write.csv(anovas.L, file = "anovas.Listeria.csv")
write.csv(anovas.S, file = "anovas.Salmonella.csv")

## --------------Making the CI Table for the Normal ANOVA (The old way)

ci.table.L <- data.frame(tidy0.L$comparison, tidy0.L$adj.p.value,
                         tidy1.L$comparison, tidy1.L$adj.p.value, 
                         tidy3.L$comparison, tidy3.L$adj.p.value, 
                         tidy6.L$comparison, tidy6.L$adj.p.value, 
                         tidy10.L$comparison, tidy10.L$adj.p.value)

write.csv(ci.table.L, file = "ci.table.normal.Listeria.csv")

ci.table.S <- data.frame(tidy0.S$comparison, tidy0.S$adj.p.value,
                         tidy1.S$comparison, tidy1.S$adj.p.value, 
                         tidy3.S$comparison, tidy3.S$adj.p.value, 
                         tidy6.S$comparison, tidy6.S$adj.p.value, 
                         tidy10.S$comparison, tidy10.S$adj.p.value)

write.csv(ci.table.S, file = "ci.table.normal.Salmonella.csv")

# ----------------- Kruskal-Wallis Pairwise Comparisons ------------- 

library(pgirmess)
library(FSA)

?kruskalmc
?dunnTest


### -----------Day 0------------------------------

### Initial: Pulls subset for initial
Day0 <- subset(survival, survival$Day == "Initial")

#### ANOVA for Listeria, Initial, by Material

##### Method 1: Dunn Test (shows p-values)
dunn0.L <- dunnTest(Day0$Listeria.LOG ~ Day0$Material, data=Day0, method = "bh")
##### Method 2: kruskalms (shows critical cutoffs)
fit0.kw.L <- kruskal.test(Day0$Listeria.LOG ~ Day0$Material)
pairs0.L <- kruskalmc(Day0$Listeria.LOG ~ Day0$Material, signif.level=0.05)
pairs0.L.tab <- data.frame(pairs0.L)

#### ANOVA for Salmonella, Initial, by Material
##### Method 1: Dunn Test (shows p-values)
dunn0.S <- dunnTest(Day0$Salmonella.LOG ~ Day0$Material, data=Day0, method = "bh")
##### Method 2: kruskalms (shows critical cutoffs)
fit0.kw.S<- kruskal.test(Day0$Salmonella.LOG ~ Day0$Material)
pairs0.S <- kruskalmc(Day0$Salmonella.LOG ~ Day0$Material)
pairs0.S.tab <- data.frame(pairs0.S)


### -----------Day 1------------------------------

### Day 1: Pulls subset for initial
Day1 <- subset(survival, survival$Day == "One Day")

#### ANOVA for Listeria, Initial, by Material
##### Method 1: Dunn Test (shows p-values)
dunn1.L <- dunnTest(Day1$Listeria.LOG ~ Day1$Material, data=Day1, method = "bh")
##### Method 2: kruskalms (shows critical cutoffs)
fit1.kw.L<- kruskal.test(Day1$Listeria.LOG ~ Day1$Material)
pairs1.L <- kruskalmc(Day1$Listeria.LOG ~ Day1$Material)
pairs1.L.tab <- data.frame(pairs1.L)

#### ANOVA for Salmonella, Initial, by Material
##### Method 1: Dunn Test (shows p-values)
dunn1.S <- dunnTest(Day1$Salmonella.LOG ~ Day1$Material, data=Day1, method = "bh")
##### Method 2: kruskalms (shows critical cutoffs)
fit1.kw.S<- kruskal.test(Day1$Salmonella.LOG ~ Day1$Material)
pairs1.S <- kruskalmc(Day1$Salmonella.LOG ~ Day1$Material)
pairs1.S.tab <- data.frame(pairs1.S)

### -----------Day 3------------------------------

### Day 3: Pulls subset for initial
Day3 <- subset(survival, survival$Day == "Three Days")

#### ANOVA for Listeria, Initial, by Material
##### Method 1: Dunn Test (shows p-values)
dunn3.L <- dunnTest(Day3$Listeria.LOG ~ Day3$Material, data=Day3, method = "bh")
##### Method 2: kruskalms (shows critical cutoffs)
fit3.kw.L<- kruskal.test(Day3$Listeria.LOG ~ Day3$Material)
pairs3.L <- kruskalmc(Day3$Listeria.LOG ~ Day3$Material)
pairs3.L.tab <- data.frame(pairs3.L)

#### ANOVA for Salmonella, Initial, by Material
##### Method 1: Dunn Test (shows p-values)
dunn3.S <- dunnTest(Day3$Salmonella.LOG ~ Day3$Material, data=Day3, method = "bh")
##### Method 2: kruskalms (shows critical cutoffs)
fit3.kw.S<- kruskal.test(Day3$Salmonella.LOG ~ Day3$Material)
pairs3.S <- kruskalmc(Day3$Salmonella.LOG ~ Day3$Material)
pairs3.S.tab <- data.frame(pairs3.S)

### -----------Day 6------------------------------

### Day 6: Pulls subset for initial
Day6 <- subset(survival, survival$Day == "Six Days")

#### ANOVA for Listeria, Initial, by Material
##### Method 1: Dunn Test (shows p-values)
dunn6.L <- dunnTest(Day6$Listeria.LOG ~ Day6$Material, data=Day6, method = "bh")
##### Method 2: kruskalms (shows critical cutoffs)
fit6.kw.L<- kruskal.test(Day6$Listeria.LOG ~ Day6$Material)
pairs6.L <- kruskalmc(Day6$Listeria.LOG ~ Day6$Material)
pairs6.L.tab <- data.frame(pairs6.L)

#### ANOVA for Salmonella, Initial, by Material
##### Method 1: Dunn Test (shows p-values)
dunn6.S <- dunnTest(Day6$Salmonella.LOG ~ Day6$Material, data=Day6, method = "bh")
##### Method 2: kruskalms (shows critical cutoffs)
fit6.kw.S<- kruskal.test(Day6$Salmonella.LOG ~ Day6$Material)
pairs6.S <- kruskalmc(Day6$Salmonella.LOG ~ Day6$Material)
pairs6.S.tab <- data.frame(pairs6.S)

### -----------Day 10------------------------------

### Day 10: Pulls subset for initial
Day10 <- subset(survival, survival$Day == "Ten Days")

#### ANOVA for Listeria, Initial, by Material
##### Method 1: Dunn Test (shows p-values)
dunn10.L <- dunnTest(Day10$Listeria.LOG ~ Day10$Material, data=Day10, method = "bh")
##### Method 2: kruskalms (shows critical cutoffs)
fit10.kw.L<- kruskal.test(Day10$Listeria.LOG ~ Day10$Material)
pairs10.L <- kruskalmc(Day10$Listeria.LOG ~ Day10$Material)
pairs10.L.tab <- data.frame(pairs10.L)

#### ANOVA for Salmonella, Initial, by Material
##### Method 1: Dunn Test (shows p-values)
dunn10.S <- dunnTest(Day10$Salmonella.LOG ~ Day10$Material, data=Day10, method = "bh")
##### Method 2: kruskalms (shows critical cutoffs)
fit10.kw.S<- kruskal.test(Day10$Salmonella.LOG ~ Day10$Material)
pairs10.S <- kruskalmc(Day10$Salmonella.LOG ~ Day10$Material)
pairs10.S.tab <- data.frame(pairs10.S)



### -----------Create and Save Data Tables as CSV------------------------------

#### Method 1:
ci.table.nonp.L.dunn <- data.frame(dunn0.L$res, dunn1.L$res, dunn3.L$res, dunn6.L$res, dunn10.L$res)
write.csv(ci.table.nonp.L.dunn, file = "ci.table.nonp.Listeria.dunn.csv")

ci.table.nonp.S.dunn <- data.frame(dunn0.S$res, dunn1.S$res, dunn3.S$res, dunn6.S$res, dunn10.S$res)
write.csv(ci.table.nonp.S.dunn, file = "ci.table.nonp.Salmonella.dunn.csv")

#### Method 2:
ci.table.nonp.L <- data.frame(pairs0.L.tab, pairs1.L.tab, pairs3.L.tab, pairs6.L.tab, pairs10.L.tab)
write.csv(ci.table.nonp.L, file = "ci.table.nonp.Listeria.csv")

ci.table.nonp.S <- data.frame(pairs0.S.tab, pairs1.S.tab, pairs3.S.tab, pairs6.S.tab, pairs10.S.tab)
write.csv(ci.table.nonp.S, file = "ci.table.nonp.Salmonella.csv")

### -----------Analysis by Pathogens------------------------------


survival2 <- data.frame(survivaldata$Day, survivaldata$Material, survivaldata$Salmonella.LOG, survivaldata$Listeria.LOG)
names(survival2) <- c("Day", "SFGM", "Salmonella", "Listeria")

factor.sal <- sprintf("Salmonella", 1:length(survival2$Salmonella))
factor.lis <- sprintf("Listeria", 1:length(survival2$Listeria))

survival.lis <- data.frame(survival2$Day, survival2$SFGM, survival2$Listeria)
names(survival.lis) <- c("Day", "SFGM", "Bacteria LOG")

survival.sal <- data.frame(survival2$Day, survival2$SFGM, survival2$Salmonella)
names(survival.sal) <- c("Day", "SFGM", "Bacteria LOG")

survival3 <- rbind(survival.lis,survival.sal)

pathogen.name <- c(factor.lis, factor.sal)

survival3$pathogen.name <- pathogen.name

names(survival3) <- c("Day", "SFGM", "log.cfu", "pathogen")

survival3<-survival3[!(survival3$SFGM=="PBS Only"),]
survival3<-survival3[!(survival3$SFGM=="Biostrate Blank"),]
survival3<-survival3[!(survival3$SFGM=="Hemp Blank"),]
survival3<-survival3[!(survival3$SFGM=="Soil Blank"),]
survival3<-survival3[!(survival3$SFGM=="Coco Coir Blank"),]

###Wrong, saved for notes
###pathogen.anova <- aov(log.cfu ~ pathogen + Day + SFGM, data = survival3)
###summary(pathogen.anova)


path.Day10 <- subset(survival3, survival3$Day == "Ten Days")

sal.num <- subset(survival3, survival3$pathogen == "Salmonella")
sal.num <- subset(sal.num, sal.num$log.cfu != "NA")
lis.num <- subset(survival3, survival3$pathogen == "Listeria")

mean(lis.num$log.cfu)
mean(sal.num$log.cfu)
length(sal.num$log.cfu)
length(lis.num$log.cfu)

sd(lis.num$log.cfu)
sd(sal.num$log.cfu)


t.test(sal.num$log.cfu, lis.num$log.cfu)
