rm(list = ls())
setwd("~/UARK/Grower Survey/Analysis/")
micro.data = read.csv("microgreendata.csv", header = T, sep = ",", skip = 1, na.strings=c("", "NA"))

completed = subset(micro.data, subset=(micro.data$Progress >= 90))

library(descr)


# Need to remember to change all the micro.data to completed if KG tells you to re-run these!!!!!!!!!

## ------For Multiple Choice Questions vs Categorical Variables

x = micro.data$gloves.used.yn

y = micro.data$hand.wash.yn
  
ct <- crosstab(x, y, missing.include = TRUE, percent = TRUE, prop.c = TRUE, chisq = TRUE)

chisq.test(x,y, simulate.p.value = TRUE)

# Convert 'ct' into a Matrix type
ct_mtrx <- descr:::CreateNewTab(ct)

# Write the file into a csv file
write.csv(ct_mtrx,"crosstabs/handwashvsgloves.csv")




## ------For Multiple Choice Questions vs. Numerical Variables (uses aggregate function)

### For environmental conditions

water = micro.data$water.temp
air = micro.data$air.temp
rh = micro.data$humidity

y1 = micro.data$system.loc

d <- data.frame(y1,water,air,rh) #make dataframe out of x and y
d[d==0] <- NA # remove all the zeros and NAs by making zero into NA
d <- na.omit(d) # Remove all the NAs

meantable.s <- aggregate(d[, 2:4], list(d$y1), FUN = sd)
meantable.m<- aggregate(d[, 2:4], list(d$y1), FUN = mean)

colnames(meantable.m) <- c("Location", "Water", "Air", "RH")
colnames(meantable.s) <- c("Location", "Water", "Air", "RH")

ct.for.n = crosstab(d$y1, d$air)
ct.for.n.mtrx <- descr:::CreateNewTab(ct.for.n)
ct.for.n.df <- data.frame(ct.for.n.mtrx)
n <- data.frame(ct.for.n.df)

meantable <- data.frame(meantable.m, meantable.s[,2:4], n)
colnames(meantable) = c("Location", "Water mean", "Air mean", "RH mean", "Water sd", "Air sd", "RH sd", "n")
write.csv(meantable,"aggmeans/envconditions.csv")

### For number of employees

x = micro.data$Revenue
y = micro.data$total.employees

d <- data.frame(x,y) #make dataframe out of x and y
d[d==0] <- NA # remove all the zeros and NAs by making zero into NA
d <- na.omit(d) # Remove all the NAs

meantable.s <- aggregate(d[, 2], list(d$x), FUN = sd)
meantable.m<- aggregate(d[, 2], list(d$x), FUN = mean)

ct.for.n = crosstab(d$x, d$x, total.c = FALSE, total.r = TRUE)
ct.for.n.mtrx <- descr:::CreateNewTab(ct.for.n)
ct.for.n.df <- data.frame(ct.for.n.mtrx)
n <- data.frame(ct.for.n.df)

meantable <- data.frame(meantable.m, meantable.s$x, n$Total)
colnames(meantable) <- c("Revenue", "Mean # Employees", "SD", "n")
write.csv(meantable,"aggmeans/employees.csv")


### For averate harvest-to-sale storage time

x = micro.data$living.tray.storage
y = micro.data$presale.storage.hours

d <- data.frame(x,y) #make dataframe out of x and y
d[d==0] <- NA # remove all the zeros and NAs by making zero into NA
d <- na.omit(d) # Remove all the NAs

meantable.s <- aggregate(d[, 2], list(d$x), FUN = sd)
meantable.m<- aggregate(d[, 2], list(d$x), FUN = mean)

ct.for.n = crosstab(d$x, d$x, total.c = FALSE, total.r = TRUE)
ct.for.n.mtrx <- descr:::CreateNewTab(ct.for.n)
ct.for.n.df <- data.frame(ct.for.n.mtrx)
n <- data.frame(ct.for.n.df)

meantable <- data.frame(meantable.m, meantable.s$x, n$Total)
colnames(meantable) <- c("Revenue", "Mean # Employees", "SD", "n")
write.csv(meantable,"aggmeans/storage-trays.csv")


## ------For Multiple Response Questions vs Categorical Variables (Water Testing by Water Source)

library(descr)
library(DescTools)
library(stringr)

x = micro.data$water.treat.yn
y = micro.data$water.source

as.data.frame(table(x))

df <- data.frame(x,y)

options = df$y %>% str_split("\\:|,") %>% unlist
(options.table = as.data.frame(table(options)))

### Cycle through each category one by one by changing the number here
index = 6
searchterm = options.table$options[index]
  
set <- subset(df, df$y %like% searchterm)
  
numchoices = set$x %>% str_split("\\;|,") %>% unlist
(num.tab = as.data.frame(table(numchoices)))
  
num.tab$Category <- NA
num.tab$Category <- searchterm
(results <- num.tab)
data.frame(results)

### Copy and paste into Excel and create the dataset - there is probably a better way to do this but I don't have time.

watersourcebytesting <- read.csv(file = "crosstabs/watersourcebytesting.csv", header = TRUE)

x = watersourcebytesting$Response
y = watersourcebytesting$Source

chisq.test(x,y, simulate.p.value = TRUE)

#### ------ for checking how many for that category -----
numchoices = set$y %>% str_split("\\;|,") %>% unlist
(num.tab = as.data.frame(table(numchoices)))






## ------For Multiple Response Questions vs. Numerical Variables



## ------Specific CrossTabs

### Year Opening
year <- as.factor(as.character(micro.data$farm.open.year))

ct <- crosstab(year ,micro.data$Revenue, missing.include = TRUE, 
               cell.layout = TRUE, percent = FALSE, prop.c = FALSE, format = "SPSS")


# Convert 'ct' into a Matrix type
ct_mtrx <- descr:::CreateNewTab(ct)

# Write the file into a csv file
write.csv(ct_mtrx,"crosstabs/openyear.csv")


### Year Groups

x = micro.data$animal.crops

micro.data$year.group <- "No Response"
micro.data$year.group[micro.data$farm.open.year >= 2010] <- "After 2010"
micro.data$year.group[micro.data$farm.open.year < 2010] <- "Before 2010"

y = micro.data$year.group

ct <- crosstab(x, y, missing.include = TRUE, percent = TRUE, prop.c = TRUE, chisq = TRUE)


# Convert 'ct' into a Matrix type
ct_mtrx <- descr:::CreateNewTab(ct)

# Write the file into a csv file
write.csv(ct_mtrx,"crosstabs/specificanimals.csv")