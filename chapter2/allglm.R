

### Clear environment, set working directory, read in the data, and remove all responses less than 90% complete.

rm(list = ls())
setwd("~/UARK/Grower Survey/Analysis/glmnet")

raw.data = read.csv("~/UARK/Grower Survey/Analysis/microgreendata.csv", header = T, sep = ",", skip = 1, na.strings=c("", "NA"))
completed = subset(raw.data, subset=(raw.data$Progress >= 90))


### Load the R packages being used

library(glmnet)
library(stringr)
library(data.table)
library(dplyr)
library(tidyr)


# Response Variables (y)
#### 1. Documentation
#### 2. Water Testing
#### 3. Pre-germination Seed Disinfection
#### 4. Employee Hygiene
#### 5. Post-harvest Washing
#### 6. Growing Media Testing
#### 7. Sanitation of surfaces and equipment

## Pulling the response variables from the raw data and turning them into numerical columna
#### Poisson = non-negative counts, Binomial = 0 or 1

##### ______________________________________________________________________ 
## FS Practice 1: Documentation (response type: Poisson)
##### This code creates the documentation vector to be used to compare to all possible correlates listed above
##### Approach: Count the number of items documented. Assumption is that more items documented = more documentation.
##### Further, NA values were omitted and any "No routine documentation" was converted to "0"

y.original = completed$doc.all
n = length(y.original)
y.count <- rep(NA,n)

counter = 1

for (i in y.original[1:n]){
  dat <- i
  dat.split <- str_split(dat,",")
  dat.new <- paste(unlist(dat.split), collapse=',')
  number <- count.fields(textConnection(dat.new), sep = ",")
  y.count[counter] <- number
  counter <- counter + 1
}

##### Put back the NA values, Make "No routine documentation" 0. It was counted as "1" because it's a response.

y.bool <- is.na(y.original)
table <- data.frame(y.original, y.count, y.bool)
table$y.count[table$y.bool == TRUE] <- NA
table$y.count[table$y.original %like% "No routine documentation"] <- 0
y.new = table$y.count

##### Here I just added that new column back to the main dataset, then gave it a variable name for easy use later.

completed$doc.count <- y.new
documentation = completed$doc.count

table(documentation)


##### ______________________________________________________________________ 
## FS Practice 2: Water Testing (response type: Poisson)
#### Using Yes/No/I don't Know and frequency column to make a new frequency column with testing 0 times and >=1 time/year
#### Need to re-group things, first assign a value to each word response, and retain NA values

completed$watertestrank <- 0
completed$watertestrank[is.na(completed$water.test.bacteria.freq)] <- NA
completed$watertestrank[completed$water.test.bacteria.freq == "Once per year"] <- 1
completed$watertestrank[completed$water.test.bacteria.freq == "2 times per year"] <- 2
completed$watertestrank[completed$water.test.bacteria.freq == "4 times per year"] <- 4
completed$watertestrank[completed$water.test.bacteria.freq == "More than 4 times per year"] <- 5
completed$watertestrank[completed$water.test.bacteria.freq == "I do not know." & completed$water.test.bacteria.yn == "Yes"] <- 1

##### Finally setting the result to the right variable name for easy use

watertestfreq = completed$watertestrank
table(watertestfreq)



##### ______________________________________________________________________ 
## FS Practice 3: Pre-Germination Seed Disinfection (response type: Binomial)

seeds = completed$seed.disinfect.yn
table(seeds)

completed$seedsbin <- 0
completed$seedsbin[is.na(completed$seed.disinfect.yn)] <- NA
completed$seedsbin[completed$seed.disinfect.yn == "Yes"] <-1
seeds <- as.factor(completed$seedsbin)

table(seeds)

##### ______________________________________________________________________
## FS Practice 4: Handwashing (response type: Binomial)

handwashing = completed$hand.wash.yn
table(handwashing)

completed$handwashbin <- as.numeric(0)
completed$handwashbin[is.na(completed$hand.wash.yn)] <- NA
completed$handwashbin[completed$hand.wash.yn == "Yes"] <- as.numeric(1)

(handwashing <- as.numeric(completed$handwashbin))



##### ______________________________________________________________________
## FS Practice 5: Post-harvest washing (response type: Binomial)

postharvest <- completed$post.harvest.wash.yn
table(postharvest)

completed$phwbin <- 0
completed$phwbin[is.na(completed$post.harvest.wash.yn)] <- NA
completed$phwbin[completed$post.harvest.wash.yn == "Yes"] <-1

postharvest <- completed$phwbin
table(postharvest)


##### ______________________________________________________________________
## FS Practice 6: Grow Media Testing (response type: Poisson)

mediatestrank <- completed$media.test.rank
table(mediatestrank)



##### ______________________________________________________________________
## FS Practice 7: Sanitation (response type: Binomial)
#### Shows presence or absence of cleaning at least one surface daily or more.


cleaning <- data.frame(completed$clean.freq.floors, completed$clean.freq.preptables, completed$clean.freq.tools, completed$clean.freq.trays)

cleaning$total <- 0

colnames(cleaning) <-c("Floors", "Tables", "Tools", "Trays", "Total")

cleaning$Total[cleaning$Floors == "Daily or more" | cleaning$Tables == "Daily or more" | cleaning$Tools == "Daily or more" | cleaning$Trays == "Daily or more"] <- 1

dailysanbin <- cleaning$Total

table(dailysanbin)

clean.counts <- data.frame(table(cleaning$Floors),
                           table(cleaning$Tables),
                           table(cleaning$Tools),
                           table(cleaning$Trays))




## SETTING Y: Response variables to turn on and off depending on which comparisons being made.

# Uncomment the y you want to use

# y = documentation #poisson
y = watertestfreq #poisson
# y = seeds #binomial
# y = handwashing #binomial
# y = postharvest #binomial
# y = mediatestrank #poisson
# y = dailysanbin #binomial




##### ______________________________________________________________________
# The Predictor Matrices
##### What survey responses predict the above food safety practices?
##### This will include only the multiple response questions.


## These predictors include:
### 1. Certification Types
### 2. Type of Food Safety Trainings
### 3. Method of Learning to Grow Microgreens
### 4. Growing Media Types
### 5. Varieties grown
### 6. Other farm products
### 7. Water Source
### 8. Water treatment Type




## 1. Certifications (SIGNIFICANT)
### Question: "What certifications does your farm have? Check all that apply."


completed$gap <- 0
completed$gap[completed$certifications %like% "Good Agricultural"] <- 1
completed$sust<-0
completed$sust[completed$certifications %like% "sustainability"] <-1
completed$org<-0
completed$org[completed$certifications %like% "Organic"] <-1
completed$oth<-0
completed$oth[completed$certifications %like% "Other"] <-1
completed$none<-0
completed$none[completed$certifications %like% "None"] <-1
completed$cert.na<-0
completed$cert.na[is.na(completed$certifications)] <-1
#### Makes the X matrix
X1 = data.frame(completed$none, completed$oth, completed$org, completed$sust, completed$gap, completed$cert.na)



## 2. Type of Food Safety Trainings
### Question: "What previous food safety trainings have you had? Check all that apply."


completed$train.type.na <- 0
completed$train.type.na[is.na(completed$training.all)] <-1
completed$gapa <- 0
completed$gapa[completed$training.all %like% "GAP Audit"] <- 1
completed$serv <- 0
completed$serv[completed$training.all %like% "ServSafe"] <- 1
completed$city <- 0
completed$city[completed$training.all %like% "City Food Handler"] <- 1
completed$state <- 0
completed$state[completed$training.all %like% "State Food Handler"] <- 1
completed$lec <- 0
completed$lec[completed$training.all %like% "A lecture-based training"] <- 1
completed$own <- 0
completed$own[completed$training.all %like% "Learned on my own"] <- 1
completed$psa <- 0
completed$psa[completed$training.all %like% "Produce Safety Alliance"] <- 1
completed$ojt <- 0
completed$ojt[completed$training.all %like% "On-the-job training"] <- 1
completed$ext <- 0
completed$ext[completed$training.all %like% "University extension program"] <- 1
completed$con <- 0
completed$con[completed$training.all %like% "Conference"] <- 1
completed$other <- 0
completed$other[completed$training.all %like% "nother training course"] <- 1
completed$gfsi <- 0
completed$gfsi[completed$training.all %like% "GFSI"] <- 1
completed$haccp <- 0
completed$haccp[completed$training.all %like% "HACCP"] <- 1
completed$nofs <- 0
completed$nofs[completed$training.all %like% "No food safety training"] <- 1
completed$county <- 0
completed$county[completed$training.all %like% "County Health Card Training"] <- 1
completed$mic <- 0
completed$mic[completed$training.all %like% "A food safety training specifically for microgreens"] <- 1
completed$edbg <- 0
completed$edbg[completed$training.all %like% "From my educational background"] <- 1
X2 = data.frame(completed$train.type.na, completed$gapa, completed$serv, completed$city, completed$state,
                completed$lec, completed$own, completed$psa, completed$ojt, completed$ext, completed$con, completed$other,
                completed$gfsi, completed$haccp, completed$nofs, completed$county, completed$mic, completed$edbg)


## 3. Method of Learning to Grow Microgreens
### Question: "How did you learn to grow microgreens? Check all that apply.


#### -----------------------------------------------------
#### To see what all the categories were. 
#### Not needed for below code to work. Uncomment to run.
# column = completed$learn.to.grow.all
# dat = data.frame(column)
# dat$X = as.numeric(1:nrow(dat))
# names(dat) = c("choices","id")
# str(dat)
# numchoices = dat$choices %>% str_split("\\;|,") %>% unlist
# (num.tab = as.data.frame(table(numchoices)))
#### -----------------------------------------------------


completed$na.learn <- 0
completed$na.learn[is.na(completed$learn.to.grow.all)] <- 1
completed$kit<-0
completed$kit[completed$learn.to.grow.all %like% "A commercial kit"] <-1
completed$chef<-0
completed$chef[completed$learn.to.grow.all %like% "from a chef"] <-1
completed$bk<-0
completed$bk[completed$learn.to.grow.all %like% "Books or magazines"] <-1
completed$conf<-0
completed$conf[completed$learn.to.grow.all %like% "Conference"] <-1
completed$fexp<-0
completed$fexp[completed$learn.to.grow.all %like% "Farming experience"] <-1
completed$fair<-0
completed$fair[completed$learn.to.grow.all %like% "Health Fair"] <-1
completed$wom<-0
completed$wom[completed$learn.to.grow.all %like% "Informally from other growers"] <-1
completed$int<-0
completed$int[completed$learn.to.grow.all %like% "Internet"] <-1
completed$otr<-0
completed$otr[completed$learn.to.grow.all %like% "Other"] <-1
completed$pod<-0
completed$pod[completed$learn.to.grow.all %like% "Podcasts"] <-1
completed$smg<-0
completed$smg[completed$learn.to.grow.all %like% "Social media groups"] <-1
completed$tre<-0
completed$tre[completed$learn.to.grow.all %like% "Trial and error"] <-1
completed$uex<-0
completed$uex[completed$learn.to.grow.all %like% "University extension program"] <-1
completed$wksh<-0
completed$wksh[completed$learn.to.grow.all %like% "Workshop or class"] <-1
X3 = data.frame(completed$na.learn, completed$wksh,completed$uex,completed$tre,completed$smg,completed$pod,completed$otr,completed$int,completed$wom,completed$fair, completed$fexp, completed$conf, completed$bk, completed$kit, completed$chef)



## 4. Growing Media Type
### This column was assembled in Excel based on a variety of grow media questions.



#### ------------------------------------------------------
#### To see what all the categories were and make columns
column = completed$growth.media.all
dat = data.frame(column)
dat$X = as.numeric(1:nrow(dat))
names(dat) = c("choices","id")
str(dat)
numchoices = dat$choices %>% str_split("\\;|,") %>% unlist
(num.tab = as.data.frame(table(numchoices)))
#### ------------------------------------------------------

#### ------------------------------------------------------
#### create the variable names column
num.tab$abbrev <- 0
num.tab$abbrev <- c("cm", "cc", "csoil", "fc", "gc", "hemp", "jute", "jute", "man", "osoil", "osoil", "osoil", "omed", "peat", "perl", "pum", "rice", "speat", "verm", "wood", "worm")
#### Add a bunch of 0 columns with equal rows to y, with the abbreviations for each media type
(num.tab$numchoices)

newcols <- num.tab$abbrev
newtable <- data.frame(y)
newtable[,newcols] <- 0
head(newtable)

newtable <- newtable[,-1]
newtable$jute.1 <- NULL
newtable$osoil.1 <- NULL
newtable$osoil.2 <- NULL

newtable$na.med <- 0
newtable$na.med[is.na(completed$growth.media.all)] <- 1
newtable$cm[completed$growth.media.all %like% "compost"] <-1
newtable$cc[completed$growth.media.all %like% "Coco Coir"] <-1
newtable$csoil[completed$growth.media.all %like% "Conventional soil"] <-1
newtable$fc[completed$growth.media.all %like% "Food compost"] <-1
newtable$gc[completed$growth.media.all %like% "Green compost"] <-1
newtable$hemp[completed$growth.media.all %like% "Hemp"] <-1
newtable$jute[completed$growth.media.all %like% "Jute"] <-1
newtable$man[completed$growth.media.all %like% "Manure"] <-1
newtable$osoil[completed$growth.media.all %like% "Organic soil"] <-1
newtable$omed[completed$growth.media.all %like% "Other"] <-1
newtable$peat[completed$growth.media.all %like% "Peat"] <-1
newtable$perl[completed$growth.media.all %like% "Perlite"] <-1
newtable$pum[completed$growth.media.all %like% "Pumic"] <-1
newtable$rice[completed$growth.media.all %like% "Rice hulls"] <-1
newtable$speat[completed$growth.media.all %like% "Sphagnum"] <-1
newtable$verm[completed$growth.media.all %like% "Vermiculite"] <-1
newtable$wood[completed$growth.media.all %like% "Wood"] <-1
newtable$worm[completed$growth.media.all %like% "Worm"] <-1
X4 = newtable


head(X4)

## 5. Varieties Grown
### Question: "What microgreen varieties do you produce?"

#### ------------------------------------------------------
#### To see what all the categories were and make columns
column = completed$varieties
dat = data.frame(column)
dat$X = as.numeric(1:nrow(dat))
names(dat) = c("choices","id")
str(dat)
numchoices = dat$choices %>% str_split("\\;|,") %>% unlist
(num.tab = as.data.frame(table(numchoices)))
#### ------------------------------------------------------

#### ------------------------------------------------------
#### create the variable names column
num.tab$abbrev <- 0
num.tab$abbrev <- num.tab$numchoices
#### Add a bunch of 0 columns with equal rows to y, with the abbreviations for each type
(num.tab$numchoices)

newtable <- data.frame(y)
newcols <- as.character(num.tab$abbrev)
newtable[,newcols] <- 0
# Someday this could be a loop
newtable$na.variety <- 0
newtable$na.variety[is.na(completed$varieties)] <- 1
newtable$Amaranth[completed$varieties %like% "Amaranth"] <-1
newtable$Arugula[completed$varieties %like% "Arugula"] <-1
newtable$Basil[completed$varieties %like% "Basil"] <-1
newtable$Bean[completed$varieties %like% "Bean"] <-1
newtable$Beet[completed$varieties %like% "Beet"] <-1
newtable$`Bok Choy`[completed$varieties %like% "Bok Choy"] <-1
newtable$Broccoli[completed$varieties %like% "Broccoli"] <-1
newtable$Cabbage[completed$varieties %like% "Cabbage"] <-1
newtable$Celery[completed$varieties %like% "Celery"] <-1
newtable$Chard[completed$varieties %like% "Chard"] <-1
newtable$Chives[completed$varieties %like% "Chives"] <-1
newtable$Cilantro[completed$varieties %like% "Cilantro"] <-1
newtable$Cress[completed$varieties %like% "Cress"] <-1
newtable$Daikon[completed$varieties %like% "Daikon"] <-1
newtable$Kale[completed$varieties %like% "Kale"] <-1
newtable$Kohlrabi[completed$varieties %like% "Kohlrabi"] <-1
newtable$Lemongrass[completed$varieties %like% "Lemongrass"] <-1
newtable$Mizuna[completed$varieties %like% "Mizuna"] <-1
newtable$Mustard[completed$varieties %like% "Mustard"] <-1
newtable$Nasturtium[completed$varieties %like% "Nasturtium"] <-1
newtable$Other[completed$varieties %like% "Other"] <-1
newtable$`Pak Choy`[completed$varieties %like% "Pak Choy"] <-1
newtable$`Pea Shoots`[completed$varieties %like% "Pea Shoots"] <-1
newtable$`Pea Tendrils`[completed$varieties %like% "Pea Tendrils"] <-1
newtable$Popcorn[completed$varieties %like% "Popcorn"] <-1
newtable$Radish[completed$varieties %like% "Radish"] <-1
newtable$Rapini[completed$varieties %like% "Rapini"] <-1
newtable$Sunflower[completed$varieties %like% "Sunflower"] <-1
newtable$Tatsoi[completed$varieties %like% "Tatsoi"] <-1
newtable$Wasabi[completed$varieties %like% "Wasabi"] <-1
newtable <- newtable[,-1] #removes the y
X5 = newtable


## 6. Other Farm Products
### Question: "What other farm products do you produce?"


#### FOR PLANT PRODUCTS
#### ------------------------------------------------------
#### To see what all the categories were and make columns
column = completed$other.crops
dat = data.frame(column)
dat$X = as.numeric(1:nrow(dat))
names(dat) = c("choices","id")
str(dat)
numchoices = dat$choices %>% str_split("\\;|,") %>% unlist
(num.tab = as.data.frame(table(numchoices)))

#### ------------------------------------------------------
num.tab$abbrev2 <- 0
num.tab$abbrev2 <- num.tab$numchoices
#### Add a bunch of 0 columns with equal rows to y, with the abbreviations for each type
(num.tab$numchoices)

newtable2 <- data.frame(y)
newcols2 <- as.character(num.tab$abbrev2)
newtable2[,newcols2] <- 0
head(newtable2)


colnames(newtable2) <- c("y", "flowers", "herbs", "nonfood", "otherveg", "seedlings", "veg.raw", "veg.cooked")
newtable2 <- newtable2[,-1]
head(newtable2)



### Fill in values here
newtable2$flowers[completed$other.crops %like% "Edible flowers"] <-1
newtable2$herbs[completed$other.crops %like% "Fresh herbs"] <-1
newtable2$nonfood[completed$other.crops %like% "Non-food crops"] <-1
newtable2$otherveg[completed$other.crops %like% "Other"] <-1
newtable2$seedlings[completed$other.crops %like% "Seedlings"] <-1
newtable2$veg.raw[completed$other.crops %like% "Vegetables eaten raw"] <-1
newtable2$veg.cooked[completed$other.crops %like% "Vegetables rarely eaten raw"] <-1




#### FOR ANIMAL PRODUCTS
#### ------------------------------------------------------
#### To see what all the categories were and make columns
column = completed$animal.crops
dat = data.frame(column)
dat$X = as.numeric(1:nrow(dat))
names(dat) = c("choices","id")
str(dat)
numchoices = dat$choices %>% str_split("\\;|,") %>% unlist
(num.tab = as.data.frame(table(numchoices)))
#### ------------------------------------------------------

(num.tab$numchoices)

num.tab$abbrev3 <- 0
num.tab$abbrev3 <- num.tab$numchoices
#### Add a bunch of 0 columns with equal rows to y, with the abbreviations for each type
newtable3 <- data.frame(y)
newcols3 <- as.character(num.tab$abbrev3)
newtable3[,newcols3] <- 0
head(newtable3)

colnames(newtable3) <- c("y", "beef", "chicken", "dairy", "eggs", "fish", "other.animal", "pork")
newtable3 <- newtable3[,-1]
head(newtable3)

### Fill in values here
newtable3$beef[completed$animal.crops %like% "Beef"] <-1
newtable3$chicken[completed$animal.crops %like% "Chicken"] <-1
newtable3$dairy[completed$animal.crops %like% "Dairy"] <-1
newtable3$eggs[completed$animal.crops %like% "Eggs"] <-1
newtable3$fish[completed$animal.crops %like% "Fish"] <-1
newtable3$other.animal[completed$animal.crops %like% "Other"] <-1
newtable3$pork[completed$animal.crops %like% "Pork"] <-1
#### ALL PRODUCTS TOGETHER in MATRIX
#### ------------------------------------------------------
X6 <- data.frame(newtable2, newtable3)



## 7. Water Source
### Question: "What is the source of your farm's irrigation water?"


#### ------------------------------------------------------
#### To see what all the categories were and make columns
column = completed$water.source
dat = data.frame(column)
dat$X = as.numeric(1:nrow(dat))
names(dat) = c("choices","id")
str(dat)
numchoices = dat$choices %>% str_split("\\;|,") %>% unlist
(num.tab = as.data.frame(table(numchoices)))
#### ------------------------------------------------------

num.tab$abbrev4 <- 0
num.tab$abbrev4 <- num.tab$numchoices
#### Add a bunch of 0 columns with equal rows to y, with the abbreviations for each type
(num.tab$numchoices)

newtable4 <- data.frame(y)
newcols4 <- as.character(num.tab$abbrev4)
newtable4[,newcols4] <- 0
#Add the NA column
newtable4$nasource <- 0
newtable4$nasource[is.na(completed$water.source)] <-1
head(newtable4)

colnames(newtable4) <- c("y", "rain", "greywater", "municipal", "otherwater", "surface", "ground", "natreat")
newtable4 <- newtable4[,-1]
head(newtable4)


### Fill in values here
newtable4$rain[completed$water.source %like% "Collected rainwater"] <-1
newtable4$greywater[completed$water.source %like% "Greywater"] <-1
newtable4$municipal[completed$water.source %like% "Municipal water"] <-1
newtable4$otherwater[completed$water.source %like% "Other"] <-1
newtable4$surface[completed$water.source %like% "Surface water"] <-1
newtable4$ground[completed$water.source %like% "Well/Groundwater"] <-1
X7 = newtable4



## 8. Water Treatment Method
### Question: If yes to water treatment, "What types of water treatment do you use? Check all that apply"


#### ------------------------------------------------------
#### To see what all the categories were and make columns
column = completed$water.treat.method
dat = data.frame(column)
dat$X = as.numeric(1:nrow(dat))
names(dat) = c("choices","id")
str(dat)
numchoices = dat$choices %>% str_split("\\;|,") %>% unlist
(num.tab = as.data.frame(table(numchoices)))
#### ------------------------------------------------------


num.tab$abbrev5 <- 0
num.tab$abbrev5 <- num.tab$numchoices
#### Add a bunch of 0 columns with equal rows to y, with the abbreviations for each type
(num.tab$numchoices)

newtable5 <- data.frame(y)
newcols5 <- as.character(num.tab$abbrev5)
newtable5[,newcols5] <- 0
#Add the NA column
newtable5$natreat <- 0
newtable5$natreat[is.na(completed$water.treat.method)] <-1
head(newtable5)

colnames(newtable5) <- c("y", "charcoal", "chlorine", "iron", "lemonjuice", "othertreat", "ozone", "reverse", "sediment", "uvlight", "untreated", "watersoftener", "natreat")
newtable5 <- newtable5[,-1]
head(newtable5)


### Fill in values here
newtable5$charcoal[completed$water.treat.method %like% "Activated charcoal filter"] <-1
newtable5$chlorine[completed$water.treat.method %like% "Chlorine filter"] <-1
newtable5$iron[completed$water.treat.method %like% "Iron removal"] <-1
newtable5$lemonjuice[completed$water.treat.method %like% "Lemon juice"] <-1
newtable5$othertreat[completed$water.treat.method %like% "Other"] <-1
newtable5$ozone[completed$water.treat.method %like% "Ozone"] <-1
newtable5$reverse[completed$water.treat.method %like% "Reverse osmosis"] <-1
newtable5$sediment[completed$water.treat.method %like% "Sediment / solids filter"] <-1
newtable5$uvlight[completed$water.treat.method %like% "Ultraviolet light"] <-1
newtable5$untreated[completed$water.treat.method %like% "Untreated"] <-1
newtable5$watersoftener[completed$water.treat.method %like% "Water softener"] <-1
X8 = newtable5




#### ---------------------------------------------------------------------------

# Modeling

#### Run the below code over for each y value. The X matrix will be the same.
#### Remember to set y above first, and change the family depending on response type.

#### Assembles the predictor matrix
X.master = data.frame(X1, X2, X3, X4, X5, X6, X7, X8)
X.master = as.matrix(X.master)
head(X.master)

#### This removes NA Values based on how many NAs are in y
cleaning <- data.frame(y, X.master)
cleaning <- drop_na(cleaning, y)

p = ncol(cleaning)

y = cleaning$y # past this point y is changed so have to re-run y above if want to start over
y = as.numeric(as.character(y))
X.master = as.matrix(cleaning[,2:p])

head(X.master)
#### Now the actual model
set.seed(1) # to avoid different results with each run

fit = cv.glmnet(X.master, y, family = "poisson")

tmp_coeffs <- coef(fit, s = "lambda.min")
plot(fit)

rsq = 1 - fit$cvm/var(y)
plot(fit$lambda,rsq)

fit$lambda.min

rsqdata = data.frame(fit$lambda, rsq)

rsqdata$rsq[rsqdata$fit.lambda == fit$lambda.min]





allcoefficients <- data.frame(name = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], coefficient = tmp_coeffs@x)




