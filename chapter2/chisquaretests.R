### Clear environment, set working directory, read in the data, and remove all responses less than 90% complete.

rm(list = ls())
setwd("~/UARK/Grower Survey/Analysis/basic stats")

raw.data = read.csv("~/UARK/Grower Survey/Analysis/microgreendata.csv", header = T, sep = ",", skip = 1, na.strings=c("", "NA"))
completed = subset(raw.data, subset=(raw.data$Progress >= 90))

### Load the R packages being used

library(stringr)
library(data.table)

# Key FS Practices To Explore
### 1. Documentation
### 2. Water Testing
### 3. Sanitation
### 4. Employee Hygiene


# Possible Correlates with each FS Practice to Test
### 1. Revenue
### 2. Number of Employees
### 3. Number of Handlers
### 4. Certifications
### 5. Number of Food Safety Trainings
### 6. Last Completed Education

# FS Practice 1: Documentation
#### This code creates the documentation vector to be used to compare to all possible correlates listed above
#### Approach: Count the number of items documented. Assumption is that more items documented = more documentation.
#### Further, NA values were omitted and any "No routine documentation" was converted to "0"

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

### NA and No Routine Documentation are both 0
y.bool <- is.na(y.original)
table <- data.frame(y.original, y.count, y.bool)
table$y.count[table$y.bool == TRUE] <- NA
table$y.count[table$y.original %like% "No routine documentation"] <- 0
y.new = table$y.count

#### Here I just added that new column back to the main dataset, then gave it a variable name for easy use later.
completed$doc.count <- y.new
documentation = completed$doc.count

## Making the Correlate vectors to do comparisons

### Revenue Category
revenue = completed$Revenue

### Number of Employees
employees = completed$total.employees

### Number of Employees Handling Microgreens
handlers = completed$total.handlers

### Certifications
certifications = as.factor(completed$certifications)

### Number of Food Safety Trainings
#### Approach is similar to counting documentation items
x.original = completed$training.all
n = length(x.original)
x.count <- rep(NA,n)

counter = 1

for (i in x.original[1:n]){
  dat <- i
  dat.split <- str_split(dat,",")
  dat.new <- paste(unlist(dat.split), collapse=',')
  number <- count.fields(textConnection(dat.new), sep = ",")
  x.count[counter] <- number
  counter <- counter + 1
}

#### Omit NA, Set "No food safety training" to zero
x.bool <- is.na(x.original)
table <- data.frame(x.original, x.count, x.bool)
table$x.count[table$x.bool == TRUE] <- NA
table$x.count[table$x.original %like% "No food safety training"] <- 0
x.new = table$x.count

training.count <- x.new

### Last Completed Education Level
education = completed$education

## STATISTICAL TESTS
#### Variables: revenue, employees, handlers, certifications, training.count, education

### Revenue
dat = data.frame(revenue,documentation)
dat = na.omit(dat)

doc.rev = table(dat$revenue, dat$documentation)
print(chisq.test(table(dat$revenue, dat$documentation), simulate.p.value = T))

### Employees
kruskal.test(x = employees, g = as.factor(documentation))
aggregate(employees ~ documentation, data = data.frame(employees, documentation), FUN=mean, na.rm=T)

### Employees who Handle Microgreens
kruskal.test(x = handlers, g = as.factor(documentation))
aggregate(handlers ~ documentation, data = data.frame(handlers, documentation), FUN=mean, na.rm=T)


### Certifications
#### For some reason, this would give an error if I didn't turn it into a matrix first. Probably need a better way to do this
dat = data.frame(certifications, documentation)
dat = na.omit(dat)
dat = as.matrix(dat)

doc.certs = table(dat[,1], dat[,2])
print(chisq.test(table(dat[,1], dat[,2]), simulate.p.value = T))

### Number of Food Safety Trainings
dat = data.frame(training.count, documentation)
dat = na.omit(dat)

doc.train = table(dat$training.count, dat$documentation)
print(chisq.test(table(dat$training.count, dat$documentation), simulate.p.value = T))

### Last Completed Education Level
dat = data.frame(education, documentation)
dat = na.omit(dat)

doc.ed = table(dat$education, dat$documentation)
print(chisq.test(table(dat$education, dat$documentation), simulate.p.value = T))


# FS Practice 2: Water Testing
### Using Yes/No/I don't Know
### Also converting the frequency column into 0 times and >=1

#### This one is fine as is.
watertestyn = completed$water.test.bacteria.yn

#### Need to re-group things, first assign a value to each word response, and retain NA values
completed$watertestrank <- 0
completed$watertestrank[is.na(completed$water.test.bacteria.freq)] <- NA
completed$watertestrank[completed$water.test.bacteria.freq == "Once per year"] <- 1
completed$watertestrank[completed$water.test.bacteria.freq == "2 times per year"] <- 2
completed$watertestrank[completed$water.test.bacteria.freq == "4 times per year"] <- 4
completed$watertestrank[completed$water.test.bacteria.freq == "More than 4 times per year"] <- 5
completed$watertestrank[completed$water.test.bacteria.freq == "I do not know." & completed$water.test.bacteria.yn == "Yes"] <- 1
##### Next, create a new column with the new categories to use in chi-square, retaining NA values
completed$watertestbin <- "No testing"
completed$watertestbin[is.na(completed$watertestrank)] <- NA
completed$watertestbin[completed$watertestrank >= 1] <- "At least once a year"

watertestfreq = completed$watertestbin

