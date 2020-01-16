

rm(list = ls())
setwd("~/UARK/Grower Survey/Analysis")
micro.data = read.csv("microgreendata.csv", header = T, sep = ",", skip = 1, na.strings=c("", "NA"))

completed <- subset(micro.data, Progress >= 90)

library(ggplot2)
library(dplyr)
library(stringr)
library(scales)


mytheme <- theme(plot.title = element_text(face = "bold", size = (24)), 
                 legend.title = element_text(size = (18)), 
                 legend.text = element_text(size = (18)), 
                 axis.title = element_text(size = (18)),
                 axis.text = element_text(size = (18)),
                 axis.text.x = element_text(angle = 0, hjust = 1, size = 18),
                 axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
                 axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))

# Summary Bar Plots

### Change the column here and the title below and a summary bar plot will be generated
column = micro.data$glove.use.steps
title = "Glove Use Steps"
  
#### Start of plot code
dat = data.frame(column)
dat$X = as.numeric(1:nrow(dat))
names(dat) = c("choices","id")
n = length(column)

str(dat)

numchoices = dat$choices %>% str_split("\\;|,") %>% unlist

(num.tab = as.data.frame(table(numchoices)))

(num.tab <- transform(num.tab, numchoices = reorder(numchoices, Freq, decreasing = T)))

bars <- ggplot(num.tab, aes(numchoices, Freq, fill=Freq)) +
  geom_bar(stat="identity") + coord_flip() + 
  geom_text(aes(label=paste0(round((Freq/n)*100,1),"%"," (", Freq,") ")), position=position_dodge(width=0.9), 
            hjust=-0.2, size = 6) + 
  ggtitle(title) + 
  scale_y_continuous(limits = c(0, 176)) +
  xlab("Responses") + ylab("Frequency") + theme(text = element_text(size=18)) + 
  mytheme

print(bars)
ggsave("GG Plots/glovesteps.png", width = 10, height = 14, dpi = 300)

#### End of plot code



#### n values for each question category excluding NAs (how many people actually answered the question)

n.rev <- length(na.omit(completed$Revenue))
n.year <- length(na.omit(completed$farm.open.year))
n.cert <- length(na.omit(completed$certifications))
n.ed <- length(na.omit(completed$education))
n.train <- length(na.omit(completed$training.yn))
n.irr <- length(na.omit(completed$water.method))
n.waters <- length(na.omit(completed$water.source))
n.harv <- length(na.omit(completed$harvest.method))
n.sys.t <- length(na.omit(completed$system.type))
n.sys.l <- length(na.omit(completed$system.loc))
n.var <- length(na.omit(completed$varieties))
n.wash <- length(na.omit(completed$post.harvest.wash.yn))
n.doc <- length(na.omit(completed$Documentation))
n.hydro <- length(na.omit(completed$h.growth.media.all))
n.cont <- length(na.omit(completed$growth.media.all))

##### Generated table of above for excel

nvalues <- data.frame(n.rev, n.year, n.cert, n.ed, n.train, n.irr, n.waters, n.harv, n.sys.t, n.sys.l, n.var, n.wash, n.doc, n.hydro, n.cont)
colnames(nvalues) <- c("revenue", "year", "certifications", "education", "fs.training", "irrigation",
                       "water.source", "harvest method", "system.type", "system.location", 
                       "varieties", "post.harvest.wash", "documentation", "hydro grow media", "other grow media")

