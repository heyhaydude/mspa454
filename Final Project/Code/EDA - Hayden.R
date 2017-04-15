
########## SETUP #########################
setwd("/Users/haydude/Development/mspa/MSPA454/Final Project/Data")
gz = gzfile('covtype.data.gz','rt')   
forest.orig = read.csv(gz,header=F)
forest.orig.colnames = t(read.csv('covtyp.colnames.csv',header=F))
colnames(forest.orig) = forest.orig.colnames
forest.var.continuous = c('Elevation','Aspect','Slope', 'HDist.Hydrology', 'VDist.Hydrology', 
                          'HDist.Roadway', 'Hillshade.9am', 'Hillshade.12pm', 'Hillshade.3pm',
                          'HDist.FirePoint')

# assign factor/discrete variables
library(dplyr)

#summary(forest.orig)

# for speed, will perform eda on subset until ready to do a full run
set.seed(33)
forest = forest.orig[sample(nrow(forest.orig),20000),]
#forest[,'SoilType15'] = NULL
forest.numeric = forest
forest.var.discrete.indices = grep("^Area|^SoilType|CoverType", colnames(forest))
forest[,forest.var.discrete.indices] = as.factor(unlist(forest[,forest.var.discrete.indices]))


########## DATA QUALITY CHECK #########################

str(forest.orig)
dim(forest.orig)
table(is.na(forest.orig))
summary(forest.orig)

options(digits=3)
my.summary <- function(x,...){
  c(mean=mean(x, ...),
    sd=sd(x, ...),
    median=median(x, ...),
    min=min(x, ...),
    max=max(x,...),
    type="Continuous")
}


forest.stats= apply(forest.orig[,1:10], 2, my.summary)

c()

library(knitr)
kable(forest.stats)

ggplot(as.data.frame(table(forest.orig$CoverType)), aes(x=Var1, y = Freq)) + ggtitle("Forest Cover
Frequency by Class") + geom_bar(stat = "identity", fill="#1f78b4", width=.5,
                                color="black") + xlab("Cover Type")


# matt r solution

## area
forest.area= forest.orig[11:14]

summary.area <- data.frame(
  Name = character(),
  Count = numeric(),
  stringsAsFactors = F)

for (i in 1:4){
  summary.area[i,1] <- names(forest.area[i])
  summary.area[i,2] <- sum(forest.area[,i])
}

summary.area

area<-summary.area[with(summary.area,order(-Count)),]
kable(area)

## soil
forest.soil= forest.orig[15:54]

summary.soil <- data.frame(
  Name = character(),
  Count = numeric(),
  stringsAsFactors = F)

for (i in 1:40){
  summary.soil[i,1] <- names(forest.soil[i])
  summary.soil[i,2] <- sum(forest.soil[,i])
}

summary.soil

soil<-summary.soil[with(summary.soil,order(-Count)),]
kable(soil)





########## DATA TRANSFORMATIONS and VARIABLE MAINTENANCE #########################

apply(sapply(forest,as.numeric),2,sum) #soil type 15 doesn't have any observations. so remove

# are any in multiple areas? NO. all belong to only one area
idx = grep("Area", colnames(forest))
temp = forest.numeric[,idx]
temp[apply(temp,1,sum) > 1,]
temp$Area = apply(temp,1,function(x) {
  return(which.max(x))
})
forest.numeric$Area = temp$Area


forest.numeric = sapply(forest,as.numeric) 

str(forest)

#### CONTINUOUS VARIABLES #################

# boxplots
library(ggplot2)
forest.scaled = scale(forest) 
st = stack(as.data.frame(forest.scaled[,forest.var.continuous]))
ggplot(as.data.frame(st)) +
  geom_boxplot(aes(x = ind, y = values)) +
  theme(axis.text.x = element_text(angle=0)) +
  scale_x_discrete(name ="") + scale_y_continuous(name ="") +
  ggtitle("Figure 3 - Boxplots of Scaled Continuous Variables") 

# density plots by CoverType
library(lattice)
density.plots = densityplot(~ Elevation + Aspect + Slope + HDist.Hydrology + VDist.Hydrology + 
                              HDist.Roadway + Hillshade.9am + Hillshade.12pm + Hillshade.3pm +
                              HDist.FirePoint,
                            data=forest, groups = CoverType, plot.points = FALSE, 
                            auto.key = list(space="right",title="Class"),
                            scales= list(x="free",y="free"), xlab = '')
plot(density.plots)

# density plots by Area
library(lattice)
density.plots = densityplot(~ Elevation + Aspect + Slope + HDist.Hydrology + VDist.Hydrology + 
                              HDist.Roadway + Hillshade.9am + Hillshade.12pm + Hillshade.3pm +
                              HDist.FirePoint,
                            data=forest.numeric, groups = Area, plot.points = FALSE, 
                            auto.key = list(space="right",title="Class"),
                            scales= list(x="free",y="free"), xlab = '')
plot(density.plots)


# correlations
library(corrplot)
corrplot(cor(forest[, forest.var.continuous]), 
         tl.col = "black", tl.cex = 0.8, tl.srt = 45,
         cl.cex = 0.8, pch.cex = 0.8, diag = FALSE,
         type="lower",
         addCoefasPercent = TRUE, addCoef.col = TRUE,number.cex = .6) #Matt added to show correlation amounts

#lda
library(MASS)
idx = grep("CoverType", colnames(forest))
forest.wo.covertype = forest[,-idx]
forest.lda = lda(CoverType ~ .,data = forest)
#plot(forest.lda, col=forest$CoverType)
forest.lda.pred = predict(forest.lda,data=forest.wo.covertype)
tbl = table(forest.lda.pred$class,forest$CoverType)
#plot(tbl)
addmargins(tbl)

##### DISCRETE VARIABLES ###################################

library(lattice)
idx = grep("SoilType|CoverType", colnames(forest.numeric))
df = as.data.frame(forest.numeric[,idx])
idx.type = grep("CoverType", colnames(df))

df.temp = df[,-idx.type]

soil.sums = apply(df.temp,2,function(x) {
  tbl = table(x,df$CoverType)
  if (dim(tbl)[1] < 2) {
    tbl = rbind('0' = tbl, '1' = rep(0,7), deparse.level = 1)
  }
  return (apply(tbl,1,sum)[2])
})
soil.sums

soil.sums.byclass = apply(df[,-idx.type],2,function(x) {
  tbl = table(x,df$CoverType)
  tbl = tbl[seq(2,14,by=2)]
  return (tbl)
})
soil.sums.byclass

soil.ratios = as.data.frame(t(soil.sums.byclass)/soil.sums)
library(RColorBrewer)
soil.ratios.m = na.omit(as.matrix(soil.ratios))
barchart(soil.ratios.m,col=brewer.pal(7, "Pastel2"))


# AREAS
library(lattice)
idx = grep("Area|CoverType", colnames(forest.numeric))
df = as.data.frame(forest.numeric[,idx])
idx.type = grep("CoverType", colnames(df))

df.temp = df[,-idx.type]

area.sums = apply(df.temp,2,function(x) {
  tbl = table(x,df$CoverType)
  if (dim(tbl)[1] < 2) {
    tbl = rbind('0' = tbl, '1' = rep(0,7), deparse.level = 1)
  }
  return (apply(tbl,1,sum)[2])
})
area.sums

area.sums.byclass = apply(df[,-idx.type],2,function(x) {
  tbl = table(x,df$CoverType)
  tbl = tbl[seq(2,14,by=2)]
  return (tbl)
})
area.sums.byclass

area.ratios = as.data.frame(t(area.sums.byclass)/area.sums)
library(RColorBrewer)
area.ratios.m = na.omit(as.matrix(area.ratios))
barchart(area.ratios.m,col=brewer.pal(7, "Pastel2"))





#Questions
# how often is same soil types together

