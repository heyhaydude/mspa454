
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


str(forest.orig)
colnames(forest)
barplot(table(forest$CoverType))
table(is.na(forest)) # no missing values


########## DATA TRANSFORMATIONS and VARIABLE MAINTENANCE #########################
apply(sapply(forest,as.numeric),2,sum) #soil type 15 doesn't have any observations. so remove

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
plot(tbl)
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
barchart(na.omit(as.matrix(soil.ratios)),col=brewer.pal(7, "Pastel2"))

#Questions
# how often is same soil types together

