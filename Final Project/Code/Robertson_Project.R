cat("\014") 
rm(list=ls())
dev.off()
options(scipen=999)

packages <- c("plyr","ggplot2","scales","reshape2","lattice","latticeExtra",
              "corrplot","corrgram","ResourceSelection","e1071","tree","MASS")
require(packages)
lapply(packages,require,character.only = TRUE)


setwd("C:/Users/matt.robertson/Documents/Grad School/PREDICT 454 Advanced Modeling/Project/Forest_Cover")

gz = gzfile('covtype.data.gz','rt')   
df <- read.csv(gz,header=F)

count.col <- ncol(df) 
count.row <- nrow(df)

#Quick Summary Function --> DOESN"T REMOVE NULLS
summary.df <- data.frame(
  Name = character(),
  Class = character(),
  NMiss = integer(),
  N = integer(),
  Mean = numeric(),
  Median = numeric(),
  SD = numeric(),
  CoV = numeric(),
  Min = numeric(),
  Max = numeric(),
  Range = numeric(),
  Q1 = numeric(),
  Q3 = numeric(),
  Outliers.Low = integer(),
  Outliers.High = integer(),
  stringsAsFactors = FALSE
)


for (i in 1:count.col){
  iqc <- 1.5*(quantile(df[,i],.75) - quantile(df[,i],.25))
  summary.df[i,1] <- names(df[i])
  summary.df[i,2] <- class(df[,i])
  summary.df[i,3] <- sum(is.na(df[,i]))
  summary.df[i,4] <- sum(!is.na(df[,i]))
  summary.df[i,5] <- mean(df[,i])
  summary.df[i,6] <- median(df[,i])
  summary.df[i,7] <- sd(df[,i])
  summary.df[i,8] <- sd(df[,i]) / mean(df[,i])
  summary.df[i,9] <- min(df[,i])
  summary.df[i,10] <- max(df[,i])
  summary.df[i,11] <- max(df[,i]) - min(df[,i])
  summary.df[i,12] <- quantile(df[,i],.25)
  summary.df[i,13] <- quantile(df[,i],.75)
  summary.df[i,14] <- sum(df[,i] < quantile(df[,i],.25) - iqc)
  summary.df[i,15] <- sum(df[,i] > quantile(df[,i],.75) + iqc)
}
summary.df


ggplot(data = melt(df[1:10]), mapping = aes(x = value)) + 
  geom_histogram(bins = 20) + facet_wrap(~variable, scales = 'free_x')

#xtabs(V11 ~ V55, data = df[df$V11==1,])
#table(df$V11)

#Density Plots by Class_ID
for (i in 1:10){
  x <-densityplot(~ df[i],
                  data = df, 
                  groups = V55,
                  plot.points = FALSE, 
                  ref = TRUE,
                  xlab = names(df[i]),
                  auto.key = list(columns = 3))
  print(x)
}




#Correlation Matrix
w <- cor(df[1:10])
par(ask = FALSE)
corrplot(w, method = "circle", 
         type = "lower",
         addgrid.col = FALSE,
         addCoef.col = TRUE,
         addCoefasPercent = TRUE,
         diag = FALSE,
         #title = "Wine Correlation Matrix",
         tl.pos = "lt",
         tl.cex=  .75,
         tl.col	= "black",
         cl.pos = "r",
         cl.ratio=0.15,
         number.cex = .6)



summary(df$V55)/length(df$V55)
#Tree Model
df$V55 <- factor(df$V55)
tree.forest <- tree(V55~. ,df)
summary(tree.forest)
plot(tree.forest)
text(tree.forest, pretty = 0)
tree.forest

#1 Elevation                	          meters
#2 Aspect			                          Aspect in degrees azimuth
#3 Slope			                          Slope in degrees
#4 Horizontal_Distance_To_Hydrology	    Horz Dist to nearest surface water features
#5 Vertical_Distance_To_Hydrology		    Vert Dist to nearest surface water features
#6 Horizontal_Distance_To_Roadways	    Horz Dist to nearest roadway
#7 Hillshade_9am 				                Hillshade index at 9am, summer solstice
#8 Hillshade_Noon				                Hillshade index at noon, summer soltice
#9 Hillshade_3pm				                Hillshade index at 3pm, summer solstice
#10 Horizontal_Distance_To_Fire_Points   Horz Dist to nearest wildfire ignition points
