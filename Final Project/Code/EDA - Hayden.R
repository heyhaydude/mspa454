packages <- c("plyr","ggplot2","scales","reshape2","lattice","latticeExtra",
              "corrplot","corrgram","ResourceSelection","e1071","tree","MASS")
require(packages)
lapply(packages,require,character.only = TRUE)

setwd("/Users/haydude/Development/mspa/MSPA454/Final Project/Data")
gz = gzfile('covtype.data.gz','rt')   
forest = read.csv(gz,header=F)
forest.colnames = read.csv('covtyp.colnames.csv',header=F)
colnames(forest) = forest.colnames$V1

str(forest)
head(forest)
colnames(forest)
