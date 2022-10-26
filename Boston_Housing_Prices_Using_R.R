library(ggplot2)
library(dplyr)
library(forecast)
library(Metrics)
library(tidyr)
library(plyr)
library(forecast)
library(Matrix)
library(tibble)
library(DBI)

boston.housing.df <- read.csv("BostonHousing.csv")
boston.housing.df
boston.df <- boston.housing.df[,-14] # as the CAT.MEDV is not mentioned in the question
boston.df

selected.var <- c("MEDV", "CRIM", "CHAS","RM")

#partitioning data for comparing training and vaidation dataset for future reference.
set.seed(1)

train.index <- sample(c(1:506),304)
train.sel.df <- boston.df[, selected.var]
train.df <- boston.df[train.index,]
valid.df <- boston.df[-train.index, ]
nrow(valid.df)
nrow(train.df)

#.........6.1 (b)
#modeling

boston.lm <- lm(MEDV~., data = train.sel.df)
boston.lm
options(scipen = 999)

summary(boston.lm)

#....... 6.1(c)

#Predict MEDV

boston.lm.pred <- predict(boston.lm, data.frame("CRIM" = 0.1, "RM" = 6, "CHAS" = 0))
boston.lm.pred

mae(boston.df$MEDV,boston.lm.pred)

#.......6.1(d.1)

plot(boston.df)
cor(boston.df)
cor(boston.df[,c("MEDV", "LSTAT", "DIS","NOX", "AGE","INDUS")]) #strong negatively correlated variables
cor(boston.df[,c("INDUS", "NOX", "TAX")]) 

#.......6.1(d.2)

boston.df[,-c(13,14)] %>% #remove last 2 columns
  cor() %>% 
  heatmap(,Rowv = NA,Colv = NA) 
positive_corr <- cor(boston.df[,c("INDUS", "NOX", "TAX","AGE","RAD")]) #strong positive correlation (corr > 0.7)
positive_corr
plot(boston.df[,c("INDUS", "NOX", "TAX","AGE","RAD")])
heatmap(positive_corr,Rowv = NA,Colv = NA)
