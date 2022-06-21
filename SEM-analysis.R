library(lavaan)  
library(psych)  
library(semPlot) 
library(haven)
library(psych)


#adding dataset
data <- read_spss(file.choose())

colnames(data)

covdatadf <- as.data.frame(data)
head(covdatadf)


#descriptive statistcs
covdatadf.deskriptivna <- describe(covdatadf)
covdatadf.deskriptivna


#mardia coef. of multivariate normality
mardia(covdatadf)

#winsor
covdatadf.winsor <- winsor(covdatadf)
describe(covdatadf.winsor)
mardia(covdatadf.winsor)


