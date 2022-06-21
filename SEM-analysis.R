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


#model specification

mod.1 ='
RIZIK ~ M.norm + P + N + ASP8.norm 
KORISNOST ~ M.norm + P + N + ASP8.norm 

SOCIJALNA_DISTANCA ~ RIZIK + KORISNOST 
HIGIJENA ~ RIZIK + KORISNOST 
'

#model estimate

mod.est = sem(
  model = mod.1,
  data = covdatadf,
  
)

summary(mod.est,
        fit.measures = TRUE)

#model plot
semPaths(
  object = mod.est,
  what = "path",
  whatLabels = "par",
  style = "ram",
  layout = "tree",
  rotation = 2,
  sizeMan = 7,
  sizeLat = 7,
  color = "lightgray",
  edge.label.cex = 1.2,
  label.cex = 1.3
)
