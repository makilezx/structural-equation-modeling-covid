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


#####
######
#TEST 1

#mediation analysis
# dependent regression
SOCIJALNA_DISTANCA ~ b11*KORISNOST + b12*RIZIK + c11*ASP8.norm + c12*N + c13*P + c14*M.norm
HIGIJENA ~ b21*KORISNOST + b22*RIZIK + c21*ASP8.norm + c22*N + c23*P + c24*M.norm

# mediator regression
KORISNOST ~ a11*ASP8.norm + a12*N + a13*P + a14*M.norm
RIZIK ~ a21*ASP8.norm + a22*N + a23*P + a24*M.norm

# mediator residual covariance
KORISNOST ~~ RIZIK

# dependent residual covariance
SOCIJALNA_DISTANCA ~~ HIGIJENA

# effect decomposition

effect.med <- '
# y1 ~ x1
ind_x1_m1_y1 := a11*b11
ind_x1_m2_y1 := a21*b12
ind_x1_y1 := ind_x1_m1_y1 + ind_x1_m2_y1
tot_x1_y1 := ind_x1_y1 + c11

# y1 ~ x2
ind_x2_m1_y1 := a12*b11
ind_x2_m2_y1 := a22*b12
ind_x2_y1 := ind_x2_m1_y1 + ind_x2_m2_y1
tot_x2_y1 := ind_x2_y1 + c12

# y1 ~ x3
ind_x3_m1_y1 := a13*b11
ind_x3_m2_y1 := a23*b12
ind_x3_y1 := ind_x3_m1_y1 + ind_x3_m2_y1
tot_x3_y1 := ind_x3_y1 + c13

# y1 ~ x4
ind_x4_m1_y1 := a14*b11
ind_x4_m2_y1 := a24*b12
ind_x4_y1 := ind_x4_m1_y1 + ind_x4_m2_y1
tot_x4_y1 := ind_x4_y1 + c14

# y2 ~ x1
ind_x1_m1_y2 := a11*b21
ind_x1_m2_y2 := a21*b22
ind_x1_y2 := ind_x1_m1_y2 + ind_x1_m2_y2
tot_x1_y2 := ind_x1_y2 + c21

# y2 ~ x2
ind_x2_m1_y2 := a12*b21
ind_x2_m2_y2 := a22*b22
ind_x2_y2 := ind_x2_m1_y2 + ind_x2_m2_y2
tot_x2_y2 := ind_x2_y2 + c22

# y2 ~ x3
ind_x3_m1_y2 := a13*b21
ind_x3_m2_y2 := a23*b22
ind_x3_y2 := ind_x3_m1_y2 + ind_x3_m2_y2
tot_x3_y2 := ind_x3_y2 + c23

# y2 ~ x4
ind_x4_m1_y2 := a14*b21
ind_x4_m2_y2 := a24*b22
ind_x4_y2 := ind_x4_m1_y2 + ind_x4_m2_y2
tot_x4_y2 := ind_x4_y2 + c24
'

res.medtest <- sem(effect.med, covdatadf)
summary(res.medtest)

#bootstraping
res.medtest.bootstrap <- sem(effect.med, covdatadf, se = "bootstrap")
summary(res.medtest.bootstrap)

