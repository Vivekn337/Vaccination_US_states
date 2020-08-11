R Code
##original spreadsheet with 49 variables
#library(readxl)
#project_data_r3 <- read_excel("R/project_data_r3.xlsx")
#View(project_data_r3)

flu_data <- project_data_r3
flu_data <- flu_data[ ,-c(1)] #remove state names

##final spreadsheet with 24 variables
library(readxl)
project_data_final <- read_excel("R/project_data_final.xlsx")
View(project_data_final)

flu_datafinal <- project_data_final
flu_datafinal <- flu_datafinal[ ,-c(1)] #remove state names

#For Data Description Table
#data description
data.frame(mean=sapply(flu_datafinal, mean),
           min = sapply(flu_datafinal, min),
           max = sapply(flu_datafinal, max),
           sd = sapply(flu_datafinal, sd),
           class = sapply(flu_datafinal, class))

#For Correlation Visualization
# correlation visualization
#install.packages("ggplot2")
library(ggplot2)
cormatrix<-signif(cor(flu_data),2)
cormatrix
corrplot::corrplot(cormatrix, type = "upper")

#Install Various Packages
library(tidyverse)
library(caret)
#install.packages("olsrr")
#install.packages("fRegression")
library(fRegression)
library(olsrr)
#install.packages("car")
library("car")

#Final Model
flu.lm2 <-lm(Flu~ Income + Visit + Uninsured + Medicaid + Hospitals + Hispanic + Black + Asian +  Children + Population55 + Population65Plus + College + Religion + Democratic + Death +  Cellular + + Public + Mrexemption + Philoexemption + Philoexemption*Income + South + Midwest + Midwest + West + Philoexemption*South + Philoexemption*Midwest, data = flu_datafinal)
summary(flu.lm2)

#test for homoskedasticity
ols_test_breusch_pagan(flu.lm2)

#without interactions for testing multicollinearity
flu.lm2 <-lm(Flu~ Income + Visit + Uninsured + Medicaid + Hospitals + Hispanic + Black + Asian +  Children + Population55 + Population65Plus + College + Religion + Democratic + Death +  Cellular  + Public + Mrexemption + Philoexemption + South + Midwest + Midwest + West, data = flu_datafinal)

#test for multicollinearity
vif(flu.lm2) 

#Specification test
resetTest(flu.lmA, power = 2:3, type = c("fitted"))
resetTest(flu.lmA, power = 2, type = c("fitted"))
resetTest(flu.lmA, power = 3, type = c("fitted"))


#Normality testing of dependent variable
qqPlot(flu_data$Flu2) #all points fall approximately along this reference line-> we can assume normality.

shapiro.test(flu_datafinal$Flu) #P-value above benchmark, assume normality

#plot residuals vs percentiles
a<- plot(flu.lm2)
a

#plot fitted vs residuals
plot(lm(Flu~ Income + Visit + Uninsured + Medicaid + Hospitals + Hispanic + Black + Asian +  Children + Population55 + Population65Plus + College + Religion + Democratic + Death +  Cellular + + Public + Mrexemption + Philoexemption + Philoexemption*Income + South + Midwest + Midwest + West + Philoexemption*South + Philoexemption*Midwest, data = flu_datafinal))
abline(flu.lm2)


############Cluster##########
clust <- read.csv("cluster.csv")
row.names(clust) <- clust[,1]
clust.name <- clust[,1]
clust <- clust[,-c(1)]

#########Data Normalization#######
clust.norm.df <- sapply(clust, scale)

#######Elbow graph##########
set.seed(10)
k.max <- 30
data <- clust.norm.df
sum_square <- sapply(1:k.max, 
                     function(k){kmeans(clust.norm.df, k, nstart=50,iter.max = 15 )$tot.withinss})
sum_square
plot(1:k.max, sum_square,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

#########K-Mean##########
set.seed(10)
km4 <- kmeans(clust.norm.df, 3)
km4
########## Cluster Plot#############
plot(c(0), xaxt = 'n', ylab = "", type = "l", 
     ylim = c(min(km4$centers), max(km4$centers)), xlim = c(0, 9))
axis(1, at = c(1:9), labels = names(clust))
for (i in c(1:3))
  lines(km4$centers[i,], lty = i, lwd = 2, col = ifelse(i %in% c(1, 3, 5),
                                                        "red", "blue"))
text(x = 0.2, y = km4$centers[, 1], labels = paste("Cluster", c(1:3)))

##########Plot on the US map##########
install.packages("maps")
library(maps)
library(usmap)
library(ggplot2)
library(dplyr)

us_states <- map_data("state")
States <- clust.name
clusters <- km4$cluster
clusters
pdata.df <- data.frame(States,clusters)
pdata.df$region <- tolower(pdata.df$States) 
us_mapplot <- left_join(us_states, pdata.df)
head(us_mapplot)

p <- ggplot(data = us_mapplot,
            aes(x = long, y = lat,
                group = group, fill = clusters))

p + geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) 



#linear regression options

#linear regression with ALL variables besides northeast
flu.lm2 <- lm(Flu2~ Income + Poverty + Religion + Uninsured + Hispanic + Black + Asian + Unemployed + Death + Graduation + Public + Hospitals + Children + Medicaid + Visit + Population_aff_flu + Population55 + Mrexemp + Philoexemp + Democratic + Republican + College + Male + Female + Divorced + Married + NeverMarried + West + South + Midwest, data = flu_data)
summary(flu.lm2)
#adjR 0.6505

## Check for multicollinearity, want VIR below 10 as benchmark


vif(flu.lm2) #lots of high numbers

#linear regression with all variables besides northeast, male, female, divorced, nevermarried (just married)
flu.lm2 <- lm(Flu2~ Income + Poverty + Religion + Uninsured + Hispanic + Black + Asian + Unemployed + Death + Graduation + Public + Hospitals + Children + Medicaid + Visit + Population_aff_flu + Population55 + Mrexemp + Philoexemp + Democratic + Republican + College + Married + West + South + Midwest, data = flu_data)
summary(flu.lm2)
#adjR 0.6188
vif(flu.lm2) #republican extremely high so remove it since we have democratic

#linear regression with allvariables besides northeast, male, female, divorced, nevermarried (just married), republican
flu.lm2 <- lm(Flu2~ Income + Poverty + Religion + Uninsured + Hispanic + Black + Asian + Unemployed + Death + Graduation + Public + Hospitals + Children + Medicaid + Visit + Population_aff_flu + Population55 + Mrexemp + Philoexemp + Democratic + College + Married + West + South + Midwest, data = flu_data)
summary(flu.lm2)
#adjR 0.607
vif(flu.lm2) #college VIR 12 and Income 14 so remove graduation

#linear regression with allvariables besides northeast, male, female, divorced, nevermarried (just married), republican, graduation
flu.lm2 <- lm(Flu2~ Income + Poverty + Religion + Uninsured + Hispanic + Black + Asian + Unemployed + Death + Public + Hospitals + Children + Medicaid + Visit + Population_aff_flu + Population55 + Mrexemp + Philoexemp + Democratic + College + Married + West + South + Midwest, data = flu_data)
summary(flu.lm2)
#adjR 0.622
vif(flu.lm2) #college VIR now 7.8 and Income 14. remove income?

#linear regression with allvariables besides northeast, male, female, divorced, nevermarried (just married), republican, graduation
flu.lm2 <- lm(Flu2~ Poverty + Religion + Uninsured + Hispanic + Black + Asian + Unemployed + Death + Public + Hospitals + Children + Medicaid + Visit + Population_aff_flu + Population55 + Mrexemp + Philoexemp + Democratic + College + Married + West + South + Midwest, data = flu_data)
summary(flu.lm2)
#adjR 0.6274
vif(flu.lm2) #visit and democratic barely over 10

ols_test_breusch_pagan(flu.lm2) # it is homoskedastic 

#linear regression with all variables besides northeast, male, female, divorced, nevermarried, married, republican, graduation
flu.lm2 <- lm(Flu2~ Poverty + Religion + Uninsured + Hispanic + Black + Asian + Unemployed + Death + Public + Hospitals + Children + Medicaid + Visit + Population_aff_flu + Population55 + Mrexemp + Philoexemp + Democratic + College + West + South + Midwest, data = flu_data)
summary(flu.lm2)
#adjR 0.6355 #increased R2!
vif(flu.lm2)


#linear regression with all variables besides northeast, male, female, divorced, nevermarried, married, republican, graduation -COLLEGE INSTEAD
flu.lm2 <- lm(Flu2~ Poverty + Religion + Uninsured + Hispanic + Black + Asian + Unemployed + Death + Public + Hospitals + Children + Medicaid + Visit + Population_aff_flu + Population55 + Mrexemp + Philoexemp + Democratic + Graduation + West + South + Midwest, data = flu_data)
summary(flu.lm2)
#adjR 0.6352 #reduced R2! go back to including college and not hs graduation rates 

##MODEL A
#linear regression with all variables besides northeast, male, female, divorced, nevermarried, married, republican, graduation
flu.lmA <- lm(Flu2~ Poverty + Religion + Uninsured + Hispanic + Black + Asian + Unemployed + Death + Public + Hospitals + Children + Medicaid + Visit + Population_aff_flu + Population55 + Mrexemp + Philoexemp + Democratic + College + West + South + Midwest, data = flu_data)
summary(flu.lmA)
#adjR 0.6355


#test for proper model fit 
#test for multicollinearity
vif(flu.lmA) #visit and democratic barely over 10


#test for heteroskedasticty 
ols_test_breusch_pagan(flu.lmA)  #is is homoskedastic 

#test for non-linear fit with ramsey reset tst http://fmwww.bc.edu/EC-C/F2014/2228/ECON2228_2014_8.slides.pdf
resetTest(flu.lmA, power = 2:3, type = c("fitted"))
resetTest(flu.lmA, power = 2, type = c("fitted"))
resetTest(flu.lmA, power = 3, type = c("fitted"))


## add internet/computer options


#MODEL A:linear regression with all variables besides northeast, male, female, divorced, nevermarried, married, republican, graduation. ADD INTERNET
flu.lm2 <- lm(Flu2~ Poverty + Religion + Uninsured + Hispanic + Black + Asian + Unemployed + Death + Public + Hospitals + Children + Medicaid + Visit + Population_aff_flu + Population55 + Mrexemp + Philoexemp + Democratic + College + Internet + West + South + Midwest, data = flu_data)
summary(flu.lm2)
#adjR 0.6331 REDuced Rsquared
vif(flu.lm2) #internet VIF 12


#MODEL A:linear regression with all variables besides northeast, male, female, divorced, nevermarried, married, republican, graduation. ADD computer
flu.lm2 <- lm(Flu2~ Poverty + Religion + Uninsured + Hispanic + Black + Asian + Unemployed + Death + Public + Hospitals + Children + Medicaid + Visit + Population_aff_flu + Population55 + Mrexemp + Philoexemp + Democratic + College + Computer + West + South + Midwest, data = flu_data)
summary(flu.lm2)
#adjR 0.6272 REDuced Rsquared
vif(flu.lm2) #computer VIF 13


#MODEL A:linear regression with all variables besides northeast, male, female, divorced, nevermarried, married, republican, graduation. ADD smartphone
flu.lm2 <- lm(Flu2~ Poverty + Religion + Uninsured + Hispanic + Black + Asian + Unemployed + Death + Public + Hospitals + Children + Medicaid + Visit + Population_aff_flu + Population55 + Mrexemp + Philoexemp + Democratic + College + Smartphone + West + South + Midwest, data = flu_data)
summary(flu.lm2)
#adjR 0.6353 almost same r squared
vif(flu.lm2) #smartphone VIF 12

#MODEL A:linear regression with all variables besides northeast, male, female, divorced, nevermarried, married, republican, graduation. ADD Nocomputer
flu.lm2 <- lm(Flu2~ Poverty + Religion + Uninsured + Hispanic + Black + Asian + Unemployed + Death + Public + Hospitals + Children + Medicaid + Visit + Population_aff_flu + Population55 + Mrexemp + Philoexemp + Democratic + College + NoComputer + West + South + Midwest, data = flu_data)
summary(flu.lm2)
#adjR 0.6223 lowered R squared a little 
vif(flu.lm2) #NoComputer VIF 4 but democratic now 11

#MODEL A:linear regression with all variables besides northeast, male, female, divorced, nevermarried, married, republican, graduation. ADD Cellular
flu.lm2 <- lm(Flu2~ Poverty + Religion + Uninsured + Hispanic + Black + Asian + Unemployed + Death + Public + Hospitals + Children + Medicaid + Visit + Population_aff_flu + Population55 + Mrexemp + Philoexemp + Democratic + College + Cellular + West + South + Midwest, data = flu_data)
summary(flu.lm2)
#adjR 0.6411 RAISED R2 
vif(flu.lm2) #medicaid now 11 but VIF cellular ok

#MODEL A:linear regression with all variables besides northeast, male, female, divorced, nevermarried, married, republican, graduation. ADD NoInternet
flu.lm2 <- lm(Flu2~ Poverty + Religion + Uninsured + Hispanic + Black + Asian + Unemployed + Death + Public + Hospitals + Children + Medicaid + Visit + Population_aff_flu + Population55 + Mrexemp + Philoexemp + Democratic + College + NoInternet + West + South + Midwest, data = flu_data)
summary(flu.lm2)
#adjR 0.6331 R2 a little lower
vif(flu.lm2) #Nointernet12

#MODEL A:linear regression with all variables besides northeast, male, female, divorced, nevermarried, married, republican, graduation. ADD Cellular REMOVE Medicaid
flu.lm2 <- lm(Flu2~ Poverty + Religion + Uninsured + Hispanic + Black + Asian + Unemployed + Death + Public + Hospitals + Children + Visit + Population_aff_flu + Population55 + Mrexemp + Philoexemp + Democratic + College + Cellular + West + South + Midwest, data = flu_data)
summary(flu.lm2)
#adjR 0.6307 lowered R2
vif(flu.lm2) #visit barely above 10

#MODEL A:linear regression with all variables besides northeast, male, female, divorced, nevermarried, married, republican, graduation. ADD Cellular REMOVE VISIT
flu.lm2 <- lm(Flu2~ Poverty + Religion + Uninsured + Hispanic + Black + Asian + Unemployed + Death + Public + Hospitals + Children + Medicaid + Visit + Population_aff_flu + Population55 + Mrexemp + Philoexemp + Democratic + College + Cellular + West + South + Midwest, data = flu_data)
summary(flu.lm2)
#adjR 0.5706 lowered R2 a LOT! t 
vif(flu.lm2) #visit barely above 10


# so add cellular to model A to make model B
#MODEL B:linear regression with all variables PLUS CELLULAR without northeast, male, female, divorced, nevermarried, married, republican, graduation. 
flu.lmB <- lm(Flu2~ Poverty + Religion + Uninsured + Hispanic + Black + Asian + Unemployed + Death + Public + Hospitals + Children + Medicaid + Visit + Population_aff_flu + Population55 + Mrexemp + Philoexemp + Democratic + College + Cellular + West + South + Midwest, data = flu_data)
summary(flu.lmB)
#adjR 0.6411 RAISED R2 
vif(flu.lmB) #medicaid 11.62, Visit 10.61

ols_test_breusch_pagan(flu.lmB) #passes p = 0.37


# if we remove medicaid 
flu.lm2 <- lm(Flu2~ Poverty + Religion + Uninsured + Hispanic + Black + Asian + Unemployed + Death + Public + Hospitals + Children + Visit + Population_aff_flu + Population55 + Mrexemp + Philoexemp + Democratic + College + Cellular + West + South + Midwest, data = flu_data)
summary(flu.lm2)
#adjR 0.6307 lowered R2
vif(flu.lm2) #visit 10.45 not much change 

ols_test_breusch_pagan(flu.lm2) # p is GREATLY reduced to 0.14- makes it more heteroskedastic?? weird. Decide if significant


#remove uninsured 
flu.lm2 <-lm(Flu2~ Poverty + Religion + Hispanic + Black + Asian + Unemployed + Death + Public + Hospitals + Children + Medicaid + Visit + Population_aff_flu + Population55 + Mrexemp + Philoexemp + Democratic + College + Cellular + West + South + Midwest, data = flu_data)
summary(flu.lm2) #R 0.6377
vif(flu.lm2) #all below 10

ols_test_breusch_pagan(flu.lm2)

#replace with death2
flu.lm2 <- lm(Flu2~ Poverty + Religion + Hispanic + Black + Asian + Unemployed + Death2 + Public + Hospitals + Children + Medicaid + Visit + Population_aff_flu + Population55 + Mrexemp + Philoexemp + Democratic + College + Cellular + West + South + Midwest, data = flu_data)
summary(flu.lm2)#R 0.6528
vif(flu.lm2) #below 10
ols_test_breusch_pagan(flu.lm2)

#removed death bc coefficient so high
flu.lm2 <-flu.lm2 <-lm(Flu2~ Poverty + Religion + Hispanic + Black + Asian + Unemployed + Public + Hospitals + Children + Medicaid + Visit + Population_aff_flu + Population55 + Mrexemp + Philoexemp + Democratic + College + Cellular + West + South + Midwest, data = flu_data)
summary(flu.lm2) #R 0.6283
vif(flu.lm2) #all below 10


#replace with new death
flu.lm2 <-lm(Flu2~ Poverty + Religion + Hispanic + Black + Asian + Unemployed + NewDeath + Public + Hospitals + Children + Medicaid + Visit + Population_aff_flu + Population55 + Mrexemp + Philoexemp + Democratic + College + Cellular + West + South + Midwest, data = flu_data)
summary(flu.lm2) #R 0.6275 #religion, black, public, medicaid, visit, pop75, democratic
vif(flu.lm2) #all below 10
ols_test_breusch_pagan(flu.lm2)
resetTest(flu.lmA, power = 2:3, type = c("fitted"))

#replace with death2 ##MODEL C  ##April 18
flu.lm2 <-lm(Flu2~ Poverty + Religion + Hispanic + Black + Asian + Unemployed + Death2 + Public + Hospitals + Children + Medicaid + Visit + Population_aff_flu + Population55 + Mrexemp + Philoexemp + Democratic + College + Cellular + West + South + Midwest, data = flu_data)
summary(flu.lm2) #R 0.6528 #religion, black, asian, death2, public, medicaid, visit, pop75, democratic
vif(flu.lm2) #all below 10
ols_test_breusch_pagan(flu.lm2)
resetTest(flu.lmA, power = 2:3, type = c("fitted"))

#remove pop aff flu
flu.lm2 <-lm(Flu2~ Poverty + Religion + Hispanic + Black + Asian + Unemployed + Death2 + Public + Hospitals + Children + Medicaid + Visit  + Population55 + Mrexemp + Philoexemp + Democratic + College + Cellular + West + South + Midwest, data = flu_data)
summary(flu.lm2) #R 0.6651 #religion, black, asian, death2, public, medicaid, visit, pop75, democratic
vif(flu.lm2) 
ols_test_breusch_pagan(flu.lm2)


#   MODEL D: removed unemployment and pop_aff_flu, replaced poverty with income and added interaction variable bw income and philoexemp


flu.lm2 <-lm(Flu2~ + Philoexemp*Income + Uninsured + Income + Religion + Hispanic + Black + Asian +  Death2 + Public + Hospitals + Children + Medicaid + Visit  + Population55 + Mrexemp + Philoexemp + Democratic + College + Cellular + West + South + Midwest, data = flu_data2)
summary(flu.lm2) #0.7041
vif(flu.lm2) # have to without interactions because automatically creates multicollinearity
ols_test_breusch_pagan(flu.lm2) # passes


flu.lm2 <-lm(Flu2~ Income + Uninsured + Religion + Hispanic + Black + Asian +  Death2 + Public + Hospitals + Children + Medicaid + Visit  + Population55 + Pop75Plus + Mrexemp + Philoexemp + Democratic + College + Cellular + West + South + Midwest, data = flu_data)
summary(flu.lm2)
vif(flu.lm2)


#with geo interactions #FINAL MODEL
flu.lm2 <-lm(Flu~ Income + Visit + Uninsured + Medicaid + Hospitals + Hispanic + Black + Asian +  Children + Population55 + Population65Plus + College + Religion + Democratic + Death +  Cellular + + Public + Mrexemption + Philoexemption + Philoexemption*Income + South + Midwest + Midwest + West + Philoexemption*South + Philoexemption*Midwest, data = flu_datafinal)
summary(flu.lm2)
ols_test_breusch_pagan(flu.lm2)
vif(flu.lm2) 
resetTest(flu.lmA, power = 2:3, type = c("fitted"))
resetTest(flu.lmA, power = 2, type = c("fitted"))
resetTest(flu.lmA, power = 3, type = c("fitted"))


#without interactions for testing multicollinearity
flu.lm2 <-lm(Flu~ Income + Visit + Uninsured + Medicaid + Hospitals + Hispanic + Black + Asian +  Children + Population55 + Population65Plus + College + Religion + Democratic + Death +  Cellular + + Public + Mrexemption + Philoexemption + South + Midwest + Midwest + West, data = flu_datafinal)
summary(flu.lm2)
ols_test_breusch_pagan(flu.lm2)
vif(flu.lm2) 
resetTest(flu.lmA, power = 2:3, type = c("fitted"))
resetTest(flu.lmA, power = 2, type = c("fitted"))
resetTest(flu.lmA, power = 3, type = c("fitted"))
