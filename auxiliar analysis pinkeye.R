---
  title: "Pinkeye"
author: "Alex Fonseca"
date: "4/27/2022"
output:
  html_document: default
word_document: default
pdf_document: default
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown


This chunk imports the data and creates two datasets. One set of data for all the animals registered in the spreadsheet and another where animals with a note are excluded (suspicion of pinkeye at enrollment) 


```{r , echo=F}
library(readxl)
library(dplyr)
library(knitr)
library(stats)


##Final weight data set

fw <- read_excel("Data Sheet November 1.xlsx", 
                 sheet = "Data Sheet November 1")
fw1 <- data.frame(fw)


##Initial weight data set

iw <- read_excel("Pinkeye Trial Data.xlsx", 
                 sheet = "all calves initial vax")
iw1 <- data.frame(iw)

## Ordering ID from initial weights 
test1 <- iw1[order(iw1$ID),]

##Selecting complete cases in initial weight dataset

test3 <- test1[complete.cases(test1$ID),]
#test1 <- data.frame(order(survey_data3$ID))


#Merging initial and final weight data sets  
dataframe_ABC = merge(test3,fw1 , by="ID")


## Changing name of pinkeye outcome 
names(dataframe_ABC)[names(dataframe_ABC) == "Pinkeye.lesions...scars"] <- "Pinkeye"

## New dataset excluding animals with initial notes 
dataframe_WO_NOTES <- dataframe_ABC %>% 
  filter(is.na(Notes.x))
## excluding (??) animal
dataframe_WO_NOTES <- dataframe_WO_NOTES %>%
  filter(ID != "066")


## creating a new variable called Bovis and Bovoculi for dataset with an without notes 
dataframe_ABCD <- dataframe_ABC %>% 
  mutate(Bovis = ifelse(VACCINE %in% c("BOVIS ONLY", "BOTH"), 1, 0 )) %>% 
  mutate(Bovoculi = ifelse(VACCINE %in% c("BOVOCULI ONLY", "BOTH"), 1, 0 ))


dataframe_ABCD_WO_NOTES <- dataframe_WO_NOTES %>% 
  mutate(Bovis = ifelse(VACCINE %in% c("BOVIS ONLY", "BOTH"), 1, 0 ))%>% 
  mutate(Bovoculi = ifelse(VACCINE %in% c("BOVOCULI ONLY", "BOTH"), 1, 0 ))

## Setting Bovoculi and Bovis as categorical variables 
dataframe_ABCD$Bovoculi <- factor(dataframe_ABCD$Bovoculi)
dataframe_ABCD$Bovis <- factor(dataframe_ABCD$Bovis)
##### without notes 
dataframe_ABCD_WO_NOTES$Bovoculi <- factor(dataframe_ABCD_WO_NOTES$Bovoculi)

dataframe_ABCD_WO_NOTES$Bovis <- factor(dataframe_ABCD_WO_NOTES$Bovis)

## creating a new dataset which include Pinkeye outcome
dataPD <- dataframe_ABCD %>%
  mutate(PD = ifelse(is.na(Pinkeye), 0,1 ))
dataPD_WO_NOTES <- dataframe_ABCD_WO_NOTES %>%
  mutate(PD = ifelse(is.na(Pinkeye), 0,1 ))%>%
  mutate(Weight_10 = Weight*10)

## Setting vaccine and Pinkeye as factors 
dataPD$VACCINE <- factor(dataPD$VACCINE)
dataPD$PD <- factor(dataPD$PD)
dataPD$Bovoculi <- factor(dataPD$Bovoculi)
dataPD$Bovis <- factor(dataPD$Bovis)



dataPD_WO_NOTES$VACCINE <- factor(dataPD_WO_NOTES$VACCINE)
dataPD_WO_NOTES$PD <- factor(dataPD_WO_NOTES$PD)

dataPD_WO_NOTES$Bovoculi <- factor(dataPD_WO_NOTES$Bovoculi)
dataPD_WO_NOTES$Bovis <- factor(dataPD_WO_NOTES$Bovis)
## setting date of vaccination as character

#dataframe_ABCD$Date.of.initial.vaccination <- #as.character(dataframe_ABCD$Date.of.initial.vaccination)




## coding dates in order: 1, 2, 3 vaccination

#dataframe_ABCD <- dataframe_ABCD %>% 
#  mutate(ordervacc = ifelse(Date.of.initial.vaccination == #"2021-06-04", 1, ifelse(Date.of.initial.vaccination == #"2021-06-07", 2, 3 )))


```

This graph illustrates the the interaction 

Interaction plot between Bovis and Bovoculi

```{r , echo=F}

library(readxl)
library(dplyr)
library(knitr)
library(stats)


interaction.plot(x.factor = dataPD$Bovis, #x-axis variable
                 trace.factor = dataPD$Bovoculi, #variable for lines
                 response = dataPD$Weaning.Weight, #y-axis variable
                 fun = median, #metric to plot
                 ylab = "Weight",
                 xlab = "Bovis",
                 col = c("pink", "blue"),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "Bovoculi")

```


This interaction plot graphically displays the average weaning weight for animals vaccinated with M. bovis and M. bovoculi, using animals without suspected pinkeye. Since there is no crossing between the lines, we can say that there is no interaction.

```{r , echo=F}

library(readxl)
library(dplyr)
library(knitr)
library(stats)


interaction.plot(x.factor = dataPD_WO_NOTES$Bovis, #x-axis variable
                 trace.factor = dataPD_WO_NOTES$Bovoculi, #variable for lines
                 response = dataPD_WO_NOTES$Weaning.Weight, #y-axis variable
                 fun = median, #metric to plot
                 ylab = "Weight",
                 xlab = "Bovis",
                 col = c("pink", "blue"),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "Bovoculi")

```


The table summarizes the number of animals enrolled, their initial and final weight excluding animals for all the animals.

```{r , echo=F }

library(dplyr)
library(knitr)
library(stats)
library(kableExtra)
library(tinytex)
library(AER)




#dataframe_ABCD   %>% group_by(VACCINE) %>% summarise(avearge = mean(Weaning.Weight))

#dataframe_ABCD   %>% group_by(VACCINE) %>% summarise(avearge = mean(Weight))

table <- dataPD %>% group_by(VACCINE) %>% summarise('Animals allocated' = n(),'Mean enrollment weight'  = mean(Weight), 'Mean weaning weight' = mean(Weaning.Weight))

table1 <- t(table)

table1  %>% kbl(align = "c")

table1


#table1 <-  table1 %>% kable_styling(bootstrap_options = "scale_down")

#kable (table1)




```

The table summarizes the number of animals enrolled, their initial and final weight excluding animals with notes (suspected pinkeye). A possible differential enrollment was analyzed by obtaining the percentage of animals without suspicion of pinkeye (188) from the total number of animals reported (241). Thus, 76.3% (100/131) of the animals that received both vaccines had no note (suspected pinkeye), 70.7% (29/41) of the animals that were vaccinated only with M bovis had no note, 82.3% (28/34) of the animals vaccinated only with M bovoculi had no note, and 88.5% (31/35) of the control animals had no note at all (suspected pinkeye). Since there are no large differences in such percentages, we conclude that there was no differential enrollment.

```{r , echo=F }

library(dplyr)
library(knitr)
library(stats)
library(kableExtra)
library(tinytex)
library(AER)




#dataframe_ABCD   %>% group_by(VACCINE) %>% summarise(avearge = mean(Weaning.Weight))

#dataframe_ABCD   %>% group_by(VACCINE) %>% summarise(avearge = mean(Weight))

table <- dataPD_WO_NOTES %>% group_by(VACCINE) %>% summarise('Animals allocated' = n(),'Mean enrollment weight'  = mean(Weight), 'Mean weaning weight' = mean(Weaning.Weight))

table1 <- t(table)

table1  %>% kbl(align = "c")

table1


#table1 <-  table1 %>% kable_styling(bootstrap_options = "scale_down")

#kable (table1)




```

Summary table for mean enrollment/weaning weight to animals diagnosed with pinkeye (PD=0) and those without diagnosis (PD=1). This table includes all the animals initially enrolled. 


```{r , echo=F }

library(dplyr)
library(knitr)
library(stats)
library(kableExtra)
library(tinytex)
library(AER)


table <- dataPD %>% group_by(PD) %>% summarise('Animals allocated' = n(),'Mean enrollment weight'  = mean(Weight), 'Mean weaning weight' = mean(Weaning.Weight))

table1 <- t(table)

table1  %>% kbl(align = "c")
table1
```


Summary table for mean enrollment/weaning weight to animals diagnosed with pinkeye (PD=0) and those without diagnosis (PD=1). This table excludes animal with notes (suspected pinkeye). 


```{r , echo=F }

library(dplyr)
library(knitr)
library(stats)
library(kableExtra)
library(tinytex)
library(AER)


table <- dataPD_WO_NOTES %>% group_by(PD) %>% summarise('Animals allocated' = n(),'Mean enrollment weight'  = mean(Weight), 'Mean weaning weight' = mean(Weaning.Weight))

table1 <- t(table)

table1  %>% kbl(align = "c")
table1
```


Chi-square test with complete dataset 


```{r , echo=F}


library(readxl)
library(dplyr)
library(knitr)
library(stats)




#result with ANOVA
#av = aov(Weight ~ Bovis, data=dataframe_ABCD) 
#summary(av) 
#av$coefficients


#####general linear regression with interaction
#logistic <- glm(PD ~ Bovis*Bovoculi, data=dataPD, family = "binomial")

#summary(logistic) 
#exp(cbind(OR = coef(logistic), confint(logistic)))


library(summarytools)
library(dplyr)

# fourth method:
dataPD %$%
  ctable(VACCINE, PD,
         prop = "r", chisq = TRUE, headings = FALSE
  ) %>%
  print(
    method = "render",
    style = "rmarkdown",
    footnote = NA
  )


```


Chi-square test excluding animals with notes

```{r , echo=F}


library(readxl)
library(dplyr)
library(knitr)
library(stats)




#result with ANOVA
#av = aov(Weight ~ Bovis, data=dataframe_ABCD) 
#summary(av) 
#av$coefficients


#####general linear regression with interaction
#logistic <- glm(PD ~ Bovis*Bovoculi, data=dataPD_WO_NOTES, family = "binomial")

#summary(logistic) 
#exp(cbind(OR = coef(logistic), confint(logistic)))

library(summarytools)
library(dplyr)

# fourth method:
dataPD_WO_NOTES %$%
  ctable(VACCINE, PD,
         prop = "r", chisq = TRUE, headings = FALSE
  ) %>%
  print(
    method = "render",
    style = "rmarkdown",
    footnote = NA
  )



```


Logistic regression with complete dataset and adjusting for initial weight


```{r , echo=F}


library(readxl)
library(dplyr)
library(knitr)
library(stats)




#result with ANOVA
#av = aov(Weight ~ Bovis, data=dataframe_ABCD) 
#summary(av) 
#av$coefficients


#####general linear regression with interaction
logistic <- glm(PD ~ Bovis*Bovoculi+Weight, data=dataPD, family = "binomial")

summary(logistic) 
#exp(cbind(OR = coef(logistic), confint(logistic)))



```

Logistic regression excluding animals with notes and adjusting for initial weight


```{r , echo=F}


library(readxl)
library(dplyr)
library(knitr)
library(stats)




#result with ANOVA
#av = aov(Weight ~ Bovis, data=dataframe_ABCD) 
#summary(av) 
#av$coefficients

####general linear regression with interaction
logistic <- glm(PD ~ Bovis*Bovoculi+Weight, data=dataPD_WO_NOTES, family = "binomial")

summary(logistic) 
#exp(cbind(OR = coef(logistic), confint(logistic)))



```

Logistic regression with complete dataset


```{r , echo=F}


library(readxl)
library(dplyr)
library(knitr)
library(stats)




#result with ANOVA
#av = aov(Weight ~ Bovis, data=dataframe_ABCD) 
#summary(av) 
#av$coefficients


#####general linear regression with interaction
logistic <- glm(PD ~ Bovis*Bovoculi, data=dataPD, family = "binomial")

summary(logistic) 
#exp(cbind(OR = coef(logistic), confint(logistic)))



```



Logistic regression with complete dataset but using VACCINE


```{r , echo=F}


library(readxl)
library(dplyr)
library(knitr)
library(stats)




#result with ANOVA
#av = aov(Weight ~ Bovis, data=dataframe_ABCD) 
#summary(av) 
#av$coefficients


#####general linear regression with interaction
logistic <- glm(PD ~ relevel(VACCINE, ref="NOTHING"), data=dataPD, family = "binomial")

summary(logistic) 
#exp(cbind(OR = coef(logistic), confint(logistic)))



```

Logistic regression excluding animals with notes


```{r , echo=F}


library(readxl)
library(dplyr)
library(knitr)
library(stats)




#result with ANOVA
#av = aov(Weight ~ Bovis, data=dataframe_ABCD) 
#summary(av) 
#av$coefficients

####general linear regression with interaction
logistic <- glm(PD ~ Bovis*Bovoculi, data=dataPD_WO_NOTES, family = "binomial")

summary(logistic) 
#exp(cbind(OR = coef(logistic), confint(logistic)))



```


This chunk contains linear analyses taking as outcome final weight and adjusting for initial weight. Given that interaction was not significant, it means that the effect of vaccinate with Bovis on weaning weight is independent on vaccinate with Bovoculi. Regarding the main effects i,e. the effect of vaccinate with Bovis or Bovoculi on weaning weight, these were not signifcant.  

```{r , echo=F}


library(readxl)
library(dplyr)
library(knitr)
library(stats)



#result with ANOVA
#av = aov(Weight ~ Bovis, data=dataframe_ABCD) 
#summary(av) 
#av$coefficients


#####general linear regression with interaction
linearMod <- lm(Weaning.Weight ~ Bovis*Bovoculi +Weight, data=dataPD)

summary(linearMod) 



```


This chunk contains linear analyses taking as outcome final weight. Given that interaction was not significant, it means that the effect of vaccinate with Bovis on weaning weight is independent on vaccinate with Bovoculi. Regarding the main effects i,e. the effect of vaccinate with Bovis or Bovoculi on weaning weight, these were not signifcant.  

```{r , echo=F}


library(readxl)
library(dplyr)
library(knitr)
library(stats)



#result with ANOVA
#av = aov(Weight ~ Bovis, data=dataframe_ABCD) 
#summary(av) 
#av$coefficients


#####general linear regression with interaction
linearMod <- lm(Weaning.Weight ~ Bovis*Bovoculi, data=dataPD)

summary(linearMod) 



```


This chunk contains linear analyses taking as outcome final weight and excluding animals with notes and (??) animal. Model adjusted for initial weight

```{r , echo=F}


library(readxl)
library(dplyr)
library(knitr)
library(stats)



#result with ANOVA
#av = aov(Weight ~ Bovis, data=dataframe_ABCD) 
#summary(av) 
#av$coefficients


#####general linear regression with interaction
linearMod <- lm(Weaning.Weight ~ Bovis*Bovoculi+Weight, data=dataPD_WO_NOTES)

summary(linearMod) 



```


This chunk contains linear analyses taking as outcome final weight and excluding animals with notes and (??) animal. 

```{r , echo=F}


library(readxl)
library(dplyr)
library(knitr)
library(stats)



#result with ANOVA
#av = aov(Weight ~ Bovis, data=dataframe_ABCD) 
#summary(av) 
#av$coefficients


#####general linear regression with interaction
linearMod <- lm(Weaning.Weight ~ Bovis*Bovoculi, data=dataPD_WO_NOTES)

summary(linearMod) 



```



This chunk contains linear analyses taking as outcome final weight and adjusting for initial weight and Pinkeye.   

```{r , echo=F}


library(readxl)
library(dplyr)
library(knitr)
library(stats)



#result with ANOVA
#av = aov(Weight ~ Bovis, data=dataframe_ABCD) 
#summary(av) 
#av$coefficients


#####general linear regression with interaction
linearMod <- lm(Weaning.Weight ~ Bovis*Bovoculi +Weight + PD, data=dataPD)

summary(linearMod) 



```


This chunk contains linear analyses taking as outcome final weight and adjusting for initial weight and Pinkeye. Animals with initial notes were excluded.    

```{r , echo=F}


library(readxl)
library(dplyr)
library(knitr)
library(stats)



#result with ANOVA
#av = aov(Weight ~ Bovis, data=dataframe_ABCD) 
#summary(av) 
#av$coefficients


#####general linear regression with interaction
linearMod <- lm(Weaning.Weight ~ Bovis*Bovoculi +Weight + PD, data=dataPD_WO_NOTES)

summary(linearMod) 



```

Regression for every 10 units of initial weight in animals without notes 

```{r , echo=F}


library(readxl)
library(dplyr)
library(knitr)
library(stats)



#result with ANOVA
#av = aov(Weight ~ Bovis, data=dataframe_ABCD) 
#summary(av) 
#av$coefficients


#####general linear regression with interaction
linearMod <- lm(Weaning.Weight ~ Weight_10 +PD, data=dataPD_WO_NOTES)

summary(linearMod) 
```

This chunk contains linear analyses taking as outcome final weight and adjusting for Pinkeye.   

```{r , echo=F}


library(readxl)
library(dplyr)
library(knitr)
library(stats)



#result with ANOVA
#av = aov(Weight ~ Bovis, data=dataframe_ABCD) 
#summary(av) 
#av$coefficients


#####general linear regression with interaction
linearMod <- lm(Weaning.Weight ~ Weight +PD, data=dataPD)

summary(linearMod) 


influencePlot(linearMod)
outlierTest(linearMod)

cutoff <- 4/(nrow(dataPD)-length(linearMod$coefficients)-2)
plot(linearMod, which=4, cook.levels=cutoff)
abline(h=cutoff, lty=2, col="red")


hist(dataPD$Weaning.Weight)
hist(dataPD$Weight)

qqPlot(dataPD$Weight)
qqPlot(dataPD$Weaning.Weight)
qqPlot(linearMod)


```

Excluding influential/outlier ID = 64
```{r , echo=F}


library(readxl)
library(dplyr)
library(knitr)
library(stats)

dataPD_64 <- dataPD %>%
  filter(ID != "064")



linearMod <- lm(Weaning.Weight ~ Weight +PD, data=dataPD_64)

summary(linearMod) 


influencePlot(linearMod)
outlierTest(linearMod)

cutoff <- 4/(nrow(dataPD_64)-length(linearMod$coefficients)-2)
plot(linearMod, which=4, cook.levels=cutoff)
abline(h=cutoff, lty=2, col="red")


hist(dataPD_64$Weaning.Weight)
hist(dataPD_64$Weight)

qqPlot(dataPD_64$Weight)
qqPlot(dataPD_64$Weaning.Weight)
qqPlot(linearMod)


```


This chunk contains linear analyses taking as outcome final weight and adjusting for Pinkeye. This analysis excludes animals with notes.   

```{r , echo=F}


library(readxl)
library(dplyr)
library(knitr)
library(stats)



#result with ANOVA
#av = aov(Weight ~ Bovis, data=dataframe_ABCD) 
#summary(av) 
#av$coefficients


#####general linear regression with interaction
linearMod <- lm(Weaning.Weight ~ Weight +PD, data=dataPD_WO_NOTES)

summary(linearMod) 

influencePlot(linearMod)
outlierTest(linearMod)

cutoff <- 4/(nrow(dataPD_WO_NOTES)-length(linearMod$coefficients)-2)
plot(linearMod, which=4, cook.levels=cutoff)
abline(h=cutoff, lty=2, col="red")


hist(dataPD_WO_NOTES$Weaning.Weight)
hist(dataPD_WO_NOTES$Weight)

qqPlot(dataPD_WO_NOTES$Weight)
qqPlot(dataPD_WO_NOTES$Weaning.Weight)
qqPlot(linearMod)
```

analyses excluding ID = 64 and excluding animals with notes

```{r , echo=F}


library(readxl)
library(dplyr)
library(knitr)
library(stats)

dataPD_WO_64 <- dataPD_WO_NOTES %>%
  filter(ID != "064") 

linearMod <- lm(Weaning.Weight ~ Weight +PD, data=dataPD_WO_64)

summary(linearMod) 


influencePlot(linearMod)
outlierTest(linearMod)

cutoff <- 4/(nrow(dataPD_WO_64)-length(linearMod$coefficients)-2)
plot(linearMod, which=4, cook.levels=cutoff)
abline(h=cutoff, lty=2, col="red")


hist(dataPD_WO_64$Weaning.Weight)
hist(dataPD_WO_64$Weight)

qqPlot(dataPD_WO_64$Weight)
qqPlot(dataPD_WO_64$Weaning.Weight)
qqPlot(linearMod)


```

This chunk contains linear analyses taking as outcome final weight and Pinkeye as explanatory variable 

```{r , echo=F}


library(readxl)
library(dplyr)
library(knitr)
library(stats)



#result with ANOVA
#av = aov(Weight ~ Bovis, data=dataframe_ABCD) 
#summary(av) 
#av$coefficients


#####general linear regression with interaction
linearMod <- lm(Weaning.Weight ~ PD, data=dataPD)

summary(linearMod) 

influencePlot(linearMod)
outlierTest(linearMod)

cutoff <- 4/(nrow(dataPD)-length(linearMod$coefficients)-2)
plot(linearMod, which=4, cook.levels=cutoff)
abline(h=cutoff, lty=2, col="red")


hist(dataPD$Weaning.Weight)
hist(dataPD$Weight)

qqPlot(dataPD$Weight)
qqPlot(dataPD$Weaning.Weight)
qqPlot(linearMod)

```


This chunk contains linear analyses taking as outcome final weight and Pinkeye as explanatory variable. This analysis excludes animals with notes.   

```{r , echo=F}


library(readxl)
library(dplyr)
library(knitr)
library(stats)



#result with ANOVA
#av = aov(Weight ~ Bovis, data=dataframe_ABCD) 
#summary(av) 
#av$coefficients


#####general linear regression with interaction
linearMod <- lm(Weaning.Weight ~ PD, data=dataPD_WO_NOTES)

summary(linearMod) 


influencePlot(linearMod)
outlierTest(linearMod)

cutoff <- 4/(nrow(dataPD_WO_NOTES)-length(linearMod$coefficients)-2)
plot(linearMod, which=4, cook.levels=cutoff)
abline(h=cutoff, lty=2, col="red")


hist(dataPD_WO_NOTES$Weaning.Weight)
hist(dataPD_WO_NOTES$Weight)

qqPlot(dataPD_WO_NOTES$Weight)
qqPlot(dataPD_WO_NOTES$Weaning.Weight)
qqPlot(linearMod)



```