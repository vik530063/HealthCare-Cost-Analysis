setwd(choose.dir())
getwd()

hosp <- read.csv("HospitalCosts.csv", stringsAsFactors = TRUE)

#hosp$AGE <- as.factor(hosp$AGE)
str(hosp)

## Analysis 1:
hist(hosp$AGE, main = "Frequency of Visits by Patients", xlab = "Age", col = "blue")

library(dplyr)
hosp%>%dplyr::count(AGE,sort = TRUE)


temp <- aggregate(hosp$TOTCHG ~ hosp$AGE, FUN = sum, na.action = na.omit)
colnames(temp) <- c("Age","Expenditure")
temp
temp[temp$Expenditure==max(temp$Expenditure),]


## Analysis 2:
hist(hosp$APRDRG, main = "Frequency of Diagnosis Related Groups", xlab = "Categories", col = "blue")


hosp$APRDRG <- sapply(hosp$APRDRG, factor)
summary(hosp$APRDRG)
which.max(summary(hosp$APRDRG))


temp2 <- aggregate(hosp$TOTCHG ~ hosp$APRDRG, FUN = sum, na.action = na.omit)
colnames(temp2) <- c("Category","Expenditure")
temp2
temp2[temp2$Expenditure==max(temp2$Expenditure),]



## Analysis 3:
hosp <- na.omit(hosp)   #Removing NA values for ANOVA to work (Because received: 1 observation deleted due to missing-ness)
hosp$RACE <- as.factor(hosp$RACE)
anova <- aov(hosp$TOTCHG ~ hosp$RACE)
anova
summary(anova)
summary(hosp$RACE)


## Analysis 4:
lr_model <- lm(formula = TOTCHG ~ FEMALE + AGE, data=hosp)
lr_model
summary(lr_model)
hosp$FEMALE <- as.factor(hosp$FEMALE)
summary(hosp$FEMALE)



## Analysis 5:
lr_model2 <- lm(formula = LOS ~ AGE + FEMALE + RACE, data = hosp)
summary(lr_model2)


## Analysis 6:
lr_model3 <- lm(formula = TOTCHG ~ ., data=hosp)
summary(lr_model3)
