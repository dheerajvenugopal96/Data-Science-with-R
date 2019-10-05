#################### HEALTHCARE COST ANALYSIS  ################################ 

#Business Objective:
#A nationwide survey of hospital costs conducted by the US Agency 
#for Healthcare consists of hospital records of inpatient samples. 
#The given data is restricted to the city of Wisconsin and relates to patients 
#in the age group 0-17 years. The agency wants to analyze the data to 
#research on healthcare costs and their utilization.

###############################################################################

#Data : Hospital.csv
#Attribute	Description
#Age -Age of the patient discharged
#Female -A binary variable that indicates if the patient is female
#Los -Length of stay in days
#Race -Race of the patient (specified numerically)
#Totchg	Hospital discharge costs
#Aprdrg	All Patient Refined Diagnosis Related Groups

###############################################################################

#Question 1 : People with maximum expenditure age group
#To record the patient statistics, the agency wants to find
#the age category of people who frequent the hospital and has the maximum
#expenditure.

rm(list=ls())

setwd(choose.dir())

data = read.csv("HospitalCosts.csv")

View(data)

head(data)

str(data)

data$FEMALE = as.factor(data$FEMALE)
data$RACE=as.factor(data$RACE)
data$APRDRG=as.factor(data$APRDRG)

data$age_bins <- ifelse((data$AGE < 1), "infant",
                        ifelse(data$AGE < 3, 'toddler',
                               ifelse(data$AGE < 11, 'child',
                                      'adolescent')))

str(data)

hist(data$AGE,breaks = 3,col = "black",border = "yellow")

# From Here we can arrive at the conclusion that children between
#the age group of (0-5) are the most frequent patients in the hospital.
#the next age group with high but comparatively lower hospital visits than (0-5)
#are patients of age group(10-15)



max(tapply(data$TOTCHG,data$AGE,sum))
# the maximum charge is found out to be 678118

which.max(tapply(data$TOTCHG,data$AGE,sum))
#Result:
# 0
#This shows that the maximum cost of 678118 is predominantly for children of
#age 0(new born babies or 0-12months).


###############################################################################



#Question 2:
#In order of severity of the diagnosis and treatments and to find out
#the expensive treatments, the agency wants to find the diagnosis
#related group that has maximum hospitalization and expenditure.

df=aggregate(data$TOTCHG~data$APRDRG,FUN = sum,data=data)
head(df)

max(tapply(data$TOTCHG,data$APRDRG,sum))
#437978

which.max(tapply(data$TOTCHG,data$APRDRG,sum))
#640 

#Result:
#based on this we found that group 640 had the maximum expenditure of
#437978

###############################################################################


#Question 3:
#To make sure that there is no malpractice, the agency needs to analyze if
#the race of the patient is related to the hospitalization costs.


# Hypothesis Testing
# H0:Race had no imapct on cost
# H1:Race had impact on cost
#Here we are trying to see whether cost(numeric) is affected by race(categorical)
#In such a case we use anova for testing hypothesis


colSums(is.na(data))

data=na.omit(data)

colSums(is.na(data))

model = aov(data$TOTCHG~data$RACE,data = data)

summary(model)

alpha = 0.05


#             Df    Sum Sq  Mean Sq F value Pr(>F)
#data$RACE     5 1.859e+07  3718656   0.244  0.943
#Residuals    493 7.524e+09 15260687  
p=0.943

#Result:
# As p>alpha we accept the null hypothesis
# We can say that race had no impact on cost


###############################################################################


#Question 4:
#To properly utilize the costs, the agency has to analyze the severity
#of the hospital costs by age and gender for proper allocation of resources.

#FEMALE - Categorical
#Cost - Continuous
#age_bins - Categorical


#Define Hypothesis
#H0:Age and Gender have no imapct on cost
#H1:Age and Gender have impact on cost
model2 =aov(data$TOTCHG~age_bins+FEMALE,data = data)
summary(model2)

alpha=0.05

#Df    Sum Sq   Mean Sq F value   Pr(>F)    
#age_bins      3 5.337e+08 177907956  12.580 6.15e-08 ***
#FEMALE        1 2.203e+07  22032532   1.558    0.213    
#Residuals   494 6.986e+09  14142420                  

p_age=8.28e-07 # there is impact of age on cost
p_gender=0.208 # there is no imapct of gender on cost

# we have to futher refine the relation using linear regression equation

model_lm = lm(TOTCHG~FEMALE+AGE,data=data)
summary(model_lm)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  2719.45     261.42  10.403  < 2e-16 ***
#  FEMALE1      -744.21     354.67  -2.098 0.036382 *  
#  AGE            86.04      25.53   3.371 0.000808 ***
#Female =1 # patient is female
#        =0 #Patient is male

#Male
TOTCGH_M=2719.45+(86.04*0)+(-744.21*0)
TOTCGH_M

#Female
TOTCGH_F=2719.45+(86.04*0)+(-744.21*1)
TOTCGH_F

#Result:
# we can see that cost from males is higher than that of female
#for a given age group


###############################################################################


#Question 5:
#Since the length of stay is the crucial factor for inpatients, 
#the agency wants to find if the length of stay can be predicted
#from age, gender, and race.


# predicting length of stay LOS(numerical and dependent) and 
# and three independent variables like AGE,RACE,FEMALE

model3 = lm(data$LOS~data$AGE+data$FEMALE+data$RACE,data = data)

summary(model3)



#Result : We can see with very high intercept Significance we can say that these are not
#adequate features for predicting LOS

###############################################################################


#Question 6:
#To perform a complete analysis, the agency wants to find the variable
#that mainly affects the hospital costs.


model4=lm(data$TOTCHG~. -APRDRG - age_bins,data = data)

summary(model4)
#multiple R-squared:  0.4368,	Adjusted R-squared:  0.4276 
#F-statistic:  47.5 on 8 and 490 DF,  p-value: < 2.2e-16

# Result: R-square and f-stats is low
#We can see that AGE , APRDG and LOS have high significance so we
# can create a model using these three


model5=lm(TOTCHG~AGE+LOS+APRDRG,data = data)

summary(model5)

#Multiple R-squared:  0.9644,	Adjusted R-squared:  0.9592 
#F-statistic: 183.7 on 64 and 434 DF,  p-value: < 2.2e-16
#Result : We have impoved our f-statistic and R-square way better than the earlier one 
#so we can use AGE , LOS and APRDRG are best suited to predict the 
#total charge for a patient

