hotel = read.table("hotel2022.dat",header= TRUE)
head(hotel)

#a Produce a plot and a correlation matrix of the data. 
#Comment on possible relationships between the response and predictors 
#and relationships between the predictors themselves.
pairs(hotel, panel = panel.smooth)
cor(hotel)
#Energy consumption is highly correlated with area. There is also a moderate correlation 
#between energy and rooms. High correlation between predictors area and rooms but not
#with other predictors.

#b
#• Fit a model using all the predictors to explain the energy response. (don't need to check assumptions)
#• Using the full model, estimate the impact each hectare of hotel area has 
#on the energy efficiency of the hotel.
#Do this by producing a 95% confidence interval that quantifies the change 
#in energy consumption for each extra hectare of hotel area and comment.

#(1)
hotel.lm = lm(energy ~ ., data = hotel)
summary(hotel.lm)
#par(mfrow = c(1, 2))   no need 
#plot(hotel.lm, which = 1:2)


#(2)
summary(hotel.lm)$coefficients
#b(area)± t(n-p,1-alpha/2) * s.e(area)
alpha = 0.05
quantile = qt(p=1-alpha/2, df =16-5); quantile
b_area = 2331.116239
se_area=250.918960
upperbound = b_area+quantile*se_area; upperbound
lowerbound = b_area-quantile*se_area; lowerbound
#For every hectare changes in hotel area, we are 95% confident that we would
#expect a increase in energy consumption between 1778.847 and 2883.385 unit

#c  Conduct an F-test for the overall regression
#i.e. is there any relationship between the response and the predictors.

summary(lm(energy ~., data = hotel))$coefficients


#Mathematical multiple regression
#Energy = -3197.3+2331.12area + 2.37age -5.38rooms + 3234.55occupancy
#Hypothese: H0: βarea =βage =βrooms =βoccupancy = 0
#H1: not all βi parameters are zero

#ANOVA table
anova(hotel.lm)
#Table in ipad


#Compute the F statistic  (week8 lecture slides 25)
FullRegSS = 232665641+556602+1293045+1625643; FullRegSS
RegMS = FullRegSS/4; RegMS
F_statistic = RegMS/1332656; F_statistic

#can double check the F-value using summary(hotel.lm), they are the same 


# State the Null distribution for the test statistic.
#Answer:df1=5-1=4, df2 = 16-5 = 11
#so the null distribution is a t4,11-distribution.

#Compute the P-Value
g=5
N=16
1-pf(F_statistic,df1=g-1,df2=N-g) #1.01904e-06

#State your conclusion (both statistical conclusion and contextual conclusion).
#Answer: The p-value of null distribution is greater or equal to F statistic is 
# 1.01904e-06, which is less than 0.05. We can reject the null hypothesis
#at the 5% significant level, therefore,there is a significant linear relationship between 
#energy response and at least one of the three predictor variables.


#d Validate the full model and comment on whether the full regression model
#is appropriate to explain the energy efficiency of the hotels.
par(mfrow = c(1,2))
qqnorm(hotel.lm$residuals,main = "Normal Q-Q plot of residuals")
qqline(hotel.lm$residuals)
plot(hotel.lm, which = 1)

#The Normal Q-Q plot of residuals looks close to linear,
#which means the errors close to normal distributed.
#The Residuals vs Fitted plot shows slight curvature, but it could still satisify 
#the assumption



#e:Find the R^2 and comment on what it means in the context of this dataset.
#Answer: R^2 is 0.9416. This means that 94.16% of the variability observed
#in the target variables area,age,rooms and occupancy are explained by the 
#regression model, which is Energy = -3197.3+2331.12area + 2.37age -5.38rooms + 3234.55occupancy

#f: Using model selection procedures discussed in the course, 
#find the best multiple regression model that explains the data. 
#State the final fitted regression model.

#Answer:

#first,regress with all the predictor variables in the model
hotel.lm = lm(energy ~ ., data = hotel)
summary(hotel.lm)

#Variable age, rooms and occupancy are insignificant conditional on the other predictors
#age has the largest P-Value,0.977. So we should drop age.

#Reduced model after dropping age
hotel.2 = lm(energy ~ area+rooms+occupancy, data = hotel)
summary(hotel.2)
#Variable rooms and occupancy have insignificant P-value. 
#Occupancy has the largest P-value 0.2265, so we should drop occupancy

#Reduced model after dropping occupancy
hotel.3 = lm(energy ~ area+rooms, data = hotel)
summary(hotel.3)

#Variable rooms has insignicant P-value, we drop it
hotel.4= lm(energy ~ area, data = hotel)
summary(hotel.4)
#The only factor variable is significant.
#Validate the final model
par(mfrow = c(1, 2))
plot(hotel.4, which = 1:2)
#The Normal Q-Q plot of residuals shows some variation away from the linear model,
#but it still looks close to linear,
#which means the errors close to normal distributed.
#The Residuals vs Fitted plot still shows slight curvature, since the sample size 
#is small,so it could still satisify the assumption.

#g Comment on the R2 and adjusted R2 in the full and final model you chose in part f.
#In particular explain why those goodness of fitness measures change but not in the same way.
#R^2 = 0.9277, Adjusted R-squared:0.9225

#Full model: 
#R2:0.9416 , Adj R2: 0.920

#Final model
#R2:0.9277,  Adj R2:0.922

#The R2 will increase by adding another predictor into the model. 
#Since three predictors have been dropped from the full model, the final model would
#have smaller R2. 
#Adjusted R2 is used to balance R2 with number of predictor.
#The value of Adjusted R2 decreases as the number of predictors increases 
#also while considering R Squared acting a penalization factor for a insignificant variable 
# and rewarding factor for a significant variable.Hence, adjusted R2 of full model 
#and final model almost the same.


bone = read.table("bone-growth.dat", header = TRUE)
str(bone)

mean(movie$Score)




