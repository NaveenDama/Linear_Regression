setwd('C:\\games')

df<-read.csv('Milk Production.csv')
head(df)

milk<-lm(df$CurrentMilk~df$Previous)
lm(CurrentMilk~Previous,data=df)

#To Print the ANOVA Table for the Linear Regression model built
summary(milk)

plot(df$CurrentMilk,df$Previous
     ,xlab="Previous",ylab="CurrentMilk")

#Adding the Regression line to the plot
abline(milk)

#Finding the quantile value of 95% i.e the 't' distribution value
alpha=0.05
n=199
p=1
qt(p=1-(alpha/2),df=n-p-1)

#To find the sse of the linear model "Milk"

SSE<-sum((df$CurrentMilk-milk$fitted.values)^2)
SSR<-sum((milk$fitted.values-mean(df$CurrentMilk))^2)
SST<-sum((df$CurrentMilk-mean(df$CurrentMilk))^2)

#Observe the f_statistic value in the ANOVA table It will be same 
f_stat=(SSR/SSE)*(n-1-1)

#To plot the "Standardized Residual Plot"

plot(milk$fitted.values,rstandard(milk),
     main="Residual Plot",
     xlab="Predicted Valu of CurrentMilk",
     ylab="Standardized Residuals")
abline(h=2,lty=2)
abline(h=-2,lty=2)


#Normal Residual plot

plot(milk$fitted.values,rstandard(milk),
     main="Residual Plot",
     xlab="Predicted Value of CurrentMilk",
     ylab="Standardized Residuals")
abline(h=2,lty=2)
abline(h=-2,lty=2)

#to print the row number beside the point we use the identify function i.e identify(x,y)

identify(milk$fitted.values,rstandard(milk))

#To remove the Residual one by one by removing the farthest point one by one

df1=df[c(-3,-11,-16,-26,-18,-13,-135,-45,-190,-138),]
milk_model1<-lm(df1$CurrentMilk~df1$Previous)
summary(milk_model1)

#Residual plot of milk_model1


plot(milk_model1$fitted.values,rstandard(milk_model1),
     main="Residual Plot",
     xlab="Predicted Value of CurrentMilk",
     ylab="Standardized Residuals")
abline(h=2,lty=2)
abline(h=-2,lty=2)

identify(milk_model1$fitted.values,rstandard(milk_model1))

df2=df1[c(-90,-89,-10,-42,-20,-12,-56,-165,-102),]
milk_model2<-lm(df2$CurrentMilk~df2$Previous)
summary(milk_model2)

plot(df2$CurrentMilk,df2$Previous)
abline(milk_model2)
