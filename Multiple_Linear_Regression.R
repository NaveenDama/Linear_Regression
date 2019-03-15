setwd('C:\\Users\\Admin\\Downloads')

df<-read.delim('MilK Production.txt')

milk_lmmodel<-lm(df$CurrentMilk~Previous+Fat+Protein+Days+Lactation+I79,data = df)

summary(milk_lmmodel)

#model after dropping fat and lactation
milk_lmmodel1<-lm(df$CurrentMilk~Previous+Protein+Days+I79,data = df)

summary(milk_lmmodel1)
