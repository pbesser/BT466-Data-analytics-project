library(readxl)
#install.packages('ggplot2')
library(ggplot2)
setwd("/Users/patrickbesser/OneDrive - stevens.edu/School/4/BT466 Data Analytics/Code")
data <- read_excel('ERIMData.xlsx')
#####################################################

data$HHhead = ifelse(data$F_Rel == '1' & data$M_Rel == '0', 'Single F',
                     ifelse(data$F_Rel == '0' & data$M_Rel == '2', 'Single M', 'Couple'))

data$HHwrkHr = data$MWrkHrs + data$FWrkHrs

data$HHedu = data$MEdu + data$FEdu

View(data)

data$Yogurt_Customer = ifelse(data$YogExp != '0','yes','no')

data$Dinner_Customer = ifelse(data$DinExp != '0','yes','no')

data$BinYog <- cut(data$YogExp, breaks=5, labels= c("1", "2", "3", "4", "5"), include.lowest=TRUE, right=FALSE)

#ResType Categorical
data$ResTypeCat <- data$ResType
data$ResTypeCat = ifelse(data$ResType == '1', "Apartment", 
                         ifelse(data$ResType == '2', "Condo",
                                ifelse(data$ResType == '3', "Single Family",
                                       ifelse(data$ResType == '4', "Multiple Family",
                                              ifelse(data$ResType == '5', "Mobile Home",
                                                     ifelse(data$ResType == '6', "Other", "NA"))))))

####################################################
#Yogurt customers (Y/N) by Household Income
ggplot(data, aes( x= HHInc,fill=Yogurt_Customer))+geom_bar()+xlab('Household Income Level')+ylab('Count')+labs(fill='Yogurt Customer')

#Proportion of yogurt customers (Y/N) by household income
ggplot(data, aes( x= HHInc,fill=Yogurt_Customer))+geom_bar(position='fill')+scale_y_continuous(labels=scales::percent)+xlab('Household')+ylab('Proportion')+labs(fill='Yogurt Customer')

#############
#Yogurt customers (Y/N) by household
ggplot(data, aes( x= HHhead,fill=Yogurt_Customer))+geom_bar()+xlab('Household')+ylab('Count')+labs(fill='Yogurt Customer')

#Proportion yogurt customers (Y/N) by household
ggplot(data, aes( x= HHhead,fill=Yogurt_Customer))+geom_bar(position='fill')+scale_y_continuous(labels=scales::percent)+xlab('Household')+ylab('Proportion')+labs(fill='Yogurt Customer')

#Proportion household by yogurt customers (Y/N) 
ggplot(data, aes( x=Yogurt_Customer ,fill=HHhead))+geom_bar(position='fill')+scale_y_continuous(labels=scales::percent)+xlab('Yogurt Customer')+ylab('Proportion')+labs(fill='Household')

#############
#Count (Y/N) Dinner customer by household income 
ggplot(data, aes( x= HHInc,fill=Dinner_Customer))+geom_bar()+xlab('Household Income Level')+ylab('Count')+labs(fill='Dinner Customer')

#Proportion dinner customers (Y/N) by household? Not sure if this makes sense
ggplot(data, aes( x= HHInc,fill=Dinner_Customer))+geom_bar(position='fill')+scale_y_continuous(labels=scales::percent)+xlab('Household')+ylab('Proportion')+labs(fill='Dinner Customer')

#############
# Count Dinner customers (Y/N) by household
ggplot(data, aes( x= HHhead,fill=Dinner_Customer))+geom_bar()+xlab('Household')+ylab('Count')+labs(fill='Dinner Customer')

#Proportion dinner customers (Y/N) by household
ggplot(data, aes( x= HHhead,fill=Dinner_Customer))+geom_bar(position='fill')+scale_y_continuous(labels=scales::percent)+xlab('Household')+ylab('Proportion')+labs(fill='Dinner Customer')

#Proportion dinner customers (Y/N) by household
ggplot(data, aes( x= Dinner_Customer,fill=HHhead))+geom_bar(position='fill')+scale_y_continuous(labels=scales::percent)+xlab('Dinner Customer')+ylab('Proportion')+labs(fill='Household')

#############
# Count Dinner customers (Y/N) by ResTypeCat
ggplot(data, aes( x= ResTypeCat,fill=Dinner_Customer))+geom_bar()+xlab('Household')+ylab('Count')+labs(fill='Dinner Customer')

#Proportion dinner customers (Y/N) by ResTypeCat
ggplot(data, aes( x= ResTypeCat,fill=Dinner_Customer))+geom_bar(position='fill')+scale_y_continuous(labels=scales::percent)+xlab('Household')+ylab('Proportion')+labs(fill='Dinner Customer')

# Count dinner expenditure by ResTypeCat
ggplot(data, aes( x= ResTypeCat,fill=DinExp))+geom_bar()+xlab('Household Type')+ylab('$ Dinner Expenditure')+labs(fill='Dinner Customer')

# Count HHWorkHours by ResTypeCat
#ggplot(data, aes( x= ResTypeCat,fill=HHwrkHr))+geom_bar()+xlab('Household Type')+ylab('Household Work Hours')+labs(fill='Dinner Customer')

#Count dinner expenditure by birth year(male)
ggplot(data, aes( x= MBirth,fill=DinExp))+geom_bar()+xlab('Birth Year (male)')+ylab('$ Dinner Expenditure')+labs(fill='Dinner Customer')+xlim(1890,1967)

#Count dinner expenditure by birth year(female)
ggplot(data, aes( x= FBirth,fill=DinExp))+geom_bar()+xlab('Birth Year (female)')+ylab('$ Dinner Expenditure')+labs(fill='Dinner Customer')+xlim(1890,1967)


#########<Statistisc>###########################################

model = lm(DinExp ~ HHedu+HHwrkHr, data = data)
summary(model)
predict(Model)

model2 = lm(DinExp ~ ResType, data = data)
summary(model2)
predict(Model2, ResType = 1)

predict(model2, data.frame(ResType = 1))
predict(model2, data.frame(ResType = 2))
predict(model2, data.frame(ResType = 3))
predict(model2, data.frame(ResType = 4))
predict(model2, data.frame(ResType = 5))
predict(model2, data.frame(ResType = 6))

model3 = lm(YogExp ~ ResType, data = data)
summary(model3)

predict(model3, data.frame(ResType = 1))
predict(model3, data.frame(ResType = 2))
predict(model3, data.frame(ResType = 3))
predict(model3, data.frame(ResType = 4))
predict(model3, data.frame(ResType = 5))
predict(model3, data.frame(ResType = 6))
