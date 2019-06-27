setwd("C:/Users/dbda4/Desktop/R pr")
#####################################################

file<-read.csv2("anorexia.csv",header = TRUE)
file

####################################################

library(sqldf)
#diff<-sqldf("select Postwt-Prewt as diff from file")
diffr<-file$Prewt-file$Postwt
new<-cbind(file,diffr)
############################

t.test(new$Postwt,new$Prewt,paired = T)
###########################################

CBT<-sqldf("select * from new where Treat=='CBT'")
CBT

FT<-sqldf("select * from new where Treat=='FT'")
FT


Cont<-sqldf("select * from new where Treat=='Cont'")
Cont
##########################################

av <- aov(new$Prewt~new$Postwt)
anova(av)
###################################

a<-lm(Postwt~Prewt, data=new)
summary(a)


###############################################
###Scatter Plot ###########
library(ggplot2)
ggplot(new,aes(x=Prewt,y=Postwt))+geom_point()


######################################

#############################################Box Plot


library(ggplot2)
qplot(Prewt,Postwt,data =new,geom = c("boxplot") )


#######################bar plot
qplot(Postwt,data =new,binwidth=3,fill=Postwt,colour="red")

###################################################


pnorm(84, mean=72, sd=15.2, lower.tail=FALSE) 

qchisq(.95, df=7)

ppois(16, lambda=12)

#############################################3
#NOrmal Distribution
pnorm(84, mean=72, sd=15.2, lower.tail=FALSE)
#Binomial
dbinom(4, size=12, prob=0.2) 
pbinom(4, size=12, prob=0.2) 
#poission dist
ppois(16, lambda=12)
ppois(16, lambda=12,lower.tail = FALSE)
#Chisqr
qchisq(.95, df=7)

#Selecting random number
runif(10, min=1, max=3) 

#Exponatial 
pexp(2, rate=1/3) 
################################################

m<-read.csv("Melanoma.csv",header = TRUE)
m
head(m)
str(m)

expt<-as.factor(m$Expt)
expt

class(expt)
class(m)
av2 <- aov(m$time ~ m$age)
anova(av2)

#boxplot on exper and speed

qplot(age,time,data =m,geom = c("boxplot") )

str(m)
#########################################################
sex<-as.factor(m$sex)
sex

ucler<-as.factor(m$ulcer)
ucler

Since1993<-sqldf("select * ,1993-year as newyear from m")
Since1993
###############################################


plot(m$age,m$time, type = "l")

scatter.smooth(m$time~m$age)

###########################################

a<-cut(melona3$age,breaks= c(0,25,50,75,100))
a
boxplot(melona3$time~a, col=c("red","green","yellow","blue"))




