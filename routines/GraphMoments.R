.libPaths("./R/")
#install.packages('tidyverse',repos = "http://cran.us.r-project.org")
#install.packages('utils',repos = "http://cran.us.r-project.org")
#installed.packages()
library(utils)

setwd("C:/Users/wb452275/Documents/gender-pension/GABest1000_10_15/outputs")

#Read in directory
#Read in moment definitions
#Read in data and simulated moments
x <- read.table("./criterion.txt", row.names= NULL, header = FALSE,sep =",", skip = 1)
#x$weights.i.[1:5]
names(x)<-c("i","m","mdata","weights","loss","nobs")
attach(x)
x[1:5,1]
x[1:5,2]
x[1:5,3]
x[1:5,4]
x[1:5,5]
x[1:5,6]
x[1:5,7]
#x$weights.i.[1:5]
#names(x)
#x$contribution<-weights.i.*loss.1.i.*loss.1.i.

x[1:5,1:5]
x$contribution<-weights*loss*loss
x$contribution<--100*x$contribution/sum(x$contribution)
x$contribution[1:20]
colours<-c("red","blue","black")
myvars<-c("m","mdata","contribution")
y<-x[myvars]

layout(matrix(c(1,1,1,2,2),1,5,byrow = TRUE))
#wealth moments (1:36)
	barplot(t(as.matrix(y))[,1:36],cex.main=1,main="Assets",beside=TRUE, col=colours)
#Transitions (96:113)
	barplot(t(as.matrix(y))[,96:113],cex.main=1,main="Transitions",beside=TRUE, col=colours)

layout(matrix(c(1,1,1,2,2),1,5,byrow = TRUE))
#LFP by age (37:66)
	barplot(t(as.matrix(y))[,37:66],cex.main=1,main="LFP by age",beside=TRUE, col=colours)
#Formal LFP by age (69:91)
	barplot(t(as.matrix(y))[,69:91],cex.main=1,main="Formal LFP by age",beside=TRUE, col=colours)

layout(matrix(c(1,1,1,1,1),1,5,byrow = TRUE))
#Earnings distributions (114:161)
	barplot(t(as.matrix(y))[,114:161],cex.main=1,main="Earnings distribution",beside=TRUE, col=colours)

layout(matrix(c(1,2,3,4,5,6,7,8),2,4,byrow = TRUE))
#Earnings [190:258]
	barplot(t(as.matrix(y))[,190:197],cex.main=1,main="Formal Earnings by schooling, men",beside=TRUE, col=colours)
	barplot(t(as.matrix(y))[,198:205],cex.main=1,main="Informal Earnings by schooling, men",beside=TRUE, col=colours)
	barplot(t(as.matrix(y))[,206:213],cex.main=1,main="Formal Earnings by schooling, women",beside=TRUE, col=colours)
	barplot(t(as.matrix(y))[,214:221],cex.main=1,main="Informal Earnings by schooling, women",beside=TRUE, col=colours)
	barplot(t(as.matrix(y))[,222:229],cex.main=1,main="Formal Earnings by XP, men",beside=TRUE, col=colours)
	barplot(t(as.matrix(y))[,230:237],cex.main=1,main="Informal Earnings by XP, men",beside=TRUE, col=colours)
	barplot(t(as.matrix(y))[,238:245],cex.main=1,main="Formal Earnings by XP, women",beside=TRUE, col=colours)
	barplot(t(as.matrix(y))[,246:253],cex.main=1,main="Informal Earnings by XP, women",beside=TRUE, col=colours)
	barplot(t(as.matrix(y))[,254:256],cex.main=1,main="Formal Earnings by cohort",beside=TRUE, col=colours)
	barplot(t(as.matrix(y))[,257:258],cex.main=1,main="Informal Earnings by cohort",beside=TRUE, col=colours)

layout(matrix(c(1,1,1,2,3),1,5,byrow = TRUE))
#Earnings transitions [162:189]
	barplot(t(as.matrix(y))[,162:189],cex.main=1,main="Earnings transitions",beside=TRUE, col=colours)
#Part-time (67:68)
	barplot(t(as.matrix(y))[,66:68],cex.main=1,main="Part-time female LFP",beside=TRUE, col=colours)
#Part-time (92:95)
	barplot(t(as.matrix(y))[,92:95],cex.main=1,main="Household LFP",beside=TRUE, col=colours)
