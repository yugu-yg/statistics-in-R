##Stat131a 
## code for the first lecture

### Exploratory Data Analysis
### dataset from 2014 containing salaries of San Franciso employees

## Set a shortcut name for where my data files are stored
dataDir <- "~/Documents/BerkeleyTeaching/131A/dataforlecture"
nameOfFile<-file.path(dataDir,"SFSalaries2014.csv")
salaries2014<-read.csv(nameOfFile,na.strings="Not Provided")
# dimension:
dim(salaries2014)

# load dplyr, package for data manipulation
library(dplyr)
glimpse(salaries2014)

# alternatively, could use base R function: str() for structure
str(salaries2014)

# column names:
names(salaries2014)
salaries2014[1:10,c("JobTitle","Benefits", "TotalPay","Status")]
## first 10 rows, and these particular columns

## ----fivenumsummary-----------------------------------------------------------
summary(salaries2014$TotalPay)


## ----zeroPay------------------------------------------------------------------
zeroPay<-subset(salaries2014,TotalPay == 0)
str(zeroPay)
nrow(zeroPay)
head(zeroPay)

summary(zeroPay)


## ----summaryWithoutZero-------------------------------------------------------
summary(subset(salaries2014,TotalPay>0))

table(zeroPay$Status)
table(salaries2014$Status)

## ----summaryFTvPT-------------------------------------------------------------
summary(subset(salaries2014,Status=="FT"))
summary(subset(salaries2014,Status=="PT"))

## ----makeFTonly---------------------------------------------------------------
salaries2014_FT <- subset(salaries2014,Status=="FT")

## ----histSalary---------------------------------------------------------------
hist(salaries2014_FT$TotalPay,main="Total Pay", xlab="Pay (in dollars)")
abline(v = mean(salaries2014_FT$TotalPay),lty="dashed")
abline(v = median(salaries2014_FT$TotalPay),lwd=1.5)
legend("topright", legend=c("Median","Mean"),lty=c("solid","dashed"))

## ----histSmallBreaks------------------------------------
par(mfrow=c(2,2))
hist(salaries2014_FT$TotalPay,
     main="Total Pay, default breaks", xlab="Pay (in dollars)")
hist(salaries2014_FT$TotalPay,
     main="Total Pay, breaks=100", xlab="Pay (in dollars)", breaks=100)
hist(salaries2014_FT$TotalPay,
     main="Total Pay, breaks=1000", xlab="Pay (in dollars)",breaks=1000)
hist(salaries2014_FT$TotalPay,
     main="Total Pay, Zoomed-in", xlab="Pay (in dollars)", 
     xlim=c(0,1e5), breaks=1000)

###### LECTURE 2, JAN. 22 ###############

## ----histFirefighterBreaks-----------------------------
salaries2014_FT_FF <- subset(salaries2014_FT, 
                    JobTitle=="Firefighter" & Status=="FT")
dim(salaries2014_FT_FF )

par(mfrow=c(2,2))
hist(salaries2014_FT_FF$TotalPay,
     main="Firefighters, default breaks", xlab="Pay (in dollars)")
hist(salaries2014_FT_FF$TotalPay,
     main="Firefighters, breaks=30", xlab="Pay (in dollars)", breaks=30)
hist(salaries2014_FT_FF$TotalPay,
     main="Firefighters, breaks=100", xlab="Pay (in dollars)", breaks=100)
hist(salaries2014_FT_FF$TotalPay,
     main="Firefighters, breaks=1000", xlab="Pay (in dollars)",breaks=1000)

## Note that these are FREQUENCY histograms, as opposed to DENSITY histograms

## ----boxplot------------------------------------------------------------------
par(mfrow=c(1,1))
boxplot(salaries2014_FT$TotalPay,
        main="Total Pay, breaks=1000", ylab="Pay (in dollars)")

## ----boxplotByJobType_noOutlier-----------------------------------------------
(tabJobType<-table(subset(salaries2014_FT, Status=="FT")$JobTitle))
tabJobType<-sort(tabJobType,decreasing=TRUE)
(topJobs<-head(names(tabJobType),10))
salaries2014_top<-subset(salaries2014_FT, 
                         JobTitle %in% topJobs & Status=="FT")
salaries2014_top<-droplevels(salaries2014_top)
#' Here droplevels is a function that removes the unused levels, so that 
#' the ones that were removed don't hang around as empty categories
dim(salaries2014_top)
par(mar=c(10,4.1,4.1,.1))
boxplot(salaries2014_top$TotalPay ~ salaries2014_top$JobTitle,
main="Total Pay, by job title, 10 most frequent job titles" , xlab="",
ylab="Pay (in dollars)",las=3)

## ----boxplotByJob-------------------------------------------------------------
## A neater way to write it
boxplot(TotalPay~JobTitle, data=salaries2014_top,
main="Total Pay, by job title, 10 most frequent job titles" , xlab="",
ylab="Pay (in dollars)",las=3, outline=FALSE)

## play with the values of las to see how the labels change orientation

## ----shapeExamples-------------------------------------
set.seed(1)
par(mfrow=c(2,2))
hist(rgamma(1000,shape=2),main="Right Skew")
hist(rnorm(1000),main="Symmetric")
breaks=seq(-20,20,1)
hist(rnorm(1000),main="Light tails",xlim=c(-20,20),breaks=breaks,freq=TRUE)
# This distribution creates such extreme values that we will subset to
# only show the range of [-20,20]
x<-rcauchy(1000)
hist(x[abs(x)<=20],main="Heavy tails",xlim=c(-20,20),breaks=breaks,freq=TRUE)

## ----bringInFlightData--------------------------------------------------------
flightSF<-read.table(file.path(dataDir,"SFO.txt"),sep="\t",header=TRUE)
# dimension:
dim(flightSF)
# column names:
names(flightSF)
## ----fivenumsummaryFlight-----------------------------------------------------
summary(flightSF$DepDelay)

naDepDf<-subset(flightSF,is.na(DepDelay))
head(naDepDf[,c("FlightDate","Carrier","FlightNum","DepDelay","Cancelled")])
summary(naDepDf[,c("FlightDate","Carrier","FlightNum","DepDelay","Cancelled")])

### hist of dep delays ---------------------

par(mfrow=c(1,1))
hist(flightSF$DepDelay,main="Departure Delay", xlab="Time (in minutes)")
abline(v=c(mean(flightSF$DepDelay,na.rm=TRUE),
           median(flightSF$DepDelay,na.rm=TRUE)), lty=c("dashed","solid"))
legend("topright", legend=c("Median","Mean"),lty=c("solid","dashed"))

## Notice na.rm - why is it here?
## note that NAs are not plotted. Is that okay? Not to plot them?

flightSF$DepDelayWithCancel<-flightSF$DepDelay
flightSF$DepDelayWithCancel[is.na(flightSF$DepDelay)]<-1200
hist(flightSF$DepDelayWithCancel,xlab="Time (in minutes)", 
     main="Departure delay, with cancellations=1200")

boxplot(flightSF$DepDelay~flightSF$Carrier,
main="Departure Delay, by airline carrier",
ylab="Time (in minutes)")



boxplot(flightSF$DepDelay~flightSF$Carrier,
main="Departure Delay, by airline carrier",
ylab="Time (in minutes)",outline=FALSE)

### should we really remove the outliers?



## ----transformFakeData-----------------------

#' Here I make some simulated data from the "gamma" 
#' distribution in order to create skewed data and 
#' show the effect of the log transformation. 
#' In the case of the gamma distribution, 
#' the ` scale `  and  ` shape `  are parameters of the distribution.
y<-rgamma(10000,scale=1,shape=0.1)

#' Note I first plot the histogram of the original y 
#' and then the histogram of the the log-transformed y. 
#' I use the  ` par(mfrow=c(1,2)) `  to draw them side-by-side as a 1x2 grid

par(mfrow=c(1,2))
hist(y,main="Original data",xlab="original scale",breaks=50)
hist(log(y),main="Log of data",xlab="log-scale",breaks=50)

##---------------
#' I decided to choose how much to add by finding the minimum value, 
#' taking the absolute value, and adding 1. 
addValue<-abs(min(flightSF$DepDelay,na.rm=TRUE))+1
par(mfrow=c(3,1))
boxplot(flightSF$DepDelay+addValue~flightSF$Carrier,
        main="Departure Delay, original", ylab="Time")
boxplot(log(flightSF$DepDelay+addValue)~flightSF$Carrier,
        main="Departure Delay, log transformed", 
        ylab=paste("log(Time+",addValue,")"))
boxplot(sqrt(flightSF$DepDelay+addValue)~flightSF$Carrier,
        main="Departure Delay, sqrt-transformed", 
        ylab=paste("sqrt(Time+",addValue,")"))
##
salaries2014_FT<-subset(salaries2014,Status=="FT")
sum(salaries2014_FT$TotalPay==72000)/nrow(salaries2014_FT)
sum(salaries2014_FT$TotalPay<=72000)/nrow(salaries2014_FT)
median(salaries2014_FT$TotalPay)
sum(salaries2014_FT$TotalPay>200000)/nrow(salaries2014_FT)

par(mfrow=c(1,2))
ylim<-c(0,0.3)
histBinProb(salaries2014_FT$TotalPay,
            main="Total Pay, Probability of Bin", xlab="Income (in dollars)", 
            ylim=ylim)
histBinProb(salaries2014_FT$TotalPay,
            main="Total Pay, Probability of Bin, more breaks", 
            xlab="Income (in dollars)", breaks=10000, ylim=ylim)
h<-hist(salaries2014_FT$TotalPay,plot = FALSE)
str(h)

###--------cond probs--------
sum(salaries2014_FT$TotalPay<=64000)/sum(salaries2014_FT$TotalPay<72000)
sum(salaries2014_FT$TotalPay<=64000)/nrow(salaries2014_FT)

##---------SRS and probs-------------

## ----histBinProbVsDensity, echo=FALSE,fig.width=figWidth*2,out.width=doubleWidth, purl=TRUE----
histBinProb<-function(x,ylab="Bin Probabilities",breaks = "Sturges",...){
  #plot=FALSE: doesn't plot, just returns the calculated values
  h<-hist(x,plot=FALSE,breaks=breaks)
  h$counts=h$counts/sum(h$counts)
  plot(h,ylab=ylab,...)
}


salariesSRS<-sample(x=salaries2014_FT$TotalPay, size= 100, replace=FALSE)
##first plot with breaks of length 10
ylim<-c(0,1)
breaks<-seq(min(salaries2014_FT$TotalPay),max(salaries2014_FT$TotalPay),
            length=10)
xlab="Pay (in dollars)"
histBinProb(salariesSRS,main="SRS of Salaries", xlab=xlab,
            border=NA,breaks=breaks,ylim=ylim,col="red",add=FALSE)
histBinProb(salaries2014_FT$TotalPay,main="Salaries", 
            xlab=xlab,col=NULL,border="black",breaks=breaks,ylim=ylim,lwd=2,add=TRUE)
legend("topright",c("SRS","Truth"),fill=c("red","white"))

### then with more breaks, notice scale, ylim=c(0,0.1)
ylim<-c(0,0.1)
breaks<-seq(min(salaries2014_FT$TotalPay),max(salaries2014_FT$TotalPay),
            length=100)
histBinProb(salariesSRS,main="Salaries, SRS", xlab=xlab,border=NA,breaks=breaks,ylim=ylim,col="red",add=FALSE)
histBinProb(salaries2014_FT$TotalPay,main="Salaries, Full Pop.", 
            xlab=xlab,col=NULL,border="black",breaks=breaks,ylim=ylim,lwd=2,add=TRUE,lwd=3)
legend("topright",c("SRS","Truth"),fill=c("red","white"))

