##################################################################################
########  CHAPTER 5: VISUALIZING MULTIVARIATE DATA
## Lecture 22 October 18, 2021
##################################################################################

library(dplyr)
library(ggplot2)

dataDir<-"~/Documents/BerkeleyTeaching/131A/Stat131A_fromEPrepo/finalDataSets"

scorecard <- read.csv(file.path(dataDir,"college.csv"), 
                      stringsAsFactors = FALSE,
                      na.strings = c("NA","PrivacySuppressed"))

## remove for-profit institutions
scorecard<-scorecard[-which(scorecard$CONTROL==3),]

str(scorecard)
scorecard = mutate(scorecard, CONTROL=factor(CONTROL,levels=c(1,2),
                              labels=c("Public","Private Non-Profit")))

## The default pairs plot requires a *matrix* of *numeric* values, 
##  and some of our values are factors. So we will leave those off 
## (the first 3 columns), as well as a number of other variables
## and some of our values are factors. So we will leave those off 
## (the first 3 columns), as well as a number of other variables

smallScores<-scorecard[,-c(1:3,4,5,6,9,11,14:17,18:22,24:27,31)]
str(smallScores)
pairs(smallScores,pch=19,col=rgb(red=0,blue=1,green=0,alpha=0.4))

#improving the pairs plot
#note that this is just instructive, we will use gpairs to do this automatically
panel.hist <- function(x, ...)
{
  #from help of pairs
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y)
}

pairs(smallScores, lower.panel = panel.smooth, 
      col=c("red","black")[smallScores$CONTROL],diag.panel = panel.hist)


## ----improvePairs2,out.width=doubleWidth--------------------------------------
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  #from help of pairs
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y,use="pairwise.complete.obs"))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
pairs(smallScores, lower.panel = panel.smooth, 
      upper.panel = panel.cor,
      col=c("red","black")[smallScores$CONTROL],
      diag.panel = panel.hist)


## ----gpairsCorr---------------------------------------------------------------
library(gpairs)
suppressWarnings(corrgram(scorecard[,-c(1:3)]))
gpairs(smallScores,lower.pars=list(scatter="loess"),
       upper.pars=list(scatter="loess",conditional = "boxplot"),
       scatter.pars = list( col = c("red","black")[smallScores$CONTROL]))

##----using GGally--------------------------------------------------------
library(GGally)
ggpairs(smallScores)
ggpairs(aes(color=CONTROL, alpha=0.4),
        data=smallScores,switch='y')

##################################################################################
## Lecture 23 October 10, 2021: BEGIN HERE
##################################################################################

####-------------------------------------------
## Categorical data
###--------------------------------------------

## ----wellbeingData,echo=FALSE-------------------------------------------------
wellbeing <- read.csv(file.path(dataDir,"wellbeing.csv"), 
                      stringsAsFactors = TRUE, header = TRUE)
wellbeing$YearFactor<-factor(wellbeing$Gss.year.for.this.respondent)
wellbeing$Decade<-cut(wellbeing$Gss.year.for.this.respondent,
                      breaks=c(1970,1980,1990,2000,2010,2015),
                      labels=c("1970s","1980s","1990s","2000s","2010s"))
glimpse(wellbeing)
levels(wellbeing$General.happiness)
#change order of levels so that make more sense
# if going to do some operation multiple times, 
# write a function to do it.
changeOrderLevels<-function(x,newLevels){
  if(is.factor(x)){
    oldLevels<-levels(x)  
    if(!all(sort(oldLevels)==sort(newLevels))) 
      stop("newLevels is not just a reorder of the old levels")
  }
  else{if(!all(sort(unique(x))==sort(newLevels))) 
    stop("newLevels is not just a ordering of existing values")}
  return(factor(x,levels=newLevels))
}
wellbeing$General.happiness<-changeOrderLevels(wellbeing$General.happiness,
          newLevels=c("Very happy","Pretty happy","Not too happy",
                      "Don't know","Not applicable","No answer"))
wellbeing$Job.or.housework<-changeOrderLevels(wellbeing$Job.or.housework,
              newLevels=c("Very satisfied","Mod. satisfied",
              "A little dissat","Very dissatisfied",
              "Don't know","Not applicable","No answer"))
wellbeing$Satisfaction.with.financial.situation<-changeOrderLevels(wellbeing$Satisfaction.with.financial.situation,
          newLevels=c("Satisfied","More or less","Not at all sat",
                      "Don't know","Not applicable","No answer"))
wellbeing$Is.life.exciting.or.dull<-changeOrderLevels(wellbeing$Is.life.exciting.or.dull,newLevels=c("Exciting","Routine","Dull","Don't know","Not applicable","No answer"))
wellbeing$Happiness.of.marriage<-changeOrderLevels(wellbeing$Happiness.of.marriage,newLevels=c("Very happy","Pretty happy","Not too happy","Don't know","Not applicable","No answer"))

#' Now we will make a dataset that only consists of the 
#' recent responses (since 2000).
wellbeingRecent<-droplevels(wellbeing[wellbeing$Decade %in% c("2000s","2010s"),])
#' Our last step will be to drop any unused levels in our data set 
#' using the `droplevels` command. 
#' This will keep a 0 sized level of any factor from being carried around.
#' 
#' 
### table and barplot------------------------------------
table(wellbeingRecent$General.happiness)
barplot(table(wellbeingRecent$General.happiness))



# ----gpairsCollege,out.width=doubleWidth--------------------------------------
library(gpairs)
 smallScores$CONTROL<-factor(smallScores$CONTROL,levels=c(1,2),
                             labels=c("public","private"))
gpairs(smallScores,lower.pars=list(scatter="loess"),
       upper.pars=list(scatter="loess",conditional = "boxplot"),
       scatter.pars = list( col = c("red","black")[smallScores$CONTROL]))




## ----crossTabPairs------------------------------------------------------------
tabGeneralJob<-with(wellbeingRecent,table(General.happiness,Job.or.housework))
tabGeneralJob


## ----barplot2Var--------------------------------------------------------------
barplot(tabGeneralJob,legend=TRUE)

## ----barplot2VarNamesFixed----------------------------------------------------
colnames(tabGeneralJob)<-paste(colnames(tabGeneralJob),"(Job)")
rownames(tabGeneralJob)<-paste(rownames(tabGeneralJob),"(General)")
barplot(tabGeneralJob,legend=TRUE)
barplot(t(tabGeneralJob),legend=TRUE)


## ----barplot2VarMore----------------------------------------------------------
barplot(tabGeneralJob,beside=TRUE,legend=TRUE,col=palette()[1:6])


## ----propTableJob-------------------------------------------------------------
prop.table(tabGeneralJob,margin=2)
barplot(prop.table(tabGeneralJob,margin=2),beside=TRUE)

table(wellbeingRecent$General.happiness, 
      wellbeingRecent$Job.or.housework)
prop.table(table(wellbeingRecent$General.happiness, 
                 wellbeingRecent$Job.or.housework), margin=2) %>% 
  round(digits = 3)



## ----propTableGeneral---------------------------------------------------------
prop.table(tabGeneralJob,margin=1)%>% 
  round(digits = 3)
barplot(t(prop.table(tabGeneralJob,margin=1)),beside=TRUE,legend=TRUE)

#' note that this depends on whether we condition on the row variable or the 
#' column variable. The numbers we get will be different.
#' We are asking, given that you are "Pretty happy" in your general
#' happiness, what is the chance that you are very satisfied with your job?
#' It is a different question and proportion of people who are pretty happy in 
#' life GIVEN that they are very satisfied with their job.
#' 

## ----contingencyMany,eval=FALSE------------------------------------------------
## #'
## #' We can do a contingency table with more than two categories. 
### However, the output is not easy to read.
## with(wellbeingRecent,table(General.happiness,Job.or.housework,
### Happiness.of.marriage))

### barplots in ggplot
str(tabGeneralJob)
ggplot(wellbeingRecent,aes(x=Job.or.housework,fill=General.happiness))+
  geom_bar()
library(viridis)

library(RColorBrewer)
ggplot(wellbeingRecent,aes(x=Job.or.housework,fill=General.happiness))+
  geom_bar(position = "dodge")+scale_fill_brewer(palette = "YlGn")

#scale_fill_viridis(discrete = TRUE)

## ----contingencyMany,eval=FALSE------------------------------------------------
## #'
## #' We can do a contingency table with more than two categories. 
## However, the output is not easy to read.
with(wellbeingRecent,table(General.happiness,Job.or.housework,
                           Happiness.of.marriage))

## ----aggregatePairs-----------------------------------------------------------
wellbeingRecent$Freq<-1
#' this creates a variable Freq so that each row should count as 1
#' when we sum
#' aggregate will create a data frame where each row 
#' is a cross-tabulation
wellbeingAggregates<-aggregate(Freq~ General.happiness+Job.or.housework ,
                               data=wellbeingRecent[,-2],FUN=sum)
head(wellbeingAggregates,10)

## ----aggregateMany------------------------------------------------------------
wellbeingAggregatesBig<-aggregate(Freq~ 
          General.happiness+Job.or.housework +Satisfaction.with.financial.situation+
            Happiness.of.marriage+Is.life.exciting.or.dull,
          data=wellbeingRecent[,-2],FUN=sum)
str(wellbeingAggregatesBig)
head(wellbeingAggregatesBig,5)

###--------alluvial plot----
library(alluvial)
alluvial( wellbeingAggregates[,c("General.happiness","Job.or.housework")], 
          freq=wellbeingAggregates$Freq, 
          col=palette()[wellbeingAggregates$General.happiness])

alluvial( wellbeingAggregatesBig[,-ncol(wellbeingAggregatesBig)], 
          freq=wellbeingAggregatesBig$Freq,
           col=palette()[wellbeingAggregatesBig$General.happiness])
          
## ----removeNA-----------------------------------------------------------------
#remove those that not applicable in all
wh<-with(wellbeingRecent,
         which(General.happiness=="Not applicable" | Job.or.housework =="Not applicable" | Satisfaction.with.financial.situation=="Not applicable"))# & Happiness.of.marriage=="Not applicable" & Is.life.exciting.or.dull=="Not applicable"))
wellbeingCondenseGroups<-wellbeingRecent[-wh,]

#' keep only those not 'No answer' or 'Don't know' 
#' in all of the variables of interest
wellbeingCondenseGroups<-subset(wellbeingCondenseGroups,
                                !General.happiness%in%c("No answer","Don't know") & !Job.or.housework %in%c("No answer","Don't know") &  !Satisfaction.with.financial.situation%in%c("No answer","Don't know")  & !Happiness.of.marriage%in%c("No answer","Don't know") & !Is.life.exciting.or.dull%in%c("No answer","Don't know") )

wellbeingCondenseGroups<-droplevels(wellbeingCondenseGroups)
wellbeingCondenseAggregates<-aggregate(Freq~ General.happiness+Job.or.housework +Satisfaction.with.financial.situation+Happiness.of.marriage+Is.life.exciting.or.dull,data=wellbeingCondenseGroups,FUN=sum)

alluvial( wellbeingCondenseAggregates[,-ncol(wellbeingCondenseAggregates)], 
          freq=wellbeingCondenseAggregates$Freq,
          hide = wellbeingCondenseAggregates$Freq < quantile(wellbeingCondenseAggregates$Freq, .50),
         col=palette()[wellbeingCondenseAggregates$General.happiness])

## ----onlyMarried--------------------------------------------------------------
wh<-with(wellbeingCondenseGroups,which(Marital.status=="Married" & Labor.force.status %in% c("Working fulltime","Working parttime","Keeping house")))
wellbeingMarried<-wellbeingCondenseGroups[wh,]
wellbeingMarried<-droplevels(wellbeingMarried)
wellbeingMarriedAggregates<-aggregate(Freq~ General.happiness+Job.or.housework +Satisfaction.with.financial.situation+Happiness.of.marriage+Is.life.exciting.or.dull,data=wellbeingMarried,FUN=sum)

## ----onlyMarriedAlluvial------------------------------------------------------
alluvial( wellbeingMarriedAggregates[,-ncol(wellbeingMarriedAggregates)], freq=wellbeingMarriedAggregates$Freq,hide = wellbeingMarriedAggregates$Freq < quantile(wellbeingMarriedAggregates$Freq, .50),
          col=palette()[wellbeingMarriedAggregates$General.happiness])

## ----titanicExample----------------------------------------------------------
data(Titanic)
tit<-as.data.frame(Titanic)
str(tit)
alluvial( tit[,1:4], freq=tit$Freq, border=NA,
          col=ifelse( tit$Survived == "No", "red", "gray") )

alluvial( tit[,c(1,4,2,3)], freq=tit$Freq, border=NA,
          col=ifelse( tit$Survived == "No", "red", "gray") )





## ----mosaic2var,out.width=doubleWidth-----------------------------------------
mosaicplot(~General.happiness+Job.or.housework,data=wellbeingMarried,
           las=1,col=palette())


## ----mosaic3var,out.width=doubleWidth-----------------------------------------
mosaicplot(~General.happiness+Job.or.housework+
             Satisfaction.with.financial.situation,
           data=wellbeingMarried,las=1,col=palette())


## ----bringInFlightData,echo=FALSE---------------------------------------------
flightSFOSRS<-read.table(file.path(dataDir,"SFO_SRS.txt"),
                         sep="\t",header=TRUE, stringsAsFactors = TRUE)
flightSFOSRS$DayOfWeek<-factor(flightSFOSRS$DayOfWeek,levels=c(1:7),
                               labels=c("M","Tu","W","Th","F","Sa","Su"))
flightSFOSRS$Cancelled<-factor(flightSFOSRS$Cancelled,levels=0:1,labels=c("No","Yes"))
flightSFOSRS$Diverted<-factor(flightSFOSRS$Diverted,levels=0:1,labels=c("No","Yes"))
flightSFOSRS$CancellationCode<-factor(flightSFOSRS$CancellationCode,levels=c("A","B","C","D",""),labels=c("Carrier","Weather","NationalAirSystem","Security","NotCancelled"))
whDelayed<-which(!is.na(flightSFOSRS$DepDelay) & flightSFOSRS$DepDelay>15)
flightSFOSRS$DelayCause<-"NoDelay"
flightSFOSRS$DelayCause[flightSFOSRS$Cancelled==1]<-NA
delayVars<-c("CarrierDelay","WeatherDelay","NASDelay","SecurityDelay" ,
             "LateAircraftDelay")
delayCause<-apply(flightSFOSRS[whDelayed,delayVars]>0,1,function(x){
  if(all(is.na(x))) return(NA) 
  else{
    if(any(x)){
      if(sum(x)==1) return(gsub("Delay","",delayVars[x]))
      else return("Multiple")
    }
    else "NoDelay"
  }
})
flightSFOSRS$DelayCause[whDelayed]<-delayCause
flightSFOSRS$DelayCause<-factor(flightSFOSRS$DelayCause)

## ----pairsDelayed,out.width=doubleWidth---------------------------------------
gpairs(droplevels(flightSFOSRS[whDelayed,
          c("AirTime","DepDelay","DayOfWeek","DelayCause")]),
       upper.pars=list(conditional = "boxplot"))


#-----------------basic heat maps----------
## from https://www.r-graph-gallery.com/215-the-heatmap-function.html
str(mtcars)
View(mtcars)
#' note that rows are the observations (each car type + model)
#' and cols are the variables of interest
#' also notice that disp and hp are big numbers compared to other variables

data <- as.matrix(mtcars)
# Default Heatmap
heatmap(data)
## it shows the dendrograms on the side
library(pheatmap)
#' since it is the columns of hp and disp causing the issue, we can 
#' normalize the columns and scale them
heatmap(data, scale="column")
heatmap(data,scale = "column", Rowv = NA,Colv = NA)
pheatmap(data, scale = "column")
pheatmap(data, scale = "column",treeheight_row = 0,treeheight_col = 0)


##---breast cancer data-----

breast<-read.csv(file.path(dataDir,"highVarBreast.csv"),stringsAsFactors=TRUE)
str(breast)


## ----corBreastNoCluster,out.width=doubleWidth,message=FALSE, warning=FALSE----

corMat<-cor(breast[,-c(1:7)]) ### removing the factor variables
pheatmap(corMat,cluster_rows = FALSE, cluster_cols = FALSE )



## ----corBreastClustered,out.width=doubleWidth---------------------------------
pheatmap(corMat,cluster_rows = TRUE, cluster_cols = TRUE, 
         treeheight_row =0, treeheight_col = 0)




## ----breastHeatmap,out.width=doubleWidth--------------------------------------
pheatmap(breast[,-c(1:7)],cluster_rows = TRUE, 
         cluster_cols = TRUE, treeheight_row =0, treeheight_col = 0)

## ----breastHeatmapFixedUp,out.width=doubleWidth-------------------------------
typeCol<-c("red","black","yellow")
names(typeCol)<-levels(breast$TypeSample)
estCol<-palette()[c(4,3,5)] ##designates a list of 3 particular colors
names(estCol)<-levels(breast$EstReceptor)
proCol<-palette()[5:7]
names(proCol)<-levels(breast$Progesteron)
## now get the 1st and 99th percentiles for the data
(qnt<-quantile(as.numeric(data.matrix((breast[,-c(1:7)]))),c(0.01,.99)))
brks<-seq(qnt[1],qnt[2],length=20)
head(brks)
seqPal5<-colorRampPalette(c("black","navyblue",
            "mediumblue","dodgerblue3","aquamarine4","green4","yellowgreen",
            "yellow"))(length(brks)-1)
row.names(breast)<-c(1:nrow(breast))
fullHeat<-pheatmap(breast[,-c(1:7)],cluster_rows = TRUE, cluster_cols = TRUE, 
                   treeheight_row =0, treeheight_col = 0,
                   color=seqPal5,
                   breaks=brks,
                   annotation_row=breast[,5:7],
                   annotation_colors = list("TypeSample"=typeCol,
                            "EstReceptor"=estCol,"Progesteron"=proCol))

## ----breastHeatmapOnlyCancer,out.width=doubleWidth----------------------------
whCancer<-which(breast$Type!="Normal")
pheatmap(breast[whCancer,-c(1:7)],cluster_rows = TRUE, cluster_cols = TRUE, 
         treeheight_row =0, treeheight_col = 0,
         color=seqPal5,
         breaks=brks,
         annotation_row=breast[whCancer,5:7],
         annotation_colors = list("TypeSample"=typeCol,
                                  "EstReceptor"=estCol,"Progesteron"=proCol))


## ----breastHeatmapCentered,out.width=doubleWidth------------------------------
breastCenteredMean<-scale(breast[,-c(1:7)],center=TRUE,scale=FALSE)
colMedian<-apply(breast[,-c(1:7)],2,median)
## subtract its col median from every col value, using sweep()
breastCenteredMed<-sweep(breast[,-c(1:7)],MARGIN=2, colMedian,"-") 
qnt<-max(abs(quantile(as.numeric(data.matrix((breastCenteredMed[,-c(1:7)]))),
                      c(0.01,.99))))
brksCentered<-seq(-qnt,qnt,length=50)
seqPal2<- colorRampPalette(c("orange","black","blue"))(length(brksCentered)-1)
seqPal2<-(c("yellow","gold2",seqPal2))
seqPal2<-rev(seqPal2)
pheatmap(breastCenteredMed[whCancer,-c(1:7)],cluster_rows = TRUE, cluster_cols = TRUE, 
         treeheight_row =0, treeheight_col = 0,
         color=seqPal2,
         breaks=brksCentered,
         annotation_row=breast[whCancer,6:7],
         annotation_colors = list("TypeSample"=typeCol,
                                  "EstReceptor"=estCol,"Progesteron"=proCol))



## ----breastHeatmapWithTree,out.width=doubleWidth------------------------------
smallBreast<-read.csv(file.path(dataDir,"smallVarBreast.csv"),
                      header=TRUE,stringsAsFactors=TRUE)
View(smallBreast)
row.names(smallBreast)<-1:nrow(smallBreast)
pheatmap(smallBreast[,-c(1:7)],cluster_rows = TRUE, 
         cluster_cols = FALSE,  treeheight_col = 0,
         breaks=brks,col=seqPal5)

pheatmap(smallBreast[,-c(1:7)],cluster_rows = TRUE, 
         cluster_cols = TRUE,  treeheight_col = 0,
         breaks=brks,col=seqPal5)

pheatmap(smallBreast[,-c(1:7)],cluster_rows = TRUE, cluster_cols = TRUE, 
         breaks=brks,col=seqPal5,
         annotation_row=smallBreast[,5:7],
         annotation_colors=list("TypeSample"=typeCol,
                                "EstReceptor"=estCol,"Progesteron"=proCol))








