## Version: 
## Last-Updated: Jun 13 2020 (11:35) 
##           By: Thomas Alexander Gerds
##     Update #: 93
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
dataMRPM <- function(){
    options(width=70)
    suppressMessages(library(prodlim))
    suppressMessages(library(survival,verbose=0L))
    suppressMessages(library(SuperLearner,verbose=0L))
    suppressMessages(library(data.table,verbose=0L))
    suppressMessages(library(Publish))
    suppressMessages(library(foreach))
    suppressMessages(library(gridExtra))
    suppressMessages(library(bootstrap))
    suppressMessages(library(plotrix))
    suppressMessages(library(randomForestSRC))
    suppressMessages(library(randomForest))
    suppressMessages(library(ranger))
    suppressMessages(library(neuralnet))
    suppressMessages(library(nnet))
    suppressMessages(library(cowplot))
    suppressMessages(library(gridGraphics))
    suppressMessages(library(party))
    suppressMessages(library(penalized,verbose=0L))
    suppressMessages(library(rms,verbose=0L))
    suppressMessages(library(riskRegression,verbose=0L))
    suppressMessages(library(ggplot2,verbose=0L))
    suppressMessages(library(ggpubr,verbose=0L))
    suppressMessages(library(rpart,verbose=0L))
    suppressMessages(library(lattice,verbose=0L))
    suppressMessages(library(grid,verbose=0L))
    theme_set(theme_pubr())
    # --------------------------------------------------------------------------------------------------
    ## in vitro fertilization study
    # --------------------------------------------------------------------------------------------------
    data(ivf)
    setDT(ivf)
    ivf[,smoking:=factor(smoking,levels=c("No","Yes"),labels=c("No","Yes"))]
    ivf[,OHSS:=factor(OHSS,levels=c("No","Yes"),labels=c("No","Yes"))]
    set.seed(17)
    ivf[,train:=1:NROW(ivf)%in%sample(1:NROW(ivf),replace=FALSE,size=round(0.63*NROW(ivf)))]
    ivf[,train:=factor(train,levels=c("FALSE","TRUE"),labels=c("FALSE","TRUE"))]
    ivftrain <- ivf[train==TRUE]
    ivftest <- ivf[train==FALSE]
    # --------------------------------------------------------------------------------------------------
    # oral cancer study
    # --------------------------------------------------------------------------------------------------
    data(oc)
    setDT(oc)
    oc[,grade:=factor(grade,levels=c("Well","Moderate","Poor"))]
    oc[,gender:=factor(gender,levels=c("Female","Male"),labels=c("Female","Male"))]
    oc[,tobacco:=factor(tobacco,levels=c("","Ever","Never"),labels=c("","Ever","Never"))]
    oc[,deep.invasion:=factor(deep.invasion,levels=c("","No","Yes"),labels=c("","No","Yes"))]
    oc[,site:=factor(site,levels=c("Buccal Mucosa","Floor of Mouth","Hard Palate","Lower Gum","Retromolar Trigone","Tongue","Upper Gum"),labels=c("Buccal Mucosa","Floor of Mouth","Hard Palate","Lower Gum","Retromolar Trigone","Tongue","Upper Gum"))]
    oc[,race:=factor(race,levels=c("Caucasian","NonCauc"),labels=c("Caucasian","NonCauc"))]
    oc[,vascular.invasion:=factor(vascular.invasion,levels=c("","No","Yes"),labels=c("","No","Yes"))]
    oc[,Grade:=factor(Grade,levels=c("Moderate/Well","Poor"),labels=c("Moderate/Well","Poor"))]
    # split into train and test
    set.seed(17)
    oc[,train:=1:NROW(oc)%in%sample(1:NROW(oc),replace=FALSE,size=round(0.63*NROW(oc)))]
    octrain <- oc[train==TRUE]
    octest <- oc[train==FALSE]
    # complete case analysis
    oc.cc <- na.omit(oc)
    octrain.cc <- na.omit(octrain)
    octest.cc <- na.omit(octest)
    # dummy variables
    ocform <- Surv(survtime,survstatus)~grade+age+tumorthickness+gender+tobacco+deep.invasion+site+race+x.posnodes+tumormaxdimension+vascular.invasion
    octest.dummy <- cbind(octest.cc[,.(survtime,survstatus)],model.matrix(ocform,octest.cc)[,-1])
    setnames(octest.dummy,gsub(" ",".",names(octest.dummy)))
    octrain.dummy <- cbind(octrain.cc[,.(survtime,survstatus)],model.matrix(ocform,octrain.cc)[,-1])
    setnames(octrain.dummy,gsub(" ",".",names(octrain.dummy)))
    oc.dummy <- cbind(oc.cc[,.(survtime,survstatus)],model.matrix(ocform,oc.cc)[,-1])
    setnames(oc.dummy,gsub(" ",".",names(oc.dummy)))
    # --------------------------------------------------------------------------------------------------
    # active surveillance prostate cancer study
    # --------------------------------------------------------------------------------------------------
    data(as)
    setDT(as)
    set.seed(17)
    as[,train:=1:NROW(as)%in%sample(1:NROW(as),replace=FALSE,size=round(0.63*NROW(as)))]
    as[,asprog:=factor(asprog,levels=0:2,labels=c("0","progression","death"))]
    setnames(as,"lpsaden","psa")
    astrain <- as[train==TRUE]
    astest <- as[train==FALSE]
    # --------------------------------------------------------------------------------------------------
    ## psa longitudinal form
    # --------------------------------------------------------------------------------------------------
    data(long)
    setDT(long)
    long[,psadate:=as.Date(psadate,format="%Y-%m-%d")]
    psadt <- function(psa.time,psa.val){(log(2)/coef(lm(log(psa.val)~psa.time))[2])/365.25}
    setkey(long,subject,psadate)
    long[,psa.doublingstime:=psadt(psa.time=psadate,psa.val=psa),by=subject]
    # --------------------------------------------------------------------------------------------------
    ## example time to event data
    # --------------------------------------------------------------------------------------------------
    d <- data.table(id=c(1:5),
                    af.date=c("2001-04-25","1995-02-16","2001-09-09","1999-12-20","1997-05-27"),
                    death.date=c(NA,"2011-10-27",NA,"2009-01-02",NA),
                    stroke.date=c("2005-11-16",NA,NA,"2007-09-01","1999-12-18"),
                    lost.date=c(NA,NA,NA,NA,"2008-08-17"))
    # --------------------------------------------------------------------------------------------------
    # output message
    # --------------------------------------------------------------------------------------------------
    assign("ivf",ivf, pos=-1)
    assign("oc",oc, pos=-1)
    assign("as",as, pos=-1)
    assign("long",long, pos=-1)
    assign("d",d, pos=-1)
    message("\nPrepared data for Medical Risk Prediction Models (Gerds & Kattan):

THE DATA PROVIDED HERE ARE NOT THE REAL DATA BUT COMPUTER MODIFIED CLONES

In vitro fertilization study
   training data: ivftrain
 validation data: ivftest

Oral cancer study
   training data: octrain
 validation data: octest

Active surveillance prostate cancer study
  training data: astrain
validation data: astest

PSA measurements
 longitudinal data: long

Example time to event data: d
")
}
######################################################################
### data-preparation.R ends here
