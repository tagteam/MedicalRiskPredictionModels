## Version: 
## Last-Updated: Jul  7 2020 (20:05) 
##           By: Thomas Alexander Gerds
##     Update #: 134
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
##' Loads libraries and data sets to illustrate the R-code of
##' the book Medical Risk Prediction Models by Gerds & Kattan
##'
##' These are synthetic data generated with the computer.
##' @title load data for Medical Risk Prediction Models
##' @return modified global environment with data and libraries loaded
##' @examples
##' prepareExamples()
##' @export 
##' @author Thomas A. Gerds <tag@@biostat.ku.dk>
prepareExamples <- function(){
    options(width=70)
    library(prodlim)
    library(survival)
    library(SuperLearner)
    library(data.table)
    library(Publish)
    library(foreach)
    library(gridExtra)
    library(bootstrap)
    library(plotrix)
    library(randomForestSRC)
    library(randomForest)
    library(ranger)
    library(neuralnet)
    library(nnet)
    library(cowplot)
    library(gridGraphics)
    library(party)
    library(penalized)
    library(rms)
    library(riskRegression)
    library(ggplot2)
    library(ggpubr)
    library(rpart)
    library(lattice)
    library(grid)
    theme_set(theme_pubr())
    # --------------------------------------------------------------------------------------------------
    ## in vitro fertilization study
    # --------------------------------------------------------------------------------------------------
    data(ivf)
    ivf$smoking  <- factor(ivf$smoking,levels=c("No","Yes"),labels=c("No","Yes"))
    ivf$OHSS <- factor(ivf$OHSS,levels=c("No","Yes"),labels=c("No","Yes"))
    set.seed(17)
    ivf$train  <- 1:NROW(ivf)%in%sample(1:NROW(ivf),replace=FALSE,size=round(0.63*NROW(ivf)))
    ivftrain  <- factor(ivf$train,levels=c("FALSE","TRUE"),labels=c("FALSE","TRUE"))
    ivftrain <- ivf[ivf$train==TRUE,]
    ivftest <- ivf[ivf$train==FALSE,]
    # --------------------------------------------------------------------------------------------------
    # oral cancer study
    # --------------------------------------------------------------------------------------------------
    data(oc)
    oc$grade=factor(oc$grade,levels=c("Well","Moderate","Poor"))
    oc$gender=factor(oc$gender,levels=c("Female","Male"),labels=c("Female","Male"))
    oc$tobacco=factor(oc$tobacco,levels=c("Ever","Never"),labels=c("Ever","Never"))
    oc$deep.invasion=factor(oc$deep.invasion,levels=c("No","Yes"),labels=c("No","Yes"))
    oc$site=factor(oc$site,levels=c("Buccal Mucosa","Floor of Mouth","Hard Palate","Lower Gum","Retromolar Trigone","Tongue","Upper Gum"),labels=c("Buccal Mucosa","Floor of Mouth","Hard Palate","Lower Gum","Retromolar Trigone","Tongue","Upper Gum"))
    oc$race=factor(oc$race,levels=c("Caucasian","NonCauc"),labels=c("Caucasian","NonCauc"))
    oc$vascular.invasion=factor(oc$vascular.invasion,levels=c("No","Yes"),labels=c("No","Yes"))
    oc$Grade=factor(oc$Grade,levels=c("Moderate/Well","Poor"),labels=c("Moderate/Well","Poor"))
    # split into train and test
    set.seed(17)
    oc$train=1:NROW(oc)%in%sample(1:NROW(oc),replace=FALSE,size=round(0.63*NROW(oc)))
    octrain <- oc[oc$train==TRUE,]
    octest <- oc[oc$train==FALSE,]
    # complete case analysis
    oc.cc <- na.omit(oc)
    octrain.cc <- na.omit(octrain)
    octest.cc <- na.omit(octest)
    # dummy variables
    ocform <- Surv(survtime,survstatus)~grade+age+tumorthickness+gender+tobacco+deep.invasion+site+race+x.posnodes+tumormaxdimension+vascular.invasion
    octest.dummy <- cbind(octest.cc[,c("survtime","survstatus")],model.matrix(ocform,octest.cc)[,-1])
    setnames(octest.dummy,gsub(" ",".",names(octest.dummy)))
    octrain.dummy <- cbind(octrain.cc[,c("survtime","survstatus")],model.matrix(ocform,octrain.cc)[,-1])
    setnames(octrain.dummy,gsub(" ",".",names(octrain.dummy)))
    oc.dummy <- cbind(oc.cc[,c("survtime","survstatus")],model.matrix(ocform,oc.cc)[,-1])
    setnames(oc.dummy,gsub(" ",".",names(oc.dummy)))
    # --------------------------------------------------------------------------------------------------
    # active surveillance prostate cancer study
    # --------------------------------------------------------------------------------------------------
    data(as)
    set.seed(17)
    as$train <- 1:NROW(as)%in%sample(1:NROW(as),replace=FALSE,size=round(0.63*NROW(as)))
    as$asprog <- factor(as$asprog,levels=0:2,labels=c("0","progression","death"))
    as$psa <- as$lpsaden
    as$lpsaden <- NULL
    astrain <- as[as$train==TRUE,]
    astest <- as[as$train==FALSE,]
    # --------------------------------------------------------------------------------------------------
    ## psa longitudinal form
    # --------------------------------------------------------------------------------------------------
    data(long)
    long$psadate <- as.Date(long$psadate,format="%Y-%m-%d")
    psadt <- function(psa.time,psa.val){(log(2)/coef(lm(log(psa.val)~psa.time))[2])/365.25}
    ## setkey(long,subject,psadate)
    ## long[,psa.doublingstime:=psadt(psa.time=psadate,psa.val=psa),by=subject]
    # --------------------------------------------------------------------------------------------------
    ## example time to event data
    # --------------------------------------------------------------------------------------------------
    data(ttedata)
    d <- ttedata
    ## d <- data.frame(id=c(1:5),
    ## af.date=c("2001-04-25","1995-02-16","2001-09-09","1999-12-20","1997-05-27"),
    ## death.date=c(NA,"2011-10-27",NA,"2009-01-02",NA),
    ## stroke.date=c("2005-11-16",NA,NA,"2007-09-01","1999-12-18"),
    ## lost.date=c(NA,NA,NA,NA,"2008-08-17"))
    ## setDT(d)
    # --------------------------------------------------------------------------------------------------
    # output message
    # --------------------------------------------------------------------------------------------------
    assign("psadt",psadt, pos=.GlobalEnv)
    assign("ivf",ivf, pos=.GlobalEnv)
    assign("ivftrain",ivftrain, pos=.GlobalEnv)
    assign("ivftest",ivftest, pos=.GlobalEnv)
    assign("oc",oc, pos=.GlobalEnv)
    assign("oc.cc",oc.cc, pos=.GlobalEnv)
    assign("octrain",octrain, pos=.GlobalEnv)
    assign("octest",octest, pos=.GlobalEnv)
    assign("octrain.cc",octrain.cc, pos=.GlobalEnv)
    assign("octest.cc",octest.cc, pos=.GlobalEnv)
    assign("octrain.dummy",octrain.dummy, pos=.GlobalEnv)
    assign("octest.dummy",octest.dummy, pos=.GlobalEnv)
    assign("as",as, pos=.GlobalEnv)
    assign("astrain",astrain, pos=.GlobalEnv)
    assign("astest",astrain, pos=.GlobalEnv)
    assign("long",long, pos=.GlobalEnv)
    assign("d",d, pos=.GlobalEnv)
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
### prepareExamples.R ends here
