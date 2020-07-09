library(MedicalRiskPredictionModels)
prepareExamples()

# Chunk1
foreach(s=c(9,4991,175)) %do% { # loop the value s through
  # three different start values 
  # for the random number generator
  set.seed(s)
  N <- NROW(oc.cc) # total sample size
  M <- round(0.632*NROW(oc.cc)) # learning sample size
  oc.cc$inbag <- 1:N%in%sample(1:N,replace=FALSE,size=M) # random split variable (TRUE for learning, FALSE for validation)
  train <- oc.cc[inbag==TRUE,]
  test <- oc.cc[inbag==FALSE,]
  # fit models in learning set 
  fit1 <- cph(Surv(survtime,survstatus)~rcs(age,3)+tumorthickness+gender+tobacco+deep.invasion+site+race+x.posnodes+tumormaxdimension+vascular.invasion,
              data=train, x=TRUE, y=TRUE, surv=TRUE)
  # seed for random forest
  set.seed(1972)
  fit2 <- rfsrc(Surv(survtime,survstatus)~ age+tumorthickness+gender+tobacco+deep.invasion+site+race+x.posnodes+tumormaxdimension+vascular.invasion,data=train)
  x <- Score(list("Conventional"=fit1, # trained Cox regression
                  "Experimental"=fit2), # trained Random forest
             data=test, # test set
             formula=Surv(survtime,survstatus)~1,
             times=60, # prediction time horizon
             )$Brier$contrast[3]$delta # difference in Brier score
}

# Chunk2 (takes a while to run, use multiple cores to speed up)
fit1 <- cph(Surv(survtime,survstatus)~rcs(age,3)+tumorthickness+gender+tobacco+deep.invasion+site+race+x.posnodes+tumormaxdimension+vascular.invasion,
            data=oc.cc, x=TRUE, y=TRUE, surv=TRUE)
set.seed(1972)
fit2 <- rfsrc(Surv(survtime,survstatus)~ age+tumorthickness+gender+tobacco+deep.invasion+site+race+x.posnodes+tumormaxdimension+vascular.invasion,data=oc.cc)
x <- Score(list("Conventional"=fit1,"Experimental"=fit2),
           data=oc.cc,
           formula=Surv(survtime,survstatus)~1,
           times=60,
           split.method="cv10", # 10-fold cross-validation
           B=10, # repeat 10-fold 10 times
           seed=9, # fix randomness of the splits
           se.fit=0, # no standard errors
           ncpus=2,  # number of cores on computer
           summary="IPA")
summary(x,what="score")[[1]]

# Chunk3
set.seed(71)
B=10000
subject=8
sample.size=399
mean(sapply(1:B,function(b){
    subject%in%sample(1:sample.size,replace=TRUE)
}))

# Chunk4
set.seed(71)
B=10000
sample.size=399
mean(sapply(1:B,function(b){
    length(unique(sample(1:sample.size,replace=TRUE)))
}))

# Chunk5
fit1 <- CSC(list(Hist(asprogtime,asprog)~age+psa+ct1+diaggs+ppb5,Hist(asprogtime,asprog)~age),data=astrain,cause="progression")
fit2 <- CSC(list(Hist(asprogtime,asprog)~age+psa+ct1+diaggs+ppb5+erg.status,Hist(asprogtime,asprog)~age+erg.status),data=astrain,cause="progression")
x <- Score(list("Conventional"=fit1,"New marker"=fit2),
           formula=Hist(asprogtime,asprog)~1,
           cause="progression",
           seed=7,  # fix randomness of the split
           split.method="bootcv",# bootstrap cross-validation
           B=200, # number of subsample bootstraps
           M=0.632*NROW(as), # learning sample size
           data=as, 
           times=3,
           summary="risks")
summary(x,what="score")

# Chunk6
fit1 <- lrm(ohss~ant.foll+cyclelen+smoking+age,data=ivftrain)
fit2 <- lrm(ohss~rcs(ant.foll,3)*smoking+cyclelen+age+fsh+bmi+ovolume,data=ivftrain,penalty=10)
x <- Score(list("Conventional"=fit1,"Experimental"=fit2),
           data=ivftest, formula=ohss~1, 
           split.method="loob", # leave-one-out bootstrap
           seed=8, # fix randomness of the split
           B=200,  # number of regular bootstrap sets
           summary="ipa")
summary(x,what="contrasts")

# Chunk7
oc.cc$survtime.5years <- pmin(oc.cc$survtime,60) # stop time after 5 years
oc.cc$survstatus.5years <- oc.cc$survstatus # take a copy 
oc.cc[oc.cc$survtime>60,]$survstatus.5years <- 0 # reset status
fit1 <- cph(Surv(survtime,survstatus)~rcs(age,3)+tumorthickness+gender+tobacco+deep.invasion+race+x.posnodes+tumormaxdimension+vascular.invasion,
            data=oc.cc, x=TRUE, y=TRUE, surv=TRUE)
fit2 <- cph(Surv(survtime.5years,survstatus.5years)~rcs(age,3)+tumorthickness+gender+tobacco+deep.invasion+race+x.posnodes+tumormaxdimension+vascular.invasion,
            data=oc.cc, x=TRUE, y=TRUE, surv=TRUE)
x <- Score(list("Unstopped"=fit1,"Stopped.5yrs"=fit2),
           data=oc.cc,
           formula=Surv(survtime,survstatus)~1,
           times=60,
           summary=c("IPA"),
           null.model=1,
           split.method="loob",
           B=200) # could be 2000
