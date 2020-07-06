library(MedicalRiskPredictionModels)
prepareExamples()

# Chunk1
B <- 200
ivftrain.small <- ivftrain[sample(1:NROW(ivftrain),size=50)]
result <- foreach(s=1:B,.combine="rbind")%dopar%{
  set.seed(s)
  # function rnorm generates normal noise
  ivftrain.small[,newMarker:=rnorm(NROW(ivftrain.small))] 
  ivftest[,newMarker:=rnorm(NROW(ivftest))] 
  fit1 <- glm(ohss~ant.foll+age,data=ivftrain.small,family="binomial")
  fit2 <- glm(ohss~ant.foll+age+newMarker,data=ivftrain.small,family="binomial")
  x <- Score(list("Conventional model"=fit1,"Random marker"=fit2),
             data=ivftest,
             se.fit=0,
             formula=ohss~1)
  library(Hmisc) # provides IDI and NRI
  p1 <- predictRisk(fit1,newdata=ivftest)
  p2 <- predictRisk(fit2,newdata=ivftest)
  y <- improveProb(p1,p2,ivftest$ohss)
  data.table(NRI=100*y$nri, 
             IDI=100*y$idi, 
             delta.AUC=100*x$AUC$contrasts$delta.AUC,
             delta.Brier=-100*x$Brier$contrasts$delta.Brier[3])
}
boxplot(result,names=c("NRI",
                       "IDI",
                       expression(paste(Delta,"AUC")),
                       expression(paste(Delta,"Brier"))))
abline(h=0,col=2)

# Chunk2
library(PredictABEL) # provides the re-classification table
OHSSprevalence <- mean(ivftrain$OHSS=="Yes")
# conventional model
fit1 <- glm(OHSS~ant.foll+age,data=ivftrain,family="binomial")
# exaggerated model
fit2 <- function(risk,prevalence){
  new.risk <- risk 
  # set risk to 100% when risk is above prevalence
  new.risk[risk>prevalence] <- 1
  # set risk to 0% when risk is below prevalence
  new.risk[risk<=prevalence] <- 0
  new.risk
}
# apply models to test set
p1 <- predictRisk(fit1,newdata=ivftest)
p2 <- fit2(p1,OHSSprevalence)
reclassification(data=ivftest,
                 cOutcome=10,
                 p1,
                 p2,
                 cutoff=c(0,.25,.5,.75,1))

# Chunk3
x <- Score(list("Conventional model"=p1,"Exaggerated model"=p2),
           data=ivftest,
           se.fit=0,
           null.model=FALSE,
           formula=ohss~1)
summary(x)
