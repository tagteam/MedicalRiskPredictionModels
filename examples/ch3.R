library(MedicalRiskPredictionModels)
prepareExamples()

# Chunk1
library(Publish)
library(data.table)
data(Diabetes)
setDT(Diabetes)
Diabetes[1:5,.(weight,height)]

# Chunk2
library(data.table)
Diabetes[,height.m:= height*0.0254]
Diabetes[,weight.kg:= weight* 0.4535929]
Diabetes[,bmi:= weight.kg/height.m^2]
Diabetes[,BMI:=cut(bmi,c(0,18,25,30,Inf),labels=c("UnderWeight","NormalWeight","OverWeight","Obese"))]
Diabetes[,.(bmi,BMI,weight,height)]

# Chunk3
library(data.table)
long <- long[,list("psa.time"=psadate-psadate[1],psadate,psa),by=subject]
long <- long[psa.time<=(2*365.25),]
# psa doubling time formula
psadt <- function(time,value){ # input date and psa value
  (log(2)/coef(lm(log(value)~time))[2])/365.25 # lm is linear model
}
# now apply function to individual subjects
long[,list("psa.doublingtime"=psadt(psa.time,psa)),by=subject]

# Chunk4 (no competing risks)
library(data.table)
d[,af.date:=as.Date(af.date)]
d[,death.date:=as.Date(death.date)]
d[,lost.date:=as.Date(lost.date)]
d[,time:=pmin(  # parallel minimum
     death.date, # event 
     lost.date,  # lost to follow up
     as.Date("2015-01-01") # administrative censoring
,na.rm=TRUE)
-af.date # date of subject specific time origin
]
d[,event:=0] # initialize all subjects
d[!is.na(death.date),event:=1] # event 
d

# Chunk5 (with competing risks)
library(data.table)
d[,af.date:=as.Date(af.date)]
d[,stroke.date:=as.Date(stroke.date)]
d[,death.date:=as.Date(death.date)]
d[,lost.date:=as.Date(lost.date)]
d[,time:=pmin(    # parallel minimum
     stroke.date, # event 
     death.date,  # competing risk 
     lost.date,   # lost to follow up
     as.Date("2015-01-01") # administrative censoring
,na.rm=TRUE)
-af.date # date of subject specific time origin
]
d[,event:=0] # initialize all subjects
d[!is.na(stroke.date),event:=1] # event 
d[!is.na(death.date) & is.na(stroke.date),event:=2] # competing
d

# Chunk6
library(data.table)
d[,time.5:=pmin(time,5*365.25)]
d[,event.5:=event]
d[time>5*365.25,event.5:=0]
d[,.(time,event,time.5,event.5)]
