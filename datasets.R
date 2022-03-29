library(readr)
library(stringr)
library(dplyr)
library(haven)

j <- read_sav("https://raw.github.com/elid123/birth_and_babies/main/Jewish%20Americans%20in%202020%20Extended%20Dataset.sav")

fp <- read_csv("https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NSFG/2017_2019_FemPregData.dat", col_names=FALSE)

f <- read_csv("https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NSFG/2017_2019_FemRespData.dat", col_names=FALSE)

n <- read_delim("https://raw.githubusercontent.com/elid123/birth_and_babies/main/Natality%2C%202007-2020.txt" , "\t", escape_double = FALSE, col_names = FALSE,trim_ws = TRUE, skip = 1)

n <- na.exclude(n[,c(3,5,7,8)])
names(n)<-c("age","plurality","weeks","births")
n<- n[n$weeks>=20 & n$weeks<99,]

caseid <- c(0,0)
preg.num <- c(0,0)
preg.end1 <- c(0,0)
preg.end2 <- c(0,0)
csection.num <- c(0,0)
num.born.live <- c(0,0)
gest.time.dead <- c(0,0)
outcome <- c(0,0)
birth.order <- c(0,0)
date.end <- c(0,0)
age.preg.end <- c(0,0)
date.conceive <- c(0,0)
age.conceive <- c(0,0)
age.interview <- c(0,0)
num.preg <- c(0,0)
parity <- c(0,0)
weight <- c(0,0)

for(i in 1:nrow(fp)){
  caseid[i] <- as.double(str_sub(fp$X1[i],start=1,end=5))
  preg.num[i] <- as.double(str_sub(fp$X1[i],start=6,end=7))
  preg.end1[i] <- as.double(str_sub(fp$X1[i],start=9,end=9))
  preg.end2[i] <- as.double(str_sub(fp$X1[i],start=10,end=10))
  csection.num[i] <- as.double(str_sub(fp$X1[i],start=19,end=19))
  num.born.live[i] <- as.double(str_sub(fp$X1[i],start=45,end=45))
  gest.time.dead[i] <- as.double(str_sub(fp$X1[i],start=103,end=103))
  outcome[i] <- as.double(str_sub(fp$X1[i],start=105,end=105))
  birth.order[i] <- as.double(str_sub(fp$X1[i],start=106,end=107))
  date.end[i] <- as.double(str_sub(fp$X1[i],start=108,end=111))
  age.preg.end[i] <- as.double(str_sub(fp$X1[i],start=112,end=113))
  date.conceive[i] <- as.double(str_sub(fp$X1[i],start=114,end=117))
  age.conceive[i]<- as.double(str_sub(fp$X1[i],start=118,end=119))
  age.interview[i]<- as.double(str_sub(fp$X1[i],start=138,end=139))
  num.preg[i]<- as.double(str_sub(fp$X1[i],start=152,end=153))
  parity[i]<-as.double(str_sub(fp$X1[i],start=154,end=155))
  weight[i]<-as.double(str_sub(fp$X1[i],start=205,end=220))
}

nsfg <- data.frame(caseid, age.interview, preg.num, date.conceive, age.conceive, date.end, age.preg.end, outcome, preg.end1, preg.end2, num.born.live, birth.order, csection.num, gest.time.dead, num.preg,parity,weight)

rm(caseid, age.interview, preg.num, date.conceive, age.conceive, date.end, age.preg.end, outcome, preg.end1, preg.end2, num.born.live, birth.order, csection.num, gest.time.dead, num.preg,parity,weight,i)

caseid <- c(0,0)
fert.help <- c(0,0)
type1 <- c(0,0)
type2 <- c(0,0)
type3 <- c(0,0)
type4 <- c(0,0)
type5 <- c(0,0)
type6 <- c(0,0)
oth.med1 <- c(0,0)
oth.med2 <- c(0,0)
oth.med3 <- c(0,0)
oth.med4 <- c(0,0)
try.time <- c(0,0)
try.unit <- c(0,0)
weights <- c(0,0)

for(i in 1:nrow(f)){
  caseid[i]<-as.double(str_sub(f$X1[i],start=1,end=5))
  fert.help[i]<-as.double(str_sub(f$X1[i],start=2429,end=2429))
  type1[i]<-as.double(str_sub(f$X1[i],start=2433,end=2433))
  type2[i]<-as.double(str_sub(f$X1[i],start=2434,end=2434))
  type3[i]<-as.double(str_sub(f$X1[i],start=2435,end=2435))
  type4[i]<-as.double(str_sub(f$X1[i],start=2436,end=2436))
  type5[i]<-as.double(str_sub(f$X1[i],start=2437,end=2437))
  type6[i]<-as.double(str_sub(f$X1[i],start=2438,end=2438))
  oth.med1[i] <- as.double(str_sub(f$X1[i],start=2443,end=2443))
  oth.med2[i] <- as.double(str_sub(f$X1[i],start=2444,end=2444))
  oth.med3[i] <- as.double(str_sub(f$X1[i],start=2445,end=2445))
  oth.med4[i] <- as.double(str_sub(f$X1[i],start=2446,end=2446))
  try.time[i]<-as.double(str_sub(f$X1[i],start=2458,end=2459))
  try.unit[i]<-as.double(str_sub(f$X1[i],start=2460,end=2460))
  weights[i]<-as.double(str_sub(f$X1[i],start=3787,end=3802))
}

fr <- data.frame(caseid,fert.help,type1,type2,type3,type4,type5,type6,oth.med1,oth.med2,oth.med3,oth.med4,try.time,try.unit,weights)

rm(caseid,fert.help,type1,type2,type3,type4,type5,type6,oth.med1,oth.med2,oth.med3,oth.med4,try.time,try.unit,weights,i)
#1 / 2 / 3 / 4,5 / 6+
