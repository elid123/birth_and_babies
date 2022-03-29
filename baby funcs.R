plurality <- function(age,samples=1){
  low <- seq(from=15,to=45,by=5)
  high <- low+5
  
  ind<-sum(age>=low)
  
  n1<-n[n$age>=low[ind] & n$age<high[ind],]
  
  p<-c(0,0)
  
  for(i in 1:4){
    p[i]<- n1[n1$plurality==i,]$births %>% sum
  }
  return(sample(1:4,size=samples,replace=TRUE,prob=p))
}

sex <- function(num){
  return(sample(c("boy","girl"),size=num,replace=TRUE))
}

miscarriage <- function(age,kids){
  age.s <- floor(age-19)
  if(age.s<1){age.s <- 1}
  if(age.s>26){age.s <- 26}
  
  misc <- c(0.138,0.125,rep(0.1,11),0.12,0.125,0.138,0.15,0.175,0.19,0.225,0.265,0.330,0.35,0.39,0.451,0.55)
  
  misc.r <- misc[age.s]
  
  m <- runif(kids)
  
  misc.week <- sample(3:20,size=sum(m<=misc.r),replace=TRUE,prob=17:0/sum(17:0))
  
  return(misc.week)
  
}

weeks.gestation <- function(age,kids){
  if(kids==1){
    low<-seq(from=15,to=49,by=5)
    high<-low+5
  }else if(kids==2){
    low<-seq(from=15,to=49,by=7)
    high <- low+7
  }else if(kids>=3){
    low<-c(15,22,35)
    high<-c(22,35,50)
  }
  ind<-sum(age>=low)
  n1 <- n[n$age>=low[ind] & n$age<high[ind] & n$plurality==kids,]
  
  return(sample(n1$weeks,size=1,prob=n1$births))
}

premature <- function(week,num.kids){
  outcome <- rep(9,length(num.kids))
  for(i in 1:num.kids){
    weeks <- 23:33
    death.rate <- (weeks-33)^2*.442/(23-33)^2
    death.rate <- c(death.rate,0,0,0)
    weeks <- 23:36
    health.rate <- (weeks-23)^2*.824/(36-23)^2
    sick.rate <- 1-death.rate-health.rate
    week.s <- week - 22
    
    if(week.s<1){
      outcome[i] <- "death"
    }else{
      outcome[i] <- sample(c("death",1,"none"),1,prob=c(death.rate[week.s],sick.rate[week.s],health.rate[week.s]))
    }
    if(outcome[i]==1){
      major <- c(.442,.526,.548,.521,.403,.219,.225,.137,.071,.087,.042,.044,.028,.018)
      minor <- c(.093,.158,.315,.349,.484,.735,.69,.786,.817,.763,.635,.51,.272,.159)
      morbidity<-sort(major/minor,decreasing=TRUE)
      
      outcome[i] <- sample(c("major","minor"),size=1,prob=c(morbidity[week-22],1)/sum(morbidity[week-22],1))
    }
  }
  return(outcome)
}

birth <- function(age){
  low <- seq(from=15,to=45,by=5)
  high <- low+5
  
  ind<-sum(age>=low)
  
  p<-c(0.3,0.25,0.2,0.15,0.1,0.05,0.01)
  p<-p[ind]
  
  
  m <- 1
  chance <- runif(1)
  while(chance>p){
    chance <- runif(1)
    m <- m+1
    if(m>12){
      break
    }
  }
  
  if(m>12){
    print(str_c("Months to get pregnant: >12 months"))
    print(str_c("Number of babies: ",0))
    print(str_c("Sex of baby: "))
    print(str_c("Miscarriage week: "))
    print(str_c("Weeks gestation: "))
    print(str_c("Current age: ",age+1))
    return(age+1)
    
  }
  
  else{
  
  num.kids <- plurality(age)
  sex.babies <- sex(num.kids)
  misc.week <- miscarriage(age,num.kids)
  weeks <- weeks.gestation(age,num.kids)
  
  if(weeks<37 & num.kids-length(misc.week)>0){
    premie <- premature(weeks,num.kids-length(misc.week))
  }
  
  
  if(length(misc.week)-num.kids==0){
    weeks <- integer(0)
    print(str_c("Months to get pregnant: ",m))
    print(str_c("Number of babies: ",num.kids))
    print(str_c("Sex of baby: ",sex.babies))
    print(str_c("Miscarriage week: ",misc.week))
    print(str_c("Weeks gestation: ",weeks))
    print(str_c("Current age: ",age+m/12+max(misc.week)/52))
    return(age+m/12+max(misc.week)/52)
  } else if(weeks>=37){
    print(str_c("Months to get pregnant: ",m))
    print(str_c("Number of babies: ",num.kids))
    print(str_c("Sex of baby: ",sex.babies))
    print(str_c("Miscarriage week: ",misc.week))
    print(str_c("Weeks gestation: ",weeks))
    print(str_c("Current age: ",age+m/12+weeks/52))
  return(age+m/12+weeks/52)
  }
  else{
    print(str_c("Months to get pregnant: ",m))
    print(str_c("Number of babies: ",num.kids))
    print(str_c("Sex of baby: ",sex.babies))
    print(str_c("Miscarriage week: ",misc.week))
    print(str_c("Weeks gestation: ",weeks))
    print(str_c("Complications of prematurity: ",premie))
    print(str_c("Current age: ",age+m/12+weeks/52))
    return(age+m/12+weeks/52)
  }
  }
}

denomination <- function(d.raised,kids=1){
  if(d.raised == "haredi"){
    j1 <- j[j$CHDENOM2 %in% c(2,3) & !is.na(j$CHDENOM2) & !is.na(j$BRANCH),]
  } else if(d.raised == "orthodox"){
    j1 <- j[j$CHDENOM2 %in% c(1,95,97,98,99) & !is.na(j$CHDENOM2) & !is.na(j$BRANCH),]
  } else if(d.raised == "conservative"){
    j1 <- j[j$CHDENOM1 %in% c(1,21,26) &!is.na(j$CHDENOM1) & !is.na(j$BRANCH),]
  } else if(d.raised == "reform"){
    j1 <- j[j$CHDENOM1 %in% c(3,5) & !is.na(j$CHDENOM1) & !is.na(j$BRANCH),]
  } else if(d.raised == "other"){
    j1 <- j[j$CHDENOM1 %in% c(4,6,8,25,901,903,919,994,995) & !is.na(j$CHDENOM1) & !is.na(j$BRANCH),]
  } else if(d.raised == "secular"){
    j1 <- j[j$CHDENOM1 %in% c(18,19,20,41) & !is.na(j$CHDENOM1) & !is.na(j$BRANCH),]
  }
  denom <- j1[sample(1:nrow(j1),size=kids,replace=TRUE,prob=j1$EXTWEIGHT),c("BRANCH","ORTHTYPE")]
  return(denom)
}

num.kids <- function(denom,times=1){
  if(denom=="haredi"){
    j1 <- j[j$ORTHTYPE %in% c(2,3) & !is.na(j$FERT) & j$FERT<99 & j$AGE4CAT>2,]
  }else if(denom=="orthodox"){
    j1 <- j[j$ORTHTYPE %in% c(1,95,97,98,99) & !is.na(j$FERT) & j$FERT<99 & j$AGE4CAT>2,]
  }else if(denom=="conservative"){
    j1 <- j[j$BRANCH %in% c(1,21,26) & !is.na(j$FERT) & j$FERT<99 & j$AGE4CAT>2,]
  }else if(denom=="reform"){
    j1 <- j[j$BRANCH %in% c(3,5) & !is.na(j$FERT) & j$FERT<99 & j$AGE4CAT>2,]
  }else if(denom=="other"){
    j1 <- j[j$BRANCH %in% c(4,6,8,25,901,903,919,994,995) & !is.na(j$FERT) & j$FERT<99 & j$AGE4CAT>2,]
  }else if(denom=="secular"){
    j1 <- j[j$BRANCH %in% c(18,19,20,41) & !is.na(j$FERT) & j$FERT<99 & j$AGE4CAT>2,]
  }
  
  j2 <- sample(j1$FERT,size=times,prob=j1$EXTWEIGHT)
  
  print(j2)
  return(j2)
  
}

age.kids <- function(kids){
  j1 <- j[j$FERT == kids & !is.na(j$FERT1) & j$FERT1<=46,]
  return(sample(j1$FERT1,size=1,replace=TRUE,prob=j1$EXTWEIGHT))
}



# 1,2 / 3,4,5 / 6+
#0.002979763
#0.9999657