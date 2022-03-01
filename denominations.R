branch.raise <- function(movement,kids=1){
  #choose a movement and the number of kids
  #the calculator will take a random sample and output the religious orientation of the child(ren).
  
  if(movement==1){
    movement = 'haredi'
  }
  if(movement==2){
    movement = 'orthodox'
  }
  if(movement==3){
    movement = 'conservative'
  }
  if(movement==4){
    movement = 'reform'
  }
  if(movement==5){
    movement = 'other'
  }
  if(movement==6){
    movement = 'secular'
  }
  
  library(readr)
  library(stringi)
  library(dplyr)
  library(haven)
  
  #pew research study data
  j <- read_sav("C:/Users/Eli/OneDrive - Knights - University of Central Florida/Jewish Americans in 2020 Extended Dataset.sav")
  
  #extracting just the columns with denomination information
  #CHDENOM1 holds the denomination the person was raised in
  #CHDENOM2 holds kind of orthodoxy if the person indicated they were raised orthodox
  #BRANCH holds the deonomination the person is now a part of
  #ORTHTYPE holds the kind of orthodoxy if the person indicated they were raised orthodox
  raised.denom <- j$CHDENOM1
  raised.orth <- j$CHDENOM2
  current.denom <- j$BRANCH
  current.orth <- j$ORTHTYPE
  
  #the numbers correspond to the coding used in the pew research survey
  #the codings for haredi and orthodox are in the CHDENOM2 and ORTHTYPE variables
  #the codings for the rest are CHDENOM1 and BRANCH
  
  haredi <- c(2,3)
  #2) hasidic or chabad; 3)yeshvish, litvish, agudah
  orthodox <- c(1,95,97,98,99)
  #1) modern or centrist orthodox; 95)mixed/in-between; 97) just orthodox; 98) other 99) no response 
  conservative <- c(1,21,26)
  #1) conservative; 21) traditional; 26) observant
  reform <- c(3,5)
  #3) reform; #5) liberal/progressive
  other <- c(4,6,8,25,901,903,919,994,995)
  #4) reconstructionist; 6) renewal; 8) humanist; 25) sephardic; 901) reform/conservative; 903) conservative/orthodox; 919) other mix; 994) other mix 995) just jewish
  secular <- c(18,19,20,41)
  #18) not practicing/religious; 19) secular; 20) culturally jewish; 41) spiritual
  not.jewish <- c(24,30,31,32,39,990)
  #24) atheist/agnostic; 30) messianic; 31) other christian; 32) christian+non-christian; 39) unitarian; 990) not jewish
  unknown <- c(999,8888,NA)
  
  #a list with the arrays of the codes
  m <- list(haredi,orthodox,conservative,reform,other,secular,not.jewish)
  
  #assigns the appropriate column for the input denomination
  if(movement %in% c("haredi","orthodox")){
    a <- raised.orth
  } else{
    a <- raised.denom
  }
  
  #assigns the array of codes appropriate to the movement
  if(movement == 'haredi'){
    b <- haredi
  }
  if(movement == 'orthodox'){
    b <- orthodox
  }
  if(movement == 'conservative'){
    b <- conservative
  }
  if(movement == 'reform'){
    b <- reform
  }
  if(movement == 'other'){
    b <- other
  }
  if(movement == 'secular'){
    b <- secular
  }
  
  #empty array to hold counts
  p <- rep(0,7)
  
  #count(j[a %in% b],ORTHTYPE) will count how many correspond to each possible answer in ORTHTYPE. Only the options for the particular movement in m are selected. This is then counted and summed.
  
  #haredi
  p[1]<- count(j[a %in% b,],ORTHTYPE)[count(j[a %in% b,],ORTHTYPE)$ORTHTYPE %in% m[[1]],]$n %>% sum
  
  #orthodox
  p[2]<- count(j[a %in% b,],ORTHTYPE)[count(j[a %in% b,],ORTHTYPE)$ORTHTYPE %in% m[[2]],]$n %>% sum
   
  #for loop for the remaining options 
  for(i in 3:7){
    p[i]<- count(j[a %in% b,],BRANCH)[count(j[a %in% b,],BRANCH)$BRANCH %in% m[[i]],]$n %>% sum
  }
  
  #take a sample of the chosen sample size where the probability of choosing each movement is proportional to the responses from the survey.
  o <- sample(c("haredi","orthdox","conservative","reform","other","secular","not jewish"),size=kids,replace=TRUE,prob=p/sum(p))
  
  #print the random sample of current movement given being raised in the movement that was input.
  print(o)
}

num.kids <- function(denom,num=1){
  if(sum(denom==haredi)==2 | sum(denom==orthodox)==5){
    n <- sample(count(j[j$ORTHTYPE %in% denom & j$AGE4CAT > 2,],FERT)$FERT,size=num,replace=TRUE,prob=count(j[j$ORTHTYPE %in% denom & j$AGE4CAT > 2,],FERT)$n)
  } else{
    n <- sample(count(j[j$BRANCH %in% denom & j$AGE4CAT > 2,],FERT)$FERT,size=num,replace=TRUE,prob=count(j[j$BRANCH %in% denom & j$AGE4CAT > 2,],FERT)$n)
  }
  return(n)
}

age.kids <- function(kids,k=1){
  m <- 32:20
  a <- c(540,522,505,489,474,460,447,435,424,414,405,397,390)/12
  n <- 1:13
  
  l <- 16:m[kids]-1
  u <- m[kids]:a[kids]
  
  u.p<-(length(u):1)/length(u)
  l.p<-head(rev(((length(l)):1)/(length(l))),-1)
  
  t<-sample(16:a[kids],size=k,replace=TRUE,prob=c(l.p,u.p))
  
  return(t)
}