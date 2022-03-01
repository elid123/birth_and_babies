library(readr)
library(stringr)
natal <- read_delim("C:/Users/Eli/Downloads/Natality, 2016-2020 expanded.txt", 
                    "\t", escape_double = FALSE, trim_ws = TRUE)
nt <- natal[,c(3,5,7,8)]

nt <-  nt %>% na.exclude

birth <- function(age){
  if(age<25){
    g <- nt[nt$`Age of Mother Year Code`<25,]
    p <- 0.25
    misc <- c(0.127,0.082,0.045,0.033,0.023,0.016,0.005)
  }
  else if(age<30){
    g <- nt[(nt$`Age of Mother Year Code`<30 & nt$`Age of Mother Year Code`>=25),]
    p <- 0.2
    misc <- c(0.127,0.082,0.045,0.033,0.023,0.016,0.005)
  }
  else if(age<35){
    g <- nt[(nt$`Age of Mother Year Code`<35 & nt$`Age of Mother Year Code`>=30),]
    p <- 0.15
    misc <- c(0.127,0.082,0.045,0.033,0.023,0.016,0.005)
  }
  else if(age<40){
    g <- nt[(nt$`Age of Mother Year Code`<40 & nt$`Age of Mother Year Code`>=35),]
    p <- 0.10
    misc <- c(0.176,0.114,0.068,0.046,0.032,0.022,0.007)
  }
  else{
    g <- nt[(nt$`Age of Mother Year Code`<45 & nt$`Age of Mother Year Code`>=40),]
    p <- 0.05
    misc <- c(0.333,0.215,0.129,0.086,0.061,0.041,0.013)
  }
  
  tot.birth <- g$`Births` %>% sum
  singles <- g[g$`Plurality Code`==1,]$Births %>% sum
  twins <- g[g$`Plurality Code`==2,]$Births %>% sum
  triplets <- g[g$`Plurality Code`==3,]$Births %>% sum
  quads <- g[g$`Plurality Code`==4,]$Births %>% sum
  
  multiples <- runif(1)
  
  if(multiples<quads/tot.birth){
    kids <- 4
  } else if(multiples<sum(quads/tot.birth,triplets/tot.birth)){
    kids <- 3
  } else if(multiples<sum(quads/tot.birth,triplets/tot.birth,twins/tot.birth)){
    kids <- 2
  } else
    kids <- 1
  
  cycle <- rep(0,24)
  cycle[1] = p
  
  for(i in 2:24){
    cycle[i] = (1-cycle[i-1])*cycle[1]+cycle[i-1]
  }
  
  mp <- runif(1)
  months <- sum(mp>cycle)+1
  try.weeks <- months*4
  
  miscarry <- runif(7)
  miscarry.week <- miscarry<misc
  
  if(miscarry.week[1]==TRUE){
    loss <- 6
  } else if(miscarry.week[2]==TRUE){
    loss <- 7
  } else if(miscarry.week[3]==TRUE){
    loss <- 8
  } else if(miscarry.week[4]==TRUE){
    loss <- 9
  } else if(miscarry.week[5]==TRUE){
    loss <- 12
  } else if(miscarry.week[6]==TRUE){
    loss <- 16
  } else{
    loss <- 0
  }
  
  
  kid.weeks <- g[g$`Plurality Code`==kids,]
  
  m <- sum(kid.weeks$`LMP Gestational Age Weekly Code`*kid.weeks$Births)/sum(kid.weeks$Births)
  
  s <- sqrt(sum((kid.weeks$`LMP Gestational Age Weekly Code`-m)**2)/sum(kid.weeks$Births))
  
  
  weeks <- rnorm(n=1,m,s)
  
  if(weeks<20){
    loss <- weeks
  }
  
  if(loss>0&months<25){
    print(str_c("You are ",age," and having ",kids," child(ren). You get pregnant after ",try.weeks," weeks. You miscarry ",loss-4," weeks later."))
  } else if(months<25){
  print(str_c("You are ",age," and having ",kids," child(ren). You get pregnant after ",try.weeks," weeks. Birth is in ",round(weeks-4)," more weeks."))
  } else{
    print(str_c("You are ",age," and did not get pregnant."))
  }
}

year.add <- function(year,weeks,add.weeks){
  new.weeks <- weeks + add.weeks
  while(new.weeks>=52){
    year = year + 1
    new.weeks = new.weeks - 52
  }
  print(str_c(year,"y",new.weeks,"w"))
}