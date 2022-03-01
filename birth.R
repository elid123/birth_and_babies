library(readr)
library(stringr)
natal <- read_delim("C:/Users/Eli/Downloads/Natality, 2007-2020.txt", 
                    "\t", escape_double = FALSE, trim_ws = TRUE)
nt <- natal[,c(3,5,7,8)]

nt <-  nt %>% na.exclude

birth <- function(age){
  if(age<20){
    p <- 0.3
    g <- nt[nt$`Age of Mother Year Code`<20 & nt$`Age of Mother Year Code`>=15,]
  } else if(age<25){
    p <- 0.25
    g <- nt[nt$`Age of Mother Year Code`<25 & nt$`Age of Mother Year Code`>=20,]
  } else if(age<30){
    p <- 0.2
    g <- nt[nt$`Age of Mother Year Code`<30&nt$`Age of Mother Year Code`>=25,]
  } else if(age<35){
    p <- 0.15
    g <- nt[nt$`Age of Mother Year Code`<35&nt$`Age of Mother Year Code`>=30,]
  } else if(age<40){
    p <- 0.1
    g <- nt[nt$`Age of Mother Year Code`<40&nt$`Age of Mother Year Code`>=35,]
  } else if(age<43){
    p <- 0.05
    g <- nt[nt$`Age of Mother Year Code`<45&nt$`Age of Mother Year Code`>=40,]
  } else{
    p <- 0.01
    g <- nt[nt$`Age of Mother Year Code`==45,]
  }
  
  m <- 1
  chance <- runif(1)
  while(chance>p){
    chance <- runif(1)
    m <- m+1
  }
  
  if(m>12){
    print(str_c("You are ",age," years old. You did not get pregnant after 12 months."))
    
    return(age+1)
    
  } else{
    age.s <- floor(age-19)
    if(age.s<1){age.s <- 1}
    if(age.s>26){age.s <- 26}
    
    misc <- c(0.138,0.125,rep(0.1,11),0.12,0.125,0.138,0.15,0.175,0.19,0.225,0.265,0.330,0.35,0.39,0.451,0.55)
    
    misc.r <- misc[age.s]
    
    miscarriage <- runif(1)
    
    if(miscarriage<misc.r){
      
      miscarry.week <- sample(3:20,size=1,prob=17:0/sum(17:0))
      
      print(str_c("You are ",age," years old. You get pregnant after ",m," month(s). You miscarry at ",miscarry.week," weeks."))
      return(age+m/12+miscarry.week/52)
      
    } else{
      sex <- runif(1)
      if(sex>0.5){
        baby.sex <- "girl"
      } else{
        baby.sex <- "boy"
      }
      
      g <- g[g$`LMP Gestational Age Weekly Code`>=20 & g$`LMP Gestational Age Weekly Code`<52,]
      g <- g[g$`Plurality or Multiple Birth Code`==1,]
      
      births.per.week <- rep(0,47-20+1)
      k<-1
      for(i in 20:47){
        births.per.week[k] <- g[g$`LMP Gestational Age Weekly Code`==i,]$Births %>% sum
        k <- k+1
      }
      
      week <- sample(20:47,size=1,prob=births.per.week/sum(births.per.week))
      
      
      
      if(week>=37){
        print(str_c("You are ",age," years old. You get pregnant after ",m," month(s). You had a baby ", baby.sex," at ",week," weeks."))
        
        return(age+m/12+week/52)
      } else{
        weeks <- 23:33
        death.rate <- (weeks-33)^2*.442/(23-33)^2
        death.rate <- c(death.rate,0,0,0)
        weeks <- 23:36
        health.rate <- (weeks-23)^2*.824/(36-23)^2
        sick.rate <- 1-death.rate-health.rate
        week.s <- week - 22
        
        if(week.s<1){
          print(str_c("You are ",age," years old. You get pregnant after ",m," month(s). You had a baby ", baby.sex," at ",week," weeks. The baby died."))
          return(age+m/12+week/52)
        } else{
          outcome <- sample(c(0,1,2),1,prob=c(death.rate[week.s],sick.rate[week.s],health.rate[week.s]))
          
          if(outcome==0){
            print(str_c("You are ",age," years old. You get pregnant after ",m," month(s). You had a baby ", baby.sex," at ",week," weeks. The baby died."))
            return(age+m/12+week/52)
            
          } else if(outcome==1){
            
            major <- c(.442,.526,.548,.521,.403,.219,.225,.137,.071,.087,.042,.044,.028,.018)
            minor <- c(.093,.158,.315,.349,.484,.735,.69,.786,.817,.763,.635,.51,.272,.159)
            morbidity<-sort(major/minor,decreasing=TRUE)
            
            complications <- sample(c("major","minor"),size=1,prob=c(morbidity[week-22],1)/sum(morbidity[week-22],1))
            
            print(str_c("You are ",age," years old. You get pregnant after ",m," month(s). You had a baby ", baby.sex," at ",week," weeks. The baby has ",complications, " health complications."))
            return(age+m/12+week/52)
            
          } else{
            print(str_c("You are ",age," years old. You get pregnant after ",m," month(s). You had a baby ", baby.sex," at ",week," weeks."))
            
            return(age+m/12+week/52)
          }
        }
      }
    }
  }
}

multiples <- function(age){
  if(age<20){
    p <- 0.3
    g <- nt[nt$`Age of Mother Year Code`<20 & nt$`Age of Mother Year Code`>=15,]
  } else if(age<25){
    p <- 0.25
    g <- nt[nt$`Age of Mother Year Code`<25 & nt$`Age of Mother Year Code`>=20,]
  } else if(age<30){
    p <- 0.2
    g <- nt[nt$`Age of Mother Year Code`<30&nt$`Age of Mother Year Code`>=25,]
  } else if(age<35){
    p <- 0.15
    g <- nt[nt$`Age of Mother Year Code`<35&nt$`Age of Mother Year Code`>=30,]
  } else if(age<40){
    p <- 0.1
    g <- nt[nt$`Age of Mother Year Code`<40&nt$`Age of Mother Year Code`>=35,]
  } else if(age<43){
    p <- 0.05
    g <- nt[nt$`Age of Mother Year Code`<45&nt$`Age of Mother Year Code`>=40,]
  } else{
    p <- 0.01
    g <- nt[nt$`Age of Mother Year Code`==45,]
  }
  
  multi <- c(0,0)
  for(i in 1:4){
    multi[i] <- g[g$`Plurality or Multiple Birth Code`==i,]$Births %>% sum
  }
  number.babies <- sample(1:4,size=1,prob=multi/sum(multi),replace=TRUE)
  return(number.babies)
}