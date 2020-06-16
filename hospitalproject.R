library(car)
library(moments)
library(rio)

# 30-day death rates from heart attack
data = read.csv("outcome-of-care-measures.csv", colClasses = "character")
str(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
str(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
hist(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, main = "30-day Death Rates from Heart Attack", xlab = "Death Rate from Heart Attack")

# Finding the best hospital in a state
best<-function(state,outcome){
  fin = paste("Hospital.30.Day.Death..Mortality..Rates.from",sub(" ",".",outcome), sep = ".")
  if(is.element(state, data$State)==TRUE){
    datastate = subset(data, data$State==state)
  }else{
    stop("invalid state")
  }
  if(is.element(fin, colnames(data))==TRUE){
    min = min(datastate[[fin]], na.rm = TRUE)
    datamin = subset(datastate, datastate[[fin]] == min)
    sort(datamin$Hospital.Name)[1]
  }else{
    stop("invalid outcome")
  }
}

# Ranking hospitals by outcome in a state
rankhospital<- function(state, outcome, num = "best"){
  fin = paste("Hospital.30.Day.Death..Mortality..Rates.from",sub(" ",".",outcome), sep = ".")
  if(is.element(state, data$State)==TRUE){
    datastate = subset(data, data$State==state)
  }else{
    stop("invalid state")
  }
  if(is.element(fin, colnames(data))==TRUE){
    if(num == "worst"){
      max = max(as.numeric(datastate[[fin]]), na.rm = TRUE)
      datamax = subset(datastate, as.numeric(datastate[[fin]]) == max)
      sort(datamax$Hospital.Name)[1]
    } else if(num == "best"){
      min = min(datastate[[fin]], na.rm = TRUE)
      datamin = subset(datastate, datastate[[fin]] == min)
      sort(datamin$Hospital.Name)[1]
    } else{
          if(num<=sum(length(datastate[[fin]]))){
            rank = sort(as.numeric(datastate[[fin]]))[num]
            dataran = subset(datastate, datastate[[fin]] == rank)
            sort(dataran$Hospital.Name)[1]
          } else {
                print(NA)
               }
         }
    } else {
      stop("invalid outcome")
    }
}

# Ranking hospitals in all states
rankall<-function(outcome, num = "best"){
  fin = paste("Hospital.30.Day.Death..Mortality..Rates.from",sub(" ",".",outcome), sep = ".")
  if(is.element(fin, colnames(data))==TRUE){
    a = split(data,data$State)
    if(num == "worst"){
      dat = data.frame()
      j=0
      for(i in 1:length(a)){
        j=a[[i]]
        max=max(as.numeric(j[[fin]]), na.rm = TRUE)
        datamax = subset(j, as.numeric(j[[fin]]) == max)
        dat[i,1]=sort(datamax$Hospital.Name)[1]
        dat[i,2]=(datamax$State)[1]
      }
      colnames(dat) = c("hospital","State")
      dat
    } else if(num == "best"){
      dat = data.frame()
      j=0
      for(i in 1:length(a)){
        j=a[[i]]
        min=min(as.numeric(j[[fin]]), na.rm = TRUE)
        datamin = subset(j, as.numeric(j[[fin]]) == min)
        dat[i,1]=sort(datamin$Hospital.Name)[1]
        dat[i,2]=(datamin$State)[1]
      } 
      colnames(dat)=c("hospital","State")
      dat
    } else {
      dat = data.frame()
      j=0
      for(i in 1:length(a)){
        j=a[[i]]
        if(num <= length(j[[fin]])){
          rank = sort(as.numeric(j[[fin]]))[num]
          dataran = subset(j, as.numeric(j[[fin]]) == rank)
          dat[i,1]=sort(dataran$Hospital.Name)[1]
          dat[i,2]=(dataran$State)[1]
        } else {
        print(NA)
        }
      } 
      colnames(dat)=c("hospital","State")
      dat
    }
  } else {
    stop("invalid outcome")
  }
}



