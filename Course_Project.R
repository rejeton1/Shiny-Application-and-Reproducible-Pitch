m1 <- list(w=0, score=c(), time=c())
m2 <- list(w=1, score=c('B','O','B','B','B'), time=c(1,6,66,68,79))
m3 <- list(w=1, score=c('B','B','B','B'), time=c(24, 43, 65, 90))
m4 <- list(w=1, score=c('B','B','B'), time=c(21, 36, 50))
m5 <- list(w=1, score=c('B','B','B','B'), time=c(55, 65, 86, 90))
m6 <- list(w=1, score=c('B','B','B'), time=c(34, 41, 48))
m7 <- list(w=1, score=c('B'), time=c(20))
m8 <- list(w=1, score=c('B'), time=c(17))
m9 <- list(w=0, score=c('O','O','B','O'), time=c(12, 35, 83, 90))
m10 <- list(w=1, score=c('B','B','B'), time=c(31, 35, 38))
m11 <- list(w=1, score=c('B','B','B','B'), time=c(12, 18, 22, 78))
m12 <- list(w=1, score=c('B'), time=c(90))
m13 <- list(w=1, score=c('B','B'), time=c(48, 62))
m14 <- list(w=1, score=c('O','B','B'), time=c(6, 48, 85))
m15 <- list(w=0, score=c('B','O'), time=c(7, 73))

total_training <- list(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15)

data <- as.data.frame(matrix(nrow=0, ncol = 4))
colnames(data) <- c('Win', 'FCB', 'Oppo', 'Time')

one_game_data <- function(y){
  w <- y$w
  score <- y$score
  time <- y$time
  nrow <- nrow(data)
  if(length(score)==0){
    data[nrow+1:90, 1] <- 0
    data[nrow+1:90, 2:3] <- 0
    data[nrow+1:90, 4] <- 1:90
  }else{
    for(i in 1:length(score)){
      #data from 1m ~ first score
      if(i == 1){
        for(t in 1:(time[i]-1)){
          data[nrow+t, 1] <- w
          data[nrow+t, 2:3] <- 0
          data[nrow+t, 4] <- t
        }
      }
      #data from last score to 90m
      if(i == length(score)){
        for(t in time[i]:90){
          if(score[i] == 'B'){
            data[nrow+t, 1] <- w
            data[nrow+t, 2] <- data[nrow+time[i]-1,2] + 1
            data[nrow+t, 3] <- data[nrow+time[i]-1,3]
            data[nrow+t, 4] <- t
          }else{
            data[nrow+t, 1] <- w
            data[nrow+t, 2] <- data[nrow+time[i]-1,2]
            data[nrow+t, 3] <- data[nrow+time[i]-1,3] + 1
            data[nrow+t, 4] <- t
          }
        }
      }
      #data at rest time
      if(i >= 1 && i < length(score)){
        if(score[i] == 'B'){
          data[nrow+time[i]:(time[i+1]-1), 1] <- w
          data[nrow+time[i]:(time[i+1]-1), 2] <- data[nrow+time[i]-1,2] + 1
          data[nrow+time[i]:(time[i+1]-1), 3] <- data[nrow+time[i]-1,3]
          for(t in time[i]:(time[i+1]-1)){
            data[nrow+t, 4] <- t
          }
        }else if(score[i]=='O'){
          data[nrow+time[i]:(time[i+1]-1), 1] <- w
          data[nrow+time[i]:(time[i+1]-1), 2] <- data[nrow+time[i]-1,2]
          data[nrow+time[i]:(time[i+1]-1), 3] <- data[nrow+time[i]-1,3] + 1
          for(t in time[i]:(time[i+1]-1)){
            data[nrow+t, 4] <- t
          }
        }
      }
    }
  }
  
  return(data)
}


for(y in total_training){
  data = one_game_data(y)
}

#Regularize data
# data$FCB <- (data$FCB - mean(data$FCB))/sd(data$FCB)
# data$Oppo <- (data$Oppo - mean(data$Oppo))/sd(data$Oppo)
# data$Time <- (data$Time - mean(data$Time))/sd(data$Time)


#Fit the Logistic Model

Win_Model <- glm(Win ~ FCB + Oppo + Time, data=data, family='binomial')


#Plotting the model
fig1 <- plot_ly(x=data$FCB, y=data$Oppo, z=Win_Model$fitted.values, color=as.factor(data$Win)) %>%
  layout(scene=list(xaxis=list(title='FCB Score'), yaxis=list(title='Opponent Score'), 
                    zaxis=list(title='Win Probability')))
fig1


#Prediction sample
predict(Win_Model, newdata = data.frame(FCB=1, Oppo=3, Time=90), type='response')