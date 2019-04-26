##this was used to calculate the mean accuracies of the homogeneous HMM

##Hidden Markov Model

#splits data into simulations
chunk2 <- function(x,n) split(x, cut(seq_along(x), n, labels = FALSE))

simulated <- read.csv('/Users/BrianHo/Downloads/mvnhmm/stat337results/amsterdam_hmm_sim', sep=' ', header=FALSE)
observed <- read.csv('/Users/BrianHo/Downloads/mvnhmm/rain.txt', sep=' ', header=FALSE)

#station 1
station1<-simulated[,1]
observed1<-observed[,1] 

sim1<-chunk2(station1, 100)

acc1 <- NULL
for (i in 1:100){
  sim <- unlist(sim1[i], use.names=FALSE)
  error <- mean(sim != observed1)
  acc1[i]<- 1-error
}

#station 2
station2<-simulated[,2]
observed2<-observed[,2] 

sim2<-chunk2(station2, 100)

acc2 <- NULL
for (i in 1:100){
  sim <- unlist(sim2[i], use.names=FALSE)
  error <- mean(sim != observed2)
  acc2[i]<- 1-error
}

#station 3
station3<-simulated[,3]
observed3<-observed[,3] 

sim3<-chunk2(station3, 100)

acc3 <- NULL
for (i in 1:100){
  sim <- unlist(sim3[i], use.names=FALSE)
  error <- mean(sim != observed3)
  acc3[i]<- 1-error
}

#station 4
station4<-simulated[,4]
observed4<-observed[,4] 

sim4<-chunk2(station4, 100)

acc4 <- NULL
for (i in 1:100){
  sim <- unlist(sim4[i], use.names=FALSE)
  error <- mean(sim != observed4)
  acc4[i]<- 1-error
}

#station 5
station5<-simulated[,5]
observed5<-observed[,5] 

sim5<-chunk2(station5, 100)

acc5 <- NULL
for (i in 1:100){
  sim <- unlist(sim5[i], use.names=FALSE)
  error <- mean(sim != observed5)
  acc5[i]<- 1-error
}

mean(acc1)
mean(acc2)
mean(acc3)
mean(acc4)
mean(acc5)
