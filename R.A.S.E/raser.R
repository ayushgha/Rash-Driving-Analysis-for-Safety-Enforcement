getwd()
setwd('../rfiles/rase')
rdata<-read.csv('rasedata1.csv', stringsAsFactors = FALSE)
str(rdata)
head(rdata)

speed<-c(0)
rdata<-cbind(rdata,speed)
rdata
for(i in 1:nrow(rdata)){
  if(i==1){
    rdata$speed[i] = 0
  }else{
    rdata$speed[i] = (abs((rdata$speed[i-1])^2 - 2*rdata$Ax[i]*rdata$x[i]))^(0.5)
  }}
summary(rdata$speed)


ggplot(rdata, aes(x=rdata$time, group = 1)) + 
  geom_line(aes(y = rdata$speed), colour = "blue") + 
  geom_line(aes(y = summary(rdata$speed)[5]), colour = "red") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Speed vs time") + xlab("Time") + ylab("Speed")


ggplot(rdata, aes(x=rdata$time, group = 1)) + 
  geom_line(aes(y = rdata$Ax), colour = "blue") + 
  geom_line(aes(y = summary(rdata$Ax)[2]), colour = "red") +
  geom_line(aes(y = summary(rdata$Ax)[5]), colour = "red") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Acceletration vs time") + xlab("Time") + ylab("Speed")
summary(rdata$Ax)

exceed<-0
max<-rdata$speed[1]
for(i in 1:nrow(rdata)){
    if(rdata$speed[i] > summary(rdata$speed)[5]){
      exceed=exceed+1
      print(rdata$speed[i])
    }
  if(rdata$speed[i]>max){
      max<-rdata$speed[i]
    }
  }
max
exceed
