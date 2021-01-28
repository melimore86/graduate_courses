library(dplyr)

rd<- read.csv("rd.csv", header= FALSE)
str(rd)

rd$V2<- as.numeric(rd$V2)

rd<- rd %>% 
  na.omit(V2)

min(rd$V2)
max(rd$V2)
mean(rd$V2)
