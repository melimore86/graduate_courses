library("dplyr")

fish<-read.csv("assignment_10/data/Gaeta_etal_CLC_data.csv", header = TRUE)

# If one condition is > 200 then it will be bigm if else it will be small
fish_cat<- fish %>% 
  mutate(length_cat= ifelse(length> 300, "big", "small"))
