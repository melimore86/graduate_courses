## Growth model
library("tidyverse")

oys_data<- read.csv ("growth_model.csv", header=TRUE)
head(oys_data)

min(oys_data$total_length_cm)

max(oys_data$total_length_cm)

ggplot(data=oys_data, x=age_months) +
  labs(x= "Month", y= "Variable") +
  geom_point(aes(x=age_months,y=total_length_cm), shape=2) +
  geom_point(aes(x=age_months,y=temp_c), shape=1) +
  geom_point(aes(x=age_months,y=salinity_ppt), shape=3)


