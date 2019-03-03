# Style Guide

# White space- where there are spaces with no code
# spaces are commas
# spaces around operators


# Naming
# using _, called snake case

# Line length
# < 80 characters per line

# Documentation and comments
# ' General description of the function'

library("dplyr")

data= data.frame(time= 1:100, runif(100,1,10))
head(data)

windows= c(1, 5, 10, 20)
counter= 0

for (window in windows){
  for (tmin in 1:max(data$time)- window){
    window_data<- filter(data, time >= tmin, time < tmin + window) 
  if (nrow(window_data) == 0){
    counter= counter + 1
    }
  }
}



# Debugging

# hypothesize a specific thing in the code
# and then make one change


#Import data
# clean data- remove missing weight
# Identify large vs small individuals
# Turn the data into a time series
# Plot th data

# Many functions
# get data
# size class, group by size_class, probably need to make a size class
#size class time series
#plots
#^ this is the final plot




get_size_class<- function (df){
  ts_datq<-
    df %>% 
    group_by(year,size_class) %>% 
    summarise(counts=n())
  return(ts_data)
}

library("dplyr")

data= read.csv ("daa/cocoli.txt", sep="\t")
window=10
min(data$y)
max(data$y)
ymins<- seq(0, max(data$y), window)
ymins<- seq(0, max(data$y), window)

results_lenth<- length(ymins) * length(xmin)

for (miny in seq(0, max(data$y), 10)){
  
  maxy<-miny + window
  subdata<-filter(data, y >=miny, y < miny + window )
  
  for (minx in seq (0, max(data$x, window))){
    
  }
  maxy<-minx + window
  subdata<-filter(data, 
                  y >= miny, 
                  y <  maxy ,
                  x >= minx,
                  x <  maxx)
}
