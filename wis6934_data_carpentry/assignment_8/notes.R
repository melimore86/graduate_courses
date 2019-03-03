for (item in list_ofitems){
  do_something(item)
  
}

# Converting volumen into masses

volumes<-c(1.6,3,8)

for (volume in volumes){
  print(2.65* volume ^0.9)
}

#one out of ten numbers
for (i in 1:10){
  print(i)
}


# created a list of positions in the vector
volumes<-c(1.6,3,8)
b0<- c(2.65,1.28,3.29)
b1<- c(0.9,1.1,1.2)

for (i in seq_along(volumes)){
  print(b0[i]* volumes[i]^b1[i])
}

# Creating an empty vector

masses<- vector(length=length(volumes))


for (i in seq_along(volumes)){
  mass<-b0[i]* volumes[i]^b1[i]
  masses[i]<- mass
}


#for every other, or odd I mean
# seq(1,10, by=2) in the for loop


# Use raster:: as.data.frame in question #6

# Looping over a bunch of files


url<- "http://www.datacarpentry.org/semester-biology/data/collar-data-2016-01.zip"
      
download.file(url=url,"assignment_8/collar.zip")

unzip("assignment_8/collar.zip")

count_obs<-function (data_file_name){
  data<-read.csv(data_file_name)
  record_count<-nrow(data)
  return(record_count)
}

count_obs("assignment_8/collar-data-2016-01-01.txt")


collar_data_files<-list.files(pattern = "collar-data.*txt*")

#Something to store the results in 
counts<- vector(mode="numeric",
                length=length(collar_data_files))

for (i in seq_along(collar_data_files)){
  count<- count_obs(collar_data_files[i])
  counts[i]<- count
}

#Unlist and turn it into a vector
counts<- unlist(lapply(collar_data_files,count_obs))
counts


