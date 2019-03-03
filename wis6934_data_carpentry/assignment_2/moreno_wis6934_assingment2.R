# Mel Moreno, 2018-08-28

###Problem 1 - Basic Expressions
#Think about what value each of the following expressions #will return? 

#Question 1
2 - 10
#-8

#Question 2
3 * 5
#15

#Question 3
9 / 2
#4.5

#Question 4
5 - 3 * 2
#1

#Question 5
(5 - 3) * 2
#4

#Question 6
4 ** 2
#16

#Question 7
8 / 2 ** 2
#2

#1.2
#Did any of the results surprise you? 
#No they did not suprise me, the operations followed PEDMAS

#1.3
#Comments are added using a hashtag



### Problem 2 - Basic Variables
#Syvilagus audubonii (rabbit)
mass_lb <- 3.5
mass_kg <- mass_lb / 2.2046
mass_kg
# 1.5 kg

### Problem 3 - More Variables

#Neotoma albigula
#Question 1
#Adding a new section starting with a comment

#Question 2
grams <- 250

#Question 3
number <- 3

#Question 4
biomass <- grams * number

#Question 5
biomass <- biomass/1000

#Question 6
biomass
#0.75

# Are the variable names grams, number, and biomass the best choice? 
# No I would say that a better name might be neoalb_g (instead of grams) or neoalb_ind (instead of number), and neoalb_kg (instead of biomass). That was the names are descriptive and they are also short and easy to remember.


#Revised names of the previous code
neoalb_g <- 250
neoalb_ind <- 3
neoalb_kg <- neoalb_g * neoalb_ind
neoalb_kg <- neoalb_kg/1000
neoalb_kg
#0.75


### Problem 4 - Built in Functions

#Question 1
#The absolute value of -15.5.
abs(-15.5)
#15.5

# Question 2
#4.483847 rounded to one decimal place. The function round() takes two arguments, the number to be rounded and the number of decimal places.
help(round)
round(4.483847, digits =1)
#4.5

# Question 3
#3.8 rounded to the nearest integer. You don't have to specify the number of decimal places in this case if you don't want to, because round() will default to using 0 if the second argument is not provided. Look at help(round) or ?round to see how this is indicated.
round(3.8)
# 4

# Question 4
#"species" in all capital letters.
toupper("species")
#"SPECIES"

#Question 5
#"SPECIES" in all lower case letters.
tolower("SPECIES")
#"species"

#Question 6
#Assign the value of the square root of 2.6 to a variable. Then round the variable you've created to 2 decimal places and assign it to another variable. Print out the rounded value.
sq<-sqrt(2.6)
sq_new<-round(sq, digits=2)
sq_new
#1.61


#Challenge: Do the same thing as task 6 (immediately above), but instead of creating the intermediate variable, perform both the square root and the round on a single line by putting the sqrt() call inside the round() call.
sq_rd<-round(sqrt(2.6), digits=2)
sq_rd
#1.61

### Problem 5 - Modify the Code

site1_g_carbon_m2_day <- 5
site2_g_carbon_m2_day <- 2.3
site1_area_m2 <- 200
site2_area_m2 <- 450
site1_npp_day <- site1_g_carbon_m2_day * site1_area_m2
site2_npp_day <- site2_g_carbon_m2_day * site2_area_m2
site1_npp_day
site2_npp_day

#Question 1
#The sum of the total daily NPP for the two sites combined.
sites_sum<-site1_npp_day + site2_npp_day 
sites_sum
#2035

#Question 2
#The difference between the total daily NPP for the two sites. We only want an absolute difference, so use abs() function to make sure the number is positive.

sites_diff<- abs(site1_npp_day - site2_npp_day )
sites_diff
#35

# Question 3
#The total NPP over a year for the two sites combined.
sites_sum_year<- sites_sum *365
sites_sum_year
#742775

### Problem 6-  Code Shuffle

# To read the data in 
ppt_data <- read.csv("https://datacarpentry.org/semester-biology/data/gainesville-precip.csv", header = FALSE)

#Calculates the average precipitation in each month across years
monthly_mean_ppt <- colMeans(ppt_data)

#Plots the monthly averages as a simple line plot
plot(monthly_mean_ppt, type = "l", xlab = "Month", ylab = "Mean Precipitation")


### Problem 7- Bird Banding

number_of_birds <- c(28, 32, 1, 0, 10, 22, 30, 19, 145, 27, 
                     36, 25, 9, 38, 21, 12, 122, 87, 36, 3, 0, 5, 55, 62, 98, 32, 
                     900, 33, 14, 39, 56, 81, 29, 38, 1, 0, 143, 37, 98, 77, 92, 
                     83, 34, 98, 40, 45, 51, 17, 22, 37, 48, 38, 91, 73, 54, 46,
                     102, 273, 600, 10, 11)

# Question 1
#How many sites are there?
length(number_of_birds)
#61

# Question 2
#How many birds were counted at site 42?
number_of_birds[42]
#83

#Question 3
#What is the total number of birds counted across all of the sites?
sum(number_of_birds)
#4366

#Question 4
# What is the smallest number of birds counted?
min(number_of_birds)
#0

# Question 5
#  What is the largest number of birds counted?
max(number_of_birds)
#900

# Question 6
#  What is the average number of birds seen at a site?
mean(number_of_birds)
#71.57377

# Question 7
# How many birds were counted at the last site? Have the computer choose the last site automatically in some way, not by manually entering its position.
tail(number_of_birds, n=1)
#11

### Problem- 8 Shrub Volume Vectors
length <- c(2.2, 2.1, 2.7, 3.0, 3.1, 2.5, 1.9, 1.1, 3.5, 2.9)
width <- c(1.3, 2.2, 1.5, 4.5, 3.1, 2.8, 1.8, 0.5, 2.0, 2.7)
height <- c(9.6, 7.6, 2.2, 1.5, 4.0, 3.0, 4.5, 2.3, 7.5, 3.2)

# Question 1
#The volume of each shrub (i.e., the length times the width times the height).
volume <- width * height * length
volume
# 27.456 35.112  8.910 20.250 38.440 21.000 15.390  1.265
# 52.500 25.056

# Question 2
#The total volume of all of the shrubs.
vol_sum<- sum(width * height * length)
vol_sum
#245.379

# Question 3
#A vector of the height of shrubs with lengths greater than 2.5.
height[length>2.5]
# 2.2 1.5 4.0 7.5 3.2


### Problem 9 - Shrub Volume Data Frame 

shrub_data <- read.csv('assignment_2/shrub-dimensions-labeled.csv')

head(shrub_data)

# Question 1
# The shrub lengths
shrub_length<-(shrub_data$length)
shrub_length
#2.2 2.1 2.7 3.0 3.1 2.5 1.9 1.1 3.5 2.9

#Question 2
# The volume of each of the shrubs
shrub_width<-(shrub_data$width)

shrub_height<-(shrub_data$height)

shrub_volume<- (shrub_height * shrub_length * shrub_width)
shrub_volume

# 27.456 35.112  8.910 20.250 38.440 21.000 15.390  1.265 52.500
# 25.056