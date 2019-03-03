#### Assignment 3- Data Visualization
#### Mel Moreno

# Problem 1 Shrub Volume Data Basics

shrub<- read.csv("assignment_3/shrub-volume-data.csv", header=T, stringsAsFactors = F)

# Question 1
# Check the column names in the data using the function names()

names(shrub)
#"site"       "experiment" "length"     "width"      "height" 

# Question 2
# Use str() to show the structure of the data frame and its individual columns

str(shrub)
#'data.frame':	12 obs. of  5 variables:
#  $ site      : int  1 1 1 2 2 2 3 3 3 4 ...
#$ experiment: int  1 2 3 1 2 3 1 2 3 1 ...
#$ length    : num  2.2 2.1 2.7 3 3.1 2.5 1.9 1.1 3.5 2.9 ...
#$ width     : num  1.3 2.2 1.5 4.5 3.1 2.8 1.8 0.5 2 2.7 ...
#$ height    : num  9.6 7.6 2.2 1.5 4 3 4.5 2.3 7.5 3.2 ...

# Question 3
# Print out the first few rows of the data using the function head().
head(shrub)

#site experiment length width height
#    1          1    2.2   1.3    9.6
#    1          2    2.1   2.2    7.6
#    1          3    2.7   1.5    2.2
#    2          1    3.0   4.5    1.5
#    2          2    3.1   3.1    4.0
#    2          3    2.5   2.8    3.0



# Using `dplyr` for the following sections

# Question 4
# Select the data from the length column and print it out.
library("dplyr")

shrub_length <- shrub %>%
  select(length)

shrub_length

#   length
#     2.2
#     2.1
#     2.7
#     3.0
#     3.1
#     2.5
#     1.9
#     1.1
#     3.5
#     2.9
#     4.5
#     1.2

# Question 5
# Select the data from the site and experiment columns and print it out.

shrub_site_exp<- shrub %>%
  select(site, experiment)

shrub_site_exp

#site   experiment
#     1          1
#     1          2
#     1          3
#     2          1
#     2          2
#     2          3
#     3          1
#     3          2
#     3          3
#     4          1
#     4          2
#     4          3


# Question 6
# Filter the data for all of the plants with heights greater than 5 and print out the result.

shrub_larger_5<- shrub %>%
  filter(height > 5)

shrub_larger_5
#site experiment length width height
#    1          1    2.2   1.3    9.6
#    1          2    2.1   2.2    7.6
#    3          3    3.5   2.0    7.5
#    4          2    4.5   4.8    6.5

# Question 7
# Create a new data frame called shrub_data_w_vols that includes all of the original data and a new column containing the volumes, and display it.

shrub_data_w_vols<- shrub %>%
  mutate(volume= (length*width*height))

shrub_data_w_vols

#site experiment length width height  volume
#     1          1    2.2   1.3    9.6  27.456
#     1          2    2.1   2.2    7.6  35.112
#     1          3    2.7   1.5    2.2   8.910
#     2          1    3.0   4.5    1.5  20.250
#     2          2    3.1   3.1    4.0  38.440
#     2          3    2.5   2.8    3.0  21.000
#     3          1    1.9   1.8    4.5  15.390
#     3          2    1.1   0.5    2.3   1.265
#     3          3    3.5   2.0    7.5  52.500
#     4          1    2.9   2.7    3.2  25.056
#     4          2    4.5   4.8    6.5 140.400
#     4          3    1.2   1.8    2.7   5.832


# Problem 2
# Shrub Volume Aggregation

# This code calculates the average height of a plant at each site:
by_site <- group_by(shrub, site)
avg_height <- summarize(by_site, avg_height = mean(height))

# Question 1
# Modify the code to calculate and print the average height of a plant in each experiment.

# Going to modify this in 1 line, instead of breaking it up

shrub_avg_height<- shrub %>%
  group_by (experiment) %>%
  summarise(avg_height = mean(height))

shrub_avg_height

#experiment avg_height
#
#        1       4.70
#        2       5.10
#        3       3.85

# Question 2
# Use max() to determine the maximum height of a plant at each site.

shrub_max_site<- shrub %>%
  group_by (site) %>%
  summarise(max_height = max(height))
shrub_max_site

#site max_height
#     1       9.60
#     2       4.00
#     3       7.50
#     4       6.50

# Problem 3 Shrub Volume

# Import the experiments data and use inner_join

exp<- read.csv ("assignment_3/shrub-volume-experiments-table.csv", header= T, stringsAsFactors = F)

shrub_exp<- inner_join(shrub, exp, by = "experiment")
shrub_exp


#site experiment length width height manipulation
#     1          1    2.2   1.3    9.6      control
#     1          2    2.1   2.2    7.6         burn
#     1          3    2.7   1.5    2.2      rainout
#     2          1    3.0   4.5    1.5      control
#     2          2    3.1   3.1    4.0         burn
#     2          3    2.5   2.8    3.0      rainout
#     3          1    1.9   1.8    4.5      control
#     3          2    1.1   0.5    2.3         burn
#     3          3    3.5   2.0    7.5      rainout
#     4          1    2.9   2.7    3.2      control
#     4          2    4.5   4.8    6.5         burn
#     4          3    1.2   1.8    2.7      rainout

# Problem 4 Portal Data Manipulation
sur<- read.csv ("assignment_3/surveys.csv", header= T, stringsAsFactors = F)

# Question 1 
# Use select() to create a new data frame with just the year, month, day, and species_id columns in that order.

survey_df<- sur %>%
  select(year, month,day,species_id)
survey_df

# Question 2
# Use mutate(), select(), and na.omit() to create a new data frame with the year, species_id, and weight in kilograms of each individual, with no null weights.

survey_mu_na<- sur %>%
  mutate(wgt_kg=(weight/1000)) %>%
  select(year,species_id, wgt_kg) %>%
  na.omit(weight)
survey_mu_na
  
# Question 3 
# Use the filter() function to get all of the rows in the data frame for the species ID SH

survey_filter<- sur %>%
  filter(species_id == "SH")
survey_filter


# Question 4
# Use the group_by() and summarize() functions to get a count of the number of individuals in each species ID.

survey_group<- sur %>%
  group_by(species_id) %>%
  summarize(count=n())
survey_group

# Question 5
# Use the group_by() and summarize() functions to get a count of the number of individuals in each species ID in each year.

survey_year_group<- sur %>%
  group_by(species_id, year) %>%
  summarize(count=n())
survey_year_group

# Question 6
# Use the filter(), group_by(), and summarize() functions to get the mean mass of species DO in each year.

survey_filter_group_summ<- sur %>%
  filter(species_id == "DO") %>%
  group_by(year) %>%
  summarize(mean_mass= mean(weight)) %>%
  na.omit(weight) # added this because there are many NAs within the species_id == "DO", might be less NAs in weight for other species
survey_filter_group_summ

#This will not generate the same table as the output in the Data Carpentry Assignment 3, but it is correct for the species_id == "DO"

# Problem 5 Fix the Code 

# Question 1
# Fix the errors in the code so that it does what it’s supposed to
# The following code is supposed to import the shrub volume data and calculate the average shrub volume for each site and, separately, for each experiment

shrub_5<- read.csv("assignment_3/shrub-volume-data.csv", header=T, stringsAsFactors = F)

shrub_5 %>%
  mutate(volume = length * width * height) %>%
  group_by(site) %>%
  summarize(mean_volume = mean(volume))


shrub_5 %>%
  mutate(volume = length * width * height) %>%
  group_by(experiment) %>%
  summarize(mean_volume = mean(volume))


# Question 2
# Add a comment to the top of the code explaining what it does

# This code is importing the .csv, and notifiying R that there is a header, and not to have any factors in the data when importing 
shrub_5<- read.csv("assignment_3/shrub-volume-data.csv", header=T, stringsAsFactors = F)

# This code will calculate the volume from the length, width, and height, grouping by site, and them summarizing the mean volume 
shrub_5 %>%
  mutate(volume = length * width * height) %>%
  group_by(site) %>%
  summarize(mean_volume = mean(volume))

# This code will calculate the volume from the length, width, and height, grouping by experiment, and them summarizing the mean volume 
shrub_5 %>%
  mutate(volume = length * width * height) %>%
  group_by(experiment) %>%
  summarize(mean_volume = mean(volume))


# Problem 6 Portal Data Joins

# Importing data
surveys<- read.csv("assignment_3/surveys.csv", header=T, stringsAsFactors = F)
species<- read.csv("assignment_3/species.csv", header=T, stringsAsFactors = F)
plots<- read.csv("assignment_3/plots.csv", header=T, stringsAsFactors = F)


# Question 1
# Use inner_join() to create a table that contains the information from both the surveys table and the species table.

sur_species <- inner_join(surveys, species, by = "species_id")
sur_species

# Question 2
# Use inner_join() twice to create a table that contains the information from all three tables.

sur_spe_plot<- inner_join(sur_species, plots, by = "plot_id")
sur_spe_plot

# Question 3
# Use inner_join() and filter() to get a data frame with the information from the surveys and plots tables where the plot_type is Control.

sur_plot<- inner_join(surveys, plots, by = "plot_id")
sur_plot

sur_plot_fil <- sur_plot %>%
  filter(plot_type == "Control")
sur_plot_fil

# Question 4
# We want to do an analysis comparing the size of individuals on the Control plots to the Long-term Krat Exclosures. Create a data frame with the year, genus, species, weight and plot_type for all cases where the plot type is either Control or Long-term Krat Exclosure. Only include cases where Taxa is Rodent. Remove any records where the weight is missing.


sur_spe_plot_4 <- sur_spe_plot %>%
  select(year, genus, species, weight, plot_type) %>%
  filter(plot_type == "Control"| plot_type == "Long-term Krat Exclosure") %>%
  na.omit(weight)
sur_spe_plot_4

