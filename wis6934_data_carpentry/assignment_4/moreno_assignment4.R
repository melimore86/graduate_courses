#### Assignment 4
#### Moreno, Melissa
#### Data Visualization

library(ggplot2)


## Exercise 1- Mass vs Metabolism 

size_mr_data <- data.frame(
  body_mass = c(32000, 37800, 347000, 4200, 196500, 100000,
                4290, 32000, 65000, 69125, 9600, 133300, 150000, 407000,
                115000, 67000,325000, 21500, 58588, 65320, 85000, 135000,
                20500, 1613, 1618),
  metabolic_rate = c(49.984, 51.981, 306.770, 10.075, 230.073, 
                     148.949, 11.966, 46.414, 123.287, 106.663, 20.619, 180.150, 
                     200.830, 224.779, 148.940, 112.430, 286.847, 46.347,
                     142.863, 106.670, 119.660, 104.150, 33.165, 4.900, 4.865),
  family = c("Antilocapridae", "Antilocapridae", "Bovidae",
             "Bovidae", "Bovidae", "Bovidae", "Bovidae", "Bovidae",
             "Bovidae", "Bovidae", "Bovidae", "Bovidae", "Bovidae",
             "Camelidae", "Camelidae", "Canidae", "Cervidae",
             "Cervidae", "Cervidae", "Cervidae", "Cervidae", "Suidae",
             "Tayassuidae", "Tragulidae", "Tragulidae"))


# 1. A plot of body mass vs. metabolic rate

ggplot(data= size_mr_data, aes ( x = body_mass, y = metabolic_rate))+
  geom_point() +
  xlab("Body Mass") +
  ylab("Metabolic Rate")

# 2. A plot of body mass vs. metabolic rate, with logarithmically scaled axes (this stretches the axis, but keeps the numbers on the original scale), and the point size set to 3.

ggplot(data= size_mr_data, aes ( x = body_mass, y = metabolic_rate))+
  geom_point(size=3) +
  scale_y_log10() +
  scale_x_log10() +
  xlab("Body Mass") +
  ylab("Metabolic Rate")

# 3. The same plot as (2), but with the different families indicated using color.

ggplot(data= size_mr_data, aes ( x = body_mass, y = metabolic_rate, color=family))+
  geom_point(size=3) +
  scale_y_log10() +
  scale_x_log10() +
  xlab("Body Mass") +
  ylab("Metabolic Rate")

# 4.The same plot as (2), but with the different families each in their own subplot.

ggplot(data= size_mr_data, aes ( x = body_mass, y = metabolic_rate))+
  geom_point(size=3) +
  scale_y_log10()  +
  scale_x_log10()  +
  xlab("Body Mass") +
  ylab("Metabolic Rate") +
  facet_wrap (~family)


## Exercise 2 - Adult vs Newborn Size

mammal<- read.csv ("http://esapubs.org/archive/ecol/E084/093/Mammal_lifehistories_v2.txt", sep = "\t", stringsAsFactors = F, na.strings = c("-999", "-999.00"))


#1.Graph adult mass vs. newborn mass. Label the axes with clearer labels than the column names.

ggplot(data= mammal, aes( x= mass.g., y= newborn.g.)) +
  geom_point() +
  xlab("Adult Mass (g)")  +
  ylab ("Newborn Mass (g)")

#2.It looks like there’s a regular pattern here, but it’s definitely not linear. Let’s see if log-transformation straightens it out. Graph adult mass vs. newborn mass, with both axes scaled logarithmically. Label the axes.

ggplot(data= mammal, aes( x= mass.g., y= newborn.g.)) +
  geom_point() +
  xlab("Adult Mass (g)")  +
  ylab ("Newborn Mass (g)") +
  scale_y_log10()  +
  scale_x_log10()

#3.This looks like a pretty regular pattern, so you wonder if it varies among different groups. Graph adult mass vs. newborn mass, with both axes scaled logarithmically, and the data points colored by order. Label the axes.
  
  ggplot(data= mammal, aes( x= mass.g., y= newborn.g., color= order)) +
    geom_point() +
    xlab("Adult Mass (g)")  +
    ylab ("Newborn Mass (g)") +
    scale_y_log10() +
    scale_x_log10()
  

#4.Coloring the points was useful, but there are a lot of points and it’s kind of hard to see what’s going on with all of the orders. Use facet_wrap to create a subplot for each order.

  ggplot(data= mammal, aes( x= mass.g., y= newborn.g.)) +
    geom_point() +
    xlab("Adult Mass (g)")  +
    ylab ("Newborn Mass (g)") +
    scale_y_log10()  +
    scale_x_log10() +
    facet_wrap(~order)
  
  
#5.Now let’s visualize the relationships between the variables using a simple linear model. Create a new graph like your faceted plot, but using geom_smooth to fit a linear model to each order. You can do this using the optional argument method = "lm" in geom_smooth.
  
  
  ggplot(data= mammal, aes( x= mass.g., y= newborn.g.)) +
    geom_point() +
    geom_smooth(method = "lm") +
    xlab("Adult Mass (g)")  +
    ylab ("Newborn Mass (g)") +
    scale_y_log10()  +
    scale_x_log10() +
    facet_wrap(~order)
  
  
## Exercise 3 - Sexual Dimorphism Exploration

bird <- read.csv ('http://www.esapubs.org/archive/ecol/E088/096/avian_ssd_jan07.txt', sep = "\t", stringsAsFactors = F, na.strings = c("-999", "-999.00"))


#1.Create a histogram of female masses (they are in the F_mass column). Change the x axis label to "Female Mass(g)".

ggplot(data= bird, aes(x=F_mass)) +
  geom_histogram() +
  xlab("Female Mass(g)")


#2. A few really large masses dominate the histogram so create a log10 scaled version. Change the x axis label to "Female Mass(g)" and the color of the bars to blue (using the fill = "blue" argument).

ggplot(data= bird, aes(x=F_mass)) +
  geom_histogram(fill = "blue") +
  xlab("Female Mass(g)") +
  scale_x_log10()


#3.Now let’s add the data for male birds as well. Create a single graph with histograms of both female and male body mass. Due to the way the data is structured you’ll need to add a 2nd geom_histogram() layer that specifies a new aesthetic. To make it possible to see both sets of bars you’ll need to make them transparent with the optional argument alpha = 0.3.


ggplot(data= bird, aes(x=F_mass)) +
  geom_histogram(fill = "blue", alpha= 0.3) +
  geom_histogram(aes(x= M_mass), fill = "red", alpha= 0.3) +
  xlab("Mass(g)") +
  scale_x_log10()


#4. These distributions seem about the same, but this is all birds together so it might be difficult to see any patterns. Use facet_wrap() to make one subplot for each family.ggplot(data= bird, aes(x=F_mass)) +

ggplot(data= bird, aes(x=F_mass)) +
  geom_histogram(fill = "blue", alpha= 0.3) +
  geom_histogram(aes(x= M_mass), fill = "red", alpha= 0.3) +
  xlab("Mass(g)") +
  scale_x_log10() +
  facet_wrap(~Family)

#5. Make the same graph as in the last task, but for wing size instead of mass. Do you notice anything strange? If so, you may have gotten caught by the use of non-standard null values. If you already noticed and fixed this, Nice Work! If not, you can use the optional na.strings = c(“-999”, “-999.0”) argument in read.csv() to tell R what value(s) indicated nulls in a dataset.

ggplot(data= bird, aes(x=F_wing)) +
  geom_histogram(fill = "blue", alpha= 0.3) +
  geom_histogram(aes(x= M_wing), fill = "red", alpha= 0.3) +
  xlab("Wing Size") +
  scale_x_log10() +
  facet_wrap(~Family)


## Exercise 4 -Sexual Dimorphism Data Manipulation

bird_size<- read.csv("http://www.esapubs.org/archive/ecol/E088/096/avian_ssd_jan07.txt", sep = "\t", 
                     na.strings = c("-999", "-999.0"), stringsAsFactors = F)

#1.In Sexual Dimorophism Exploration you created a plot of the histograms of female and male masses by family. This resulted in a lot of plots, but many of them had low sample sizes.

#Use dplyr to filter out null values for mass, group the the data by family, and then filter the grouped data to return data only for families with more than 25 species. Add a comment to the top of the block of code describing what it does.
library(dplyr)

#Creating a new variable, grouping by family, removing all families with species < 25, also removing female and male mass that has NAs
bird_large_size<-bird_size %>% 
  group_by(Family)  %>%
  filter(!is.na(M_mass), !is.na(F_mass))%>% 
  summarize (species_num= n())%>% 
  filter(species_num > 25)

bird_join<- inner_join(bird_large_size, bird_size, by= "Family")

#Now, remake your original graph using only the data on families with greater than 25 species.

ggplot(data= bird_join, aes(x=F_mass)) +
  geom_histogram(fill = "blue", alpha= 0.3) +
  geom_histogram(aes(x= M_mass), fill = "red", alpha= 0.3) +
  xlab("Mass(g)") +
  scale_x_log10() +
  facet_wrap(~Family)

#2. Sexual size dimorphism doesn’t seem to show up clearly when visually comparing the distributions of male and female masses across species. Maybe the differences among species are too large relative to the differences between sexes to see what is happening; so, you decide to calculate the difference between male and female masses for each species and look at the distribution of those values for all species in the data.
  
#In the original data frame, use mutate() to create a new column which is the relative size difference between female and male masses

#(F_mass - M_mass) / F_mass

#and then make a single histogram that shows all of the species-level differences. Add a vertical line at 0 difference for reference.

bird_mutate<- bird_size %>%
  mutate(diff=(F_mass - M_mass) / F_mass)

ggplot(data= bird_mutate, aes(x=diff)) +
  geom_histogram() +
  geom_vline(xintercept = 0) +
  xlab("Relative Mass")

#3. Combine the two other tasks to produce histograms of the relative size difference for each family, only including families with more than 25 species.

bird_larger<-bird_size %>% 
  group_by(Family)  %>%
  filter(!is.na(M_mass), !is.na(F_mass))%>% 
  summarize (species_num= n())%>% 
  filter(species_num > 25)

birder_join<- inner_join(bird_larger, bird_size, by= "Family")

birder_mutate<- birder_join %>%
  mutate(diff=(F_mass - M_mass) / F_mass)

bird_family<-ggplot(data= birder_mutate, aes(x=diff)) +
  geom_histogram() +
  geom_vline(xintercept = 0) +
  xlab("Relative Mass") +
  facet_wrap(~Family)

bird_family

#4. Save the figure from task 3 as a jpg file with the name sexual_dimorphism_histogram.jpg.
ggsave(bird_family, path="assignment_4/fig", filename= "bird_family.png", width= 6)

