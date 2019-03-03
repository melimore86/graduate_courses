# Assignment 9
# Melissa Moreno

# Exercise 1- Format the Code

library("dplyr")
library("ggplot2")

# calculates percent value of a group of interest. first vector arg is the group of interest, the second is the total population.

percent_value<- function(sub_list, total_list) {
  fraction<- sum(sub_list) / sum(total_list)
  percent<- round(fraction, 3)*100
  return(percent)
}

# Reading in the trees data information
trees<- read.csv("http://www.esapubs.org/archive/ecol/E090/251/datafiles/swamp_all_modern.txt", sep = '\t')

# Reading in species data information
species<- read.csv("http://www.esapubs.org/archive/ecol/E090/251/datafiles/species_codes.txt", sep = '\t')

# Summary for trees

trees<- mutate(trees, mass = 0.124 * DBH ** 2.53)

trees_grouped<- group_by(trees, Year, Plot, Species)

tree_summary<- summarize(trees_grouped, col_1 = n(), col_2 = round(mean(DBH), 2), col_3 = round(sum(mass), 0))


#Species ranking
species_rank<- data.frame(year=c(), sp_code=c(), percent_ind=c(), percent_mass=c())

# For loop that will use the year of the tree to calculate the percent values and rbind with species rank
for (year in unique(tree_summary$Year)) { 
  target_year <- filter(tree_summary, Year == year)
for (sp_code in unique(tree_summary$Species)) {
  spp<-filter( target_year, Species==sp_code )
  percent_ind<- percent_value(spp$col_1, 
                              target_year$col_1 )
  percent_mass<- percent_value(spp$col_3, 
                               target_year$col_3 )
  species_rank<- rbind( species_rank, 
                        data.frame(year, sp_code, percent_ind, percent_mass) )
}
}

# Joining species by sp_code
species_rank <- left_join(species_rank, species, by = c("sp_code" = "SPCODE"))

# Plotting the species 
ggplot(species_rank, aes(x = percent_ind, 
                         y = percent_mass)) +
  geom_point(aes(color = factor(SPECIES))) +
  facet_grid(. ~ year) +
  labs(x = "Percent Individuals", 
       y = "Percent mass", color = "Species")

# Exercise 2-Dinosaur Size Distribution 

# Download the data, estimate the mass of each species, and then make a histogram of these masses with a logarithmically scaled size axis to reproduce their Figure 2a.

# This should be done in a maximally automated way. The equations listed above should only need be entered once and the code should automatically use the right set of parameters based on the Clade data to estimate the mass of each species.

#Ornithischia: a = 0.002 and b = 3.0587
#Sauropodomorpha: a = 0.509 and b = 2.3459
#Theropoda: a = 0.0007 and b = 3.1854

library("dplyr")
library("ggplot2")

dino<-read.csv("assignment_9/data/dinosaur_femur_lengths.csv", header= TRUE)


get_mass<-function(FL, Clade){
  if (Clade== "Ornithischia"){
    mass<-0.002 * FL ^ 3.0587
  } else if (Clade== "Sauropodomorpha" ){
    mass<- 0.509 * FL ^ 2.3459
  } else if (Clade== "Theropoda" ){
    mass<- 0.0007 * FL ^ 3.5184
  }
  return(mass)
}


masses<- vector(mode= "numeric", length=nrow(dino))


for (i in 1:nrow(dino)){
 FL<- dino[i,4]
 Clade<- dino[i,1]
  mass<-get_mass (Clade= Clade, FL= FL)
  masses[i]<- mass
  }

mass_df<- as.data.frame(masses)

ggplot(data= mass_df, aes(x=masses)) + 
  geom_histogram()+
  scale_x_log10()


# Exercise 3 - Cocili Data Exploration 

library("dplyr")

data<- read.csv ("assignment_9/data/cocoli/cocoli.txt", sep="\t")
species_name<- read.csv("assignment_9/data/cocoli/cocolisp.txt", sep="\t")


# 1 Make a single plot showing the location of each tree for all species with more than 100 individuals. Each species should be in its own subplot (i.e., facet). Label the subplots with the genus and species names, not the species code. Scale the size of the point by its stem diameter (use dbh1) so that larger trees display as larger points. Have the code save the plot in a figures folder in your project.

species_name<- species_name %>% 
  mutate(genus_species= paste( genus, species)) %>% 
  select(spcode, genus_species)

#Combining with inner joine the whole species name with species code
all_species<- inner_join(data,species_name, by = "spcode")

#joining the multi

many_species<- all_species %>% 
  select(genus_species,x,y,dbh1) %>% 
  group_by (genus_species) %>% 
  add_count(genus_species) %>% 
  filter(n> 100 )

min(many_species$dbh1)
max(many_species$dbh1)


ggplot(many_species, aes(x=x, y=y,size= dbh1))+
  geom_point() +
  #scale_size(range=c(-9,1262)) +
  facet_wrap(~ genus_species)
  

ggsave("assignment_9/figures/cocoli_species.png", width=13, height=10)
  
  
  
