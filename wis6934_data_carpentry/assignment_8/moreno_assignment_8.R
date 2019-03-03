# Assignment 8
# Melissa Moreno
# October 11, 2018

# Exercise 1- Use and Modify with Apply

# 1 Write a function that takes length as an argument to get an estimate of mass values for the dinosaur Theropoda. Use the equation mass <- 0.73 * length ** 3.63, where 0.73 is a and 3.63 is b. Then use sapply() and the following vector of lengths to get a vector of estimated masses.

theropoda_lengths <- c(17.8013631070471, 20.3764452071665, 14.0743486294308, 25.65782386974, 26.0952008049675, 20.3111541103134, 17.5663244372533, 11.2563431277577, 20.081903202614, 18.6071626441984, 18.0991894513166, 23.0659685685892, 20.5798853467837, 25.6179254233558, 24.3714331573996, 26.2847248252537, 25.4753783544473, 20.4642089867304, 16.0738256364701, 20.3494171706583, 19.854399305869, 17.7889814608919, 14.8016421998303, 19.6840911485379, 19.4685885050906, 24.4807784966691, 13.3359960054899, 21.5065994598917, 18.4640304608411, 19.5861532398676, 27.084751999756, 18.9609366301798, 22.4829168046521, 11.7325716149514, 18.3758846100456, 15.537504851634, 13.4848751773738, 7.68561192214935, 25.5963348603783, 16.588285389794)


estimate_mass<-function(length){
  a<- 0.73
  b<- 3.63
  mass<- a * length ** b
  return(mass)
}


masses<-sapply(theropoda_lengths, estimate_mass)
masses


#2 Rewrite the function with a and b as arguments, then use mapply() to determine the mass estimates for the dinosaurs using the following vectors of a and b values for each dinosaur.


a_values <- c(0.759, 0.751, 0.74, 0.746, 0.759, 0.751, 0.749, 0.751, 0.738, 0.768, 0.736, 0.749, 0.746, 0.744, 0.749, 0.751, 0.744, 0.754, 0.774, 0.751, 0.763, 0.749, 0.741, 0.754, 0.746, 0.755, 0.764, 0.758, 0.76, 0.748, 0.745, 0.756, 0.739, 0.733, 0.757, 0.747, 0.741, 0.752, 0.752, 0.748)

b_values <- c(3.627, 3.633, 3.626, 3.633, 3.627, 3.629, 3.632, 3.628, 3.633, 3.627, 3.621, 3.63, 3.631, 3.632, 3.628, 3.626, 3.639, 3.626, 3.635, 3.629, 3.642, 3.632, 3.633, 3.629, 3.62, 3.619, 3.638, 3.627, 3.621, 3.628, 3.628, 3.635, 3.624, 3.621, 3.621, 3.632, 3.627, 3.624, 3.634, 3.621)


estimate_masses<-function(length, a,b){
  mass<- a * length ** b
  return(mass)
}

dino_masses<-mapply(estimate_masses, length= theropoda_lengths, a=a_values, b=b_values)
dino_masses


# Exercise 2- Crown Volume Calculation

library("dplyr")

tree_data <- read.csv("http://www.esapubs.org/archive/ecol/E095/064/TREE_SURVEYS.txt",
                      sep = '\t',
                      na.strings = c("dead", "missing", "MISSING",
                                     "NA", "?", "3.3."),
                      stringsAsFactors = FALSE)


# Acacia
#volume = 0.16 * HEIGHT^0.8 * pi * AXIS_1 * AXIS_2

#Balanites
#volume = 1.2 * HEIGHT^0.26 * pi * AXIS_1 * AXIS_2

#Other genera
#volume = 0.5 * HEIGHT^0.6 * pi * AXIS_1 * AXIS_2

# 1 Write a function called tree_volume_calc that calculates the canopy volume for the Acacia species in the dataset. To do so, use an if statement in combination with the str_detect() function from the stringr R package. The code str_detect(SPECIES, "Acacia") will return TRUE if the string stored in this variable contains the word “Acacia” and FALSE if it does not. This function will have to take the following arguments as input: SPECIES, HEIGHT, AXIS_1, AXIS_2. Then run the following line:

library("stringr")

tree_volume_calc<- function(SPECIES, HEIGHT, AXIS_1, AXIS_2) {
  if (str_detect(SPECIES, "Acacia")==TRUE) {
    volume = 0.16 * HEIGHT^0.8 * pi * AXIS_1 * AXIS_2
} else if (str_detect(SPECIES, "Balanites")==TRUE) {
  volume = 1.2 * HEIGHT^0.26 * pi * AXIS_1 * AXIS_2
} else {
  volume = 0.5 * HEIGHT^0.6 * pi * AXIS_1 * AXIS_2
}
  return(volume)
}


tree_volume_calc("Acacia_brevispica", 2.2, 3.5, 1.12)
# 3.702491

#2 Expand this function to additionally calculate canopy volumes for other types of trees in this dataset by adding if/else statements and including the volume equations for the Balanites genus and other genera. Then run the following lines:

tree_volume_calc("Balanites", 2.2, 3.5, 1.12) 
# 18.14041
tree_volume_calc("Croton", 2.2, 3.5, 1.12)
# 9.882335


# 3 Now get the canopy volumes for all the trees in the tree_data dataframe and add them as a new column to the data frame. You can do this using tree_volume_calc() and either mapply() or using dplyr with rowwise and mutate.

tree_data$VOLUME<-mapply(tree_volume_calc, SPECIES= tree_data$SPECIES, HEIGHT= tree_data$HEIGHT, AXIS_1= tree_data$AXIS_1, AXIS_2= tree_data$AXIS_2)
tree_data$VOLUME



# Exercise 3- Use and Modify with Loops 

theropoda_lengths <- c(17.8013631070471, 20.3764452071665, 14.0743486294308, 25.65782386974, 26.0952008049675, 20.3111541103134, 17.5663244372533, 11.2563431277577, 20.081903202614, 18.6071626441984, 18.0991894513166, 23.0659685685892, 20.5798853467837, 25.6179254233558, 24.3714331573996, 26.2847248252537, 25.4753783544473, 20.4642089867304, 16.0738256364701, 20.3494171706583, 19.854399305869, 17.7889814608919, 14.8016421998303, 19.6840911485379, 19.4685885050906, 24.4807784966691, 13.3359960054899, 21.5065994598917, 18.4640304608411, 19.5861532398676, 27.084751999756, 18.9609366301798, 22.4829168046521, 11.7325716149514, 18.3758846100456, 15.537504851634, 13.4848751773738, 7.68561192214935, 25.5963348603783, 16.588285389794)


# 1 Write a function that takes length as an argument to get an estimate of mass values for the dinosaur Theropoda (or copy the one you wrote as part of Use and Modify). Use the equation mass <- 0.73 * length ** 3.63, where 0.73 is a and 3.63 is b. Then use a for loop over the values of the following vector of lengths to print out the estimated masses for each length.

estimate_mass<-function(length){
  a<- 0.73
  b<- 3.63
  mass<- a * length ** b
  return(mass)
}

estimate_mass(theropoda_lengths)

for (length in theropoda_lengths){
  mass<- 0.73 * length ** 3.63
  print(mass)
  }

# 2 Write a new version of the function that takes a and b as arguments (or copy the one you wrote as part of Use and Modify), then use a for loop over an index to print the mass estimates for the dinosaurs using the following vectors of a and b values for each dinosaur.


a_values <- c(0.759, 0.751, 0.74, 0.746, 0.759, 0.751, 0.749, 0.751, 0.738, 0.768, 0.736, 0.749, 0.746, 0.744, 0.749, 0.751, 0.744, 0.754, 0.774, 0.751, 0.763, 0.749, 0.741, 0.754, 0.746, 0.755, 0.764, 0.758, 0.76, 0.748, 0.745, 0.756, 0.739, 0.733, 0.757, 0.747, 0.741, 0.752, 0.752, 0.748)

b_values <- c(3.627, 3.633, 3.626, 3.633, 3.627, 3.629, 3.632, 3.628, 3.633, 3.627, 3.621, 3.63, 3.631, 3.632, 3.628, 3.626, 3.639, 3.626, 3.635, 3.629, 3.642, 3.632, 3.633, 3.629, 3.62, 3.619, 3.638, 3.627, 3.621, 3.628, 3.628, 3.635, 3.624, 3.621, 3.621, 3.632, 3.627, 3.624, 3.634, 3.621)


estimate_masses<-function(length, a,b){
  a<-a_values
  b<-b_values
  mass<- a * length ** b
  return(mass)
}

estimate_masses(theropoda_lengths)


for (i in seq_along(theropoda_lengths)){
  a<-a_values
  b<-b_values
  mass<- a[i] * theropoda_lengths[i] ^ b[i]
  print (mass)
}


# 3 Write a new version of the loop from Task 2 that stores the resulting masses in a vector instead of printing them out. Once the loop is finished display the resulting vector.

dino_mass<- vector(mode="numeric",length=length(theropoda_lengths))

for (i in seq_along(theropoda_lengths)){
  a<-a_values
  b<-b_values
  mass<- a[i] * theropoda_lengths[i] ^ b[i] 
  dino_mass[i]<-mass
}

dino_mass

# Exercise 4- Multiple Files

#Installing from 
#source("https://bioconductor.org/biocLite.R")
#biocLite("ShortRead")
#biocLite("Biostrings")

# 1. Use list.files(), with full.names set to true, to generate a list of the names of all the sequence files. Then create a for loop that uses the above code to read in each sequence file and calculate it’s GC content. Store the resulting values in a data frame with one column with file names and a second column with GC contents.


library("ShortRead")
library("Biostrings")

archaea_files<-list.files(path= "assignment_8/archaea-dna",full.names=TRUE)

gc_percent<- function(data_file){
  reads <- readFasta(data_file)
  seq <- sread(reads)
  base_freq <- alphabetFrequency(seq)
  gc_contents<- (base_freq[1, "G"] + base_freq[1, "C"]) / sum(base_freq) * 100
  
}

gc_contents<- vector(length=length(archaea_files))

for(i in seq_along(archaea_files)){
 gc_contents[i]<-gc_percent(archaea_files[i])
}

results <- data.frame(archaea_files, gc_contents)
results

#Exercise-5 DNA or RNA Iteration

sequences = c("ttgaatgccttacaactgatcattacacaggcggcatgaagcaaaaatatactgtgaaccaatgcaggcg", "gauuauuccccacaaagggagugggauuaggagcugcaucauuuacaagagcagaauguuucaaaugcau", "gaaagcaagaaaaggcaggcgaggaagggaagaagggggggaaacc", "guuuccuacaguauuugaugagaaugagaguuuacuccuggaagauaauauuagaauguuuacaacugcaccugaucagguggauaaggaagaugaagacu", "gauaaggaagaugaagacuuucaggaaucuaauaaaaugcacuccaugaauggauucauguaugggaaucagccggguc")

dna_or_rna<- function(seq){
  if (grepl("t",seq)==TRUE) {
    print("DNA") 
  } else if(grepl("u",seq) == TRUE){
    print("RNA")
  } else{
    print("UKNOWN")
  }
}  

for(i in seq_along(sequences)){
  dna_or_rna(sequences[i])
}


# 2 Use the function and sapply to print the type of the sequences in the following list.


sapply(sequences, dna_or_rna)


# Exercise 6 Climate Space Iteration

library("ggplot2")
library("raster")
library("maps")
library("spocc")
library("dplyr")

# 1. Using the functions you created in Climate Space Rewrite iterate over the following list of species to create one plot per species from the list. Include a title for each plot that is the species name using the ggtitle() function. You can use any type of automated iteration that we’ve learned.

world_clim_rast<-getData('worldclim', var = 'bio', res = 10)
world_clim_rast_df<-as.data.frame (world_clim_rast)

world_clim_rast_df<-world_clim_rast_df%>% 
  dplyr::select(bio1, bio12) %>% 
  dplyr::rename(temperature=bio1, precipitation=bio12) %>%
  na.omit() %>% 
  sample_n(size= 10000)

world_clim_rast_df$temperature<- (world_clim_rast_df$temperature /10)

tree_plotting<-function(species){
  
  tree_df = occ(query = species,  
                from = "gbif",
                limit = 1000,
                has_coords = TRUE)
  
  tree_df = data.frame(tree_df$gbif$data)
  
  tree_df_df<-tree_df %>% 
    dplyr::rename(longitude= 2, latitude= 3) %>% 
    filter(!is.na(longitude), !is.na(latitude)) %>% 
    dplyr::filter(latitude != 0 & longitude != 0)%>% 
    dplyr::select(longitude,latitude)
  
  tree_clim<-raster::extract(world_clim_rast, tree_df_df)
  
  tree_clim<-as.data.frame(tree_clim)
  
  tree_clim<-tree_clim %>% 
    dplyr::rename(temperature=1, precipitation=12) %>% 
    filter(!is.na(temperature), !is.na(precipitation)) %>%
    mutate(temperature=temperature/10)
  
  plot<- ggplot()+
    labs(title= species)+
    geom_point(data= world_clim_rast_df ,aes(x=temperature , y=precipitation),alpha=0.3) +
    geom_point(data=tree_clim,aes(x=temperature , y=precipitation), color= "red") +
    xlab("Mean Annual Temperature (C)") +
    ylab("Mean Annual Precipitation (mm)")
  
  return(plot)
}


species <- c("Juniperus occidentalis", "Quercus alba", "Picea glauca", "Ceiba pentandra", "Quercus rubra", "Larrea tridentata", "Opuntia pusilla")


for (i in seq_along(species)){
  plot(tree_plotting(species[i]))
}


