### Assignment 7
### Mel Moreno
### Data Carpentry
### October 2, 2018


# Exercise 1- Writing Functions 

#Write a function that converts pounds to grams (there are 453.592 grams in one pound). It should take a value in pounds as the input and return the equivalent value in grams (i.e., the number of pounds times 453.592). Use that function to calculate how many grams there are in 3.75 pounds.


grams_from_pounds <- function(pounds){
  grams <- pounds * 453.592
  return(grams)
}
grams_from_pounds(3.75)
# 1700.97

# Exercise 2 - Use and Modify

#1 Add a comment to this function so that you know what it does.

#Comment: This function estimates the mass of an organism in kg based on it's length (in meters), so the function requires the length, and from there it will calculate the mass, and then return the mass

get_mass_from_length_theropoda <- function(length){
  mass <- 0.73 * length ** 3.63
  return(mass)
}


#2 Use this function to print out the mass of a Spinosaurus that is 16 m long based on its reassembled skeleton. Spinosaurus is a predator that is bigger, and therefore, by definition, cooler, than that stupid Tyrannosaurus that everyone likes so much.

get_mass_from_length_theropoda(16)
#17150.56

#3 Create a new version of this function called get_mass_from_length() that estimates the mass of an organism in kg based on its length in meters by taking length, a, and b as parameters. To be clear we want to pass the function all 3 values that it needs to estimate a mass as parameters. This makes it much easier to reuse for all of the non-theropod species. Use this new function to estimate the mass of a Sauropoda (a = 214.44, b = 1.46) that is 26 m long.


get_mass_from_length <- function(length){
  mass <- 214.44 * length ** 1.46
  return(mass)
}

get_mass_from_length(26)
#24955.54

# Exercise 3- Combining Functions 

#1 Measuring things using the metric system is the standard approach for scientists, but when communicating your results more broadly it may be useful to use different units (at least in some countries). Write a function that converts kilograms into pounds (there are 2.205 pounds in a kilogram). Use that function along with your dinosaur mass function from Use and Modify to estimate the weight, in pounds, of a 12 m long Stegosaurus (12 m is about as big as they come and nothing gets folks excited like a giant dinosaur). In Stegosauria, a has been estimated as 10.95 and b has been estimated as 2.64 (Seebacher 2001).

weight_in_pounds_of_steg<- function(length){
  mass <- 10.95 * length ** 2.64
  pounds <- mass * 2.205
  return(pounds)
}

weight_in_pounds_of_steg(12)
# 17055.37

# Exercise 4 - Choice Operator

w <- 10.2
x <- 1.3
y <- 2.8
z <- 17.5
dna1 <- "attattaggaccaca"
dna2 <- "attattaggaacaca"
colors <- c("green", "pink", "red")


#1. w is greater than 10

if (w > 10) {
  print(TRUE)
} else {
  print(FALSE)
}

w > 10

#2. w + x is less than 15

if ("green" %in% colors) {
  print(TRUE)
} else {
  print(FALSE)
}


"green" %in% colors

#3. x is greater than y

if (x >y ) {
  print(TRUE)
} else {
  print(FALSE)
}

x >y

#4 .2 * x + 0.2 is equal to y

if (0.2 * x + 0.2 == y) {
  print(TRUE)
} else {
  print(FALSE)
}

0.2 * x + 0.2 == y


#5. dna1 is the same as dna2

if (dna1 == dna2) {
  print(TRUE)
} else {
  print(FALSE)
}

dna1 == dna2

#6. dna1 is not the same as dna2

if (dna1 != dna2) {
  print(TRUE)
} else {
  print(FALSE)
}

dna1 != dna2

#7. w is greater than x, and y is greater than z


if (w > x && y >z) {
  print(TRUE)
} else {
  print(FALSE)
}

w > x && y >z


#8. x times w is between 13.2 and 13.5

if (x* w > 13.2 && x*w < 13.5) {
  print(TRUE)
} else {
  print(FALSE)
}

x* w > 13.2 && x*w < 13.5


#9. dna1 is longer than 5 bases (use nchar() to figure out how long a string is), or z is less than w * x


if (nchar(dna1) | z< w*x) {
  print(TRUE)
} else {
  print(FALSE)
}

nchar(dna1) | z< w*x


# Exercise 5  - Simple If Statement

# 1 Use the %in% operator to write a conditional statement that checks to see if thesis_data.csv is in this list.

'thesis_data.csv' %in% list.files()

# 2 Write an if statement that loads the file using read.csv() only if the file exists.

if (file.exists('thesis_data.csv')) {
 thesis<-read.csv ('thesis_data.csv', header= TRUE)
}

# 3 Add an else clause that prints “OMG MY THESIS DATA IS MISSING. NOOOO!!!!” if the file doesn’t exist

if (file.exists('thesis_data.csv')) {
  thesis<-read.csv ('thesis_data.csv', header= TRUE)
} else {
  print("OMG MY THESIS DATA IS MISSIN NOOOO!!!!")
}


#4 Make sure your actual thesis data is backed up.

#You got it!


# Exercise 6 - Size Estimates by Name

get_mass_from_length_by_name<- function(length, group){
  if (group == "Stegosauria") {
  mass <- 10.95 * length ** 2.64
} else if (group== "Theropoda"){
  mass <- 0.73 * length ** 3.63
} else if (group=="Sauropoda"){
  mass <- 214.44 * length ** 1.46
}
  return(mass)
} 

get_mass_from_length_by_name(length=10,group="Stegosauria")
# 4779.848

get_mass_from_length_by_name(length=8,group="Theropoda")
# 1385.286

get_mass_from_length_by_name(length=12,group="Sauropoda")
# 8070.685


#Challenge-  If the name doesn’t match any of these values have the function return NA and print out a message that it doesn’t know how to convert that group.


get_mass_from_length_dino<- function(length, group){
  if (group == "Stegosauria") {
    mass <- 10.95 * length ** 2.64
  } else if (group== "Theropoda"){
    mass <- 0.73 * length ** 3.63
  } else if (group=="Sauropoda"){
    mass <- 214.44 * length ** 1.46
  } else {
    warning("Group doesn't match, check spelling")
    return("NA")
  }
  return(mass)
} 

get_mass_from_length_dino(length= 10, "Stegosauri")

# Exercise 7- DNA or RNA

seq1 <- "ttgaatgccttacaactgatcattacacaggcggcatgaagcaaaaatatactgtgaaccaatgcaggcg"
seq2 <- "gauuauuccccacaaagggagugggauuaggagcugcaucauuuacaagagcagaauguuucaaaugcau"
seq3 <- "gaaagcaagaaaaggcaggcgaggaagggaagaagggggggaaacc"

dna_or_rna<- function(seq){
if (grepl("t",seq)==TRUE) {
      print("DNA") 
    } else if(grepl("u",seq) == TRUE){
      print("RNA")
    } else{
      print("UKNOWN")
    }
}  
  
dna_or_rna(seq=seq1)
#DNA
dna_or_rna(seq=seq2)
#RNA
dna_or_rna(seq=seq3)
#UNKNOWN


# Exercise 8- Climate Space Rewrite

# 1 Create a function to download occurrence data and extract the corresponding climate data, which should return a dataset of all the bioclim variables for each species. Because the latitude and longitude columns for each occurrence dataset will be different, generalize them using the column index, instead of the column name, to get only those columns (e.g., select(longitude = 2, latitude = 3).

library("ggplot2")
library("raster")
library("maps")
library("spocc")
library("dplyr")


world_clim_rast<-getData('worldclim', var = 'bio', res = 10)
world_clim_rast_df<-as.data.frame (world_clim_rast)
  
world_clim_rast_df<-world_clim_rast_df%>% 
    dplyr::select(bio1, bio12) %>% 
    dplyr::rename(temperature=bio1, precipitation=bio12) %>%
    na.omit() %>% 
    sample_n(size= 10000)
  
world_clim_rast_df$temperature<- (world_clim_rast_df$temperature /10)


tree_data<-function(species){
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
}

querc<-tree_data(species="Quercus alba")
picea<-tree_data(species="Picea glauca")
ceiba<-tree_data(species="Ceiba pentandra" )



# 2 Create a second function for plotting species occurrences onto the available climate space, then use this function to generate a plot for each of the three tree species.

tree_plotting<- function(tree, color){
plot<- ggplot()+
  labs(title= tree)+
  geom_point(data= world_clim_rast_df ,aes(x=temperature , y=precipitation),alpha=0.3) +
  geom_point(data=tree,aes(x=temperature , y=precipitation), color= color) +
  xlab("Mean Annual Temperature (C)") +
  ylab("Mean Annual Precipitation (mm)")

return(plot)
  }


tree_plotting(tree= querc, color="red")
tree_plotting(tree= picea, color= "white")
tree_plotting(tree= ceiba, color= "blue")


