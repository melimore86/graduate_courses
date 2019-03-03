calc_shrub_vol<- function(length, width, height) {
  volume<- length * width *height
  return(volume)
}

calc_shrub_vol(1,2,3)


10>5

"aang"=="aang"

3 != 3

"dog" %in% c("cat","snake", "beetle", "dog")

# and is like are all of the conditions true

5> 2 & 6>= 10 
#^ this is the and

5> 2 | 6>= 10
# ^ thisis the or 

all.equal(2* 1.3 + 0.2, 2.8)


veg_type<- "shrub"
volume<- 16.08
if (veg_type== "tree"){
  mass<- 2.65 * volume ^0.9
} else if (veg_type== "shrub"){
  mass<- 0.65 * volume ^1.2
} else {
  warning("unkonwn vegetation type, cannot calculate mass.")
  mass<- NA
}