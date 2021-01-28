##Three counties are Dixie, Levy and Taylor

library(tidyverse)

county_pop<- read.csv("county_data.csv", header=TRUE)

county_pop$Year<- as.factor(county_pop$Year)
county_pop$Population<- as.numeric(county_pop$Population)

cbPalette <- c("#0072B2", "#D55E00", "#CC79A7")


ggplot(county_pop, aes(x= Year, y= Population, color= County))+
  #geom_point(size= 2.5) +
  geom_line(size=1.5) +
  scale_color_manual(values =cbPalette) +
  theme(legend.position="top",
        panel.grid = element_blank(), 
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", 
                                    fill=NA,size=1, 
                                    linetype="solid"))

ggsave("county_data.png")
