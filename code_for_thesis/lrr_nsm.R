library(tidyverse)
library(cowplot)
library(viridis)
library(scales)

f <- function(x) factor(x, levels = unique(x))

lrr_1994<- read.csv("lrr_1994.csv", header=TRUE)

#lrr_1994<- factor(lrr_1994,levels=unique(lrr_1994$range))
lrr_1994$count<- as.numeric(lrr_1994$count)


lrr_1994_plot<-ggplot(lrr_1994, aes(f(range), y=count, fill=range))+
  geom_col() +
  labs(x= "Erosion(-) and Accretion(+) rates (m/yr)", 
       y= "Transect count", fill= "Meters (-/+)",
       title = "LRR (m/yr) for years 1994-2007")+
  scale_fill_viridis(option="magma", discrete = TRUE)+
  geom_text(aes(label = scales::percent(count/sum(count))), vjust = -.5)+
  theme(legend.position="none",
        panel.grid = element_blank(), 
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", 
                                    fill=NA,size=1, 
                                    linetype="solid"),
        axis.text.x = element_text(angle = 90, size= 12))


nsm_1994<- read.csv("nsm_1994.csv", header=TRUE)

nsm_1994$range<- as.factor(nsm_1994$range)
nsm_1994$count<- as.numeric(nsm_1994$count)


nsm_1994_plot<-ggplot(nsm_1994, aes(f(range), y=count, fill=range))+
  geom_col() +
  labs(x= "Erosion(-) and Accretion(+) in meters", 
       y= "Transect count", fill= "Meters (-/+)", 
       title = "NSM (m) for years 1994-2007")+
  scale_fill_viridis(option="magma", discrete = TRUE)+
  geom_text(aes(label = scales::percent(count/sum(count))), vjust = -.5)+
  theme(legend.position="none",
        panel.grid = element_blank(), 
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", 
                                    fill=NA,size=1, 
                                    linetype="solid"),
        axis.text.x = element_text(angle = 90, size= 12))


plot1994<-plot_grid(lrr_1994_plot, nsm_1994_plot, labels = c('A', 'B'), ncol = 1)

save_plot("plot1994.png", plot1994, base_height = 13, base_width=7)



lrr_2010<- read.csv("lrr_2010.csv", header=TRUE)

lrr_2010$range<- as.factor(lrr_2010$range)
lrr_2010$count<- as.numeric(lrr_2010$count)


lrr_2010_plot<-ggplot(lrr_2010, aes(f(range), y=count, fill=range))+
  geom_col() +
  labs(x= "Erosion(-) and Accretion(+) rates (m/yr)", 
       y= "Transect count", fill= "Meters (-/+)",
       title = "LRR (m/yr) for years 2010-2019")+
  scale_fill_viridis(option="magma", discrete = TRUE)+
  geom_text(aes(label = scales::percent(count/sum(count))), vjust = -.5)+
  theme(legend.position="none",
        panel.grid = element_blank(), 
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", 
                                    fill=NA,size=1, 
                                    linetype="solid"),
        axis.text.x = element_text(angle = 90, size= 12))



nsm_2010<- read.csv("nsm_2010.csv", header=TRUE)

nsm_2010$range<- as.factor(nsm_2010$range)
nsm_2010$count<- as.numeric(nsm_2010$count)


nsm_2010_plot<-ggplot(nsm_2010, aes(f(range), y=count, fill=range))+
  geom_col() +
  labs(x= "Erosion(-) and Accretion(+) in meters", 
       y= "Transect count", fill= "Meters (-/+)", 
       title = "NSM (m) for years 2010-2019")+
  scale_fill_viridis(option="magma", discrete = TRUE)+
  geom_text(aes(label = scales::percent(count/sum(count))), vjust = -.5)+
  theme(legend.position="none",
        panel.grid = element_blank(), 
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", 
                                    fill=NA,size=1, 
                                    linetype="solid"),
        axis.text.x = element_text(angle = 90, size= 12))


plot2010<-plot_grid(lrr_2010_plot, nsm_2010_plot, labels = c('A', 'B'), ncol = 1)

save_plot("plot2010.png", plot2010, base_height = 13, base_width=7)




lrr_2019<- read.csv("lrr_2019.csv", header=TRUE)

lrr_2019$range<- as.factor(lrr_2019$range)
lrr_2019$count<- as.numeric(lrr_2019$count)


lrr_2019_plot<-ggplot(lrr_2019, aes(f(range), y=count, fill=range))+
  geom_col() +
  labs(x= "Erosion(-) and Accretion(+) rates (m/yr)", 
       y= "Transect count", fill= "Meters (-/+)",
       title = "LRR (m/yr) for years 1994-2019")+
  scale_fill_viridis(option="magma", discrete = TRUE)+
  geom_text(aes(label = scales::percent(count/sum(count))), vjust = -.5)+
  theme(legend.position="none",
        panel.grid = element_blank(), 
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", 
                                    fill=NA,size=1, 
                                    linetype="solid"),
        axis.text.x = element_text(angle = 90, size= 12))




nsm_2019<- read.csv("nsm_2019.csv", header=TRUE)

nsm_2019$range<- as.factor(nsm_2019$range)
nsm_2019$count<- as.numeric(nsm_2019$count)


nsm_2019_plot<-ggplot(nsm_2019, aes(f(range), y=count, fill=range))+
  geom_col() +
  labs(x= "Erosion(-) and Accretion(+) in meters", 
       y= "Transect count", fill= "Meters (-/+)", 
       title = "NSM (m) for years 1994-2019")+
  scale_fill_viridis(option="magma", discrete = TRUE)+
  geom_text(aes(label = scales::percent(count/sum(count))), vjust = -.5)+
  theme(legend.position="none",
        panel.grid = element_blank(), 
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", 
                                    fill=NA,size=1, 
                                    linetype="solid"),
        axis.text.x = element_text(angle = 90, size= 12))



plot2019<-plot_grid(lrr_2019_plot, nsm_2019_plot, labels = c('A', 'B'), ncol = 1)

save_plot("plot2019.png", plot2019, base_height = 13, base_width=7)





