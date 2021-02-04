library(tidyverse)
library(cowplot)
library(viridis)

lrr_1994<- read.csv("lrr_1994.csv", header=TRUE)

lrr_1994$Range..LRR.<- as.factor(lrr_1994$Range..LRR.)
lrr_1994$Count<- as.numeric(lrr_1994$Count)


lrr_1994_plot<-ggplot(lrr_1994, aes(x=Range..LRR., y=Count, fill=Range..LRR.))+
  geom_col() +
  labs(x= "Erosion(-) and Accretion(+) rates (m/yr)", 
       y= "Transect Count", fill= "Meters (-/+)",
       title = "LRR (m/yr) for years 1994-2007")+
  scale_fill_viridis(option="magma", discrete = TRUE)+
  theme(legend.position="none",
        panel.grid = element_blank(), 
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", 
                                    fill=NA,size=1, 
                                    linetype="solid"),
        axis.text.x = element_text(angle = 90))


nsm_1994<- read.csv("nsm_1994.csv", header=TRUE)

nsm_1994$Range..NSM.<- as.factor(nsm_1994$Range..NSM.)
nsm_1994$Count<- as.numeric(nsm_1994$Count)


nsm_1994_plot<-ggplot(nsm_1994, aes(x=Range..NSM., y=Count, fill=Range..NSM.))+
  geom_col() +
  labs(x= "Erosion(-) and Accretion(+) in meters", 
       y= "Transect Count", fill= "Meters (-/+)", 
       title = "NSM (m) for years 1994-2007")+
  scale_fill_viridis(option="magma", discrete = TRUE)+
  theme(legend.position="none",
        panel.grid = element_blank(), 
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", 
                                    fill=NA,size=1, 
                                    linetype="solid"),
        axis.text.x = element_text(angle = 90))


plot1994<-plot_grid(lrr_1994_plot, nsm_1994_plot, labels = c('A', 'B'), ncol = 1)

save_plot("plot1994.png", plot1994, base_height = 10, base_width=7)



lrr_2010<- read.csv("lrr_2010.csv", header=TRUE)

lrr_2010$Range..LRR.<- as.factor(lrr_2010$Range..LRR.)
lrr_2010$Count<- as.numeric(lrr_2010$Count)


lrr_2010_plot<-ggplot(lrr_2010, aes(x=Range..LRR., y=Count, fill=Range..LRR.))+
  geom_col() +
  labs(x= "Erosion(-) and Accretion(+) rates (m/yr)", 
       y= "Transect Count", fill= "Meters (-/+)",
       title = "LRR (m/yr) for years 2010-2019")+
  scale_fill_viridis(option="magma", discrete = TRUE)+
  theme(legend.position="none",
        panel.grid = element_blank(), 
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", 
                                    fill=NA,size=1, 
                                    linetype="solid"),
        axis.text.x = element_text(angle = 90))



nsm_2010<- read.csv("nsm_2010.csv", header=TRUE)

nsm_2010$Range..NSM.<- as.factor(nsm_2010$Range..NSM.)
nsm_2010$Count<- as.numeric(nsm_2010$Count)


nsm_2010_plot<-ggplot(nsm_2010, aes(x=Range..NSM., y=Count, fill=Range..NSM.))+
  geom_col() +
  labs(x= "Erosion(-) and Accretion(+) in meters", 
       y= "Transect Count", fill= "Meters (-/+)", 
       title = "NSM (m) for years 2010-2019")+
  scale_fill_viridis(option="magma", discrete = TRUE)+
  theme(legend.position="none",
        panel.grid = element_blank(), 
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", 
                                    fill=NA,size=1, 
                                    linetype="solid"),
        axis.text.x = element_text(angle = 90))


plot2010<-plot_grid(lrr_2010_plot, nsm_2010_plot, labels = c('A', 'B'), ncol = 1)

save_plot("plot2010.png", plot2010, base_height = 10, base_width=7)




lrr_2019<- read.csv("lrr_2019.csv", header=TRUE)

lrr_2019$Range..LRR.<- as.factor(lrr_2019$Range..LRR.)
lrr_2019$Count<- as.numeric(lrr_2019$Count)


lrr_2019_plot<-ggplot(lrr_2019, aes(x=Range..LRR., y=Count, fill=Range..LRR.))+
  geom_col() +
  labs(x= "Erosion(-) and Accretion(+) rates (m/yr)", 
       y= "Transect Count", fill= "Meters (-/+)",
       title = "LRR (m/yr) for years 1994-2019")+
  scale_fill_viridis(option="magma", discrete = TRUE)+
  theme(legend.position="none",
        panel.grid = element_blank(), 
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", 
                                    fill=NA,size=1, 
                                    linetype="solid"),
        axis.text.x = element_text(angle = 90))




nsm_2019<- read.csv("nsm_2019.csv", header=TRUE)

nsm_2019$Range..NSM.<- as.factor(nsm_2019$Range..NSM.)
nsm_2019$Count<- as.numeric(nsm_2019$Count)


nsm_2019_plot<-ggplot(nsm_2019, aes(x=Range..NSM., y=Count, fill=Range..NSM.))+
  geom_col() +
  labs(x= "Erosion(-) and Accretion(+) in meters", 
       y= "Transect Count", fill= "Meters (-/+)", 
       title = "NSM (m) for years 1994-2019")+
  scale_fill_viridis(option="magma", discrete = TRUE)+
  theme(legend.position="none",
        panel.grid = element_blank(), 
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", 
                                    fill=NA,size=1, 
                                    linetype="solid"),
        axis.text.x = element_text(angle = 90))



plot2019<-plot_grid(lrr_2019_plot, nsm_2019_plot, labels = c('A', 'B'), ncol = 1)

save_plot("plot2019.png", plot2019, base_height = 10, base_width=7)





