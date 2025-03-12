# This code is for "Xie & Xi et al. 2025 The Scientific Naturalist-Ecology How a monogamous male cope with two wives——a case study in shorebirds"
# update 2025-3-10 Xi Lin
# ps. Most of the t-test results are displayed directly on the figures with the original p-values. 
# ps. For column name explanations, refer to "Column Name Explanation.txt"




rm(list = ls())
# Set working directory
setwd("C:/Users/LX/Desktop")

# Set the path for saving images
dir.create("paper plot", recursive = TRUE, showWarnings = FALSE)




# Figure2a----

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr) # Add test results
library(ggpubr)# Significance testing

load("Organized data/summary_incu_days_upload.Rdata")

nestdaydata<-nestdaydata[!nestdaydata$lable %in% c("α-couple monogamy"),]


plot<-nestdaydata
plot$attendance<-(plot$count_total_incu/plot$total_count)*100


## Start plotting

p2a <- ggplot(data=plot,aes(x=factor(lable, levels = c("Normal condition", "Cooperative polygyny","Aggressive polygyny_α", "Aggressive polygyny_β")), y=attendance)) +
  geom_boxplot(width = 0.5,outlier.shape = NA,alpha = 0.8) +
  stat_summary(fun="mean", geom="point", shape=20, size=3, color="red", fill="red",alpha=0.7) +
  geom_point( aes(y = attendance),position = position_jitter(width = 0.2),alpha = 0.4,size=2)+ 
  stat_compare_means(comparisons = list(c("Cooperative polygyny", "Normal condition"),c("Aggressive polygyny_β", "Aggressive polygyny_α"), c("Aggressive polygyny_α", "Normal condition"), c("Aggressive polygyny_β", "Normal condition")), 
                     method = "t.test") +
  guides(fill = FALSE,color = FALSE)+  
  theme_classic()+
  scale_x_discrete(labels = c("Normal condition" = "Regular\nmonogamy", "Cooperative polygyny" = "Cooperative\npolygyny", "Aggressive polygyny_β" = "Aggressive\npolygyny_β", "Aggressive polygyny_α" = "Aggressive\npolygyny_α")) +
  ylab("Total daily nest attendance %") +
  xlab("Breeding types") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(size=10))+ 
  theme(text = element_text(color = "black"))


p2a

ggsave("paper plot/Figure2a.png", plot = p2a, 
       width = 18,
       height = 8,
       dpi = 600,   
       units = "cm")  




## Average daily total nest attendance of different breeding events----

### regular monogamy
summary(plot$attendance[which(plot$lable=="Normal condition")])

### cooperative polygyny
summary(plot$attendance[which(plot$lable=="Cooperative polygyny")])

### aggressive polygyny-α
summary(plot$attendance[which(plot$lable=="Aggressive polygyny_α")])

### aggressive polygyny-β
summary(plot$attendance[which(plot$lable=="Aggressive polygyny_β")])



# Figure2b----

## cooperative polygyny
load("Organized data/summary_incu_days_upload.Rdata")
nestdaydata<-nestdaydata[nestdaydata$lable=="Cooperative polygyny",]
nestdaydata <- nestdaydata[, !colnames(nestdaydata) %in% c("count_4","count_0")]

plot2 <- nestdaydata %>%
  pivot_longer(
    cols = starts_with("count_"),
    names_to = "type",
    values_to = "count"
  ) 

plot2$attendance<-(plot2$count/plot2$total_count)*100

plot2 <- plot2 %>%
  mutate(
    Day = as.numeric(date(date) - min(date(date)) + 1),  
    Status = factor(type, 
                    levels = c("count_total_incu", "count_1", "count_2", "count_3"), 
                    labels = c("All bird", "Male_α", "Female_α", "Female_β"))
  )

plot2 <- plot2[plot2$Status!="All bird",]

p2b2 <- ggplot(data=plot2, aes(x=Status, y=attendance, fill=Status)) +
  geom_boxplot(width = 0.5,outlier.shape = NA,alpha = 0.8) +
  stat_summary(fun="mean", geom="point", shape=20, size=3, color="red", fill="red",alpha=0.7) +
  geom_point( aes(y = attendance,color = Status),position = position_jitter(width = 0.2),alpha = 0.4,size=2)+ 
  stat_compare_means(comparisons = list(c("Female_α", "Male_α"), c("Female_α", "Female_β"), c("Male_α", "Female_β")), 
                     method = "t.test") +
  geom_hline(aes(yintercept=50),linetype="dashed",colour="#525252",size=1)+
  guides(fill = FALSE,color = FALSE)+  
  theme_classic()+
  scale_color_manual(values = c("#465a8b",'#99322E', "#a05d1a")) +
  scale_fill_manual(values =c('#4682B4',"#99322E","#e9b383")) +
  ylab("Daily Nest attendance %") +
  xlab("") +
  scale_y_continuous(breaks = seq(0, 90, by = 20),limits = c(0,90))+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("Cooperative\npolygyny")+
  theme(
    plot.title = element_text(hjust = 0.5, size = 8)  
  )+
  theme(text=element_text(size=10))+ 
  theme(text = element_text(color = "black"))

p2b2   



## regular monogamy
load("Organized data/summary_incu_days_upload.Rdata")
nestdaydata<-nestdaydata[nestdaydata$lable=="Normal condition",]

nestdaydata <- nestdaydata[, !colnames(nestdaydata) %in% c("count_4","count_0","count_3")]


plot3 <- nestdaydata %>%
  pivot_longer(
    cols = starts_with("count_"),
    names_to = "type",
    values_to = "count"
  ) 

plot3$attendance<-(plot3$count/plot3$total_count)*100

plot3 <- plot3 %>%
  mutate(
    Day = as.numeric(date(date) - min(date(date)) + 1),  
    Status = factor(type, 
                    levels = c("count_total_incu", "count_1", "count_2"), 
                    labels = c("All bird", "Male", "Female"))
  )


plot3 <- plot3[plot3$Status!="All bird",]

p2b1 <- ggplot(data=plot3, aes(x=Status, y=attendance, fill=Status)) +
  geom_boxplot(width = 0.5,outlier.shape = NA,alpha = 0.8) +
  stat_summary(fun="mean", geom="point", shape=20, size=3, color="red", fill="red",alpha=0.7) +
  geom_point( aes(y = attendance,color = Status),position = position_jitter(width = 0.2),alpha = 0.4,size=2)+ 
  stat_compare_means(comparisons = list(c("Female", "Male")), 
                     method = "t.test") +
  geom_hline(aes(yintercept=50),linetype="dashed",colour="#525252",size=1)+
  guides(fill = FALSE,color = FALSE)+  
  theme_classic()+
  scale_color_manual(values = c("#465a8b",'#99322E')) +
  scale_fill_manual(values =c('#4682B4',"#99322E")) +
  ylab("Individual daily nest attendance %") +
  xlab("") +
  scale_y_continuous(breaks = seq(0, 90, by = 20),limits = c(0,90))+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("Regular\nmonogamy")+
  theme(
    plot.title = element_text(hjust = 0.5, size = 8)  
  )+
  theme(text=element_text(size=10))+ 
  theme(text = element_text(color = "black"))


p2b1   



## aggressive polygyny-α
load("Organized data/summary_incu_days_upload.Rdata")
nestdaydata<-nestdaydata[nestdaydata$lable=="Aggressive polygyny_α",]

nestdaydata <- nestdaydata[, !colnames(nestdaydata) %in% c("count_4","count_0","count_3")]


plot4 <- nestdaydata %>%
  pivot_longer(
    cols = starts_with("count_"),
    names_to = "type",
    values_to = "count"
  ) 

plot4$attendance<-(plot4$count/plot4$total_count)*100

plot4 <- plot4 %>%
  mutate(
    Day = as.numeric(date(date) - min(date(date)) + 1), 
    Status = factor(type, 
                    levels = c("count_total_incu", "count_1", "count_2"), 
                    labels = c("All bird", "Male_α", "Female_α"))
  )

plot4 <- plot4[plot4$Status!="All bird",]

p2b3 <- ggplot(data=plot4, aes(x=Status, y=attendance, fill=Status)) +
  geom_boxplot(width = 0.5,outlier.shape = NA,alpha = 0.8) +
  stat_summary(fun="mean", geom="point", shape=20, size=3, color="red", fill="red",alpha=0.7) +
  geom_point( aes(y = attendance,color = Status),position = position_jitter(width = 0.2),alpha = 0.4,size=2)+ 
  stat_compare_means(comparisons = list(c("Female_α", "Male_α")), 
                     method = "t.test") +
  geom_hline(aes(yintercept=50),linetype="dashed",colour="#525252",size=1)+
  guides(fill = FALSE,color = FALSE)+  
  theme_classic()+
  scale_color_manual(values = c("#465a8b",'#99322E')) +
  scale_fill_manual(values =c('#4682B4',"#99322E")) +
  ylab("Daily Nest attendance %") +
  xlab("") +
  scale_y_continuous(breaks = seq(0, 90, by = 20),limits = c(0,90))+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("Aggressive\npolygyny_α")+
  theme(
    plot.title = element_text(hjust = 0.5, size = 8) 
  )+
  theme(text=element_text(size=10))+ 
  theme(text = element_text(color = "black"))


p2b3




## aggressive polygyny-β
load("Organized data/summary_incu_days_upload.Rdata")
nestdaydata<-nestdaydata[nestdaydata$lable=="Aggressive polygyny_β",]

nestdaydata <- nestdaydata[, !colnames(nestdaydata) %in% c("count_4","count_0","count_3")]


plot5 <- nestdaydata %>%
  pivot_longer(
    cols = starts_with("count_"),
    names_to = "type",
    values_to = "count"
  ) 

plot5$attendance<-(plot5$count/plot5$total_count)*100



plot5 <- plot5 %>%
  mutate(
    Day = as.numeric(date(date) - min(date(date)) + 1), 
    Status = factor(type, 
                    levels = c("count_total_incu", "count_1", "count_2"), 
                    labels = c("All bird", "Male_α", "Female_β"))
  )



plot5 <- plot5[plot5$Status!="All bird",]

p2b4 <- ggplot(data=plot5, aes(x=Status, y=attendance, fill=Status)) +
  geom_boxplot(width = 0.5,outlier.shape = NA,alpha = 0.8) +
  stat_summary(fun="mean", geom="point", shape=20, size=3, color="red", fill="red",alpha=0.7) +
  geom_point( aes(y = attendance,color = Status),position = position_jitter(width = 0.2),alpha = 0.4,size=2)+ 
  stat_compare_means(comparisons = list(c("Female_β", "Male_α")), 
                     method = "t.test") +
  geom_hline(aes(yintercept=50),linetype="dashed",colour="#525252",size=1)+
  guides(fill = FALSE,color = FALSE)+  
  theme_classic()+
  scale_color_manual(values = c("#465a8b", "#a05d1a")) +
  scale_fill_manual(values =c('#4682B4',"#e9b383")) +
  ylab("Daily Nest attendance %") +
  xlab("") +
  scale_y_continuous(breaks = seq(0, 90, by = 20),limits = c(0,90))+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("Aggressive\npolygyny_β")+
  theme(
    plot.title = element_text(hjust = 0.5, size = 8) 
  )+
  theme(text=element_text(size=10))+ 
  theme(text = element_text(color = "black"))


p2b4 




p2b2 <- p2b2 + theme(axis.title.y = element_blank(), axis.text.y = element_blank())
p2b3 <- p2b3 + theme(axis.title.y = element_blank(), axis.text.y = element_blank())
p2b4 <- p2b4 + theme(axis.title.y = element_blank(), axis.text.y = element_blank())
p2b<-ggarrange(p2b1, p2b2, p2b3, p2b4, ncol = 4, widths = c(1.1,1.3,1,1))
p2b


ggsave("paper plot/Figure2b.png", plot = p2b, 
       width = 18, 
       height = 10, 
       dpi = 600,   
       units = "cm")




p2<-ggarrange(p2a, p2b, nrow = 2, heights = c(1,1),labels = c("a", "b"))
p2

ggsave("paper plot/Figure2.png", plot = p2, 
       width = 18,
       height = 16,
       dpi = 600,   
       units = "cm")  




## Average daily individual nest attendance of regular monogamy----

### male
summary(plot3$attendance[which(plot3$Status=="Male")])
### female
summary(plot3$attendance[which(plot3$Status=="Female")])

## Average daily individual nest attendance of cooperative polygyny----

### α-male
summary(plot2$attendance[which(plot2$Status=="Male_α")])
### α-female
summary(plot2$attendance[which(plot2$Status=="Female_α")])
### β-female
summary(plot2$attendance[which(plot2$Status=="Female_β")])


## Average daily individual nest attendance of aggressive polygyny-α----

### α-male
summary(plot4$attendance[which(plot4$Status=="Male_α")])
### α-female
summary(plot4$attendance[which(plot4$Status=="Female_α")])


## Average daily individual nest attendance of aggressive polygyny-β----

### α-male
summary(plot5$attendance[which(plot5$Status=="Male_α")])
### β-female
summary(plot5$attendance[which(plot5$Status=="Female_β")])



# Figure3a----
rm(list=ls())
load("Organized data/summary_incu_days_upload.Rdata")

# Create a dataset for Aggressive polygyny_α+
E051<-nestdaydata[nestdaydata$NEST %in% c("E051"),]
E050<-nestdaydata[nestdaydata$NEST %in% c("E050"),]
# Keep only the overlapping dates
E051<-E051[which(E051$date %in% E050$date),]
E050<-E050[which(E050$date %in% E051$date),]

for(i in 1:length(E051$date)){
  if(length(which(E050$date==E051$date[i]))>0){
    E051$count_1[i]<-E050$count_1[which(E050$date==E051$date[i])]+E051$count_1[i]
  }
}
E051$NEST<-"E050+E051"
E051$lable<-"Aggressive polygyny_α+β"

# Merge with the rest of the data
nestdaydata<-nestdaydata[!nestdaydata$NEST %in% c("E050","E051"),]
nestdaydata<-rbind(E051,nestdaydata)

nestdaydata <- nestdaydata[, !colnames(nestdaydata) %in% c("count_4","count_0","count_2","count_3","count_total_incu")]



plot <- nestdaydata %>%
  pivot_longer(
    cols = starts_with("count_"),
    names_to = "type",
    values_to = "count"
  ) 

plot$attendance<-(plot$count/plot$total_count)*100


plot <- plot %>%
  mutate(
    Day = as.numeric(date(date) - min(date(date)) + 1),  
    Status = factor(type, 
                    levels = c("count_1"), 
                    labels = c("Male"))
  )


p3a <- ggplot(data=plot,aes(x=factor(lable, levels = c("Normal condition","α-couple monogamy", "Cooperative polygyny","Aggressive polygyny_α+β")), y=attendance)) +
  geom_boxplot(width = 0.5,outlier.shape = NA,alpha = 0.8) +
  stat_summary(fun="mean", geom="point", shape=20, size=3, color="red", fill="red",alpha=0.7) +
  geom_point( aes(y = attendance,color = lable),position = position_jitter(width = 0.2),alpha = 0.6,size=2)+ 
  stat_compare_means(comparisons = list(c("Normal condition", "α-couple monogamy"),c("Normal condition", "Cooperative polygyny"),c("Aggressive polygyny_α+β", "Normal condition"),c("Aggressive polygyny_α+β", "α-couple monogamy")),
                     method = "t.test",
                     label.y = c(60, 65, 70, 75, 80)) +
  geom_hline(aes(yintercept=50),linetype="dashed",colour="#525252",size=1.5)+
  guides(fill = FALSE,color = FALSE)+ 
  scale_x_discrete(labels = c("Normal condition" = "Regular\nmonogamy","α-couple monogamy" = "α couple\nmonogamy", "Cooperative polygyny" = "Cooperative\npolygyny", "Aggressive polygyny_α+β" = "Aggressive\npolygyny_α+β")) +
  theme_classic()+
  scale_color_manual(values = c("#465a8b","#465a8b","#465a8b","#465a8b")) +
  scale_fill_manual(values =c('#4682B4','#4682B4','#4682B4','#4682B4')) +
  ylab("Male's daily nest attendance %") +
  xlab("Breeding types") +
  scale_y_continuous(limits = c(10,85))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(size=10))


p3a   


# Figure3b----
load("Organized data/summary_incu_days_upload.Rdata")


nestdaydata <- nestdaydata[, !colnames(nestdaydata) %in% c("count_4","count_0","count_1","count_3","count_total_incu")]
plot2 <- nestdaydata %>%
  pivot_longer(
    cols = starts_with("count_"),
    names_to = "type",
    values_to = "count"
  ) 

plot2$attendance<-(plot2$count/plot2$total_count)*100


plot2 <- plot2 %>%
  mutate(
    Day = as.numeric(date(date) - min(date(date)) + 1),  
    Status = factor(type, 
                    levels = c("count_2"), 
                    labels = c("Female"))
  )


p3b <- ggplot(data=plot2,aes(x=factor(lable, levels = c("Normal condition","α-couple monogamy", "Cooperative polygyny","Aggressive polygyny_α","Aggressive polygyny_β")), y=attendance)) +
  geom_boxplot(width = 0.5,outlier.shape = NA,alpha = 0.8) +
  stat_summary(fun="mean", geom="point", shape=20, size=3, color="red", fill="red",alpha=0.7) +
  geom_point( aes(y = attendance,color = lable),position = position_jitter(width = 0.2),alpha = 0.4,size=2)+ 
  stat_compare_means(comparisons = list(c("Normal condition", "α-couple monogamy"),c("Cooperative polygyny", "α-couple monogamy"),c("Normal condition", "Cooperative polygyny"),c("Aggressive polygyny_α", "Normal condition"),c("Aggressive polygyny_β", "Normal condition")),
                     method = "t.test",
                     label.y = c(60, 65, 70, 75, 80)) +
  geom_hline(aes(yintercept=50),linetype="dashed",colour="#525252",size=1.5)+
  guides(fill = FALSE,color = FALSE)+  
  scale_x_discrete(labels = c("Normal condition" = "Regular\nmonogamy","α-couple monogamy" = "α couple\nmonogamy", "Cooperative polygyny" = "Cooperative\npolygyny", "Aggressive polygyny_β" = "Aggressive\npolygyny_β", "Aggressive polygyny_α" = "Aggressive\npolygyny_α")) +
  theme_classic()+
  scale_color_manual(values = c('#99322E',"#a05d1a",'#99322E','#99322E','#99322E')) +
  scale_fill_manual(values =c("#99322E","#e9b383",'#99322E','#99322E','#99322E')) +
  ylab("Female's daily Nest attendance %") +
  xlab("Breeding types") +
  scale_y_continuous(limits = c(10,85))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(size=10))

p3b    



p3<-ggarrange(p3a, p3b, ncol = 2, widths = c(1,1.2),labels = c("a", "b"))
p3


ggsave("paper plot/Figure3.png", plot = p3, 
       width = 18, 
       height = 8, 
       dpi = 600,   
       units = "cm")  



## Average daily individual nest attendance of aggressive polygyny-α+β----
summary(plot5$attendance[which(plot5$Status=="Female_β")])
summary(plot$attendance[which(plot$lable=="Aggressive polygyny_α+β")])






# Figure4----

rm(list=ls())

load("Organized data/summary_incu_hours_upload.Rdata")


## cooperative polygyny
hourly_stats2<-summary_incu_hours[summary_incu_hours$lable=="Cooperative polygyny",]
hourly_stats2 <- hourly_stats2[, !colnames(hourly_stats2) %in% c("count_4","count_0")]


hourly_stats_long <- hourly_stats2 %>%
  pivot_longer(
    cols = starts_with("count_"),
    names_to = "type",
    values_to = "count"
  ) 

hourly_stats_long$attendance<-(hourly_stats_long$count/hourly_stats_long$total_count)*100

#拟合曲线
library(ggplot2)
col4<-c('#4682B4','#99322E', "#F5DEB3","#D3D3D3")

p4b<-ggplot(hourly_stats_long, aes(x=hour, y=attendance, group=type,colour=type)) + 
  geom_smooth(aes(fill = type, ymin = pmax(..ymin.., 0), ymax = pmin(..ymax.., 100)),size=0.7,method = "gam")+
  scale_colour_manual(values = col4, labels = c("Male_α", "Female_α", "Female_β", "Total")) +
  scale_fill_manual(values = col4, labels = c("Male_α", "Female_α", "Female_β", "Total")) +
  xlab("") +
  ylab("")+
  theme_classic()+   
  theme(text=element_text(size=9), 
        legend.position = "bottom",
        axis.text.x = element_text(margin = margin(t = 0)),  
        axis.text.y = element_text(margin = margin(r = 0)))+
  ggtitle("Cooperative polygyny")+
  theme(
    plot.title = element_text(hjust = 0.5, size = 8) 
  )+
  scale_y_continuous(breaks = seq(0, 100, by = 25),limits = c(0,100))+
  scale_x_continuous(breaks=seq(from=0, to=23, by=1),expand = c(0.01, 0))

p4b






## Aggressive polygyny_β
hourly_stats2<-summary_incu_hours[summary_incu_hours$lable=="Aggressive polygyny_β",]
hourly_stats2 <- hourly_stats2[, !colnames(hourly_stats2) %in% c("count_4","count_0","count_3")]


hourly_stats_long <- hourly_stats2 %>%
  pivot_longer(
    cols = starts_with("count_"),
    names_to = "type",
    values_to = "count"
  ) 

hourly_stats_long$attendance<-(hourly_stats_long$count/hourly_stats_long$total_count)*100

#拟合曲线
library(ggplot2)
col4<-c('#4682B4',"#F5DEB3","#D3D3D3")

p4d<-ggplot(hourly_stats_long, aes(x=hour, y=attendance, group=type,colour=type)) + 
  geom_smooth(aes(fill = type, ymin = pmax(..ymin.., 0), ymax = pmin(..ymax.., 100)),size=0.7,method = "gam")+
  scale_colour_manual(values = col4, labels = c("Male_α", "Female_β", "Total")) +
  scale_fill_manual(values = col4, labels = c("Male_α", "Female_β", "Total")) +
  xlab("") +
  ylab("")+
  theme_classic()+ 
  theme(text=element_text(size=9),  
        legend.position = "bottom",
        axis.text.x = element_text(margin = margin(t = 0)),  
        axis.text.y = element_text(margin = margin(r = 0)))+
  ggtitle("Aggressive polygyny_β")+
  theme(
    plot.title = element_text(hjust = 0.5, size = 8) 
  )+
  scale_y_continuous(breaks = seq(0, 100, by = 25),limits = c(0,100))+
  scale_x_continuous(breaks=seq(from=0, to=23, by=1),expand = c(0.01, 0))

p4d





## Aggressive polygyny_α
hourly_stats2<-summary_incu_hours[summary_incu_hours$lable=="Aggressive polygyny_α",]
hourly_stats2 <- hourly_stats2[, !colnames(hourly_stats2) %in% c("count_4","count_0","count_3")]


hourly_stats_long <- hourly_stats2 %>%
  pivot_longer(
    cols = starts_with("count_"),
    names_to = "type",
    values_to = "count"
  ) 

hourly_stats_long$attendance<-(hourly_stats_long$count/hourly_stats_long$total_count)*100

#拟合曲线
library(ggplot2)
col4<-c('#4682B4','#99322E',"#D3D3D3")

p4c<-ggplot(hourly_stats_long, aes(x=hour, y=attendance, group=type,colour=type)) + 
  geom_smooth(aes(fill = type, ymin = pmax(..ymin.., 0), ymax = pmin(..ymax.., 100)),size=0.7,method = "gam")+
  scale_colour_manual(values = col4, labels = c("Male_α", "Female_α", "Total")) +
  scale_fill_manual(values = col4, labels = c("Male_α", "Female_α", "Total")) +
  xlab("") +
  ylab("")+
  ylab("Nest attendance [%]")+
  theme_classic()+  
  theme(text=element_text(size=9), 
        legend.position = "bottom",
        axis.text.x = element_text(margin = margin(t = 0)),   
        axis.text.y = element_text(margin = margin(r = 0)))+
  ggtitle("Aggressive polygyny_α")+
  theme(
    plot.title = element_text(hjust = 0.5, size = 8)  
  )+
  scale_y_continuous(breaks = seq(0, 100, by = 25),limits = c(0,100))+
  scale_x_continuous(breaks=seq(from=0, to=23, by=1),expand = c(0.01, 0))

p4c





## regular monogamy
hourly_stats2<-summary_incu_hours[!summary_incu_hours$lable=="Normal condition",]
hourly_stats2 <- hourly_stats2[, !colnames(hourly_stats2) %in% c("count_4","count_0","count_3")]


hourly_stats_long <- hourly_stats2 %>%
  pivot_longer(
    cols = starts_with("count_"),
    names_to = "type",
    values_to = "count"
  ) 

hourly_stats_long$attendance<-(hourly_stats_long$count/hourly_stats_long$total_count)*100

#拟合曲线
library(ggplot2)
col4<-c('#4682B4','#99322E',"#D3D3D3")

p4a<-ggplot(hourly_stats_long, aes(x=hour, y=attendance, group=type,colour=type)) + 
  geom_smooth(aes(fill = type, ymin = pmax(..ymin.., 0), ymax = pmin(..ymax.., 100)),size=0.7,method = "gam")+
  scale_colour_manual(values = col4, labels = c("Male", "Female", "Total")) +
  scale_fill_manual(values = col4, labels = c("Male", "Female", "Total")) +
  xlab("") +
  ylab("")+
  ylab("Nest attendance [%]")+
  theme_classic()+   
  theme(text=element_text(size=9),  
        legend.position = "bottom",
        axis.text.x = element_text(margin = margin(t = 0)),   
        axis.text.y = element_text(margin = margin(r = 0)))+
  ggtitle("Regular monogamy")+
  theme(
    plot.title = element_text(hjust = 0.5, size = 8)  
  )+
  scale_x_continuous(breaks=seq(from=0, to=23, by=1),expand = c(0.01, 0))

p4a



library(ggpubr)

p4a <- p4a + theme(legend.position = "none")
p4b <- p4b + theme(legend.position = "none")
p4c <- p4c + theme(legend.position = "none")
p4d <- p4d + theme(legend.position = "none")

p4<-ggarrange(p4a, p4b,p4c,p4d, nrow = 2, ncol = 2,labels = c("a", "b", "c", "d"))
p4

final_plot <- annotate_figure(p4,
                              bottom = text_grob("Time of the day [h]", 
                                                 size = 8, vjust = -1.5))

ggsave("paper plot/Figure4.png", plot = final_plot, 
       width = 16, 
       height = 12, 
       dpi = 600,  
       units = "cm")  




# Other ----

load("Organized data/summary_bout_data_upload.Rdata")
## Average gap between shifts of different breeding events 

### regular monogamy
summary(summary_bout_data$gap_length[summary_bout_data$lable=="Normal condition"])

### cooperative polygyny
summary(summary_bout_data$gap_length[summary_bout_data$lable=="Cooperative polygyny"])

### aggressive polygyny-α
summary(summary_bout_data$gap_length[summary_bout_data$lable=="Aggressive polygyny_α"])

### aggressive polygyny-β
summary(summary_bout_data$gap_length[summary_bout_data$lable=="Aggressive polygyny_β"])

# regular monogamy vs cooperative polygyny
t.test(summary_bout_data$gap_length[summary_bout_data$lable=="Cooperative polygyny"], summary_bout_data$gap_length[summary_bout_data$lable=="Normal condition"])

# aggressive polygyny-β vs regular monogamy
t.test(summary_bout_data$gap_length[summary_bout_data$lable=="Aggressive polygyny_β"], summary_bout_data$gap_length[summary_bout_data$lable=="Normal condition"])



