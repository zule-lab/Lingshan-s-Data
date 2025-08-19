p <- c("ggplot2")
lapply(p, library, character.only=T)

setwd("/Users/L_INGSHA/OneDrive - Concordia University - Canada/Thesis/Data/")

#Load the data
Data<-read.csv('chapter1/Chapter1_result_final.csv')
attach(Data)
#Median Income~ supply-demand Mismatches of ES
ggplot(Data,aes(x = log(AT_Income), y = Mismatch))+
  geom_point(color = "darkred")+
  geom_smooth(method = "lm")+
  labs(title = "",
       x = "log(Median after-tax Income)",
       y = "Supply-demand Mismatches of heat reduction") +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        axis.text=element_text(size = 20,hjust = 0.5,vjust = 0.5),
        axis.title=element_text(size = 20,hjust = 0.5,vjust = 0.5),
        title = element_text(size = 20,hjust = 0.5,vjust = 0.5))

#Visible minorities~ supply-demand Mismatches of ES
ggplot(Data,aes(x = Minority_total/POP2021, y = Mismatch))+
  geom_point(color = "darkred")+
  stat_smooth(method = lm)+
  labs(title = "",
       x = "Proportion of visible minorities",
       y = "Supply-demand Mismatches of heat reduction") +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        axis.text=element_text(size = 20,hjust = 0.5,vjust = 0.5),
        axis.title=element_text(size = 20,hjust = 0.5,vjust = 0.5),
        title = element_text(size = 20,hjust = 0.5,vjust = 0.5))

#Postsecondary certificate~ supply-demand Mismatches of ES
ggplot(Data,aes(x = Postsecondary/POP2021, y = Mismatch))+
  geom_point(color = "darkred")+
  stat_smooth(method = lm)+
  labs(title = "",
       x = "Proportion of people with Postsecondary certificate",
       y = "Supply-demand Mismatches of heat reduction") +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        axis.text=element_text(size = 20,hjust = 0.5,vjust = 0.5),
        axis.title=element_text(size = 20,hjust = 0.5,vjust = 0.5),
        title = element_text(size = 20,hjust = 0.5,vjust = 0.5))


#no certificate~ supply-demand Mismatches of ES
ggplot(Data,aes(x = NoCert/POP2021, y = Mismatch))+
  geom_point(color = "darkred")+
  stat_smooth(method = lm)+
  labs(title = "",
       x = "Proportion of people with no certificate, diploma or degree",
       y = "Supply-demand Mismatches of heat reduction") +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        axis.text=element_text(size = 20,hjust = 0.5,vjust = 0.5),
        axis.title=element_text(size = 20,hjust = 0.5,vjust = 0.5),
        title = element_text(size = 20,hjust = 0.5,vjust = 0.5))

#Median Income~ supply index of ES
ggplot(Data,aes(x = log(AT_Income), y = Supply))+
  geom_point(color = "darkred")+
  geom_smooth(method = "lm")+
  labs(title = "",
       x = "log(Median after-tax Income)",
       y = "supply index of heat reduction") +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        axis.text=element_text(size = 20,hjust = 0.5,vjust = 0.5),
        axis.title=element_text(size = 20,hjust = 0.5,vjust = 0.5),
        title = element_text(size = 20,hjust = 0.5,vjust = 0.5))

#Visible minorities~ supply index of ES
ggplot(Data,aes(x = Minority_total/POP2021, y = Supply))+
  geom_point(color = "darkred")+
  stat_smooth(method = "lm")+
  labs(title = "",
       x = "Proportion of visible minorities",
       y = "supply index of heat reduction") +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        axis.text=element_text(size = 20,hjust = 0.5,vjust = 0.5),
        axis.title=element_text(size = 20,hjust = 0.5,vjust = 0.5),
        title = element_text(size = 20,hjust = 0.5,vjust = 0.5))

#Postsecondary certificate~ supply index of ES
ggplot(Data,aes(x = Postsecondary/POP2021, y = Supply))+
  geom_point(color = "darkred")+
  stat_smooth(method = "lm")+
  labs(title = "",
       x = "Proportion of people with Postsecondary certificate",
       y = "supply index of heat reduction") +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        axis.text=element_text(size = 20,hjust = 0.5,vjust = 0.5),
        axis.title=element_text(size = 20,hjust = 0.5,vjust = 0.5),
        title = element_text(size = 20,hjust = 0.5,vjust = 0.5))


#no certificate~ supply index of ES
ggplot(Data,aes(x = NoCert/POP2021, y = Supply))+
  geom_point(color = "darkred")+
  stat_smooth(method = "lm")+
  labs(title = "",
       x = "Proportion of people with no certificate, diploma or degree",
       y = "supply index of heat reduction") +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        axis.text=element_text(size = 20,hjust = 0.5,vjust = 0.5),
        axis.title=element_text(size = 20,hjust = 0.5,vjust = 0.5),
        title = element_text(size = 20,hjust = 0.5,vjust = 0.5))