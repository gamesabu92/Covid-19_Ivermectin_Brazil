
library(tidyverse) 
library(scales)
library(lubridate)
library(ggplot2)
library(RColorBrewer)
library(grid)
library(gridExtra)
library(ggrepel)

 
setwd("~/ivermectina/ivermectin/brasil")
getwd()


# Download .rar file " https://covid.saude.gov.br/"HIST_PAINEL_COVIDBR_08jun2021.rar"
#Unpack rar archives 



# read and rename file
covid_dataset1 = read.csv("~/ivermectina/ivermectin/brasil/HIST_PAINEL_COVIDBR_2020_Parte1_08jun2021.csv", sep=";")
covid_dataset2 = read.csv("~/ivermectina/ivermectin/brasil/HIST_PAINEL_COVIDBR_2020_Parte2_08jun2021.csv", sep=";")
covid_dataset3 = read.csv("~/ivermectina/ivermectin/brasil/HIST_PAINEL_COVIDBR_Parte3_08jun2021.csv", sep=";")

#joint all files
covid_dataset<-rbind(covid_dataset1,covid_dataset2,covid_dataset3 )

head(covid_dataset)


# Function to extract cities data c("Itajaí", "Chapecó","Macapá", "Ananindeua", "Natal", "João Pessoa" ))

subseta<-function(x) {
city_May31 <-subset(covid_dataset, municipio %in% c(x)& data %in% c("2020-05-31"))

city_Jun30 <-subset(covid_dataset, municipio %in% c(x)& data %in% c("2020-06-30"))

city_Jul31 <- subset(covid_dataset, municipio %in% c(x)& data %in% c("2020-07-31"))

city_Agost31 <-subset(covid_dataset, municipio %in% c(x)& data %in% c("2020-08-31"))

city_Sep30 <-subset(covid_dataset, municipio %in% c(x)& data %in% c("2020-09-30"))

city_Oct31 <- subset(covid_dataset, municipio %in% c(x)& data %in% c("2020-10-31"))

city_Nov30 <-subset(covid_dataset, municipio %in% c(x)& data %in% c("2020-11-30"))

city_Dec31 <-subset(covid_dataset, municipio %in% c(x)& data %in% c("2020-12-31"))
city<-rbind(
            city_May31, 
           
            city_Jun30,
         
            city_Jul31,
          
            city_Agost31,
       
            city_Sep30,
        
            city_Oct31,
        
            city_Nov30,
          
            city_Dec31 )

return(city )
}

#Data by city
set.seed(1234)
# c("Itajaí", "Chapecó","Macapá", "Ananindeua", "Natal", "João Pessoa" ))
Itajai<-subseta("Itajaí")
Chapeco<-subseta("Chapecó")
Macapa<-subseta("Macapá")
Ananindeua<-subseta("Ananindeua")
Natal<-subseta("Natal")
Joao_Pessoa<-subseta("João Pessoa")

#all data
#todo<-rbind(Itajai,Chapeco, Macapa, Ananindeua,Natal, Joao_Pessoa)
#head(todo)
#save data
#write_csv(todo, "Todo_all.csv")

#Cities by region

todo1<-rbind(Itajai,Chapeco)

todo2<-rbind(Macapa, Ananindeua)

todo3<-rbind(Natal, Joao_Pessoa)

#rename columns in English

todo1<-rename(todo1, Cases= casosAcumulado)
todo2<-rename(todo2,  Cases= casosAcumulado)
todo3<-rename(todo3, Cases= casosAcumulado)
todo1<-rename(todo1, City= municipio)
todo2<-rename(todo2,  City= municipio)
todo3<-rename(todo3, City= municipio)
todo1<-rename(todo1, Deaths= obitosAcumulado)
todo2<-rename(todo2,  Deaths= obitosAcumulado)
todo3<-rename(todo3, Deaths= obitosAcumulado)



#Figures
themeX<-theme(axis.title.x = element_text( size=12),
              axis.text.x  = element_text(angle=0, vjust=0.5, size=8))
themeX1<-theme(axis.title.x = element_blank())+ theme(axis.text.x= element_blank())

theme_legend<-theme(
  legend.title = element_text(color = "black", size = 10),
  legend.text = element_text(color = "black", size = 8))

#Accumulated cases by region
#
c1<-ggplot(todo1, aes(x = data, y = Cases, group= City, color = City)) +
  geom_line(aes(linetype=City, color=City))+ ylim(0, 45000)+  scale_x_discrete(labels=c("MAY","JUN","JUL","AUG", "SEP", "OCT","NOV","DEC") )+
  geom_point(aes(color=City))  + theme(legend.title = element_text(colour="black", size=10))+themeX+
  geom_text_repel(
    aes(data, Cases, label = Cases),
    show.legend = FALSE,direction='y', color = 'black',
    color = 'black',size= 3,
    force_pull   = 1, # do not pull toward data points
    nudge_y      = 0,
    angle        = 360,
    hjust        = 1,
    max.iter = 1e4, max.time = 1,
    box.padding = unit(0.35, "lines"),
    point.padding = unit(0.5, "lines"))+ theme(legend.position=c(0.2, 0.8))+theme_legend



c2<-ggplot(todo2, aes(x = data, y = Cases, group= City, color = City)) +
  geom_line(aes(linetype=City, color=City), size=1)+
  geom_point(aes(color=City))  + ylim(0, 45000)+ scale_x_discrete(labels=c("MAY","JUN","JUL","AUG", "SEP", "OCT","NOV","DEC") )+ themeX+ 
  geom_text_repel(
    aes(data, Cases, label = Cases, fill = factor(City)),
    color = 'black',size= 3,
    force_pull   = 1, # do not pull toward data points
    nudge_y      = 0,
    angle        = 360,
    hjust        = 1,
    segment.size = 0.5,
    max.iter = 1e4, max.time = 1,
    box.padding = unit(0.35, "lines"),
    point.padding = unit(0.5, "lines"),
    segment.color = 'grey50' ) + theme(legend.position=c(0.3, 0.8))

c3<-ggplot(todo3, aes(x = data, y = Cases, group= City, color = City)) +
  geom_line(aes(linetype=City, color=City), size=1)+
  geom_point(aes(color=City))  + ylim(0, 45000)+ scale_x_discrete(labels=c("MAY","JUN","JUL","AUG", "SEP", "OCT","NOV","DEC") )+ themeX+ 
  geom_text_repel(
    aes(data, Cases, label = Cases, fill = factor(City)),
    color = 'black',size= 3,
    force_pull   = 1, # do not pull toward data points
    nudge_y      = 0,
    angle        = 360,
    hjust        = 1,
    segment.size = 0.5,
    max.iter = 1e4, max.time = 1,
    box.padding = unit(0.35, "lines"),
    point.padding = unit(0.5, "lines"),
    segment.color = 'grey50' )+ theme(legend.position=c(0.3, 0.8))

# Cumulative number of confirmed cases 
grid.arrange(c1,c2, c3, ncol=3, nrow=1,
             top = textGrob("Cumulative number of confirmed cases- May-Dec 2020", gp = gpar(fontface = 3, fontsize = 12)), 
             bottom = textGrob("Source: Brazilian Health Ministry- https://covid.saude.gov.br/", 
                               gp = gpar(fontface = 3, fontsize = 12 )))


#Deaths by region


c4 <- ggplot(todo1, aes(x = data, y = Deaths, group= City, color = City)) +
  geom_line(aes(linetype=City, color=City), size=1)+
  geom_point(aes(color=City))  + ylim(0, 1500)+ scale_x_discrete(name="Months", labels=c("MAY","JUN","JUL","AUG", "SEP", "OCT","NOV","DEC") )+ themeX+
  geom_text_repel(
    aes(data, Deaths, label = Deaths, fill = factor(City)),
    color = 'black',size= 3,
    force_pull   = 1, # do not pull toward data points
    nudge_y      = 0.5,
    angle        = 360,
    hjust        = 1,
    segment.size = 0.5,
    max.iter = 1e4, max.time = 1,
    box.padding = unit(0.35, "lines"),
    point.padding = unit(0.5, "lines"),
    segment.color = 'grey50')+ theme(legend.position=c(0.3, 0.8))
c5<-ggplot(todo2, aes(x = data, y = Deaths, group= City, color = City)) +
  geom_line(aes(linetype=City, color=City), size=1)+
  geom_point(aes(color=City))  + ylim(0, 1500)+ scale_x_discrete(name="Months",labels=c("MAY","JUN","JUL","AUG", "SEP", "OCT","NOV","DEC") )+ themeX+
  geom_text_repel(
    aes(data, Deaths, label = Deaths, fill = factor(City)),
    color = 'black',size= 3,
    force_pull   = 1, # do not pull toward data points
    nudge_y      = 0.5,
    angle        = 360,
    hjust        = 1,
    segment.size = 0.5,
    max.iter = 1e4, max.time = 1,
    box.padding = unit(0.35, "lines"),
    point.padding = unit(0.5, "lines"),
    segment.color = 'grey50' )+ theme(legend.position=c(0.3, 0.8))
c6<-ggplot(todo3, aes(x = data, y = Deaths, group= City, color = City)) +
  geom_line(aes(linetype=City, color=City), size=1)+
  geom_point(aes(color=City))  + ylim(0, 1500)+ scale_x_discrete(name="Months",labels=c("MAY","JUN","JUL","AUG", "SEP", "OCT","NOV","DEC") )+ themeX+
  geom_text_repel(
    aes(data, Deaths, label = Deaths, fill = factor(City)),
    color = 'black', size= 3,
    force_pull   = 1, # do not pull toward data points
    nudge_y      = 0.5,
    angle        = 360,
    hjust        = 1,
    segment.size = 0.5,
    max.iter = 1e4, max.time = 1,
    box.padding = unit(0.35, "lines"),
    point.padding = unit(0.5, "lines"),
    segment.color = 'grey50'
  )+ theme(legend.position=c(0.3, 0.8))

grid.arrange (c4, c5, c6, ncol=3, nrow=1,
              top = textGrob("COVID-19 deaths - May-Dec 2020", gp = gpar(fontface = 3, fontsize = 12)), 
              bottom = textGrob("Source: Brazilian Health Ministry- https://covid.saude.gov.br/", 
                                gp = gpar(fontface = 3, fontsize = 12)))


#all figures


c1a<- c1+theme(axis.title.x = element_blank())+ theme(axis.text.x= element_blank())+ ggtitle("A") +
  theme(plot.title = element_text(hjust = -0.1))
c2a<-  c2+theme(axis.title.x = element_blank())+theme(axis.title.y = element_blank())+ theme(axis.text.x= element_blank())
c4a<-c4 +ggtitle("B") + theme(plot.title = element_text(hjust = -0.1))
c3a<- c3 +theme(axis.title.x = element_blank())+theme(axis.title.y = element_blank())+ theme(axis.text.x= element_blank())
c5a<- c5+theme(axis.title.y = element_blank())
c6a<- c6+theme(axis.title.y = element_blank())


grid.arrange (c1a, c2a,c3a, c4a, c5a, c6a, ncol=3, nrow=2,
              top = textGrob("COVID-19 accumulated number of confirmed cases (A) and deaths (B) by month - May-Dec 2020", gp = gpar(fontface = 3, fontsize = 12)), 
              bottom = textGrob("Source: Brazilian Health Ministry- https://covid.saude.gov.br/", 
                                gp = gpar(fontface = 3, fontsize = 12)))

