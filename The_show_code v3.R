
###############
#title: The Show Andy Sun
#MSDS 455 The Show Introduction
###############
library(readxl)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(ggthemes)
library(stringr)
library(tidyr)
library(data.table)

#Color Theme (Dark Blue "#0072B2" as primary color)
color_palette<-c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Load data
safety<-read.csv("transportation_fatalities_usafacts.csv")
air_traffic <- read.csv("AviationPassengers.csv")
market_share<- read.csv("OnewayT_T100_MARKET_ALL_CARRIER2021.csv")

#safety clean up
colnames(safety)[1]<-"Trans_mode"
F_1990<-safety[c(3,20:23),c(1,32)]
F_1990$Year<-"1990"
colnames(F_1990)[2]<-"Fatalities"
F_2020<-safety[c(3,20:23),c(1,62)]
F_2020$Year<-"2020"
colnames(F_2020)[2]<-"Fatalities"
safety_trim<-union_all(F_1990,F_2020) 

safety_trim$Fatalities <- round(as.numeric(safety_trim$Fatalities)/1000,3)
safety_trim$Trans_mode<-gsub('People','',safety_trim$Trans_mode)
safety_trim$Trans_mode<-gsub('[^[:alnum:] ]','',safety_trim$Trans_mode)

#1990 vs 2020 Fatality percent change
F_delta<-safety[c(3,20:23),c(1,32,62)]
F_delta$Trans_mode<-gsub('People','',F_delta$Trans_mode)
F_delta$Trans_mode<-gsub('[^[:alnum:] ]','',F_delta$Trans_mode)

F_delta$p_change<-round((F_delta$X1990-F_delta$X2020)/F_delta$X1990,3)*100

#air traffic clean up
head(air_traffic)
summary(air_traffic)

air_traffic$Date <- as.Date(paste(air_traffic$Year, sprintf("%02d", air_traffic$Month), 1, sep = "-"))
air_traffic$DOMESTIC_M <- round(air_traffic$DOMESTIC/1000000,2)
air_traffic$INTERNATIONAL_M <- round(air_traffic$INTERNATIONAL/1000000,2)

domestic<-air_traffic[ , c("Date","DOMESTIC_M")]    
domestic$Travel_type<-"Domestic"
colnames(domestic)[colnames(domestic) == "DOMESTIC_M"] <- "Passengers_In_Mil" # Rename column
international<-air_traffic[ , c("Date","INTERNATIONAL_M")]
international$Travel_type<-"International"
colnames(international)[colnames(international) == "INTERNATIONAL_M"] <- "Passengers_In_Mil" # Rename column

air_traffic2 <- union_all(domestic,international)

mean(domestic$Passengers_In_Mil)/mean(international$Passengers_In_Mil)

#Market share clean up
market_share<-market_share[market_share$AIRLINE_ID != "All Rows", ]
market_share<-separate(market_share, col=Description, into=c('Airline', 'Airline Code'), sep=':')
colnames(market_share)[colnames(market_share) == "Sum.PASSENGERS."] <- "Passengers" 
market_share <- market_share %>%
  arrange(desc(Passengers))
ms_top9<-head(market_share,9)
ms_other<-market_share[market_share$AIRLINE_ID != ms_top9$AIRLINE_ID,]
other<-c('Others','Others','Others', sum(ms_other$Passengers))
ms_top10<-rbind(ms_top9,other)
ms_top10$clr<-c("0","0","0","0","0","0","0","0","0","1")
ms_top10$Airline <- word(ms_top10$Airline, 1)
ms_top10$Passengers<- as.numeric(ms_top10$Passengers)  # Convert one variable to numeric
#ms_top10$Passengers <- round(ms_top10$Passengers/1000000,2)

########################Visualization###################

#Safety
x<-reorder(safety_trim$Trans_mode, safety_trim$Fatalities)
x2<-reorder(F_delta$Trans_mode, F_delta$p_change)

grid.arrange(
ggplot(safety_trim,aes(x=x, y=Fatalities))+
  geom_line(aes(group = Trans_mode)) +
  geom_point(aes(color = Year))+
  labs(x="Transporation Mode", y="Fatality(K)")+
  coord_flip()+
  scale_y_continuous(n.breaks = 10,position="right")+
  scale_color_manual(values=c("#999999", "#0072B2"))+ #add color gray and dark blue
  theme_tufte()+
  theme(legend.position="bottom",
        axis.title.y=element_blank())+
  ggtitle("U.S. Fatality by Transportation Mode 1990 vs 2020"),

ggplot(F_delta,aes(x=x2, y=p_change))+
  geom_segment( aes(x=x2, xend=x2, y=0, yend=p_change)) +
  geom_point( color="#0072B2", size=4, alpha=0.6) + #add color dark blue
  labs(x="Transporation Mode", y="% Change")+
  coord_flip()+
  scale_y_continuous(n.breaks = 10,position="right")+
  theme_tufte()+
  theme(axis.title.y=element_blank())
)

#Air Traffic
ggplot(air_traffic2, aes(x=Date, y=Passengers_In_Mil, colour=Travel_type))+
  geom_line()+
  scale_x_date(date_labels = "%Y-%m")+
  geom_vline(xintercept=as.Date("2020-01-01"),
             linetype=4, colour="black")+
  annotate("text",
           x = as.Date("2020-01-01"),
           y = 40,
           angle = 90,
           label = "Covid-19 Outbreak\n") +
  labs(x="Date", y="Passengers In Millions")+
  theme_tufte(ticks = TRUE)+
  theme(legend.position="bottom",
        legend.title= element_blank())+
  scale_color_manual(values=c("#E69F00", "#0072B2"))+ #add color orange and dark blue
  scale_y_continuous(n.breaks = 15)+
  ggtitle("Air Traffic 2002-2022")

#unique airline ID count
n_distinct(market_share$AIRLINE_ID)
#Market Share
ggplot(ms_top10,aes(fill=clr))+
  geom_bar(stat='identity',
           aes(x=reorder(Airline, -Passengers), y=Passengers/sum(Passengers)))+
  scale_fill_manual(values=c("#0072B2","#999999"))+ #add color dark blue and gray 
  labs(x="Airline", y="Market Share in %")+
  geom_text(aes(x=reorder(Airline, -Passengers), y=Passengers/sum(Passengers)
                , label = paste0(round(Passengers/sum(Passengers)*100,1),"%")
                , vjust = -0.5))+
  theme_tufte(ticks = FALSE,)+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "none")+
  ggtitle("2022 Airline Market Share by Passengers (U.S)")
