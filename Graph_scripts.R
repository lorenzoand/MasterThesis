# OECD graph on employment protection 
source("packages.r")
OECD<- read_excel("EPL-timeseries.xlsx", sheet = 2)
OECD <- OECD %>% 
        filter(country == "Italy" )
library(RColorBrewer)
myColors <- brewer.pal(4,"Set1")
names(myColors) <- levels(OECD3$LOCATION)
colScale <- scale_colour_manual(name = "Legend:",values = myColors)

### export in high resolution
tiff('figure3.tiff', units="in", width=8, height=5, res=1200)
plot3+ colScale 
dev.off()


tiff('figure1.tiff', units="in", width=8, height=5, res=1200)
plot2+ colScale
dev.off()

tiff('figure3.tiff', units="in", width=10, height=5, res=1200)
pie(slices,labels = lbls,
    main="Figure 4: Unemployment benefits recipients, Italy, 2006", family="serif")
dev.off()

tiff('figure4.tiff', units="in", width=8, height=5, res=1200)
plot1
dev.off()

####
plot1 <- ggplot(OECD, aes(year)) +
        geom_line(aes(y=eprc_v1, colour = "Regular")) +
        geom_line(aes(y=ept_v1, colour = "Temporary")) +
        scale_x_continuous(limits = c(1985,2013), expand = c(0, 0), breaks= 1985:2013) +
        labs(x = "Years",
             y = "Index of Employment Protection",
             color = "Legend:",
             title = "Figure 4: Italian Legislation on Employment Protection",
             subtitle = "The reforms are marked by vertical dashed lines",
             caption= "Source: OECD online employment database, 2018") +
        theme_light()+ 
        theme(axis.text.x = element_text(angle = 90,family="serif", vjust = (-0.00001)),
              plot.title = element_text(size=12, face = "bold", family="serif"),
              axis.title.x = element_text(size = 10, family="serif"),
              axis.title =  element_text(size = 10, family="serif"),
              axis.title.x.bottom =  element_text(size = 10, family="serif"),
              legend.text = element_text(size = 10, family="serif"), 
              text = element_text(size=10,  family="serif"),
              legend.position = 'bottom')+ 
        geom_vline(xintercept =1997,linetype="dashed", color = "black", size=0.3)+
        geom_vline(xintercept =2001,linetype="dashed", color = "black", size=0.3)+
        geom_vline(xintercept =2012,linetype="dashed", color = "black", size=0.3)
        
plot1 


### unemployemnt rat to be changed 
OECD2 <- read_csv("DP_LIVE_21042018195217180.csv")
OECD2<- OECD2 %>% 
        filter(LOCATION %in% c("ITA", "FRA","DEU"))
OECD2$LOCATION= factor(OECD2$LOCATION, levels = c("ITA", "FRA", "DEU"))


plot2 <- OECD2 %>% 
        ggplot(aes(x=TIME, y=Value,group=LOCATION, color= LOCATION)) +
        geom_line()+ 
        scale_x_continuous(limits = c(1991,2007), expand = c(0, 0), breaks= 1991:2007) +
        labs(x = "Years",
             y = "Unemployment Rate",
             color = "Legend:",
             subtitle ="Comparison of Italy, Germany and France",
             caption= "Source:OECD online employment database, 2018") +
        theme_light()+ 
        theme(axis.text.x = element_text(angle = 90,family="serif", vjust = (-0.00001)),
              plot.title = element_text(size=12, face = "bold", family="serif"),
              axis.title.x = element_text(size = 10, family="serif"),
              axis.title =  element_text(size = 10, family="serif"),
              axis.title.x.bottom =  element_text(size = 10, family="serif"),
              legend.text = element_text(size = 10, family="serif"), 
              text = element_text(size=10,  family="serif"),
              legend.position = 'bottom')

plot2+ colScale

### temporary work across europe --- done just change the source 
OECD3 <- read_csv("OECD_Temp_Emp.csv")
OECD3<- OECD3 %>% 
        filter(LOCATION %in% c("ITA", "EU28", "DEU","FRA"))
OECD3$LOCATION= factor(OECD3$LOCATION, levels = c("ITA", "FRA", "DEU", "EU28"))

tiff('test.tiff', units="in", width=8, height=5, res=300)
plot3+ colScale 
dev.off()

plot3 <- OECD3 %>% 
        ggplot(aes(x=TIME, y=Value,group=LOCATION, color= LOCATION)) +
        geom_line() +
        scale_x_continuous(limits = c(1990,2016), expand = c(0, 0), breaks= 1990:2016) +
        labs(x = "Years",
             y = "Percentage",
             color = "Legend",
             title = "Figure 2: Temporary employment between 1990 and 2016",
             subtitle = "Total, % of dependent employment; Reforms are marked by vertical dashed lines", 
             caption = "Source: OECD online employment database") +
        theme_light()+
        theme(axis.text.x = element_text(angle = 90,family="serif", vjust = (-0.00001)),
              plot.title = element_text(size=12, face = "bold", family="serif"),
              axis.title.x = element_text(size = 10, family="serif"),
              axis.title =  element_text(size = 10, family="serif"),
              axis.title.x.bottom =  element_text(size = 10, family="serif"),
              legend.text = element_text(size = 10, family="serif"), 
              text = element_text(size=10,  family="serif"),
              legend.position = 'bottom')+
        geom_vline(xintercept =1997,linetype="dashed" ,color = "black", size=0.2)+
        geom_vline(xintercept =2003,linetype="dashed" ,color = "black", size=0.2)
        
plot3+ colScale 

#### employment rate by age group 
OECD4 <- read_csv("Employment_Age_Group.csv")
OECD4<-OECD4[1:60,]
plot4 <- OECD4 %>% 
        ggplot(aes(x=TIME, y=Value,group=SUBJECT, color= SUBJECT)) +
        geom_line() +
        labs(x = "Years",
             y = "Percentage",
             color = "Legend",
             title = "Employment rate",
             subtitle = "Total, % of dependent employment", 
             caption = "Source: OECD online employment database, 2018") +
        theme_minimal()+
        theme(axis.text.x = element_text(angle = 90, vjust = (-.1)),
              plot.title = element_text(size=12, face = "bold"),
              axis.title.x = element_text(size = 10),
              axis.title.y = element_text(size = 10),
              legend.text = element_text(size = 10), 
              legend.position = 'bottom')
       

plot4 
##### piecart of unemployment benefit in Italy in 2006
slices <- c(68.9, 5.7,8.4, 7.8, 7.6,1.6)
pct <- round(slices/sum(slices)*100)
lbls <- c("With No Unemployment benefits", "Mobility Allowance", "Ordinary Unemployment Benefit", "Unemployment Benefit with Reduced Elegibility", "Special Benefits for Construction Sector")
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls,
    main="Unemployment benefits recipients, Italy, 2006")


##
EUROSTAT<-read_csv("EUSTAT_Part_Temp.csv")

