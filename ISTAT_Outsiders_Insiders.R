source("packages.r")
library("margins")
load("~/Desktop/1.Social-media-data-mining /GitHub/Thesis/RCFL_2017_Terzo_trimestre_IT_R/MICRODATI/DF_RCFL_A2017.RData")
df <- DF_RCFL_A2017
df$ISCO3D

df1<- mutate(df, groups = ifelse(C1 %in% 1 & C20 %in% 2 & C27 %in% 1, "1", # tempo ind -full-time
                          ifelse(C1 %in% 1 & C20 %in% 2 & C27 %in% 2 & C28 %in% 1, "2", #temp ind -parttime vol
                          ifelse(C1 %in% 1 & C20 %in% 2 & C27 %in% 2 & C28 %in% c(2,3), "3", #temp ind-parttime involontario
                          ifelse(C1 %in% 1 & C20 %in% 1 & C25 %in% c(1,997) & C27 %in% 1, "4", # ter-vol-ful
                          ifelse(C1 %in% 1 & C20 %in% 1 & C25 %in% 2 & C27 %in% 1, "5", # ter-inv-ful
                          ifelse(C1 %in% 1 & C20 %in% 1 & C25 %in% c(1,997) & C27 %in% 2, "6", # term - vol - partime
                          ifelse(C1 %in% 1 & C20 %in% 1 & C25 %in% 2 & C27 %in% 2, "7", # termine- invo - partime
                          ifelse(C1 %in% 4, "8", #imprenditori
                          ifelse(C1 %in% 5, "9", #liberi prof
                          ifelse(C1 %in% 8, "10", #socio cop
                          ifelse(C1 %in% 6 & C4 %in% 1, "11", #lav.proprio con dipe        
                          ifelse(C1 %in% 6 & C4 %in% 2, "12", #lav.proprio senza dipendenti  
                          ifelse(C1 %in% 7, "13", # coadivuanti impresa familiare 
                          ifelse(C1 %in% 2, "14", # co.co.co
                          ifelse(C1 %in% 3, "15", # lavoratore occasionale 
                          ifelse(COND10 %in% c(2,3,4), "16", #disoccupati / persone in cerca
                          ifelse(COND10 %in% 5, "17", # cercano non attivamente ma disponibili
                          ifelse(COND10 %in% 6, "18", # cercano ma non disponibili 
                          ifelse(COND10 %in% 7, "19", # non cercano ma disponibili      
                          "NA"))))))))))))))))))))


df1<- mutate(df, groups = ifelse(C1 %in% 1 & C20 %in% 2 & C27 %in% 1, "1", # tempo ind -full-time
                                 ifelse(C1 %in% 1 & C20 %in% 2 & C27 %in% 2 & C28 %in% 1, "2", #temp ind -parttime vol
                                        ifelse(C1 %in% 1 & C20 %in% 2 & C27 %in% 2 & C28 %in% c(2,3), "3", #temp ind-parttime involontario
                                               ifelse(C1 %in% 1 & C20 %in% 1 & C25 %in% c(1,997) & C27 %in% 1, "4", # ter-vol-ful
                                                      ifelse(C1 %in% 1 & C20 %in% 1 & C25 %in% 2 & C27 %in% 1, "5", # ter-inv-ful
                                                             ifelse(C1 %in% 1 & C20 %in% 1 & C25 %in% c(1,997) & C27 %in% 2, "6", # term - vol - partime
                                                                    ifelse(C1 %in% 1 & C20 %in% 1 & C25 %in% 2 & C27 %in% 2, "7", # termine- invo - partime
                                                                           ifelse(C1 %in% 4, "8", #imprenditori
                                                                                  ifelse(C1 %in% 5, "9", #liberi prof
                                                                                         ifelse(C1 %in% 8, "10", #socio cop
                                                                                                ifelse(C1 %in% 6 & C4 %in% 1, "11", #lav.proprio con dipe        
                                                                                                       ifelse(C1 %in% 6 & C4 %in% 2, "12", #lav.proprio senza dipendenti  
                                                                                                              ifelse(C1 %in% 7, "13", # coadivuanti impresa familiare 
                                                                                                                     ifelse(C1 %in% 2, "14", # co.co.co
                                                                                                                            ifelse(C1 %in% 3, "15", # lavoratore occasionale 
                                                                                                                                   ifelse(COND10 %in% c(2,3,4), "16", #disoccupati / persone in cerca
                                                                                                                                          ifelse(COND10 %in% 5, "17", # cercano non attivamente ma disponibili
                                                                                                                                                 ifelse(COND10 %in% 6, "18", # cercano ma non disponibili 
                                                                                                                                                        ifelse(COND10 %in% 7, "19", # non cercano ma disponibili      
                                                                                                                                                               "NA"))))))))))))))))))))

df1<-mutate(df1, outins = ifelse(groups %in% c(1,4,6,2,8,9,11), "1",
                                 ifelse(groups %in% c(5,7,3,12,13,10,15,14,16,17,18,19), "2", "0")))
###################
df2<-df1 %>% 
        filter(!COND10 %in% c(9,10)) %>% 
        filter(!groups %in% "NA")

table(df2$outins, df2$REG)
n=sum(table(df2$outins))

table(df2$outins)/n
round(table(df2$outins)/n*100,2)

CrossTable(df2$outins, df2$REG)

mytable<- xtabs(~outins+REG, data=df2)
ftable(mytable)
summary(mytable)

x<- prop.table(mytable, 2)


a <- data.frame("REG" = 1:20, 
                "vote" = c(26.25,24.24,21.35,19.28,24.53,24.26,29.62,
                           26.94,24.36,27.03,35.22,32.86,39.29,44.49,
                           48.74,44.11,43.00,43.56,48.08,42.15))
                        
             
df0 <- merge(a, x1, x2)        
df3<- filter(df0, outins=="2")

scatter <- qplot(vote,Freq, data=df3)  
fit <- lm(vote~Freq, data = df3)
df3$predicted <-predict(fit)
df3$residuals <- residuals(fit)
zzz<-df3 %>% select(vote, predicted,residuals) %>% head(40)
write.csv(zzz)

write.csv2(zzz, "ALBERTA1.csv")
write.table(kt, "D:/kt.txt", sep="\t", row.names=FALSE)

ggplot(df3, aes(x = vote, y = Freq))+
        geom_point()+ 
        geom_point(aes(x = predicted), shape = 1)



plot5<-ggplot(df3, aes(x = vote, y = Freq)) +
        geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  # Plot regression slope
        geom_point() +
        geom_label_repel(aes(label=ifelse(Freq>0.5,as.character(REG),'')),box.padding   = 0.5, 
                         point.padding = 0.5,
                         segment.color = 'grey50')+
        labs(x = "Votes to M5S",
             y = "Outsiderness rate",
             title ="Outsiderness and vote for M5S")+
        theme(axis.text.x = element_text(family="serif"),
              plot.title = element_text(size=12, face = "bold", family="serif"),
              axis.title.x = element_text(size = 10, family="serif"),
              axis.title =  element_text(size = 10, family="serif"))+ 
        theme_light()  # Add theme for cleaner look
tiff('figure_ineq.tiff', units="in", width=8, height=5, res=1200)
plot5
dev.off()



df3$REG[df3$REG %in% "1"] <- "Piemonte"
df3$REG[df3$REG %in% "2"] <- "Valle d'Aosta"
df3$REG[df3$REG %in% "3"] <- "Lombardia"
df3$REG[df3$REG %in% "4"] <- "Trentino alto Adige"
df3$REG[df3$REG %in% "5"] <- "Veneto"
df3$REG[df3$REG %in% "6"] <- "Friuli Venezia Giulia"
df3$REG[df3$REG %in% "7"] <- "Liguria"
df3$REG[df3$REG %in% "8"] <- "Emilia Romagna"
df3$REG[df3$REG %in% "9"] <- "Toscana"
df3$REG[df3$REG %in% "10"] <- "Umbria"
df3$REG[df3$REG %in% "11"] <- "Marche"
df3$REG[df3$REG %in% "12"] <- "Lazio"
df3$REG[df3$REG %in% "13"] <- "Abruzzo"
df3$REG[df3$REG %in% "14"] <- "Molise"
df3$REG[df3$REG %in% "15"] <- "Campania"
df3$REG[df3$REG %in% "16"] <- "Puglia"
df3$REG[df3$REG %in% "17"] <- "Basilicata"
df3$REG[df3$REG %in% "18"] <- "Calabria"
df3$REG[df3$REG %in% "19"] <- "Sicilia"
df3$REG[df3$REG %in% "20"] <- "Sardegna"




library(ggrepel)
ggplotly(plot5, legend.position = 'bottom')

###
attach(df1)
summary(groups)

df2= na.omit(groups)
summary(df2)
length(df2)
######
table(groups)
n=sum(table(groups))
n

table(groups)/n
table(groups)/n*100
round(table(groups)/n*100,2)
round(table(groups)/n*100,1)
####
table(COND3)
n1=sum(table(COND3))
n1
round(table(COND3)/n1*100,2)
