##############################################
#By: Austin Schwinn
#Date: March 3, 2017
#Subject: Disproving the idea that humans and co2 aren't 
#the cause of climate change, its simply a 
#climate cycle. Inspired by Pruitt Interview
##############################################
#install.packages('rstudioapi')
#install.packages('ggplot2')
#install.packages('dplyr')
#install.packages('reshape2')
#install.packages('gtable')
#install.packages('grid')
#install.packages('scales')
#install.packages('gridExtra')
#install.packages('plyr')
#install.packages("RColorBrewer")
library(rstudioapi)
library(ggplot2)
library(dplyr)
library(reshape2)
library(gtable)
library(grid)
library(scales)
library(gridExtra)
library(plyr)
library(RColorBrewer)

#Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

##############################################
#Data Understanding

#CO2 (measured in PPM) 800,000BCE-2015 From EPA Climate Change Indicators
hist_co2 <- read.csv('Data/ghg_concentrations_co2.csv',na.strings="")
head(hist_co2)
summary(hist_co2)

#Methane (measured in PPB) 800,000BCE-2015 From EPA Climate Change Indicators
hist_methane <- read.csv('Data/ghg_concentrations_methane.csv',na.strings="")
head(hist_methane)
summary(hist_methane)

#n2o (measured in PPB) 800,000BCE-2015 From EPA Climate Change Indicators
hist_n2o <- read.csv('Data/ghg_concentrations_n2o.csv',na.strings="")
head(hist_n2o)
summary(hist_n2o)

#Climate Forcing 1979-2015 From NOAA AGGI
rad_force     <- read.csv('Data/noaa_aggi_forcing.csv')
head(rad_force)
summary(rad_force)

#Global Average Temperature 1750-2015 From Berkeley Earth
global_temp <- read.csv('Data/berkley_earth_GlobalTemperatures.csv')
head(global_temp)
summary(global_temp)

##############################################
#Data Preparation

#Create a function to wrangle the 800k BCE Datasets
hist_df_wrangle <- function(ds,bl,gas){
  #Save station names for later reference
  stations            <- ds[6,c(2:ncol(ds))]
  rownames(stations)  <- seq(length=nrow(stations))
  colnames(stations)  <- seq(length=ncol(stations))
  #Remove data set descriptions
  if(deparse(substitute(ds)) != "hist_n2o"){
    ds            <- ds[-bl,]
  }
  ds            <- ds[-c(1:7),]
  rownames(ds)  <- seq(length=nrow(ds))
  colnames(ds)  <- c("year",c(1:(length(ds)-1)))
  #Convert to cols from factor to numeric
  ds[1:ncol(ds)] <- lapply(ds[1:ncol(ds)], function(x) as.numeric(as.character(x)))
  #Get one averaged temp for each year
  ds$average <- rowMeans(ds[,2:ncol(ds)], na.rm=TRUE)
  #Output dataframes
  assign(paste("hist_",gas,sep=""),ds,envir=parent.frame())
  assign(paste(gas,"_stations",sep=""),stations,envir=parent.frame())
}

#Wrangle CO2
hist_df_wrangle(ds=hist_co2,bl=1310,gas="co2")

#Wranlge Methane
hist_df_wrangle(ds=hist_methane,bl=2183,gas="methane")
hist_methane <- hist_methane[-2153,]

#wrangle n2o
hist_df_wrangle(ds=hist_n2o,bl=0,gas="n2o")

#prepare 1950s - to current sets
hist_50_co2      <- hist_co2[hist_co2$year>=1750,c(1,ncol(hist_co2))]
hist_50_methane  <- hist_methane[hist_methane$year>=1750,c(1,ncol(hist_methane))]
hist_50_n2o      <- hist_n2o[hist_n2o$year>=1750,c(1,ncol(hist_n2o))]

#Global Temp Yearly Averages
global_temp$year  <- format(as.Date(global_temp$dt, format="%Y-%m-%d"),"%Y")
global_avg        <- ddply(subset(global_temp,is.na(LandAndOceanAverageTemperature)==FALSE), .(year), summarize,  temp=mean(LandAndOceanAverageTemperature))
global_co2        <- subset(hist_50_co2, hist_50_co2$year >= min(global_avg$year))

#Clean radical forcing
rad_force               <- rad_force[-c(38,39),]
colnames(rad_force)[7]  <- "Other15"
#Melt for easy plotting
rad_force_m <- melt(rad_force[,c(1:7)],id.vars=1)
colnames(rad_force_m) <- c("year","Gas","value")

##############################################
#Data Modeling

#Function to combine two plots into a single two y-axis plot
two_plots <- function(p1,p2) {
  win.graph(800,600,10)
  #Plot dual axis gplot2
  grid.newpage()
  
  p1 <- p1 + theme_bw()
  p2 <- p2 +theme_bw() %+replace% theme(panel.background=element_rect(fill=NA))
  
  
  
  #Extract gtable
  g1 <- ggplot_gtable(ggplot_build(p1))
  g2 <- ggplot_gtable(ggplot_build(p2))
  
  #overlap the panel of plot2 on that of plot1
  pp <- c(subset(g1$layout, name=="panel", se=t:r))
  g  <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name=="panel")]],
                        pp$t,pp$l,pp$b,pp$l)
  
  #axis tweaks
  ia <- which(g2$layout$name=="axis-l")
  ga <- g2$grobs[[ia]]
  ax <- ga$children[[2]]
  ax$widths <- rev(ax$widths)
  ax$grobs <- rev(ax$grobs)
  ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
  g <- gtable_add_cols(g, g2$widths[g2$layout[ia,]$l], length(g$widths) - 1)
  g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
  
  #draw it
  win.graph(800,600,10)
  grid.draw(g)
  
  g <<- g
}

#Get subplots ready for 800k BCE-2015 Greenhouse Gas plot
#CO2 and Methane Plot
plot1 <- ggplot(xmin=-800000) +
  geom_ribbon(data=hist_n2o, aes(x=year, y=average, ymin=0, ymax=average), fill="steelblue3", color="steelblue3")+
  geom_line(data=hist_n2o, aes(x=year, y=average), color="steelblue4",lwd=1)+
  geom_point(data=hist_n2o, aes(x=max(year),y=hist_n2o[hist_n2o$year==max(hist_n2o$year),"average"]),size=8,color="steelblue3",shape=13,stroke = 1.25, ylab="n2o")+
  geom_ribbon(data=hist_co2, aes(x=year, y=average, ymin=0, ymax=average), fill="orangered3", color="orangered3")+
  geom_line(data=hist_co2, aes(x=year, y=average), color="orangered4",lwd=1,alpha=.9)+
  geom_point(data=hist_co2, aes(x=max(year),y=hist_co2[hist_co2$year==max(hist_co2$year),"average"]),size=8,color="orangered3",shape=7,stroke = 1,ylab="co2")+
  geom_point(data=hist_co2, aes(x=max(year),y=hist_co2[hist_co2$year==max(hist_co2$year),"average"]),size=8,color="darkseagreen3",shape=9,stroke = 1.25,ylab="methane")+
  labs(x="Year\n800,000 BCE - 2015", y="",title="CO2, N20, and Methane") +
  scale_x_continuous(labels = comma,expand = c(0,10000))+
  annotation_custom(textGrob("2015", gp = gpar(col = "red")), 
                    xmin=-20000, xmax=-12000,ymin=-10, ymax=-10) +
  annotation_custom(segmentsGrob(gp = gpar(col = "red", lwd = 2)), 
                    xmin=2015, xmax=2015,ymin=-30, ymax=-10)
#methane on seperate axis because max > the other 2 maxes
plot2 <- ggplot(xmin=-800000) +
  geom_ribbon(data=hist_methane, aes(x=year, y=average, ymin=0, ymax=average), fill="darkseagreen3", color="darkseagreen3")+
  geom_line(data = hist_methane, aes(x=year, y=average), color="darkseagreen4") +
  labs(x="Year", y="Methane Concentration (PPB)")+
  scale_x_continuous(labels = comma,expand = c(0,10000))

#Combine and plot the two plots
two_plots(p1=plot1,p2=plot2)

#Design the charts
#CO2
h50_co2 <- ggplot()+
  geom_line(data=hist_50_co2, aes(x=year, y=average), color="orangered3",lwd=2)+
  geom_hline(aes(yintercept=mean(hist_co2$average)),color="azure4",lwd=1, linetype="longdash")+
  geom_hline(aes(yintercept=max(hist_co2[hist_co2$year<=0,"average"])),color="azure4",lwd=1, linetype="dotted")+
  labs(x="Year\n1750 - 2015", y="", title="CO2 (PPM)")+
  scale_x_continuous(limits=c(1750,2015),breaks=seq(1755,2015,65))+
  scale_y_continuous(limits=c(0,400),breaks=seq(0,400,100))+
  theme_bw()
#N2O
# h50_n2o <- ggplot()+
#   geom_line(data=hist_50_n2o, aes(x=year, y=average), color="steelblue3",lwd=1)+
#   geom_hline(aes(yintercept=mean(hist_n2o$average)),color="azure4",lwd=1, linetype="longdash")+
#   geom_hline(aes(yintercept=max(hist_n2o[hist_n2o$year<=0,"average"])),color="azure4",lwd=1, linetype="dotted")+
#   geom_point(data=hist_n2o, aes(x=max(year),y=hist_n2o[hist_n2o$year==max(hist_n2o$year),"average"]),size=3,color="steelblue3",shape=13,stroke = 1,ylab="n2o")+
#   labs(x="", y="", title="N2O (PPB)", subtitle="1750 - 2015")+
#   scale_x_continuous(limits=c(1750,2015),breaks=seq(1765,2015,50))
# #Methane
# h50_methane <- ggplot()+
#   geom_line(data=hist_50_methane, aes(x=year, y=average), color="darkseagreen3",lwd=1)+
#   geom_hline(aes(yintercept=mean(hist_methane$average)),color="azure4",lwd=1, linetype="longdash")+
#   geom_hline(aes(yintercept=max(hist_methane[hist_methane$year<=0,"average"])),color="azure4",lwd=1, linetype="dotted")+
#   geom_point(data=hist_methane, aes(x=max(year),y=hist_methane[hist_methane$year==max(hist_methane$year),"average"]),size=2,color="darkseagreen3",shape=9,stroke = 1,ylab="methane")+
#   labs(x="", y="", title='Methane (PPB)',subtitle="1750 - 2015")+
#   scale_y_continuous(limits=c(0,2000),breaks=seq(0,2000,500))+
#   scale_x_continuous(limits=c(1750,2015),breaks=seq(1765,2015,50))

#Combine sections
t1 <- textGrob("Historical Greenhouse Gas Concentration",gp=gpar(fontsize=20, col="black"))
t2 <- textGrob("CO2 PPM",gp=gpar(col="orangered3"),rot=90,just="left")
t3 <- textGrob("N20 PPB",gp=gpar(col="steelblue3"),rot=90,just="right")
t4 <- textGrob("Methane PPB",gp=gpar(col="darkseagreen4"),rot=270)
b1 <- textGrob("            ")
l1 <- rbind(c(5,2,3),
            c(1,2,3),
            c(4,2,3),
            c(5,2,3))
g1 <- arrangeGrob(t2,g,t4,t3,b1, layout_matrix=l1,widths=c(.5,12,.25),heights=c(6,1,1,6))
g2 <- h50_co2

#Symbols for the legend
p1 <-data.frame(rbind(c(1,1),
                      c(2,1),
                      c(3,1)))
colnames(p1) <- c("x","y")


#create plot of symbols
line1           <- data.frame(rbind(c(1.6,1),
                                    c(1.9,1)))
colnames(line1) <- c("x","y")
line2           <- data.frame(rbind(c(2.35,1),
                                    c(2.65,1)))
colnames(line2) <- c("x","y")
p2 <- ggplot(data=p1,aes(x=x,y=y,xmax=3,ymax=1.25,xmin=0,ymin=.25))+
  geom_point(aes(x=0,y=1),size=4,color="orangered3",shape=7,ylab="co2") +
  geom_point(aes(x=.5,y=1),size=4,color="steelblue3",shape=13,ylab="N20") + 
  geom_point(aes(x=1,y=1),size=4,color="darkseagreen4",shape=9,ylab="Methane") +
  geom_line(data=line1,aes(x=x,y=y),color="azure4",linetype="dotted",lwd=1)+
  geom_line(data=line2,aes(x=x,y=y),color="azure4",linetype="longdash",lwd=1)+
  annotate("text",x=0,y=.5,label="CO2",size=3,color="orangered3")+
  annotate("text",x=.5,y=,.5,label="N2O",size=3,color="steelblue3")+
  annotate("text",x=1,y=.5,label="Methane",size=3,color="darkseagreen4")+
  annotate("text",x=1.75,y=.5,label="Max BC Concentration",size=3,color="azure4")+
  annotate("text",x=2.5,y=.5,label="Average Concentration",size=3,color="azure4")+
  theme(axis.text.x = element_blank(),axis.text.y = element_blank(),
        axis.ticks = element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank(),
        panel.background = element_blank())
t5 <- textGrob("SOURCE: EPA Compilation of 10 underlying datasets")
l2 <- rbind(c(1,2),
            c(1,3))
g3 <- grid.arrange(p2,t5, ncol=1,heights=c(3,2))
g4 <- grid.arrange(b1,g3,b1, ncol=3,widths=c(1,8,1))

#Create layout
lay <- rbind(c(1,1),
             c(2,3),
             c(4,4))

#Combine into 1 plot
win.graph(800,600,10)
grid.arrange(t1,g1,g2,g4,layout_matrix = lay, widths=c(2,1), heights=c(1,9,1))

#Global temp with CO2 overlay plot
#Global temp subplot
plot1 <- ggplot()+
  geom_ribbon(data=global_avg, aes(x=as.numeric(year), y=temp, ymin=14.5, ymax=temp), fill="orangered3", color="orangered3")+
  scale_x_continuous(name="Year", limits=c(1850,2015),breaks=seq(1850,2015,15))+
  geom_line(data=global_avg, aes(x=as.numeric(year), y=temp), color="orangered4",lwd=1)+
  labs(title="Average Global Temperature vs CO2 Concentration", subtitle="1850 - 2015",y="")
#CO2 concentrations subplot
plot2 <- ggplot(data=global_co2, aes(x=as.numeric(year), y=average))+
  stat_smooth(lwd=3, se=FALSE, color="steelblue4")
scale_y_continuous(name="Average Temperature", limits=c(250,400),breaks=seq(250,400,50))

#combine temp and co2 subplots
two_plots(p1=plot1,p2=plot2)

#Add some labels
t6 <- textGrob("Average Global Temperature (Celcius)",gp=gpar(col="orangered4"),rot=90)
t7 <- textGrob("CO2 Concentration (PPM)",gp=gpar(col="steelblue4"),rot=270)
g1 <- arrangeGrob(t6,g,t7, ncol=3 ,widths=c(1,15,1))


#Add legend
p3 <- ggplot(data=p1,aes(x=x,y=y,xmax=2,ymax=1,xmin=0,ymin=.25))+
  geom_point(aes(x=.75,y=1),size=4,color="orangered3",shape=15,ylab="Temp") +
  geom_point(aes(x=1.25,y=1),size=4,color="steelblue4",shape=15,ylab="CO2") + 
  annotate("text",x=.75,.5,label="AVG Temp",size=3,color="orangered3")+
  annotate("text",x=1.25,.5,label="CO2",size=3,color="steelblue3")+
  theme(axis.text.x = element_blank(),axis.text.y = element_blank(),
        axis.ticks = element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank(),
        panel.background = element_blank())
t8 <- textGrob("SOURCE: Berkeley Earth Dataset Provided by Data.World")
g5 <- grid.arrange(b1,t8, ncol=1)
g6 <- grid.arrange(b1, g5,p3,ncol=3,widths=c(1,2,1))

#Ouput temp/co2
win.graph(800,600,10)
grid.arrange(g1,g6,ncol=1,heights=c(12,1))

#Plot climate forcing
plot3 <- ggplot()+
  geom_area(data=rad_force_m,aes(x=as.numeric(as.character(year)),y=value, fill=Gas))+
  scale_fill_brewer(palette="GnBu",direction = -1)+
  labs(x="Year", y="Radiative Forcing (W m^2)",title="Greenhouse Gas Radiative Forcing", subtitle="1979 - 2015")+
  scale_y_continuous(limits=c(0,3),breaks=seq(0,3,.25))+
  scale_x_continuous(limits=c(1979,2015),breaks=seq(1980,2015,5))
t5 <- textGrob("SOURCE: NOAA Annual Greenhouse Gas Index")

win.graph(800,600,10)
grid.arrange(plot3,t5,ncol=1,heights=c(20,1))
