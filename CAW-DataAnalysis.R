#########################################################################################
#                                                                                       #
#                       Crime Against Women Data Analysis                               #
#                             using R Visualization                                     #
#                       =====================================                           #
#                            Author: Anuradha Aggarwal                                  #
#                                                                                       #
#                                                                                       #
#########################################################################################


#===Plots using Normalized dataset per lakh women================
#====Crime wise (State Vs Crime per lakh women)==================
Nplots <- function(Col_name,y,colorName){
  print("inside function")
  Rset<- read.csv("NormalizedPLW.csv",header=TRUE)
  Rset
  library(ggplot2)
  titleName= paste("State Vs ",y,"Cases Per Lakh Women")
  xname=paste("Number of ",y, "Cases Per Lakh Women")
  ggplot(Rset, aes(Col_name,fill=STATE.UT) ) +
    geom_histogram(color="white")+
    labs(title=titleName, y="Number of States", x=xname)+
    theme(plot.title=element_text(size=20, 
                                  face="bold", 
                                  family="American Typewriter",
                                  color=colorName,
                                  hjust=0.5,
                                  lineheight=1.2),  # title
          plot.subtitle=element_text(size=15, 
                                     family="American Typewriter",
                                     face="bold",
                                     hjust=0.5),  # subtitle
          plot.caption=element_text(size=15),  # caption
          axis.title.x=element_text(vjust=3,  
                                    size=15),  # X axis title
          axis.title.y=element_text(size=15),  # Y axis title
          axis.text.x=element_text(size=10, 
                                   angle = 30,
                                   vjust=.5),  # X axis text
          axis.text.y=element_text(size=10))  # Y axis text
}
Rset<- read.csv("NormalizedPLW.csv",header=TRUE)
Rset
Nplots(Rset$Rape,"Rape","tomato")
Nplots(Rset$KIDNAPPING...ABDUCTION,"KIDNAPPING & ABDUCTION","blue")
Nplots(Rset$DOWRY.DEATH,"DOWRY DEATH","violetred")
Nplots(Rset$ASSAULT.ON.WOMEN.WITH.INTENT.TO.OUTRAGE.HER.MODESTY,"ASSAULT ON WOMEN WITH INTENT TO OUTRAGE HER MODESTY","orange")
Nplots(Rset$INSULT.TO.THE.MODESTY.OF.WOMEN,"INSULT TO THE MODESTY OF WOMEN","darkgreen")
Nplots(Rset$CRUELTY.BY.HUSBAND.OR.RELATIVES ,"CRUELTY BY HUSBAND OR RELATIVES ","brown")
Nplots(Rset$IMMORAL.TRAFFIC.PREVENTION.ACT,"IMMORAL TRAFFIC(PREVENTION)ACT","purple")
Nplots(Rset$INDECENT.REPRESENTATION.OF.WOMEN.PREVENTION.ACT,"INDECENT REPRESENTATION OF WOMEN(PREVENTION)ACT","magenta")


#===========Region Wise (Year vs crime Rate)======================
Rplot <-function(reg,CrimeRate){
  
          Region<- read.csv(reg,header=TRUE)
          Region

          library(ggplot2)
          #ggplot(north, aes(Year)) + 
          # geom_bar(aes(fill = State), position = "fill")
          titleName=paste("Year Vs",CrimeRate,"Region")
 ggplot(data= Region,mapping= aes(x = Year, y = Region$CrimeRate, group = State,color=State))+
         geom_line()+xlim(c(2000, 2013))+
  labs(title=titleName, y="Total Number of Crimes", x="Years")+
  theme(plot.title=element_text(size=20, 
                    face="bold", 
                    family="American Typewriter",
                    color="tomato",
                    hjust=0.5,
                    lineheight=1.2),  # title
                    plot.subtitle=element_text(size=15, 
                    family="American Typewriter",
                    face="bold",
                   hjust=0.5),  # subtitle
                   plot.caption=element_text(size=15),  # caption
                   axis.title.x=element_text(vjust=10,  
                   size=15),  # X axis title
                   axis.title.y=element_text(size=15),  # Y axis title
                   axis.text.x=element_text(size=10, 
                  angle = 30,
                  vjust=.5),  # X axis text
                  axis.text.y=element_text(size=10))  # Y axis text

}
Rplot("north1.csv","CrimeRate.In.North")
Rplot("south.csv","CrimeRate.In.South")
Rplot("east.csv","CrimeRate.In.East")
Rplot("west.csv","CrimeRate.In.West")
Rplot("central.csv","CrimeRate.In.Central")
Rplot("northEast.csv","CrimeRate.In.NorthEast")


#======TreeMaps============
install.packages("treemap")
Treemaplot <-function(reg,CrimeRate){
  Region<- read.csv(reg,header=TRUE)
  Region
  
  library(treemap)
  treemap(Region, index=c( "Year","State"), vSize=CrimeRate, type="index")
  treemap(Region, index=c( "State","Year"), vSize=CrimeRate, type="index")
  
}

Treemaplot("north1.csv","CrimeRate.In.North")
Treemaplot("south.csv","CrimeRate.In.South")
Treemaplot("east.csv","CrimeRate.In.East")
Treemaplot("west.csv","CrimeRate.In.West")
Treemaplot("central.csv","CrimeRate.In.Central")
Treemaplot("northEast.csv","CrimeRate.In.NorthEast")


#=========For Union Territory============
UT<- read.csv("UT.csv",header=TRUE)
UT

#install.packages("treemap")
library(treemap)
treemap(UT, index=c( "UT","Year"), vSize="CrimeRate.In.UT", type="index")
library(ggplot2)
#ggplot(north, aes(Year)) + 
# geom_bar(aes(fill = UT), position = "fill")

ggplot(data= UT,mapping= aes(x = Year, y = CrimeRate.In.UT, group = UT,color=UT)) +
  geom_line()+xlim(c(2000, 2013))+
  labs(title="Year Vs Crime-Rate In Union Territories", y="Total Number of Crimes", x="Years")+
  theme(plot.title=element_text(size=20, 
                                face="bold", 
                                family="American Typewriter",
                                color="tomato",
                                hjust=0.5,
                                lineheight=1.2),  # title
        plot.subtitle=element_text(size=15, 
                                   family="American Typewriter",
                                   face="bold",
                                   hjust=0.5),  # subtitle
        plot.caption=element_text(size=15),  # caption
        axis.title.x=element_text(vjust=10,  
                                  size=15),  # X axis title
        axis.title.y=element_text(size=15),  # Y axis title
        axis.text.x=element_text(size=10, 
                                 angle = 30,
                                 vjust=.5),  # X axis text
        axis.text.y=element_text(size=10))  # Y axis text


#=======State Vs Crime Rate in 2001==========
Dset<- read.csv("CAW2001-2012.csv",header=TRUE)
Dset

ggplot(Dset, aes(X2001,STATE.UT, colour= CRIME.HEAD)) +
  geom_point()+coord_cartesian(xlim=c(50, 3000))+
  
  #geom_point(aes(shape=CRIME.HEAD))+coord_cartesian(xlim=c(50, 10000))+
  labs(title="States Vs Crime-Rate In 2001", y="States/Union Territory", x="Crime Rate in 2001")+
  theme(plot.title=element_text(size=20, 
                                face="bold", 
                                family="American Typewriter",
                                color="blue",
                                hjust=0.5,
                                lineheight=1.2),  # title
        plot.subtitle=element_text(size=15, 
                                   family="American Typewriter",
                                   face="bold",
                                   hjust=0.5),  # subtitle
        plot.caption=element_text(size=15),  # caption
        axis.title.x=element_text(vjust=4,  
                                  size=15),  # X axis title
        axis.title.y=element_text(size=15),  # Y axis title
        axis.text.x=element_text(size=10, 
                                 angle = 30,
                                 vjust=.5),  # X axis text
        axis.text.y=element_text(size=10))  # Y axis text

#======Tree Map========
library(treemap)
treemap(dataSet, index=c("CRIME.HEAD", "STATE.UT"), vSize="X2001", type="index")
