    library(reshape)
    setwd("/Users/will2/repository/ExpertElicitation")

    data<-read.table("data.txt", header=TRUE)
    labels<-read.delim("labels.txt", header=TRUE)
    cat<-read.delim("categories.txt", header=TRUE)
    weights<-read.delim("weights.txt",header=TRUE)
   
    levels(data$Dist)
    levels(data$Param)
    l.params<-length(levels(data$Param))

#   Define the resolution of the x-axis (how many points to plot for each line)
    l.xaxis<-1000

    i<-0
    j<-0

#############################################################################################
#
#   Generate the historical parameter functions
#
#############################################################################################

hist_oil<-function(moment)
{
  #  Historical oil data from IEA.
  #  Citation: International Energy Agency (2011): Energy Prices and Taxes (Edition: 2011, Quarter 3).
  #  ESDS International, University of Manchester. DOI: http://dx.doi.org/10.5257/iea/ept/2011q3

  histoil<-matrix(c(1990,22.92,1991,20.06,1992,19.07,1993,16.58,1994,15.83,1995,17.29,1996,21.08,1997,19.32,
1998,12.64,1999,18.01,2000,28.45,2001,24.45,2002,24.58,2003,29.13,2004,37.75,
2005,53.79,2006,65,2007,73.8,2008,99.34,2009,62.39,2010,80.6),nrow=21,ncol=2,byrow=TRUE)

  #  Deflation data from HM Treasury
  #  http://www.hm-treasury.gov.uk/Economic_Data_and_Tools/GDP_Deflators/data_gdp_index.cfm

#  defl<-matrix(c(1990,59.739,1991,63.597,1992,65.988,1993,67.886,1994,68.960,1995,70.807,1996,73.370,1997,75.323,
#1998,	76.833,1999,78.315,2000,78.800,2001,79.941,2002,81.968,2003,83.895,2004,85.987,2005,87.867,2006,90.708,
#2007,	92.763,2008,95.671,2009,97.254,2010,100.000),nrow=21,ncol=2,byrow=TRUE)

  #  Deflation data from World Bank
  #  http://data.worldbank.org/indicator/NY.GDP.DEFL.KD.ZG

defl<-matrix(c(1990,65.21,1991,67.43,1992,68.85,1993,70.36,1994,71.79,1995,73.46,1996,74.76,1997,76.20,
1998,77.28,1999,78.41,2000,80.10,2001,81.92,2002,83.25,2003,85.05,2004,87.46,2005,90.37,2006,93.31,2007
,96.06,2008,98.15,2009,99.06,2010,100.00),nrow=21,ncol=2,byrow=TRUE)

  #  Real prices of oil

  histoild<-histoil[,2]/defl[,2]*100

  if (moment=="mean"){
    mean(histoild)}
  else if (moment=="min"){
    min(histoild)}
  else if (moment=="max"){
    max(histoild)}
  else if (moment=="density"){
    histoild
  }
  }

  hist_gdp<-function(moment)
  {

  #   Historical GDP figures derived from Office of National Statistics.  Figures are ABMI index of 
  #   Gross domestic product

  histgdp<-matrix(c(1990,0.0078,1991,-0.0139,1992,0.0015,1993,0.0222,1994,0.0428,1995,0.0305,1996,0.0289,
1997,0.0331,1998,0.0361,1999,0.0347,2000,0.0392,2001,0.0246,2002,0.0210,2003,0.0281,2004,0.0295,
2005,0.0217,2006,0.0279,2007,0.0268,2008,-0.0007,2009,-0.0487,2010,0.0135),ncol=2,byrow=TRUE)

  if (moment=="mean"){
    mean(histgdp[,2])}
  else if (moment=="min"){
    min(histgdp[,2])}
  else if (moment=="max"){
    max(histgdp[,2])}
  }

  hist_pop<-function(moment)
  {

  #   Historical population figures derived from ONS
  #   BB 2010 edition - Population, employment and GDP per head

  histpop<-matrix(c(1990,	57.237,1991	,57.439,1992,57.585,1993,57.714,1994,57.862,1995,58.025,1996,58.164,
1997,58.314,1998,	58.475,1999,58.684,2000,58.886,2001,59.113,2002,59.319,2003,59.552,2004,59.846,
2005,	60.238,2006,60.584,2007,60.986,2008,61.398,2009,61.792,2010,62.195),ncol=2,byrow=TRUE)

  if (moment=="mean"){
    mean(histpop[,2])}
  else if (moment=="min"){
    min(histpop[,2])}
  else if (moment=="max"){
    max(histpop[,2])}
  }

####    Establish upper and lower bounds for distribution plots
    
    bottom.m<-by(data$Lo, data$Param, min) * .5
    top.m<-by(data$Up, data$Param, max) * 1.5

####    Create the values for the x-axis from the data

    by.m<-((top.m-bottom.m))/l.xaxis
    x.m<-matrix(,l.params,l.xaxis+1)
    for (i in 1:l.params){
        x.m[i,]<-seq(bottom.m[i], top.m[i],by.m[i])
    }

    N.expert.m<-by(data$Expert,data$Param,length)
    N.expert.max<-max(N.expert.m)

#   Fill array for density plots
    dists.m<-array(,c(N.expert.max,l.xaxis+1,l.params))

#   Fill array for boxplots
    cumdists.m<-array(,c(N.expert.max,l.xaxis+1,l.params))

####    Fill an array with the list of distributions        

    c <- melt(data,id.var=c("Expert","Param"),measure.var=c("Dist"),value=na) 
    cdist.m<-cast(c,Expert~...~Param)     


####    Populate a multi-dimensional array with p(x)

    m <- melt(data,id.var=c("Expert","Param"),measure.var=c("Lo","Up","A","B"),na.rm=TRUE)
    data.m<-cast(m,Expert~...~Param)
    
for (i in 1:l.params){
    for (j in 1:N.expert.max){

    if (is.na(cdist.m[j,1,i])){
        dists.m[j,,i]<-0    
    } else if (cdist.m[j,1,i] =="dbeta"|cdist.m[j,1,i] =="Beta"){
        dists.m[j,,i]<-1/(data.m[j,2,i]-data.m[j,1,i])*dbeta((x.m[i,]-data.m[j,1,i])/(data.m[j,2,i]-data.m[j,1,i]),data.m[j,3,i],data.m[j,4,i])
        cumdists.m[j,,i]<-data.m[j,1,i] + rbeta(l.xaxis+1,data.m[j,3,i],data.m[j,4,i])*(data.m[j,2,i]-data.m[j,1,i])
    } else if (cdist.m[j,1,i] =="norm"|cdist.m[j,1,i] =="Norm"|cdist.m[j,1,i] =="Normal"){
        dists.m[j,,i]<-dnorm(x.m[i,],data.m[j,3,i],data.m[j,4,i])
        cumdists.m[j,,i]<-rnorm(l.xaxis+1,data.m[j,3,i],data.m[j,4,i])
    } else if (cdist.m[j,1,i] =="dgamma"|cdist.m[j,1,i] =="Gamma"){
        dists.m[j,,i]<-dgamma(x.m[i,],data.m[j,3,i],1/data.m[j,4,i])
        cumdists.m[j,,i]<-rgamma(l.xaxis+1,data.m[j,3,i],1/data.m[j,4,i])
    } else if (cdist.m[j,1,i] =="dlnorm"|cdist.m[j,1,i] =="Lognorm"|cdist.m[j,1,i] =="lognorm"){
        dists.m[j,,i]<-dlnorm(x.m[i,],data.m[j,3,i],data.m[j,4,i])
        cumdists.m[j,,i]<-rlnorm(l.xaxis+1,data.m[j,3,i],data.m[j,4,i])
    } else {
        dists.m[j,,i]<-0
    }

}}

#############################################################################################
# 
#   Calculate xlims
#
##############################################################################################

    threshold<-100
    dists.m2<-dists.m
    dists.m2[dists.m2=="Inf"]<-0
    max.dist.m<-apply(dists.m2,3,max,na.rm=TRUE)
    
    test.m<-array(max.dist.m/threshold,c(l.params,length(x.m[1,])))
    #sum.m<-array(colSums(dists.m),c(l.params,length(x.m[1,])))
    sum.m2<-(t(apply(dists.m,c(2,3),max,na.rm=TRUE))>=test.m)*x.m 
    sum.m2[sum.m2==0]<-NA   
    minx.m<-apply(sum.m2,1,min, na.rm=T)
    maxx.m<-apply(sum.m2,1,max, na.rm=T)        


#############################################################################################
####
####    Set up frame for plots
####
#############################################################################################

    pdf("Density and Box Plots.pdf", width=6,height=10,onefile=TRUE)

#   Set background to white for journal publications.  Ensures one param plot per page in pdf

    par(mfcol=c(2,1),bg="white")

#############################################################################################
####
####    Make the plots (looping over the parameters)
####
#############################################################################################

####  LOOP BEGIN
for (i in 1:l.params){


### Calculate the linear average of the beliefs (unweighted linear opinion pool)
    
    best.linear.pool.m<-apply(dists.m[,,i],2,mean,na.rm=TRUE)

### Build the axes and plot the lines of the distributions
    
    par(mar=c(1,4,1,2) + 0.1)   
    matplot(x.m[i,],t(dists.m[,,i]),xlim=c(minx.m[i],maxx.m[i]),
      ylim=c(0,max.dist.m[i]),type="l", lwd=2,lty=3,xlab="",ylab="p(x)",cex.axis=0.9,xaxt="n",las=1)
    axis(1,labels=NA,xlim=c(minx.m[i],maxx.m[i]))

### Add units to x-axis         
    title(sub=labels[which(labels[,1]==dimnames(data.m)$Param[i]),5])

### Add the linear opinion pool to the plot    
    matlines(x.m[i,],best.linear.pool.m, lty=1, lwd=2)    


#############################################################################################
#
#   Add the historical ranges to the plot (depending on which parameter is described in plot)  
#  
#############################################################################################

drawline<-function(x,text,unit,rounding=0,colour="black",plot=0,pch=25){
    if(plot==1){
    plot(x,0,col=colour,pch=pch,axes=FALSE,ann=FALSE,type="p",xlim=c(minx.m[i],maxx.m[i]),ylim=c(0,0),
    mar=c(0,0,0,0))
}

    if(plot==0){
    points(x,0,col=colour,pch=pch)
}

}

drawlineonly<-function(x)
{
  abline(v=x,lty=2,lwd=2,col="red")
}


###    Set legend constants
strwidth<-60	# width of wrap
legtextsz<-0.8	# size of legend text (0<x<1)
legplace<-"topright"	#  location of legend

if (dimnames(data.m)$Param[i]=="Pop"){

  
  drawline(hist_pop("min"),"UK Population in 1990: "," million",2,"blue",0,24)
  drawline(hist_pop("max"),"UK Population in 2010: "," million",2,"red",0,25)
  legend(legplace,,c(
  paste("Linear opinion pool"),
  paste("UK population in 1990: ",signif(hist_pop("min"),digits=2)," million"),
  paste("UK population in 2010: ",signif(hist_pop("max"),digits=2)," million")),
    pch=c(NA,24,25),lty=c(1,NA,NA),lwd=c(2,NA,NA),col=c("black","blue","red"),bg="white",cex=legtextsz,bty="n")
}

if(dimnames(data.m)$Param[i]=="GDP"){
  drawline(hist_gdp("mean")*100,"Historical GDP 1990 to 2010: "," %",2)
  legend(legplace,,
  c("Linear opinion pool",
    paste("UK GDP growth 1990 to 2010: ",signif(hist_gdp("mean")*100,digits=2)," %",sep="")),
  pch=c(NA,25),col="black",lty=c(1,NA),lwd=c(2,NA),
    bg="white",cex=legtextsz,bty="n")
}

if(dimnames(data.m)$Param[i]=="Oil"){
  drawline(hist_oil("min"),"Oil price in 1990: $"," /barrel",2,"black",0,24)
  drawline(hist_oil("max"),"Oil price in 2010: $"," /barrel",3,"red",0,25)
  lines(density(hist_oil("density")),col="blue",lwd=2)
    legend(legplace,,
      c(
        paste("Linear opinion pool"),
        paste("Oil price in 1990: $",signif(hist_oil("min"),digits=2)," /barrel"),
        paste("Oil price in 2010: $",signif(hist_oil("max"),digits=2)," /barrel"),
        paste("KDE of oil price 1990 to 2010")
        ),
        pch=c(NA,24,25,NA),lty=c(1,NA,NA,1),lwd=c(2,NA,NA,2),col=c("black","black","red","blue"),bg="white",cex=legtextsz,bty="n")
}

if(dimnames(data.m)$Param[i]=="calib"){
  drawline(301.2,,,4,"black")
  legend(legplace,,
  c("Linear opinion pool",
    "Actual length of Moscow underground: 301.2 km"),
    pch=c(NA,25),lty=c(1,NA),lwd=c(2,NA),col="black",bg="white",cex=legtextsz,bty="n")
}


if(dimnames(data.m)$Param[i]=="Temp"){
  drawline(19.1,"","C",3,"black")
  legend("topleft",,c("Linear opinion pool",strwrap("Median standardised daytime living room temperature: 19.1\217C",width=strwidth)),
pch=c(NA,25),lty=c(1,NA),lwd=c(2,NA),col="black",bg="white",cex=legtextsz,bty="n")
}
if(dimnames(data.m)$Param[i]=="GHG"){
  drawline(14.8,"Average price of CO2 in the EU ETS in 2010: £","",3,"black")
  legend(legplace,,c("Linear opinion pool","Average price of CO2 in the EU ETS in 2010: £14.8 /tCO2e"),pch=c(NA,25),col="black",
    ,lty=c(1,NA),lwd=c(2,NA),bg="white",cex=legtextsz,bty="n")
}
if(dimnames(data.m)$Param[i]=="Elec"){
  #drawline()
  legend(legplace,,c("Linear opinion pool"),col="black",
    ,lty=1,lwd=2,bg="white",cex=legtextsz,bty="n")

}


#############################################################################################
#
#   Draw the box plots
#
#############################################################################################
    par(mar=c(5,4,0,2)+0.1)
    boxplot(t(cumdists.m[,,i]),outline=F,horizontal=T,ylim=c(minx.m[i],maxx.m[i]),
cex.axis=0.9,las=1,yaxt="n",range=0,lwd=1)

### Shows Expert Affiliation and Calibration rating on right axis       
    title(ylab="Expert",
xlab=labels[which(labels[,1]==dimnames(data.m)$Param[i]),3])

    axis(labels=labels(data.m[,1,i]),side=2,at=1:N.expert.max,cex.axis=0.9,las=1)

#############################################################################################
#
#   Add historical lines to box plots
#
#############################################################################################

if (dimnames(data.m)$Param[i]=="Pop"){
  drawlineonly(hist_pop("min"))
  drawlineonly(hist_pop("max"))
}
if(dimnames(data.m)$Param[i]=="GDP"){
  drawlineonly(hist_gdp("mean")*100)
}
if(dimnames(data.m)$Param[i]=="Oil"){
  drawlineonly(hist_oil("min"))
  drawlineonly(hist_oil("max"))
  }
if(dimnames(data.m)$Param[i]=="calib"){
  drawlineonly(301.2)
}
if(dimnames(data.m)$Param[i]=="Temp"){
  drawlineonly(19.1)
}
if(dimnames(data.m)$Param[i]=="GHG"){
  drawlineonly(14.8)
}
if(dimnames(data.m)$Param[i]=="Elec"){
  #drawlineonly()
}
 
}   
####  LOOP END

#############################################################################################
#
#   Print the PDFs to file
#
#############################################################################################

dev.off()