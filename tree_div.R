
library(vegan)
library(sp)
library(rgdal)
library(foreach)
library(doParallel)
library(scales)
library(ggplot2)
library(gridExtra)
library(gstat)
library(nlme)
library(ggeffects)
library(vegan)

### set path and working directory
# make sure all files in the data folder are in your chosen working directory
path<-getwd()
path<-"C:/Users/User/Documents/GitHub/WT_FIA_TreeDiv/data"
path<-"C:/Users/rouf1703/Documents/UdeS/GitHub/WT_FIA_TreeDiv/data"


#################################################
### Load Data ###################################
#################################################


### data ########################################
d <- read.csv(file.path(path,"tree_div.csv"),header=TRUE)
d$township<-paste(d$TOWNNAME,d$STATE,sep="_")

spwt<-names(d)[grep("_WT",names(d))]
spfia<-names(d)[grep("_FIA",names(d))][-1]

setdiff(gsub("_WT","",spwt),gsub("_FIA","",spfia))
setdiff(gsub("_FIA","",spfia),gsub("_WT","",spwt))
intersect(gsub("_FIA","",spfia),gsub("_WT","",spwt))

simpson<-function(x){
  s<-sum(x^2)
  #s/(1-s)
  1/s
}

shannon<-function(x){
  s<-x[x>0]
  exp(-sum(s*log(s)))
}

type<-"full" # c("full","alpha","trees")

l<-split(d,1:nrow(d))
l<-lapply(l,function(i){
  iwt<-i[,spwt]
  ifia<-i[,spfia]
  i$alpha_wt<-sum(i[,spwt]>0)  
  i$alpha_fia<-sum(i[,spfia]>0)
  i$alpha_dif<-i$alpha_fia-i$alpha_wt
  if(type=="trees" & (i$Trees_Wit!=i$Trees_FIA)){ # if the same number of trees, no resampling is done
    w<-which.max(c(i$Trees_Wit,i$Trees_FIA))
    n<-min(c(i$Trees_Wit,i$Trees_FIA))
    nreps<-1000
    if(w==1){
      s<-lapply(1:nreps,function(j){
        tab<-table(sample(as.factor(spwt),n,prob=as.vector(iwt),replace=TRUE))
        tab<-tab/sum(tab)
        c(shannon(tab),simpson(tab))
      })
      ss<-colMeans(do.call("rbind",s))
      i$Shan_fia<-apply(ifia,1,shannon)
      i$Shan_wt<-ss[1]
      i$Shan_dif<-i$Shan_fia-i$Shan_wt
      i$Simp_fia<-apply(ifia,1,simpson)
      i$Simp_wt<-ss[2]
      i$Simp_dif<-i$Simp_fia-i$Simp_wt
    }else{
      s<-lapply(1:nreps,function(j){
        tab<-table(sample(as.factor(spfia),n,prob=as.vector(ifia),replace=TRUE))
        tab<-tab/sum(tab)
        c(shannon(tab),simpson(tab))
      })
      ss<-colMeans(do.call("rbind",s))
      i$Shan_fia<-ss[1]
      i$Shan_wt<-apply(iwt,1,shannon)
      i$Shan_dif<-i$Shan_fia-i$Shan_wt
      i$Simp_fia<-ss[2]
      i$Simp_wt<-apply(iwt,1,simpson)
      i$Simp_dif<-i$Simp_fia-i$Simp_wt
    }
  }else{
    if(type=="alpha" & (i$alpha_wt!=i$alpha_fia)){ # if same alpha diversity, no resampling is done
      w<-which.max(c(i$alpha_wt,i$alpha_fia))
      n<-min(c(i$alpha_wt,i$alpha_fia))
      if(w==1){
        spsel<-names(rev(sort(i[,spwt]))[1:n])  
        iwt<-i[,spsel]
      }else{
        spsel<-names(rev(sort(i[,spfia]))[1:n])
        ifia<-i[,spsel]  
      }
    }
    i$Shan_fia<-apply(ifia,1,shannon)
    i$Shan_wt<-apply(iwt,1,shannon)
    i$Shan_dif<-i$Shan_fia-i$Shan_wt
    i$Simp_fia<-apply(ifia,1,simpson)
    i$Simp_wt<-apply(iwt,1,simpson)
    i$Simp_dif<-i$Simp_fia-i$Simp_wt
  }
  i
})
d<-do.call("rbind",l)
hist(d$alpha_fia-d$alpha_wt)

### #############################################

plot(d$FIA_Plots,d$Trees_FIA)

plot(d$FIA_Plots,jitter(d$alpha_fia))

plot(d$FIA_Plots,jitter(d$alpha_fia-d$alpha_wt))

plot(d$FIA_Plots,d$Shan_fia)
plot(d$FIA_Plots,d$Simp_fia)

ggplot(d,aes(FIA_Plots,Shan_fia))+geom_point()+geom_smooth()+theme_bw()
ggplot(d,aes(FIA_Plots,Simp_fia))+geom_point()+geom_smooth()+theme_bw()

ggplot(d,aes(FIA_Plots,Shan_dif))+geom_point()+geom_smooth()+theme_bw()
ggplot(d,aes(FIA_Plots,Simp_dif))+geom_point()+geom_smooth()+theme_bw()

ggplot(d,aes(FIA_Plots,alpha_fia))+geom_point()+geom_smooth()+theme_bw()
ggplot(d,aes(FIA_Plots,alpha_fia))+geom_point()+geom_smooth()+theme_bw()
ggplot(d,aes(FIA_Plots,alpha_dif))+geom_point()+geom_smooth()+theme_bw()

ggplot(d,aes(Trees_FIA,alpha_dif))+geom_point()+geom_smooth()+theme_bw()
ggplot(d,aes(Trees_Wit,alpha_dif))+geom_point()+geom_smooth()+theme_bw()

ggplot(d,aes(log(Trees_Wit),Shan_dif))+geom_point()+geom_smooth()+theme_bw()
ggplot(d,aes(log(Trees_Wit),Simp_dif))+geom_point()+geom_smooth()+theme_bw()
ggplot(d,aes(log(Trees_Wit),alpha_dif))+geom_point()+geom_smooth()+theme_bw()

ggplot(d,aes(log(Trees_Wit/Trees_FIA),Shan_dif))+geom_point()+geom_smooth()+theme_bw()
ggplot(d,aes(log(Trees_Wit/Trees_FIA),Simp_dif))+geom_point()+geom_smooth()+theme_bw()
ggplot(d,aes(log(Trees_Wit/Trees_FIA),alpha_dif))+geom_point()+geom_smooth()+theme_bw()

ggplot(d,aes(log(Trees_Wit),Simp_dif))+geom_point()+geom_smooth()+theme_bw()
ggplot(d,aes(log(Trees_Wit),peak_ag))+geom_point()+geom_smooth()+theme_bw()
ggplot(d,aes(peak_ag,log(Trees_Wit)))+geom_point()+geom_smooth()+theme_bw()
cor(d$peak_ag,log(d$Trees_Wit))

ggplot(d,aes(peak_ag,alpha_dif))+geom_point()+geom_smooth()+theme_bw()
ggplot(d,aes(log(Trees_Wit/Trees_FIA),peak_ag))+geom_point()+geom_smooth()+theme_bw()

### compute diversity indices ###################

dd<-d

vs<-c("Shan_fia","Shan_wt","Shan_dif","Simp_fia","Simp_wt","Simp_dif")
range(as.matrix(dd[,vs])-as.matrix(d[,vs]))
hist(as.matrix(dd[,vs])-as.matrix(d[,vs]))

### environmental PCA ###########################
pca.input<-d[,c("clay","elevation","ph_soil","sand","ruggedness")]
env.pca<-rda(pca.input,scale=TRUE)
env.pca$CA$eig/env.pca$tot.chi
scores(env.pca)$species
biplot(env.pca,display=c("sites","species"),type=c("text","points"))
d$envPCA1<-scores(env.pca)$sites[,1]
d$envPCA2<-scores(env.pca)$sites[,2]

### turn data to spatial object #################
ds<-d
coordinates(ds)<-~LONGITUDE+LATITUDE
proj4string(ds)<-"+init=epsg:4326"

### climate data ################################
div<-readOGR(path,"climate_div")
div<-div[!is.na(div$ST),]

temp<-read.csv(file.path(path,"temperature_fig-3.csv"),skip=6)
names(temp)<-c("div","tempc")
temp$div<-ifelse(nchar(temp$div)==5,gsub("00","0",temp$div),temp$div)
div$div<-paste0(div$ST_,formatC(div$DIV_,flag="0",width=2))
div$tempc<-temp$tempc[match(div$div,temp$div)]

### nitrogen data ################################
n<-read.csv(file.path(path,"US_historical_deposition_rev.csv"))
coordinates(n)<- ~lon + lat
proj4string(n)<-"+init=epsg:4326"

# project data to climate data projection
ds<-spTransform(ds,CRS(proj4string(div)))
n<-spTransform(n,CRS(proj4string(div)))


################################################
### Interpolate Temperature
#################################################

x <- idw(tempc~1,locations=div,newdata=ds,nmax=4,idp=2)

vals<-x@data[,1]

d$tempdiff_i<-vals
ds$tempdiff_i<-vals

o<-over(spTransform(ds,CRS(proj4string(div))),div)

d$tempdiff<-o$tempc
ds$tempdiff<-o$tempc


#################################################
### Interpolate Nitrogen
#################################################

nm<-c("N_khy_1850","N_khy_2000","N_to_1984","N_to_2000")
logn<-as.data.frame(apply(n@data[,nm],2,identity))
n@data[,paste0("log",nm)]<-logn
nm<-paste0("log",nm)

l<-list()
for(i in seq_along(nm)){
	n$value<-n@data[,nm[i]]
	x<-idw(value~1,locations=n,newdata=ds,nmax=4,idp=2)
 vals<-x@data[,1]
	l[[i]]<-vals
}

nvals<-do.call("data.frame",l)
d[,paste0(nm,"_i")]<-nvals
ds@data[,paste0(nm,"_i")]<-nvals


#################################################
### Models
#################################################

### add projected coordinates to data.frame
ds$X<-coordinates(ds)[,1]
ds$Y<-coordinates(ds)[,2]

### variable names and scaled variable names
ds$logratio<-log(ds$Trees_Wit/ds$Trees_FIA)
v<-c("peak_ag","tempdiff_i","N_to_1984_i","Area_SqKM","envPCA1","temp_gdd","logratio")
vs<-paste0(v,"_sc")

### scale variables and save means and sds of unscaled variables
s<-lapply(ds@data[,v],function(i){c(mean(i),sd(i))})
names(s)<-v
ds@data[,vs]<-lapply(1:length(v),function(i){
	(ds@data[,v[i]]-s[[i]][1])/s[[i]][2]
})

### check for spatial autocorrelation
fit<-lme(formula(paste0("Simp_dif~",paste(vs,collapse="+"))),ds@data,random=~1|Ecoregion)
ds$resid<-resid(fit,type="n")
va<-variogram(resid~1,data=ds,width=1000,cutoff=200000)
plot(va)

### models
m_Simp<-lme(formula(paste0("Simp_dif~",paste(vs,collapse="+"))),ds@data,random=~1|Ecoregion,correlation=corExp(30000,form=~X+Y,nugget=TRUE))
m_Shan<-lme(formula(paste0("Shan_dif~",paste(vs,collapse="+"))),ds@data,random=~1|Ecoregion,correlation=corExp(30000,form=~X+Y,nugget=TRUE))
m_Alpha<-lme(formula(paste0("alpha_dif~",paste(vs,collapse="+"))),ds@data,random=~1|Ecoregion,correlation=corExp(30000,form=~X+Y,nugget=TRUE))
#m_Alpha<-lme(alpha_dif ~ peak_ag_sc + tempdiff_i_sc + N_to_1984_i_sc + Area_SqKM_sc + envPCA1_sc + temp_gdd_sc,ds@data,random=~1|Ecoregion,correlation=corExp(30000,form=~X+Y,nugget=TRUE))

#plot(ggpredict(m_Alpha,terms="peak_ag_sc"),add=TRUE)
#plot(ggpredict(m_Alpha,terms="logratio_sc"),add=TRUE)

#plot(ggpredict(m_Shan,terms="peak_ag_sc"),add=TRUE)
#plot(ggpredict(m_Shan,terms="logratio_sc"),add=TRUE)

### model coefficients
as.data.frame(summary(m_Shan)$tTable)
as.data.frame(summary(m_Simp)$tTable)
as.data.frame(summary(m_Alpha)$tTable)

### Figure 1
### marginal effects and change distributions
png(file.path(path,paste0("peak_ag",paste0("_",type),"test22.png")),pointsize=4,width=10,height=8,units="in",res=300)

g1<-as.data.frame(ggpredict(m_Simp,terms=c("peak_ag_sc[n=100]")))
g1[,c("x")]<-(g1[,c("x")]*s[["peak_ag"]][2])+s[["peak_ag"]][1]
g1<-ggplot(g1)+
	geom_hline(yintercept=0,linetype=2,colour=gray(0.2))+
	geom_point(data=d,aes(peak_ag,Simp_dif),size=1.75,alpha=0.5,colour="green4")+
	geom_ribbon(aes(x=x,ymin=conf.low,ymax=conf.high),fill=gray(0.5,0.75))+
	geom_line(aes(x=x,y=predicted),size=1)+
	xlab("Maximum historical agriculture (proportion)")+
	ylab("Change in Simpson diversity")+
	theme_light()+
	theme(axis.text=element_text(size=rel(1.25)),axis.title=element_text(size=rel(1.25)),panel.grid=element_blank(),plot.margin=unit(rep(0.5,4),"cm"),panel.border = element_rect(colour=gray(0.15), fill = NA))+
	scale_y_continuous(breaks=seq(-8,8,by=2))+scale_x_continuous(breaks=seq(0,1,by=0.2))+
	annotate(geom='text',label='D',x=-Inf,y=Inf,hjust=-0.6,vjust=1.4,size=8)

g2<-as.data.frame(ggpredict(m_Shan,terms=c("peak_ag_sc[n=100]")))
g2[,c("x")]<-(g2[,c("x")]*s[["peak_ag"]][2])+s[["peak_ag"]][1]
g2<-ggplot(g2)+
	geom_hline(yintercept=0,linetype=2,colour=gray(0.2))+
	geom_point(data=d,aes(peak_ag,Shan_dif),size=1.75,alpha=0.5,colour="green4")+
	geom_ribbon(aes(x=x,ymin=conf.low,ymax=conf.high),fill=gray(0.5,0.75))+
	geom_line(aes(x=x,y=predicted),size=1)+
	xlab("Maximum historical agriculture (proportion)")+
	ylab("Change in Shannon diversity")+
	theme_light()+
	theme(axis.text=element_text(size=rel(1.25)),axis.title=element_text(size=rel(1.25)),panel.grid=element_blank(),plot.margin=unit(rep(0.5,4),"cm"),panel.border = element_rect(colour=gray(0.15), fill = NA))+
	scale_y_continuous(breaks=seq(-8,8,by=2))+scale_x_continuous(breaks=seq(0,1,by=0.2))+
	annotate(geom='text',label='C',x=-Inf,y=Inf,hjust=-0.6,vjust=1.4,size=8)

g3<-ggplot(data=d,aes(Shan_dif))+
	geom_histogram(fill=gray(0.75,1),colour="grey65",breaks=seq(-8,8,by=1))+
	geom_vline(xintercept=mean(d$Shan_dif),size=0.7,colour="tomato")+
	xlab("Change in Shannon diversity")+
	ylab("Frequency")+
	theme_light()+
	theme(axis.text=element_text(size=rel(1.25)),axis.title=element_text(size=rel(1.25)),panel.grid=element_blank(),plot.margin=unit(rep(0.5,4),"cm"),panel.border = element_rect(colour=gray(0.15), fill = NA))+
	scale_x_continuous(breaks=seq(-8,8,by=2))+
	annotate(geom='text',label='A',x=-Inf,y=Inf,hjust=-0.6,vjust=1.4,size=8)

g4<-ggplot(data=d,aes(Simp_dif))+
	geom_histogram(fill=gray(0.75,1),colour="grey65",breaks=seq(-8,8,by=1))+
	geom_vline(xintercept=mean(d$Simp_dif),size=0.7,colour="tomato")+
	xlab("Change in Simpson diversity")+
	ylab("Frequency")+
	theme_light()+
	theme(axis.text=element_text(size=rel(1.25)),axis.title=element_text(size=rel(1.25)),panel.grid=element_blank(),plot.margin=unit(rep(0.5,4),"cm"),panel.border = element_rect(colour=gray(0.15), fill = NA))+
	scale_x_continuous(breaks=seq(-8,8,by=2))+
	annotate(geom='text',label='B',x=-Inf,y=Inf,hjust=-0.6,vjust=1.4,size=8)


grid.arrange(grobs=list(g3,g4,g2,g1),ncol=2)
dev.off()


#################################################
### Bray-Curtis Dissimilarity
#################################################

d<-read.csv(file.path(path,"tree_div.csv"))

### species names for each surveys
fix<-c("_WT","_FIA")
g1<-grep(fix[1],names(d))
g2<-grep(fix[2],names(d))[-1] # removes the generic trees column

### Bray-Curtis dissimilarities
bc1<-as.matrix(vegdist(d[,g1]))
bc2<-as.matrix(vegdist(d[,g2]))

### calculates great circle distances
s<-spDists(as.matrix(d[,c("LONGITUDE","LATITUDE")]),longlat=TRUE)

### remove duplicate pairs
bc1[upper.tri(bc1,diag=TRUE)]<-NA
bc2[upper.tri(bc2,diag=TRUE)]<-NA
s[upper.tri(s,diag=TRUE)]<-NA
x<-data.frame(dist=as.vector(s),bc1=as.vector(bc1),bc2=as.vector(bc2))
x<-x[!is.na(x$dist),]

### set number of cores used
registerDoParallel(detectCores()-1) 
getDoParWorkers()

### bootstrap loess curves and difference and generate predictions
nboot<-200 # 500 in paper 
nsamp<-5000 #50000 in paper
res<-foreach(i=1:nboot,.packages=c("stats"),.verbose=TRUE) %dopar% {
	samp<-sample(1:nrow(x),nsamp,replace=TRUE)
	v<-seq(0,max(x$dist),by=1)
	m1<-loess(bc1~dist,data=x[samp,],degree=2)
	m2<-loess(bc2~dist,data=x[samp,],degree=2)
	p1<-predict(m1,data.frame(dist=v))
	p2<-predict(m2,data.frame(dist=v))
	cbind(p1,p2)
}

### bind bootstrapped predictions together
val1<-do.call("cbind",lapply(res,function(i){i[,1]}))
val2<-do.call("cbind",lapply(res,function(i){i[,2]}))

### extract confidence intervals
ci1<-t(apply(val1,1,function(i){quantile(i,c(0.025,0.975),na.rm=TRUE)}))
ci2<-t(apply(val2,1,function(i){quantile(i,c(0.025,0.975),na.rm=TRUE)}))

### colors
colsp<-c("dodgerblue3","tomato","black")
colsl<-c("dodgerblue4","darkred","black")


### Figure 2
### Dissimilarity
png(file.path(path,"beta_div.png"),pointsize=9,width=10,height=8,units="in",res=100)
par(mar=c(4,4.5,3,3))
plot(x$dist,x$bc1,pch=16,col=alpha(colsp[1],0.15),cex=0.45,ylim=c(-0.1,1),xlab="",ylab="",axes=FALSE,xlim=c(0,1300),xaxs="i")
points(x$dist,x$bc2,pch=16,col=alpha(colsp[2],0.15),cex=0.45)
acol<-gray(0.15)
box(col=acol)
axis(1,tcl=-0.2,mgp=c(1.5,0.75,0),col=acol,cex.axis=1.75)
mtext("Distance (km)",side=1,line=2.5,cex=2)
axis(2,las=2,tcl=-0.2,mgp=c(1.5,0.5,0),col=acol,cex.axis=1.75)
mtext("Bray-Curtis dissimilarity",side=2,line=3,cex=2)

v<-seq(0,max(x$dist),by=1)

pol1<-na.omit(cbind(c(v,rev(v),v[1]),c(ci1[,1],rev(ci1[,2]),ci1[,1][1])))
polygon(pol1,col=alpha(colsl[1],0.5),border=NA)
lines(v,rowMeans(val1,na.rm=TRUE),col=colsl[1],lwd=2)

pol2<-na.omit(cbind(c(v,rev(v),v[1]),c(ci2[,1],rev(ci2[,2]),ci2[,1][1])))
polygon(pol2,col=alpha(colsl[2],0.5),border=NA)
lines(v,rowMeans(val2,na.rm=TRUE),col=colsl[2],lwd=2)

### difference
dif<-do.call("cbind",lapply(res,function(i){i[,2]-i[,1]}))
ci<-t(apply(dif,1,function(i){quantile(i,c(0.025,0.975),na.rm=TRUE)}))

pol<-na.omit(cbind(c(v,rev(v),v[1]),c(ci[,1],rev(ci[,2]),ci[,1][1])))
polygon(pol,col=alpha(colsp[3],0.25),border=NA,xpd=TRUE)
lines(v,rowMeans(dif,na.rm=TRUE),col=colsl[3],lwd=2,xpd=TRUE)

abline(0,0,lty=2,col=acol)

lx<-rep(600,3)
ly<-0.15-(0:2)*0.05
points(lx,ly,pch=15,col=alpha(colsl,c(0.5,0.5,0.25)),cex=3)
w<-9
segments(lx-w,ly,x1=lx+w,y1=ly,col=colsl,lwd=2,lend=2)
text(lx+20,ly,labels=c("Historical","Contemporary","Difference (Contemporary - Historical)"),adj=c(0,0.5),cex=2)
dev.off()



