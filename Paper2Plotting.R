library(lme4)
library(viridis)

path0<-"C:/Users/matth/Dropbox/NinaCollab_Covid/paper2_where_dis_info/"

params1<-read.csv(paste0(path0,"model_params.csv"))
params2<-read.csv(paste0(path0,"params1.csv"))

path<-"C:/Users/matth/Dropbox/NinaCollab_Covid/paper2_where_dis_info/resultsNEW/"

conc_res<-list()
exp_res<-list()
inf_res<-list()
hosp_res<-list()

c<-1
for(lt in c("A","B")){
  for(nt in c(1,2,3,4,5,6,7,8,9)){
    for(md in seq(1,50,1)){
      for(r in 1:10){
        for(p in c(0.05,0.2,0.5)){
          tmp_in<-readRDS(paste0(path,lt,"nets",params1$NetSelect[nt],"mods",md,"d_eff",r,"p",p,".RDS"))
          conc_res[[c]]<-tmp_in[[1]]
          exp_res[[c]]<-tmp_in[[2]]
          inf_res[[c]]<-tmp_in[[3]]
          hosp_res[[c]]<-tmp_in[[4]]
          c<-c+1
        }
      }
    }
  }
}


set<-c("A","B")
net<-seq(1,9,1)
params<-seq(1,50,1)
reps<-seq(1,10,1)
mods<-seq(1,10,1)
ps<-c(0.05,0.2,0.5)

set2<-rep(set,each=length(net)*length(params)*length(reps)*length(mods)*length(ps))
net2<-rep(rep(net,each=length(params)*length(reps)*length(mods)*length(ps)),length(set))
par2<-rep(rep(params,each=length(reps)*length(mods)*length(ps)),length(net)*length(set))
reps2<-rep(rep(reps,each=length(mods)*length(ps)),length(params)*length(net)*length(set))
ps2<-rep(rep(ps,each=length(mods)),length(net)*length(params)*length(reps)*length(set))
mods2<-rep(mods,length(params)*length(reps)*length(net)*length(set)*length(ps))

dat<-data.frame(set2,net2,par2,reps2,ps2,mods2)

min_finder<-function(object){
  return(apply(object,1,min))
}

dat$MinConc<-unlist(lapply(conc_res,min_finder))

whichmin_finder<-function(object){
  return(apply(object,1,which.min))
}

dat$WhichMinConc<-unlist(lapply(conc_res,whichmin_finder))


start_finder<-function(object){
  return(as.numeric(object[,1]>0))
}

max_finder<-function(object){
  return(apply(object,1,max))
}

whichmax_finder<-function(object){
  return(apply(object,1,which.max))
}

dat$Seeded<-unlist(lapply(exp_res,start_finder))

ep_start<-function(input,thresh=5){
  return(min(which(input>(thresh-1))))
}

es_finder<-function(object){
  return(apply(object,1,ep_start))
}

dat$EpStartExp<-unlist(lapply(exp_res,es_finder))
dat$MaxExp<-unlist(lapply(exp_res,max_finder))
dat$WhichMaxExp<-unlist(lapply(exp_res,whichmax_finder))

dat$EpStartInf<-unlist(lapply(inf_res,es_finder))
dat$MaxInf<-unlist(lapply(inf_res,max_finder))
dat$WhichMaxInf<-unlist(lapply(inf_res,whichmax_finder))

dat$MaxHosp<-unlist(lapply(hosp_res,max_finder))
dat$WhichMaxHosp<-unlist(lapply(hosp_res,whichmax_finder))

names(dat)[1:6]<-c("SetID","NetworkID","ParameterID","Rep","Probability","Community")

dat$soc_eff<-rep(NA,nrow(dat))
dat$hea_eff<-rep(NA,nrow(dat))

for(i in 1:nrow(dat)){
  dat$soc_eff[i]<-params2$soc_eff[params2$X==dat$ParameterID[i]]
  dat$hea_eff[i]<-params2$hea_eff[params2$X==dat$ParameterID[i]]
}

dat$mc_soc_eff<-dat$soc_eff-mean(dat$soc_eff)
dat$mc_hea_eff<-dat$hea_eff-mean(dat$hea_eff)

dat$r_eff<-paste0("S",dat$SetID,"N",dat$NetworkID,"P",dat$ParameterID,"R",dat$Rep)

d_eff<-c(0,0.1,0.2,0.4,0.6,0.8,1,1.2,1.4,1.6)

############################

t_inf<-lapply(inf_res,colSums)
t_exp<-lapply(exp_res,colSums)
t_conc<-lapply(conc_res,colMeans)

set<-c("A","B")
net<-seq(1,9,1)
params<-seq(1,50,1)
reps<-seq(1,10,1)
ps<-c(0.05,0.2,0.5)

set2<-rep(set,each=length(net)*length(params)*length(reps)*length(ps))
net2<-rep(rep(net,each=length(params)*length(reps)*length(ps)),length(set))
par2<-rep(rep(params,each=length(reps)*length(ps)),length(net)*length(set))
reps2<-rep(rep(reps,each=length(ps)),length(params)*length(net)*length(set))
ps2<-rep(ps,length(params)*length(net)*length(set)*length(reps))

datB<-data.frame(set2,net2,par2,reps2,ps2)

datB$MaxInf<-unlist(lapply(t_inf,max))
datB$WhichMaxInf<-unlist(lapply(t_inf,which.max))
datB$MaxExp<-unlist(lapply(t_exp,max))
datB$WhichMaxExp<-unlist(lapply(t_exp,which.max))
datB$MinConc<-unlist(lapply(t_conc,max))
datB$WhichMinConc<-unlist(lapply(t_conc,which.max))


names(datB)[1:5]<-c("SetID","NetworkID","ParameterID","Rep","Probability")

datB$soc_eff<-rep(NA,nrow(datB))
datB$hea_eff<-rep(NA,nrow(datB))

for(i in 1:nrow(datB)){
  datB$soc_eff[i]<-params2$soc_eff[params2$X==datB$ParameterID[i]]
  datB$hea_eff[i]<-params2$hea_eff[params2$X==datB$ParameterID[i]]
}

datB$mc_soc_eff<-datB$soc_eff-mean(datB$soc_eff)
datB$mc_hea_eff<-datB$hea_eff-mean(datB$hea_eff)

d_eff<-c(0,0.1,0.2,0.4,0.6,0.8,1,1.2,1.4,1.6)

datB$dis_eff<-d_eff[datB$Rep]

he_pal<-plasma(40)
he_qs<-quantile(datB$hea_eff,probs=seq(0,1,length.out=41))

datB$he_col<-rep(NA,nrow(datB))

for(i in 1:nrow(datB)){
  datB$he_col[i]<-sum(datB$hea_eff[i]<=he_qs)
}

Aw<-c(0,0.1,0.2,0.4,0.6,0.8,1,1.2,1.4,1.6)

########################################################
########################################################

######################################################################################
######################################################################################


path0<-"C:/Users/matth/Dropbox/NinaCollab_Covid/paper2_where_dis_info/"

params1<-read.csv(paste0(path0,"model_params.csv"))
params2<-read.csv(paste0(path0,"params1.csv"))

path<-"C:/Users/matth/Dropbox/NinaCollab_Covid/paper2_where_dis_info/results/"

conc_res<-list()
exp_res<-list()
inf_res<-list()
hosp_res<-list()

c<-1
for(lt in c("A","B","C","D")){
  for(nt in c(1,2,3,4,5,6,7,8,9)){
    for(md in seq(1,50,1)){
      for(r in 1:10){
        tmp_in<-readRDS(paste0(path,lt,"nets",params1$NetSelect[nt],"mods",md,"d_eff",r,".RDS"))
        conc_res[[c]]<-tmp_in[[1]]
        exp_res[[c]]<-tmp_in[[2]]
        inf_res[[c]]<-tmp_in[[3]]
        hosp_res[[c]]<-tmp_in[[4]]
        c<-c+1
      }
    }
  }
}

set<-c("A","B","C","D")
net<-seq(1,9,1)
params<-seq(1,50,1)
reps<-seq(1,10,1)
mods<-seq(1,10,1)

set2<-rep(set,each=length(net)*length(params)*length(reps)*length(mods))
net2<-rep(rep(net,each=length(params)*length(reps)*length(mods)),length(set))
par2<-rep(rep(params,each=length(reps)*length(mods)),length(net)*length(set))
reps2<-rep(rep(reps,each=length(mods)),length(params)*length(net)*length(set))
mods2<-rep(mods,length(params)*length(reps)*length(net)*length(set))

dat_o<-data.frame(set2,net2,par2,reps2,mods2)

min_finder<-function(object){
  return(apply(object,1,min))
}

dat_o$MinConc<-unlist(lapply(conc_res,min_finder))

whichmin_finder<-function(object){
  return(apply(object,1,which.min))
}

dat_o$WhichMinConc<-unlist(lapply(conc_res,whichmin_finder))


start_finder<-function(object){
  return(as.numeric(object[,1]>0))
}

max_finder<-function(object){
  return(apply(object,1,max))
}

whichmax_finder<-function(object){
  return(apply(object,1,which.max))
}

dat_o$Seeded<-unlist(lapply(exp_res,start_finder))

ep_start<-function(input,thresh=5){
  return(min(which(input>(thresh-1))))
}

es_finder<-function(object){
  return(apply(object,1,ep_start))
}

dat_o$EpStartExp<-unlist(lapply(exp_res,es_finder))
dat_o$MaxExp<-unlist(lapply(exp_res,max_finder))
dat_o$WhichMaxExp<-unlist(lapply(exp_res,whichmax_finder))

dat_o$EpStartInf<-unlist(lapply(inf_res,es_finder))
dat_o$MaxInf<-unlist(lapply(inf_res,max_finder))
dat_o$WhichMaxInf<-unlist(lapply(inf_res,whichmax_finder))

dat_o$MaxHosp<-unlist(lapply(hosp_res,max_finder))
dat_o$WhichMaxHosp<-unlist(lapply(hosp_res,whichmax_finder))

names(dat_o)[1:5]<-c("SetID","NetworkID","ParameterID","Rep","Community")

dat_o$soc_eff<-rep(NA,nrow(dat_o))
dat_o$hea_eff<-rep(NA,nrow(dat_o))

for(i in 1:nrow(dat_o)){
  dat_o$soc_eff[i]<-params2$soc_eff[params2$X==dat_o$ParameterID[i]]
  dat_o$hea_eff[i]<-params2$hea_eff[params2$X==dat_o$ParameterID[i]]
}

dat_o$mc_soc_eff<-dat_o$soc_eff-mean(dat_o$soc_eff)
dat_o$mc_hea_eff<-dat_o$hea_eff-mean(dat_o$hea_eff)

dat_o$r_eff<-paste0("S",dat_o$SetID,"N",dat_o$NetworkID,"P",dat_o$ParameterID,"R",dat_o$Rep)

############################

t_inf<-lapply(inf_res,colSums)
t_exp<-lapply(exp_res,colSums)
t_conc<-lapply(conc_res,colMeans)

set<-c("A","B","C","D")
net<-seq(1,9,1)
params<-seq(1,50,1)
reps<-seq(1,10,1)

set2<-rep(set,each=length(net)*length(params)*length(reps))
net2<-rep(rep(net,each=length(params)*length(reps)),length(set))
par2<-rep(rep(params,each=length(reps)),length(net)*length(set))
reps2<-rep(rep(reps,each=),length(params)*length(net)*length(set))

dat_oB<-data.frame(set2,net2,par2,reps2)

dat_oB$MaxInf<-unlist(lapply(t_inf,max))
dat_oB$WhichMaxInf<-unlist(lapply(t_inf,which.max))
dat_oB$MaxExp<-unlist(lapply(t_exp,max))
dat_oB$WhichMaxExp<-unlist(lapply(t_exp,which.max))
dat_oB$MinConc<-unlist(lapply(t_conc,max))
dat_oB$WhichMinConc<-unlist(lapply(t_conc,which.max))

names(dat_oB)[1:4]<-c("SetID","NetworkID","ParameterID","Rep")

dat_oB$soc_eff<-rep(NA,nrow(dat_oB))
dat_oB$hea_eff<-rep(NA,nrow(dat_oB))

for(i in 1:nrow(dat_oB)){
  dat_oB$soc_eff[i]<-params2$soc_eff[params2$X==dat_oB$ParameterID[i]]
  dat_oB$hea_eff[i]<-params2$hea_eff[params2$X==dat_oB$ParameterID[i]]
}

dat_oB$mc_soc_eff<-dat_oB$soc_eff-mean(dat_oB$soc_eff)
dat_oB$mc_hea_eff<-dat_oB$hea_eff-mean(dat_oB$hea_eff)

dat_oB$dis_eff<-d_eff[dat_oB$Rep]

he_pal<-plasma(40)
he_qs<-quantile(dat_oB$hea_eff,probs=seq(0,1,length.out=41))

dat_oB$he_col<-rep(NA,nrow(dat_oB))

for(i in 1:nrow(dat_oB)){
  dat_oB$he_col[i]<-sum(dat_oB$hea_eff[i]<=he_qs)
}

##################################################################

datB2<-datB[datB$Rep%in%c(1,2,3,4,7),]
dat_oB2<-dat_oB[dat_oB$Rep%in%c(1,2,3,4,7),]

##################################################################
##################################################################
##################################################################

##Figure 1

par(mfrow=c(2,2))
plot(MaxInf~jitter(Rep),data=dat_oB[dat_oB$SetID=="A"&dat_oB$soc_eff<0.3,],pch=16,col=adjustcolor(he_pal[dat_oB$he_col[dat_oB$SetID=="A"&dat_oB$soc_eff<0.3]],alpha.f=0.3),ylim=c(0,800),las=1,ylab="Height of epidemic peak",xlab="Strength of Awareness Effect",cex.lab=1.2,xaxt="n")
lines(x=c(-100,100),y=c(0,0))
mtext(text=Aw,side=1,line=0,at=seq(1,10,1))
mtext(text="a)",side=3,line=-2,adj=0.02,cex=1.2)
plot(MaxInf~jitter(Rep),data=dat_oB[dat_oB$SetID=="B"&dat_oB$soc_eff<0.3,],pch=16,col=adjustcolor(he_pal[dat_oB$he_col[dat_oB$SetID=="B"&dat_oB$soc_eff<0.3]],alpha.f=0.3),ylim=c(0,800),las=1,ylab="Height of epidemic peak",xlab="Strength of Awareness Effect",cex.lab=1.2,xaxt="n")
lines(x=c(-100,100),y=c(0,0))
mtext(text=Aw,side=1,line=0,at=seq(1,10,1))
mtext(text="b)",side=3,line=-2,adj=0.02,cex=1.2)
plot(MaxInf~jitter(Rep),data=dat_oB[dat_oB$SetID=="C"&dat_oB$soc_eff<0.3,],pch=16,col=adjustcolor(he_pal[dat_oB$he_col[dat_oB$SetID=="C"&dat_oB$soc_eff<0.3]],alpha.f=0.3),ylim=c(0,800),las=1,ylab="Height of epidemic peak",xlab="Strength of Awareness Effect",cex.lab=1.2,xaxt="n")
lines(x=c(-100,100),y=c(0,0))
mtext(text=Aw,side=1,line=0,at=seq(1,10,1))
mtext(text="c)",side=3,line=-2,adj=0.02,cex=1.2)
boxplot(MaxInf~Rep,data=dat_oB2[dat_oB2$SetID=="A"&dat_oB2$soc_eff<0.3,],ylim=c(0,800),xlim=c(0,35),las=1,ylab="Height of Epidemic Peak",lty=1,range=0,at=seq(1,29,7),boxwex=1.5,col="grey",xaxt="n",xlab="Awareness effect",cex.lab=1.5)
boxplot(MaxInf~Rep,data=dat_oB2[dat_oB2$SetID=="B"&dat_oB2$soc_eff<0.3,],ylim=c(0,800),xlim=c(0,40),las=1,ylab="Height of Epidemic Peak",lty=1,range=0,at=seq(3,31,7),add=TRUE,boxwex=1.5,col="dodgerblue",xaxt="n")
boxplot(MaxInf~Rep,data=dat_oB2[dat_oB2$SetID=="C"&dat_oB2$soc_eff<0.3,],ylim=c(0,800),xlim=c(0,40),las=1,ylab="Height of Epidemic Peak",lty=1,range=0,at=seq(5,33,7),add=TRUE,boxwex=1.5,col="darkblue",xaxt="n")
par(xpd=NA)
text(y=-50,x=seq(3,31,7),labels=Aw[c(1,2,3,4,7)],cex=1.5)
par(xpd=FALSE)
points(x=22,y=760,pch=15,cex=3,col="grey")
points(x=22,y=700,pch=15,cex=3,col="dodgerblue")
points(x=22,y=640,pch=15,cex=3,col="darkblue")
text(x=23,y=760,labels="Communication",adj=c(0,0.5),cex=1.5)
text(x=23,y=700,labels="Infection",adj=c(0,0.5),cex=1.5)
text(x=23,y=640,labels="Both",adj=c(0,0.5),cex=1.5)

#########################################
#########################################

#Figure 2

par(mfrow=c(3,1))
plot(MaxInf~jitter(Rep),data=dat_oB[dat_oB$SetID=="A"&dat_oB$soc_eff>0.3,],pch=16,col=adjustcolor(he_pal[dat_oB$he_col[dat_oB$SetID=="A"&dat_oB$soc_eff>0.3]],alpha.f=0.3),ylim=c(0,800),las=1,ylab="Height of epidemic peak",xlab="Strength of Awareness Effect",cex.lab=1.2,xaxt="n")
lines(x=c(-100,100),y=c(0,0))
mtext(text=Aw,side=1,line=0,at=seq(1,10,1))
mtext(text="a)",side=3,line=-2,adj=0.02,cex=1.2)
plot(MaxInf~jitter(Rep),data=dat_oB[dat_oB$SetID=="B"&dat_oB$soc_eff>0.3,],pch=16,col=adjustcolor(he_pal[dat_oB$he_col[dat_oB$SetID=="B"&dat_oB$soc_eff>0.3]],alpha.f=0.3),ylim=c(0,800),las=1,ylab="Height of epidemic peak",xlab="Strength of Awareness Effect",cex.lab=1.2,xaxt="n")
lines(x=c(-100,100),y=c(0,0))
mtext(text=Aw,side=1,line=0,at=seq(1,10,1))
mtext(text="b)",side=3,line=-2,adj=0.02,cex=1.2)
plot(MaxInf~jitter(Rep),data=dat_oB[dat_oB$SetID=="C"&dat_oB$soc_eff>0.3,],pch=16,col=adjustcolor(he_pal[dat_oB$he_col[dat_oB$SetID=="C"&dat_oB$soc_eff>0.3]],alpha.f=0.3),ylim=c(0,800),las=1,ylab="Height of epidemic peak",xlab="Strength of Awareness Effect",cex.lab=1.2,xaxt="n")
lines(x=c(-100,100),y=c(0,0))
mtext(text=Aw,side=1,line=0,at=seq(1,10,1))
mtext(text="c)",side=3,line=-2,adj=0.02,cex=1.2)

#########################################
#########################################

#Figure 3

par(mfrow=c(1,1))
boxplot(MaxInf~Rep,data=dat_oB[dat_oB$SetID=="A"&dat_oB$soc_eff<0.3,],ylim=c(0,600),xlim=c(0,61),las=1,range=0,lty=1,col="grey",at=seq(1,55,6),xaxt="n",xlab="Awareness Effect",ylab="Height of Epidemic Peak",cex.lab=1.5)
boxplot(MaxInf~Rep,data=dat_oB[dat_oB$SetID=="B"&dat_oB$soc_eff<0.3,],ylim=c(0,600),las=1,range=0,lty=1,col="dodgerblue",at=seq(2,56,6),add=TRUE,xaxt="n")
boxplot(MaxInf~Rep,data=datB[datB$SetID=="B"&datB$Probability==0.5&datB$soc_eff<0.3,],ylim=c(0,600),las=1,range=0,lty=1,col="yellow",at=seq(3,57,6),add=TRUE,xaxt="n")
boxplot(MaxInf~Rep,data=datB[datB$SetID=="B"&datB$Probability==0.2&datB$soc_eff<0.3,],ylim=c(0,600),las=1,range=0,lty=1,col="orange",at=seq(4,58,6),add=TRUE,xaxt="n")
boxplot(MaxInf~Rep,data=datB[datB$SetID=="B"&datB$Probability==0.05&datB$soc_eff<0.3,],ylim=c(0,600),las=1,range=0,lty=1,col="firebrick",at=seq(5,59,6),add=TRUE,xaxt="n")
par(xpd=NA)
text(Aw,x=seq(3,57,6),y=-25,cex=1.3)
par(xpd=FALSE)
points(x=25,y=590,col="grey",cex=3,pch=15)
points(x=25,y=560,col="dodgerblue",cex=3,pch=15)
points(x=45,y=590,col="yellow",cex=3,pch=15)
points(x=45,y=560,col="orange",cex=3,pch=15)
points(x=45,y=530,col="firebrick",cex=3,pch=15)
text(x=26,y=590,"Communication Layer",adj=c(0,0.5),cex=1.5)
text(x=26,y=560,"Infection Layer 100%",adj=c(0,0.5),cex=1.5)
text(x=46,y=590,"Infection Layer 50%",adj=c(0,0.5),cex=1.5)
text(x=46,y=560,"Infection Layer 20%",adj=c(0,0.5),cex=1.5)
text(x=46,y=530,"Infection Layer 5%",adj=c(0,0.5),cex=1.5)

#########################################
#########################################

#Figure 4

par(mfrow=c(1,1))
boxplot(MaxInf~NetworkID,data=dat_oB2[dat_oB2$SetID=="A"&dat_oB2$soc_eff<0.3&dat_oB2$Rep==3,],ylim=c(0,600),xlim=c(0,50),las=1,range=0,lty=1,col="grey",at=seq(1,9,1),xaxt="n",xlab="",ylab="Height of Epidemic Peak",cex.lab=1.5)
boxplot(MaxInf~NetworkID,data=dat_oB2[dat_oB2$SetID=="B"&dat_oB2$soc_eff<0.3&dat_oB2$Rep==3,],ylim=c(0,600),las=1,range=0,lty=1,col="dodgerblue",at=seq(11,19,1),add=TRUE,xaxt="n")
boxplot(MaxInf~NetworkID,data=datB2[datB2$SetID=="B"&datB2$Probability==0.5&datB2$soc_eff<0.3&datB2$Rep==3,],ylim=c(0,600),las=1,range=0,lty=1,col="yellow",at=seq(21,29,1),add=TRUE,xaxt="n")
boxplot(MaxInf~NetworkID,data=datB2[datB2$SetID=="B"&datB2$Probability==0.2&datB2$soc_eff<0.3&datB2$Rep==3,],ylim=c(0,600),las=1,range=0,lty=1,col="orange",at=seq(31,39,1),add=TRUE,xaxt="n")
boxplot(MaxInf~NetworkID,data=datB2[datB2$SetID=="B"&datB2$Probability==0.05&datB2$soc_eff<0.3&datB2$Rep==3,],ylim=c(0,600),las=1,range=0,lty=1,col="firebrick",at=seq(41,49,1),add=TRUE,xaxt="n")
par(xpd=NA)
text(x=seq(1,9,1),y=-20,labels=seq(1,9,1))
text(x=seq(11,19,1),y=-20,labels=seq(1,9,1))
text(x=seq(21,29,1),y=-20,labels=seq(1,9,1))
text(x=seq(31,39,1),y=-20,labels=seq(1,9,1))
text(x=seq(41,49,1),y=-20,labels=seq(1,9,1))
text(x=25,y=-60,"Network ID",cex=2)
points(x=5,y=590,col="grey",cex=3,pch=15)
points(x=5,y=560,col="dodgerblue",cex=3,pch=15)
points(x=5,y=530,col="yellow",cex=3,pch=15)
points(x=5,y=500,col="orange",cex=3,pch=15)
points(x=5,y=470,col="firebrick",cex=3,pch=15)
text(x=6,y=590,"Communication Layer",adj=c(0,0.5),cex=1.5)
text(x=6,y=560,"Infection Layer 100%",adj=c(0,0.5),cex=1.5)
text(x=6,y=530,"Infection Layer 50%",adj=c(0,0.5),cex=1.5)
text(x=6,y=500,"Infection Layer 20%",adj=c(0,0.5),cex=1.5)
text(x=6,y=470,"Infection Layer 5%",adj=c(0,0.5),cex=1.5)
text(x=48,y=590,paste0("Awareness Effect = ",Aw[2]),cex=2,adj=c(1,0.5))
par(xpd=FALSE)

#########################################
#########################################

#Figure 5

pathp<-"C:/Users/matth/Dropbox/NinaCollab_Covid/"
pathn<-"C:/Users/matth/Dropbox/NinaCollab_Covid/networks2/"

source(paste0(pathp,"FunctionsForHealthPaper.r"))

params1<-read.csv(paste0(pathp,"model_params.csv"))
params2<-read.csv(paste0(pathp,"model_params2.csv"))

net_params<-read.csv(paste0(pathp,"network_params.csv"))

dis_mats<-list()
info_mats<-list()

#start loop over networks
for(nt in 1:nrow(params1)){
  
  par_id<-params1[nt,2]
  
  pop_size=2000
  ncomms=10
  prop_belA=net_params$prop_belA[par_id]
  prop_old=0.13
  prop_young=0.63
  prop_child=0.24
  
  pop_info<-pop_gen(pop_size,ncomms,prop_belA,prop_old,prop_young,prop_child)
  
  ############################################
  
  dis_input<-readRDS(paste0(pathn,params1[nt,3],"net_and_parents.RDS"))
  info_input<-readRDS(paste0(pathn,params1[nt,2],"net_and_parents.RDS"))
  
  parents<-info_input[[2]]
  dis_mat<-dis_input[[1]]
  info_mat<-info_input[[1]]
  
  info_mats[[nt]]<-info_mat
  
  dis_mat<-par_ex(pop_info=pop_info,parents=parents,dis_mat=dis_mat)
  
  dis_mats[[nt]]<-dis_mat
  
}

A<-which(dis_mats[[1]]==1)
B<-which(info_mats[[1]]==1)

sum(A%in%B)/length(A)
sum(B%in%A)/length(B)

c_in_i<-numeric()
i_in_c<-numeric()

for(i in 1:9){
  A<-which(dis_mats[[i]]==1)
  B<-which(info_mats[[i]]==1)
  
  c_in_i[i]<-sum(A%in%B)/length(A)
  i_in_c[i]<-sum(B%in%A)/length(B)
}

c_in_i_mat<-matrix(NA,nr=2000,nc=9)
i_in_c_mat<-matrix(NA,nr=2000,nc=9)

for(i in 1:9){
  for(j in 1:2000){
    A<-which(dis_mats[[i]][,j]==1)
    B<-which(info_mats[[i]][,j]==1)
    
    c_in_i_mat[j,i]<-sum(A%in%B)/length(A)
    i_in_c_mat[j,i]<-sum(B%in%A)/length(B)
    
  }
}

par(mfrow=c(2,1),mar=c(5,5.5,2,2))
boxplot(c_in_i_mat,lty=1,range=0,las=1,col="grey",ylab="Proportion of infection layer contacts\n in communication layer",xlab="Network ID",cex.lab=1.2)
lines(x=c(-10,100),y=c(0,0),lty=3,col="black")
lines(x=c(-10,100),y=c(0.05,0.05),lty=2,col="grey")
lines(x=c(-10,100),y=c(0.1,0.1),lty=1,col="grey")
lines(x=c(-10,100),y=c(0.15,0.15),lty=2,col="grey")
lines(x=c(-10,100),y=c(0.2,0.2),lty=1,col="grey")
boxplot(c_in_i_mat,lty=1,range=0,las=1,col=c(rep("dodger blue",3),rep("goldenrod",3),rep("forestgreen",3)),add=TRUE)
boxplot(i_in_c_mat,lty=1,range=0,las=1,col="grey",ylab="Proportion of communication layer contacts\n in infection layer",xlab="Network ID",cex.lab=1.2)
lines(x=c(-10,100),y=c(0,0),lty=3,col="black")
lines(x=c(-10,100),y=c(0.05,0.05),lty=2,col="grey")
lines(x=c(-10,100),y=c(0.1,0.1),lty=1,col="grey")
lines(x=c(-10,100),y=c(0.15,0.15),lty=2,col="grey")
lines(x=c(-10,100),y=c(0.2,0.2),lty=1,col="grey")
lines(x=c(-10,100),y=c(0.25,0.25),lty=2,col="grey")
lines(x=c(-10,100),y=c(0.3,0.3),lty=1,col="grey")
boxplot(i_in_c_mat,lty=1,range=0,las=1,col=c(rep("dodger blue",3),rep("goldenrod",3),rep("forestgreen",3)),add=TRUE)

#########################################
#########################################

#Figure S1

par(mfrow=c(3,3))

for(i in 1:9){
  
  boxplot(MaxInf~Rep,data=dat_oB2[dat_oB2$SetID=="A"&dat_oB2$soc_eff<0.3&dat_oB2$NetworkID==i,],ylim=c(0,600),xlim=c(0,31),las=1,range=0,lty=1,col="grey",at=seq(1,25,6),xaxt="n",xlab="Awareness Effect",ylab="Height of Epidemic Peak",cex.lab=1.5)
  boxplot(MaxInf~Rep,data=dat_oB2[dat_oB2$SetID=="B"&dat_oB2$soc_eff<0.3&dat_oB2$NetworkID==i,],ylim=c(0,600),las=1,range=0,lty=1,col="dodgerblue",at=seq(2,26,6),add=TRUE,xaxt="n")
  boxplot(MaxInf~Rep,data=datB2[datB2$SetID=="B"&datB2$Probability==0.5&datB2$soc_eff<0.3&datB2$NetworkID==i,],ylim=c(0,600),las=1,range=0,lty=1,col="yellow",at=seq(3,27,6),add=TRUE,xaxt="n")
  boxplot(MaxInf~Rep,data=datB2[datB2$SetID=="B"&datB2$Probability==0.2&datB2$soc_eff<0.3&datB2$NetworkID==i,],ylim=c(0,600),las=1,range=0,lty=1,col="orange",at=seq(4,28,6),add=TRUE,xaxt="n")
  boxplot(MaxInf~Rep,data=datB2[datB2$SetID=="B"&datB2$Probability==0.05&datB2$soc_eff<0.3&datB2$NetworkID==i,],ylim=c(0,600),las=1,range=0,lty=1,col="firebrick",at=seq(5,29,6),add=TRUE,xaxt="n")
  par(xpd=NA)
  text(Aw[c(1,2,3,4,7)],x=seq(3,27,6),y=-25,cex=1.3)
  
}


#########################################
#########################################

#Figure S2

cols<-c("grey","dodgerblue","yellow","orange","firebrick")
labs<-c("Communication Layer","Infection Layer 100%","Infection Layer 50%","Infection Layer 20%","Infection Layer 5%")
par(mfrow=c(2,5))
for(i in 1:10){
  boxplot(MaxInf~NetworkID,data=dat_oB[dat_oB$SetID=="A"&dat_oB$soc_eff<0.3&dat_oB$Rep==i,],ylim=c(0,600),xlim=c(0,50),las=1,range=0,lty=1,col="grey",at=seq(1,9,1),xaxt="n",xlab="",ylab="Height of Epidemic Peak",cex.lab=1.5)
  boxplot(MaxInf~NetworkID,data=dat_oB[dat_oB$SetID=="B"&dat_oB$soc_eff<0.3&dat_oB$Rep==i,],ylim=c(0,600),las=1,range=0,lty=1,col="dodgerblue",at=seq(11,19,1),add=TRUE,xaxt="n")
  boxplot(MaxInf~NetworkID,data=datB[datB$SetID=="B"&datB$Probability==0.5&datB$soc_eff<0.3&datB$Rep==i,],ylim=c(0,600),las=1,range=0,lty=1,col="yellow",at=seq(21,29,1),add=TRUE,xaxt="n")
  boxplot(MaxInf~NetworkID,data=datB[datB$SetID=="B"&datB$Probability==0.2&datB$soc_eff<0.3&datB$Rep==i,],ylim=c(0,600),las=1,range=0,lty=1,col="orange",at=seq(31,39,1),add=TRUE,xaxt="n")
  boxplot(MaxInf~NetworkID,data=datB[datB$SetID=="B"&datB$Probability==0.05&datB$soc_eff<0.3&datB$Rep==i,],ylim=c(0,600),las=1,range=0,lty=1,col="firebrick",at=seq(41,49,1),add=TRUE,xaxt="n")
  par(xpd=NA)
  text(x=seq(1,9,1),y=-20,labels=seq(1,9,1),cex=0.75)
  text(x=seq(11,19,1),y=-20,labels=seq(1,9,1),cex=0.75)
  text(x=seq(21,29,1),y=-20,labels=seq(1,9,1),cex=0.75)
  text(x=seq(31,39,1),y=-20,labels=seq(1,9,1),cex=0.75)
  text(x=seq(41,49,1),y=-20,labels=seq(1,9,1),cex=0.75)
  text(x=25,y=-60,"Network ID",cex=1.5)
  if(i>5){
    points(x=15,y=670,pch=15,cex=3,col=cols[i-5])
    text(x=18,y=670,labels=labs[i-5],adj=c(0,0.5),cex=1.5)
  }
  par(xpd=FALSE)
  text(x=25,y=580,paste0("Awareness Effect = ",Aw[i]),cex=1.5)
}