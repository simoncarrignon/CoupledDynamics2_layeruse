old <- Sys.time() 

path0<-"./"
path1<-"./networks2/"
path2<-"./resultsparnew/"

source(paste0(path0,"FunctionsForPaper2_nv.R"))
source(paste0(path0,"model.R"))
source(paste0(path0,"parrallelFunction.R"))

listsubproc=generateListSubproc("hostandprocs.csv")

params1<-read.csv(paste0(path0,"model_params.csv"))
params2<-read.csv(paste0(path0,"params1.csv"))
#params2<-read.csv(paste0(path0,"model_params2.csv"))

net_params<-read.csv(paste0(path0,"network_params.csv"))

for(type in c("A","B","C")[1]){
nt<-1
#start loop over networks
for(nt in 1:nrow(params1)){
#for(nt in 1){
  
par_id<-params1[nt,2]

pop_size=2000
ncomms=10
prop_belA=net_params$prop_belA[par_id]
prop_old=0.13
prop_young=0.63
prop_child=0.24

pop_info<-pop_gen(pop_size,ncomms,prop_belA,prop_old,prop_young,prop_child)

############################################

dis_input<-readRDS(paste0(path1,params1[nt,3],"net_and_parents.RDS"))
info_input<-readRDS(paste0(path1,params1[nt,2],"net_and_parents.RDS"))

parents<-info_input[[2]]
dis_mat<-dis_input[[1]]
info_mat<-info_input[[1]]

dis_mat<-par_ex(pop_info=pop_info,parents=parents,dis_mat=dis_mat)

############################################

dis.eff<-c(0,0.1,0.2,0.4,0.6,0.8,1,1.2,1.4,1.6)
# Here we define the prior beliefs of young adults (which will be used as probabilities in a bernoulli draw)
# e.g. currently there is a 50% chance a young adult of political belief A is concerned about the virus
A_concern_y<-0.2
B_concern_y<-0.2

#Here we define an additive effect of being old (to accomodate the fact they may be more likely to start concerned)
# e.g. There will be a 70% chance of an old adult of political belief A being concerned about the virus
A_concern_o<-0
B_concern_o<-0

#Here we define a daily extrinsic input into the belief of each political belief (I figured this would suffice to represent exposure to politicians/news/wider social media)
#N.B. These numbers are already defined on a logit scale. But our starting assumption is that concern of political belief A gets puched up and political belief B gets ushed down by these extrinsic factors
#(can obviously set to zero if preferred)
lA_ex<- 0
lB_ex<- 0

#This now needs to be changed as a parameter
p_infs<-c(0.05,0.2,0.5)

  
start<-concern_setup(A_concern_y,B_concern_y,A_concern_o,B_concern_o,pop_info,info_mat)


############################################

#Probability of becoming exposed having contacted an infectious individual (daily)
#Need to work out R0 based on other parameters
S_E<-0.3/mean(colSums(dis_mat))
#S_E<-0.2

#lambda for a Poisson draw for the length of this period
E_I1<-5.1

#probability of transitioning to serious case for young (daily)
yI1_I2<-0.01
#and same for old
oI1_I2<-0.05

#probability of transitioning to critical (HOSPITALISED) case for young (daily)
yI2_I3<-0.0125
#and same for old
oI2_I3<-0.025

#probability of death for young (daily)
yI3_D<-0.012
#and same for old
oI3_D<-0.092

#lambda for a Poisson draw for duration of a pre-symptomatic/mild infection - this is now misnamed as impossible to recover - simply transition to I2.
yI1_R<-6.7
#and same for old
oI1_R<-6.7

#lambda for a Poisson draw for duration of a serious infection
yI2_R<-10
#and same for old
oI2_R<-10

#lambda for a Poisson draw for duration of a critical/hospitalised infection
yI3_R<-4.2
#and same for old
oI3_R<-4.2

for(md in 1:50){


#Define a linear relationship between proportion of connections concerned (at the previous time-step) and concern levels in young adults
l_conc<-params2[md,2]

#and an additive effect used to calculate the same parameter for old adults
l_conc_o<-0


#Define a threshold relationship whereby concern decreases while all immediate network conncections are fully healthy (at the previous time-step) and concern levels in young adults
l_hea<-params2[md,3]
#and an additive effect used to calculate the same parameter for old adults
l_hea_o<-0


for(s in 1:3){

for(r in 1:10){

#Define a linear relationship between number of connections infected (at the previous time-step) and concern levels in young adults
l_inf<-dis.eff[r]
  #and an additive effect used to calculate the same parameter for old adults
l_inf_o<-0
  

############################################

time<-300

print("=====================================================")
print(paste0("md:",md,",nt",nt,",type:",type,",s:",s,",r:",r))

name=paste0(type,"nets",params1$NetSelect[nt],"mods",md,"d_eff",r,"p",p_infs[s])
outputname=file.path(path2,paste0(name,".RDS"))
 
## this list is used to save all parameter in a fille that will be read by the sub process
list_allparam=list(
 time=time,
 dis_mat=dis_mat,
 info_mat=info_mat,
 start=start,
 pop_info=pop_info,
 type=type,
 p_inf=p_infs[s],
 lA_ex=lA_ex,
 lB_ex=lB_ex,
 l_conc=l_conc,
 l_conc_o=l_conc_o,
 l_inf=l_inf,
 l_inf_o=l_inf_o,
 l_hea=l_hea,
 l_hea_o=l_hea_o,
 S_E=S_E,
 E_I1=E_I1,
 yI1_I2=yI1_I2,
 oI1_I2=oI1_I2,
 yI2_I3=yI2_I3,
 oI2_I3=oI2_I3,
 yI3_D=yI3_D,
 oI3_D=oI3_D,
 yI1_R=yI1_R,
 oI1_R=oI1_R,
 yI2_R=yI2_R,
 oI2_R=oI2_R,
 yI3_R=yI3_R,
 oI3_R=oI3_R,
 minlog=T,
 outputname=outputname
)
paramfilename=file.path("listparams",paste0(name,".RDS"))
saveRDS(list_allparam,paramfilename)

while(length(which(sapply(listsubproc,"[[","free")))<1){
    Sys.sleep(5)
    print(paste("no free slot among our", length(sapply(listsubproc,"[[","free")),"list of available CPU"))
    print(paste(length(list.files(path2,pattern="*.RDS")),"simulations have finished so far"))
    listsubproc=checkHost(listsubproc)
}

freehost=min(which(sapply(listsubproc,"[[","free")))
listsubproc[[freehost]]$file=outputname
listsubproc[[freehost]]$free=FALSE

cmd=paste0('ssh ',listsubproc[[freehost]]$host,' "cd ',getwd(),'; Rscript launchOneSimu.R ',paramfilename,'> logs/',listsubproc[[freehost]]$host,"_",name,'.log "')
#cmd=paste0("ssh ",listsubproc[[freehost]]$host," \"cd ",getwd(),"; Rscript launchOneSimu.R ",paramfilename,"> logs/",name,".log &\"\n")
system(cmd,ignore.stderr=T,wait=F)

print(paste("expe",name,"launched on",listsubproc[[freehost]]$host,"waiting for",outputname,"to finish"))
########################################
########################################

} #end r loop
  
} #end s loop

} #end md loop

} #end nt loop 

} #end type loop


new <- Sys.time() - old # calculate difference
print(new) # print in nice format



