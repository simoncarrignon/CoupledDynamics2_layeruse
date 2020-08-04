#' Main function that run the model other all networks
#' @param time number of time step
#' @param dis_mat 
#' @param start
#' @param belief
#' @param log should the number of agent in each status being printed at each time step?
#' @param minlog should the time step being printed?
#' @param outputname a name to save the results of the simulation 

runModel <- function(time,dis_mat,start,pop_info,type="A",p_inf,lA_ex,lB_ex,l_conc,l_conc_o,l_inf,l_inf_o,l_hea,l_hea_o,log=TRUE,S_E,E_I1,yI1_I2,oI1_I2,yI2_I3,oI2_I3,yI3_D,oI3_D,yI1_R,oI1_R,yI2_R,oI2_R,yI3_R,oI3_R,s,Xplot=F,minlog=F,outputname=NULL){

    concern<-list()
    belief<-list()

    belief[[1]]<-start[[1]]
    concern[[1]]<-start[[2]]

    #start with 2 infected individuals
    exp<-sample(1:pop_info$pop,5,replace=FALSE)

    #create dataframe to store disease state
    S<-rep(1,pop_info$pop)
    E<-rep(0,pop_info$pop)
    I1<-rep(0,pop_info$pop)
    I2<-rep(0,pop_info$pop)
    I3<-rep(0,pop_info$pop)
    R<-rep(0,pop_info$pop)
    D<-rep(0,pop_info$pop)
    status<-data.frame(S,E,I1,I2,I3,R,D)

    status$S[exp]<-0
    status$E[exp]<-1

    d_exp<-rep(NA,pop_info$pop)
    d_inf1<-rep(NA,pop_info$pop)
    d_inf2<-rep(NA,pop_info$pop)
    d_inf3<-rep(NA,pop_info$pop)

    d_exp[status$E==1]<-rpois(sum(status$E),E_I1)
    statuses<-list()
    statuses[[1]]<-status

    progression<-matrix(0,nr=time+1,nc=ncol(status))
    progression[1,]<-colSums(status)
    #be ready to change the network in this
    for(t in 2:time){

        if(t==2){

            #time-step 2

            dis_mat<-network_rewire_concern(net=dis_mat,concern=start[[2]],concern.prev=rep(0,pop_info$pop),pop_info=pop_info,
                                            prop_cut=0.5,cut_to=0.001)
            dis_mat<-network_rewire_infectionS(net=dis_mat,status=statuses[[t-1]],
                                               pop_info=pop_info,cut_to=0.001)
            #dis_mat<-network_rewire_infectionM(net=dis_mat,status=statuses[[t-1]],
            #                                   pop_info=pop_info,cut_to=0.001)
            dis_mat<-network_rewire_infectionR(net=dis_mat,status=statuses[[t-1]],
                                               pop_info=pop_info,cut_to=0.001)

            inf<-cbind(sign(statuses[[t-1]]$I2+statuses[[t-1]]$I3),sign(statuses[[t-1]]$I2+statuses[[t-1]]$I3))


            if(type=="A"){
                current<-concern_timestep1(pop_info=pop_info,net_b=info_mat,net_d=dis_mat,belief=start[[1]],concern=start[[2]],inf=inf,lA_ex,lB_ex,l_conc,l_conc_o,l_inf,l_inf_o,l_hea,l_hea_o,p_inf=p_inf)
            }

            if(type=="B"){
                current<-concern_timestep2(pop_info=pop_info,net_b=info_mat,net_d=dis_mat,belief=start[[1]],concern=start[[2]],inf=inf,lA_ex,lB_ex,l_conc,l_conc_o,l_inf,l_inf_o,l_hea,l_hea_o,p_inf=p_inf)
            }
            if(type=="C"){
                current<-concern_timestep2(pop_info=pop_info,net_b=info_mat,net_d=dis_mat,belief=start[[1]],concern=start[[2]],inf=inf,lA_ex,lB_ex,l_conc,l_conc_o,l_inf,l_inf_o,l_hea,l_hea_o,p_inf=p_inf)
            }


            belief[[2]]<-current[[1]]
            concern[[2]]<-current[[2]]

            dis<-infection_timestep(pop_info=pop_info,status=statuses[[t-1]],net=dis_mat,d_exp=d_exp,d_inf1=d_inf1,d_inf2=d_inf2,d_inf3=d_inf3,S_E=S_E,E_I1=E_I1,yI1_I2=yI1_I2,oI1_I2=oI1_I2,yI2_I3=yI2_I3,oI2_I3=oI2_I3,yI3_D=yI3_D,oI3_D=oI3_D,yI1_R=yI1_R,oI1_R=oI1_R,yI2_R=yI2_R,oI2_R=oI2_R,yI3_R=yI3_R,oI3_R=oI3_R)

        }
        if(t>2){

            dis_mat<-network_rewire_concern(net=dis_mat,concern=current[[2]],concern.prev=concern[[t-2]],pop_info=pop_info,
                                            prop_cut=0.5,cut_to=0.001)
            dis_mat<-network_rewire_infectionS(net=dis_mat,status=statuses[[t-1]],
                                               pop_info=pop_info,cut_to=0.001)
            #dis_mat<-network_rewire_infectionM(net=dis_mat,status=statuses[[t-1]],
            #                                   pop_info=pop_info,cut_to=0.001)
            dis_mat<-network_rewire_infectionR(net=dis_mat,status=statuses[[t-1]],
                                               pop_info=pop_info,cut_to=0.001)

            inf<-cbind(sign(statuses[[t-1]]$I2+statuses[[t-1]]$I3),sign(statuses[[t-1]]$I2+statuses[[t-1]]$I3))
            if(type == "A")
            {
                current<-concern_timestep1(pop_info=pop_info,net_b=info_mat,net_d=dis_mat,belief=current[[1]],
                                           concern=current[[2]],inf=inf,lA_ex,lB_ex,l_conc,l_conc_o,l_inf,
                                           l_inf_o,l_hea,l_hea_o,p_inf=p_inf)
            }
            if(type == "B")
            {
                current<-concern_timestep2(pop_info=pop_info,net_b=info_mat,net_d=dis_mat,belief=current[[1]],
                                           concern=current[[2]],inf=inf,lA_ex,lB_ex,l_conc,l_conc_o,l_inf,
                                           l_inf_o,l_hea,l_hea_o,p_inf=p_inf)
            }
            if(type == "C"){
                current<-concern_timestep3(pop_info=pop_info,net_b=info_mat,net_d=dis_mat,belief=current[[1]],
                                           concern=current[[2]],inf=inf,lA_ex,lB_ex,l_conc,l_conc_o,l_inf,
                                           l_inf_o,l_hea,l_hea_o,p_inf=p_inf)
            }

            belief[[t]]<-current[[1]]
            concern[[t]]<-current[[2]]  

            dis<-infection_timestep(pop_info=pop_info,status=statuses[[t-1]],net=dis_mat,d_exp=dis[[2]],d_inf1=dis[[3]],d_inf2=dis[[4]],d_inf3=dis[[5]],S_E=S_E,E_I1=E_I1,yI1_I2=yI1_I2,oI1_I2=oI1_I2,yI2_I3=yI2_I3,oI2_I3=oI2_I3,yI3_D=yI3_D,oI3_D=oI3_D,yI1_R=yI1_R,oI1_R=oI1_R,yI2_R=yI2_R,oI2_R=oI2_R,yI3_R=yI3_R,oI3_R=oI3_R)
        }
        if(log)print(colSums(dis$status))
        if(minlog)print(paste0("t:",t))
        progression[t,]<-colSums(dis$status)
        statuses[[t]]<-dis$status

        if(sum(colSums(statuses[[t]])[c(2,3,4,5)])==0){break()}

    }


    mod_concerns<-matrix(0,nr=10,nc=length(concern))
    mod_exps<-matrix(0,nr=10,nc=length(statuses))
    mod_infs<-matrix(0,nr=10,nc=length(statuses))
    mod_hosps<-matrix(0,nr=10,nc=length(statuses))

    for(i in 1:length(concern)){
        mod_concerns[,i]<-aggregate(concern[[i]],by=list(pop_info$comms),mean)[,2]
        mod_exps[,i]<-aggregate(statuses[[i]][,2],by=list(pop_info$comms),sum)[,2]
        mod_infs[,i]<-aggregate(statuses[[i]][,4],by=list(pop_info$comms),sum)[,2]
        mod_hosps[,i]<-aggregate(statuses[[i]][,5],by=list(pop_info$comms),sum)[,2]
    }


    ###################################
    ###################################
    if(Xplot){
        cols=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#882255", "#AA4499")

        plot(NULL,xlim=c(0,250),ylim=c(0,1))
        for(i in 1:10){
            lines(x=seq(1,length(concern)),y=mod_concerns[i,],col=cols[i],lwd=3)
        }

        plot(NULL,xlim=c(0,250),ylim=c(0,50))
        for(i in 1:10){
            lines(x=seq(1,length(statuses)),y=mod_infs[i,],col=cols[i],lwd=3)
        }
    }

    OUT<-list(mod_concerns,mod_exps,mod_infs,mod_hosps)
    names(OUT)<-c("concern","exps","infs","hosps")

    if(is.null(outputname))return(OUT)
    else{
        saveRDS(OUT, paste0(outputname,".RDS"))
    }
}
