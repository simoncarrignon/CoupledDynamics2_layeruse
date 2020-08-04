source("model.R")

args=commandArgs(trailingOnly = TRUE) #pass name of the file with arguments

file=args[1]#first argument is the number of slave

allpararm=readRDS(file)
na=lapply(names(allpararm),function(a)assign(a,allpararm[[a]],pos=.GlobalEnv)) #this allow to assign all elements of the list as variable with the same name that there name in the list
runModel(
         time=time,
          dis_mat=dis_mat,
          info_mat=info_mat,
         start=start,
         pop_info=pop_info,
         type=type,
         p_inf=p_inf,
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
         outputname=outputname,
         log=F,
         minlog=F,
         Xplot=F
         )

