#tqake a list of host with number of proc
generateListSubproc <- function(file){

    hostnproc=read.csv(file,header=F)
    all=as.character(rep(hostnproc[,1],hostnproc[,2]))
    lapply(all,function(i)list(host=i,free=TRUE,file=NULL))

}

#update a list of host and running proc
checkHost <- function(listsubproc){
    for(busy in which(!sapply(listsubproc,"[[","free"))){
        sp=listsubproc[[busy]]
        if(file.exists(sp$file)){
            print(paste("experiment:",sp$file,"on host",sp$host,"ended. proc freed"))
            listsubproc[[busy]]$file=NULL
            listsubproc[[busy]]$free=TRUE
        }
    }
    return(listsubproc)
}
