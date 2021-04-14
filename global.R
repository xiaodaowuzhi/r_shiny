library(haven)
library(readxl)
library(tools)

#setwd('~/shiny/')
#setwd('E:/Global/Develope/Bimo/R_shiny/shiny/')
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#read all sas datasets
filex <- list.files(path = ".", pattern = "*.sas7bdat", full.names = F, recursive = T)
filex2 <- file_path_sans_ext(list.files(pattern = "*.sas7bdat"))
for (i in 1:length(filex)){
  do.call("<-",list(substr(filex[i], 1, nchar(filex[i])-9), read_sas(filex[i])))
}

#adsl<-read_sas("adsl.sas7bdat")
#adtte<-read_sas("adtte.sas7bdat")
#advs<-read_sas("advs.sas7bdat")
#adrs<-read_sas("adrs.sas7bdat")
#adae<-read_sas("adae.sas7bdat")
#adtr<-read_sas("adtr.sas7bdat")

freq <- function(dsin,freqvar,trt,subkey="usubjid",popds,trtsl){
  if(length(dsin)==0){
    print("Please input dataset name!")
  }else{
    dsinname<-get(dsin)
    
    if(grepl("\\||\\s",freqvar)){
      #style 1, occurrence table, freqvar=studyid||aesoc||aesoc aedecod
      #print("style 1")
      list1<-strsplit(freqvar,"||",fixed=TRUE)
      num0<-sapply(strsplit(freqvar,"||",fixed=TRUE),length)
      tmpds<-NULL
      for (i in 1:eval(num0)){
        var<-list1[[1]][[i]]
        num<-sapply(strsplit(var," ",fixed=TRUE),length)
        var2<-gsub(" ","+",var,fixed=TRUE)
        var3<-gsub(" ",",",trimws(unlist(strsplit(var,split = " ",fixed=TRUE))),fixed=TRUE)
        #var_names=trimws(unlist(strsplit(var,split = " ",fixed=TRUE)))
        dsinname<-dsinname%>%distinct(dsinname[c(trt)],dsinname[c(subkey)],dsinname[c(var3)],.keep_all=TRUE)
        tmpds1<-NULL
        tmpds1<-select(as.data.frame(summary(arsenal::freqlist(as.formula(paste0("~",trt,"+",var2)),data=dsinname))),1:eval(num+2))
        names(tmpds1)<-c(trt,var3,"freq")
        tmpds1[tmpds1==""]<-NA
        tmpds1<-as.data.frame(tmpds1%>%fill(as.name(trt),as.name(strsplit(var2,"+",fixed=TRUE)[[1]][[1]])))
        tmpds1<-reshape2::dcast(reshape2::melt(tmpds1,id.vars=1:eval(num+1),measure.vars=eval(num+2)),as.formula(paste0(var2,"~",trt)))
        tmpds1[is.na(tmpds1)]<-0
        tmpds1<-mutate(tmpds1,grp=i)
        #remove space from variables names
        names(tmpds1) <- gsub(" ","_",names(tmpds1))
        tmpds<-bind_rows(tmpds,tmpds1)
      }
      
    }else{
      #style 2, freqvar=studyid+usubjid
      #print("style 2")
      list1<-strsplit(freqvar,"+",fixed=TRUE)
      num0<-sapply(strsplit(freqvar,"+",fixed=TRUE),length)
      tmpds<-NULL
      for (i in 1:eval(num0)){
        var<-list1[[1]][[i]]
        var3<-gsub(" ",",",trimws(unlist(strsplit(var,split = " ",fixed=TRUE))),fixed=TRUE)
        #var_names=trimws(unlist(strsplit(var,split = " ",fixed=TRUE)))
        dsinname<-get(dsin)
        dsinname<-dsinname%>%distinct(dsinname[c(trt)],dsinname[c(subkey)],dsinname[c(var3)],.keep_all=TRUE)
        tmpds1<-NULL
        tmpds1<-select(as.data.frame(summary(arsenal::freqlist(as.formula(paste0("~",trt,"+",var)),data=dsinname))),1:3)
        names(tmpds1)<-c(trt,var,"freq")
        tmpds1[tmpds1==""]<-NA
        tmpds1<-as.data.frame(tmpds1%>%fill(as.name(trt),as.name(var)))
        tmpds1<-reshape2::dcast(reshape2::melt(tmpds1,id.vars=1:2,measure.vars=3),as.formula(paste0(var,"~",trt)))
        tmpds1[is.na(tmpds1)]<-0
        tmpds1<-mutate(tmpds1,grp=i)
        #remove space from variables names
        names(tmpds1) <- gsub(" ","_",names(tmpds1))
        tmpds<-bind_rows(tmpds,tmpds1)
      }
    }
    if (!missing(popds)){
      #adsl population
      adslnam<-get(popds)
      adslnam<-adslnam%>%filter(adslnam[c(trtsl)]!="")
      pop<-as.data.frame(summary(arsenal::freqlist(as.formula(paste0("~",trtsl)),data=adslnam)))
      names(pop)=c(trtsl,"freqsl")
      pop<-reshape2::melt(pop,id.vars=1,measure.vars=2)
      pop<-reshape2::dcast(pop,as.formula(paste0("variable~",trtsl)))
      #remove space from variables names
      names(pop) <- gsub(" ","_",names(pop))
      #same variables in pop and tmpds
      namlist <- intersect(names(tmpds), names(pop))
      pop2 <- pop[namlist]
      names(pop2) = paste0(namlist, "_n")
      tmpds<-bind_cols(tmpds,pop2)
      num1 <- which( colnames(tmpds) %in% namlist)
      num2 <- which( colnames(tmpds) %in% paste0(namlist, "_n"))
      tmpds[num1] <- sapply(tmpds[num1],as.numeric)
      tmpds[num2] <- sapply(tmpds[num2],as.numeric)
      pords=tmpds[num1]/tmpds[num2]
      names(pords)=c(paste0(namlist, "_p"))
      tmpds<-bind_cols(tmpds,pords)
      for (j in 1:length(namlist)){
        nam_cur <- namlist[j]
        nam_curc <- paste0(nam_cur,"_c")
        nam_curp <- paste0(nam_cur,"_p")
        tmpds[[nam_curc]] <- paste0(tmpds[[nam_cur]]," (",round(tmpds[[nam_curp]]*100,digits=1),"%)")
      }
    }
    return(tmpds)
  } 
}
