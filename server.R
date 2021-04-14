library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(readxl)
library(DT)
library(gt)
library(knitr)
library(dplyr)
library(tidyr)
library(gtsummary)
library(survival)
library(survminer)
library(ggplot2)
library(arsenal)
library(stringr)
library(echarts4r)
library(shinyjs)


#server data
shinyServer <- (function(input, output, session) {
  
  onevent("mouseenter", "sidebarCollapsed", shinyjs::removeCssClass(selector = "body", class = "sidebar-collapse"))
  onevent("mouseleave", "sidebarCollapsed", shinyjs::addCssClass(selector = "body", class = "sidebar-collapse"))

  #onevent("mouseleave", "sidebarCollapsed", shinyjs::hide(id = "sidebar_1"))
  #shinyjs::onclick("listing", shinyjs::show(id = "sidebar_1"))

  lst_sex <- c('All values' = ' ', unique(adsl$SEX))
  updateSelectInput(session = session, "sex",choices=lst_sex,selected=NULL)
  
  observeEvent(input$reset_filter, {
    shinyjs::reset("controlbar")
    #updateCheckboxInput(session = session, id = "ittfl", label = NULL, value = FALSE)
    #updateCheckboxInput(session = session, id = "saffl", label = NULL, value = FALSE)
    #updateSelectInput(session = session, id = "sex", choices = lst_sex,selected = NULL)
    #updateSliderInput(session = session, id = "age", min = 1, max = 100)
  })
  
  
  
  #filter adsl and display
  lst_namel1 = reactive({
    c("All variables",names(get(tolower(input$datasetl1))))
  })
  observe({
    updateSelectInput(session = session, "variable",choices=lst_namel1(),selected="All variables") 
  })

  runDTadsl <- reactive({
    
    newds<-get(input$datasetl1)
    if (input$ittfl==TRUE) newds<-newds%>%filter(ITTFL %in% c("Y"))
    if (input$saffl==TRUE) newds<-newds%>%filter(SAFFL %in% c("Y"))
    if (input$sex %in% unique(adsl$SEX)) newds<-newds%>%filter(SEX %in% c(input$sex))
    newds<-newds%>%filter(AGE>=input$age[1] & AGE<=input$age[2])
    req(input$variable)
    if (grepl("All variables",paste(input$variable,collapse=' '))){newds
    }else{newds <- select(newds,!!!input$variable)}
    datatable(newds,options=list(pageLength=10))
  })
  
  output$dtadsl <- renderDT({
    runDTadsl()
    
  })
  
  #import ds
  runimportds <- reactive({
    infile <- input$importfile
    if (is.null(infile))
      return(NULL)
    if (grepl(".csv",infile[1])){
      newimportds <- read.csv(infile$datapath)}
    if (grepl(".xls",infile[1])){
      newimportds <- read_excel(infile$datapath)}
    if (grepl(".sas7bdat",infile)){
      newimportds <- read_sas(infile$datapath)}
    if (grepl(".xpt",infile[1])){
      newimportds <- read_xpt(infile$datapath)}
    if (grepl(".rda",infile[1])){
      load(infile$datapath)
      newimportds <- substr(infile$name,1,length(infile$name)-4)
    }
    datatable(newimportds,options=list(pageLength=10))
  })
  
  output$importds <- renderDT({
    runimportds()
    
  })
  

  # Downloadable csv of selected dataset ----
  thedata <- reactive({

  if (input$sidebar == "listing" & input$tabset1=="tabset11"){
    newds<-get(input$datasetl1)
    if (input$ittfl==TRUE) newds<-newds%>%filter(ITTFL %in% c("Y"))
    if (input$saffl==TRUE) newds<-newds%>%filter(SAFFL %in% c("Y"))
    if (input$sex %in% unique(adsl$SEX)) newds<-newds%>%filter(SEX %in% c(input$sex))
    newds<-newds%>%filter(AGE>=input$age[1] & AGE<=input$age[2])
    req(input$variable)
    if (grepl("All variables",paste(input$variable,collapse=' '))){newds
    }else{newds <- select(newds,!!!input$variable)}
  }else if(input$sidebar == "listing" & input$tabset1=="tabset12"){
    infile <- input$importfile
    if (is.null(infile))
      return(NULL)
    if (grepl(".csv",infile[1])){
      newimportds <- read.csv(infile$datapath)}
    if (grepl(".xls",infile[1])){
      newimportds <- read_excel(infile$datapath)}
    if (grepl(".sas7bdat",infile)){
      newimportds <- read_sas(infile$datapath)}
    if (grepl(".xpt",infile[1])){
      newimportds <- read_xpt(infile$datapath)}
    if (grepl(".rda",infile[1])){
      load(infile$datapath)
      newimportds <- substr(infile$name,1,length(infile$name)-4)
    }
    newimportds
  }else if(input$sidebar == "table" & input$tabset2=="tabset21"){
    newadsl<-adsl
    if (input$ittfl==TRUE) newadsl<-newadsl%>%filter(ITTFL %in% c("Y"))
    if (input$saffl==TRUE) newadsl<-newadsl%>%filter(SAFFL %in% c("Y"))
    if (input$sex %in% unique(adsl$SEX)) newadsl<-newadsl%>%filter(SEX %in% c(input$sex))
    newadsl<-newadsl%>%filter(AGE>=input$age[1] & AGE<=input$age[2])
    
    req(input$variable2)
    thedata <- select(newadsl,input$trtvariable,!!!input$variable2)%>%
      tbl_summary(by=input$trtvariable,missing_text="(Missing)",type=all_continuous()~"continuous2",statistic=all_continuous()~c("{N_nonmiss}", "{mean} ({sd})", "{median}", "{min}, {max}"))%>%
      add_overall(last=TRUE) %>% as_gt()
  }else if(input$sidebar == "table" & input$tabset2=="tabset22"){
    newadae<-adae
    if (input$ittfl==TRUE) newadae<-newadae%>%filter(ITTFL %in% c("Y"))
    if (input$saffl==TRUE) newadae<-newadae%>%filter(SAFFL %in% c("Y"))
    if (input$sex %in% unique(adsl$SEX)) newadae<-newadae%>%filter(SEX %in% c(input$sex))
    newadae<-newadae%>%filter(AGE>=input$age[1] & AGE<=input$age[2])
    
    popadsl<-adsl
    if (input$ittfl==TRUE) popadsl<-popadsl%>%filter(ITTFL %in% c("Y"))
    if (input$saffl==TRUE) popadsl<-popadsl%>%filter(SAFFL %in% c("Y"))
    if (input$sex %in% unique(adsl$SEX)) popadsl<-popadsl%>%filter(SEX %in% c(input$sex))
    popadsl<<-popadsl%>%filter(AGE>=input$age[1] & AGE<=input$age[2])  
    
    newadae2 <<- newadae %>% filter(TRTEMFL=="Y")
    #any teae
    teae1<-freq(dsin="newadae2",freqvar="STUDYID",trt="TRTA",subkey="USUBJID",popds="popadsl",trtsl="TRT01A")
    
    #any teae with sae
    newadae3 <<- newadae2 %>% filter(AESER=="Y")
    teae2<-freq(dsin="newadae3",freqvar="STUDYID",trt="TRTA",subkey="USUBJID",popds="popadsl",trtsl="TRT01A")
    
    #any death teae
    newadae3 <<- newadae2 %>% filter(AESDTH=="Y")
    teae3<-freq(dsin="newadae3",freqvar="STUDYID",trt="TRTA",subkey="USUBJID",popds="popadsl",trtsl="TRT01A")
    
    #any drug related teae
    newadae3 <<- newadae2 %>% filter(AEREL=="PROBABLE" | AEREL=="POSSIBLE")
    teae4<-freq(dsin="newadae3",freqvar="STUDYID",trt="TRTA",subkey="USUBJID",popds="popadsl",trtsl="TRT01A")
    
    #CQ01NAM
    newadae3 <<- newadae2 %>% filter(!CQ01NAM=="")
    teae5<-freq(dsin="newadae3",freqvar="STUDYID",trt="TRTA",subkey="USUBJID",popds="popadsl",trtsl="TRT01A")
    
    teae <- bind_rows(teae1,teae2,teae3,teae4,teae5) %>% 
      mutate(id=row_number())
    teae$STUDYID <- ifelse(teae$id==1,"Patients with any TEAE",ifelse(teae$id==2,"Patients with SAE",ifelse(teae$id==3,"Patients with death",ifelse(teae$id==4,"Patients with drug related","Patients with dermatologic events"))))
    
    teae_f <- cbind(teae$STUDYID, select(teae, ends_with("_c")))
    
    col1n <- mean(teae$Placebo_n,na.rm = TRUE)
    col2n <- mean(teae$Xanomeline_High_Dose_n,na.rm = TRUE)
    col3n <- mean(teae$Xanomeline_Low_Dose_n,na.rm = TRUE)
    teae_f[is.na(teae_f)] <- 0
    names(teae_f)=c('Label',paste('Placebo','(N=',col1n,')'),paste('Xanomeline High Dose','(N=',col2n,')'),paste('Xanomeline Low Dose','(N=',col3n,')'))
    

    thedata <- teae_f
  }else if(input$sidebar == "table" & input$tabset2=="tabset23"){
    newds<-get(input$dsbds)
    if (input$ittfl==TRUE) newds<-newds%>%filter(ITTFL %in% c("Y"))
    if (input$saffl==TRUE) newds<-newds%>%filter(SAFFL %in% c("Y"))
    if (input$sex %in% unique(adsl$SEX)) newds<-newds%>%filter(SEX %in% c(input$sex))
    newds<-newds%>%filter(AGE>=input$age[1] & AGE<=input$age[2])%>%mutate(newcombinecol=paste(PARAM,"||",AVISIT))
    
    mycontrols  <- tableby.control(test=FALSE, total=TRUE,
                                   numeric.test="kwt", cat.test="chisq",
                                   numeric.stats=c("N", "meansd","median", "range"),
                                   cat.stats=c("countpct"),
                                   stats.labels=list(N='n', meansd='Mean (SD)',median='Median', range='Min, Max'))
    
    f2 <- reactive({ as.formula(paste(input$trtvariablebds, "~", input$avalvariablebds)) })
    
    df31 <- as.data.frame(summary(tableby(f2(),strata=newcombinecol,data=newds,control=mycontrols),text=TRUE))
    names(df31)[2]=c("stat")
    df32 <- as.data.frame(stringr::str_split_fixed(df31$newcombinecol,"\\|\\|",2))
    names(df32)[1]=c("Parmater")
    names(df32)[2]=c("Avisit")
    thedata <- bind_cols(df32,df31[2:ncol(df31)])
  }else if(input$sidebar == "eff" ){
    #filter data
    if (input$ittfl==TRUE) adtte<-adtte%>%filter(ITTFL %in% c("Y"))
    if (input$saffl==TRUE) adtte<-adtte%>%filter(SAFFL %in% c("Y"))
    if (input$sex %in% unique(adsl$SEX)) adtte<-adtte%>%filter(SEX %in% c(input$sex))
    adtte<-adtte%>%filter(AGE>=input$age[1] & AGE<=input$age[2])
    str1 <<- input$trtvariablebeff1
    fit <- survfit(as.formula(paste("Surv(AVAL, CNSR)~", str1)),
                   data = adtte)
    
    #patients with event, censored
    ev <- data.frame(summary(fit)$table)
    #rownames
    x <- data.frame(rownames(ev))
    names(x)='trt'
    
    ev <- ev %>% mutate(censor=records-events)%>% 
      select(records,events,censor)
    
    #median and 95%CI
    d_50 <- data.frame(quantile(fit, percent=c(0.25,0.5,0.75)))
    
    d_50 <- d_50 %>% mutate(ci_95_1=paste(lower.50 , ',', upper.50), r25_75=paste(quantile.25 , ',', quantile.75)) %>% 
      select(quantile.50,ci_95_1,r25_75)
    
    #30, 60, 90
    a <- summary(fit,times=c(30,60,90))
    
    a0 <- do.call(cbind,lapply(list(a$n.risk,a$surv,a$lower,a$upper), data.frame))
    names(a0)=c('risk','survn','low','up')
    a0 <- a0 %>% mutate(id=row_number(),surv2=paste(round(survn,3)*100,"%"),ci=paste("(",round(low,3)*100,"%, ",round(up,3)*100,"%)")) 
    
    
    a10 <- a0 %>% filter(id %in% c(1,4,7)) %>% select(risk,surv2,ci)
    names(a10)=c('risk_30','surv_30','ci_30')
    a20 <- a0 %>% filter(id %in% c(2,5,8)) %>% select(risk,surv2,ci)
    names(a20)=c('risk_60','surv_60','ci_60')
    a30 <- a0 %>% filter(id %in% c(3,6,9)) %>% select(risk,surv2,ci)
    names(a30)=c('risk_90','surv_90','ci_90')
    
    all <- cbind(x,ev,d_50,a10,a20,a30)
    names(all)=c('trt','Number of Patients','Events','Censored','Median','95%CI of median',
                 '25% - 75%','At risk(30 days)','Survival rate(30 days)','95%CI(30 days)',
                 'At risk(60 days)','Survival rate(60 days)','95%CI(60 days)',
                 'At risk(90 days)','Survival rate(90 days)','95%CI(90 days)')
    all2 <- data.frame(t(all)) 
    tmp1 <- data.frame(rownames(all2))
    names(tmp1)='label'
    all3 <- cbind(tmp1,all2) %>% filter(!label=='trt')
    names(all3)=c('Label','Placebo','Xanomeline High Dose','Xanomeline Low Dose')
    all3
    thedata <- all3
  }
  })

  # Downloadable csv of selected dataset ----

  output$download <- downloadHandler(
    filename = function(){"shinyds.csv"}, 
    content = function(fname){
      write.csv(thedata(), fname)
    }
  )

  #summary table
  runsummaryds1 <- reactive({
    
    newadsl<-adsl
    if (input$ittfl==TRUE) newadsl<-newadsl%>%filter(ITTFL %in% c("Y"))
    if (input$saffl==TRUE) newadsl<-newadsl%>%filter(SAFFL %in% c("Y"))
    if (input$sex %in% unique(adsl$SEX)) newadsl<-newadsl%>%filter(SEX %in% c(input$sex))
    newadsl<-newadsl%>%filter(AGE>=input$age[1] & AGE<=input$age[2])
    
    req(input$variable2)
    select(newadsl,input$trtvariable,!!!input$variable2)%>%
      tbl_summary(by=input$trtvariable,missing_text="(Missing)",type=all_continuous()~"continuous2",statistic=all_continuous()~c("{N_nonmiss}", "{mean} ({sd})", "{median}", "{min}, {max}"))%>%
      add_overall(last=TRUE) %>% as_gt()
    
  })
  
  output$summaryds1 <- render_gt({
    runsummaryds1()
    
  })
  

  runsummaryds2 <- reactive({
    
    newadae<-adae
    if (input$ittfl==TRUE) newadae<-newadae%>%filter(ITTFL %in% c("Y"))
    if (input$saffl==TRUE) newadae<-newadae%>%filter(SAFFL %in% c("Y"))
    if (input$sex %in% unique(adsl$SEX)) newadae<-newadae%>%filter(SEX %in% c(input$sex))
    newadae<-newadae%>%filter(AGE>=input$age[1] & AGE<=input$age[2])
   
    popadsl<-adsl
    if (input$ittfl==TRUE) popadsl<-popadsl%>%filter(ITTFL %in% c("Y"))
    if (input$saffl==TRUE) popadsl<-popadsl%>%filter(SAFFL %in% c("Y"))
    if (input$sex %in% unique(adsl$SEX)) popadsl<-popadsl%>%filter(SEX %in% c(input$sex))
    popadsl<<-popadsl%>%filter(AGE>=input$age[1] & AGE<=input$age[2])  
     
    newadae2 <<- newadae %>% filter(TRTEMFL=="Y")
    #any teae
    teae1<-freq(dsin="newadae2",freqvar="STUDYID",trt="TRTA",subkey="USUBJID",popds="popadsl",trtsl="TRT01A")
    
    #any teae with sae
    newadae3 <<- newadae2 %>% filter(AESER=="Y")
    teae2<-freq(dsin="newadae3",freqvar="STUDYID",trt="TRTA",subkey="USUBJID",popds="popadsl",trtsl="TRT01A")
    
    #any death teae
    newadae3 <<- newadae2 %>% filter(AESDTH=="Y")
    teae3<-freq(dsin="newadae3",freqvar="STUDYID",trt="TRTA",subkey="USUBJID",popds="popadsl",trtsl="TRT01A")
    
    #any drug related teae
    newadae3 <<- newadae2 %>% filter(AEREL=="PROBABLE" | AEREL=="POSSIBLE")
    teae4<-freq(dsin="newadae3",freqvar="STUDYID",trt="TRTA",subkey="USUBJID",popds="popadsl",trtsl="TRT01A")
    
    #CQ01NAM
    newadae3 <<- newadae2 %>% filter(!CQ01NAM=="")
    teae5<-freq(dsin="newadae3",freqvar="STUDYID",trt="TRTA",subkey="USUBJID",popds="popadsl",trtsl="TRT01A")

    teae <- bind_rows(teae1,teae2,teae3,teae4,teae5) %>% 
      mutate(id=row_number())
    teae$STUDYID <- ifelse(teae$id==1,"Patients with any TEAE",ifelse(teae$id==2,"Patients with SAE",ifelse(teae$id==3,"Patients with death",ifelse(teae$id==4,"Patients with drug related","Patients with dermatologic events"))))
    
    teae_f <- cbind(teae$STUDYID, select(teae, ends_with("_c")))
    
    col1n <- mean(teae$Placebo_n,na.rm = TRUE)
    col2n <- mean(teae$Xanomeline_High_Dose_n,na.rm = TRUE)
    col3n <- mean(teae$Xanomeline_Low_Dose_n,na.rm = TRUE)
    
    names(teae_f)=c('Label',paste('Placebo','(N=',col1n,')'),paste('Xanomeline High Dose','(N=',col2n,')'),paste('Xanomeline Low Dose','(N=',col3n,')'))
    teae_f[is.na(teae_f)] <- 0
    teae_f

  })
  
  output$summaryds2 <- renderTable({
    runsummaryds2()
    
  })
  
  
  runsummaryds3 <- reactive({
    newds<-get(input$dsbds)
    if (input$ittfl==TRUE) newds<-newds%>%filter(ITTFL %in% c("Y"))
    if (input$saffl==TRUE) newds<-newds%>%filter(SAFFL %in% c("Y"))
    if (input$sex %in% unique(adsl$SEX)) newds<-newds%>%filter(SEX %in% c(input$sex))
    newds<-newds%>%filter(AGE>=input$age[1] & AGE<=input$age[2])%>%mutate(newcombinecol=paste(PARAM,"||",AVISIT))
 
    mycontrols  <- tableby.control(test=FALSE, total=TRUE,
                                   numeric.test="kwt", cat.test="chisq",
                                   numeric.stats=c("N", "meansd","median", "range"),
                                   cat.stats=c("countpct"),
                                   stats.labels=list(N='n', meansd='Mean (SD)',median='Median', range='Min, Max'))
    
    f2 <- reactive({ as.formula(paste(input$trtvariablebds, "~", input$avalvariablebds)) })
    
    df31 <- as.data.frame(summary(tableby(f2(),strata=newcombinecol,data=newds,control=mycontrols),text=TRUE))
    names(df31)[2]=c("stat")
    df32 <- as.data.frame(stringr::str_split_fixed(df31$newcombinecol,"\\|\\|",2))
    names(df32)[1]=c("Parmater")
    names(df32)[2]=c("Avisit")
    df33 <- bind_cols(df32,df31[2:ncol(df31)])
  
  })
  
  output$summaryds3 <- renderTable({
    runsummaryds3()
    
  })
  
  
  
  lst_names1 = reactive({
    c("",names(get(tolower(input$datasets1))))
  })
  lst_names2 = reactive({
    c("",names(get(tolower(input$datasets2))))
  })
  lst_names3 = reactive({
    c("",names(get(tolower(input$datasets3))))
  })
  lst_names4 = reactive({
    c("",names(get(tolower(input$datasets4))))
  })
  observe({
    updateSelectInput(session = session, "plotvariables1",choices=lst_names1()) 
  })
  observe({
    updateSelectInput(session = session, "plotvariables4",choices=lst_names4()) 
  })
  observe({
    updateSelectInput(session = session, "plotvariables2",choices=lst_names2()) 
    updateSelectInput(session = session, "plotvariables22",choices=lst_names2())
  })
  observe({
    updateSelectInput(session = session, "plotvariables3",choices=lst_names3()) 
    updateSelectInput(session = session, "plotvariables32",choices=lst_names3())
  })
  
  #plots using echarts4r
  
  runplotds1 <- reactive({
      
      if (!input$plotvariables1==""){
          tmpplotds <- get(tolower(input$datasets1))
          #filter data
          if (input$ittfl==TRUE) tmpplotds<-tmpplotds%>%filter(ITTFL %in% c("Y"))
          if (input$saffl==TRUE) tmpplotds<-tmpplotds%>%filter(SAFFL %in% c("Y"))
          if (input$sex %in% unique(adsl$SEX)) tmpplotds<-tmpplotds%>%filter(SEX %in% c(input$sex))
          tmpplotds<-tmpplotds%>%filter(AGE>=input$age[1] & AGE<=input$age[2])
          
          
          tmpplotds %>% count(get(input$plotvariables1)) %>% arrange(n) %>% 
          e_charts(get(input$plotvariables1)) %>% 
          e_bar(n,legend = FALSE)%>%
          e_datazoom(
            x_index=0,
            toolbox = FALSE
          ) %>%
          e_datazoom(
            y_index=0,
            toolbox = FALSE
          ) %>%
          e_tooltip(trigger = "item")%>%
          e_labels(position = "top") %>% 
          e_x_axis(splitLine = list(show = FALSE))%>%
          e_color(c('#DDA0DD' , '#eeeeee', '#59c4e6', '#edafda')) %>% 
          e_toolbox_feature(feature = "saveAsImage") %>% 
          e_toolbox_feature(feature = "dataZoom") %>% 
          e_toolbox_feature(feature = "dataView") %>% 
          e_toolbox_feature(feature = "restore") 
 
      }

  })

  #bar chart
  output$plot1 <- renderEcharts4r({
    runplotds1()
  })
  
  
  runplotds2 <- reactive({
    
    if (!input$plotvariables2=="" & !input$plotvariables22==""){
      tmpplotds <- get(tolower(input$datasets2))
      #filter data
      if (input$ittfl==TRUE) tmpplotds<-tmpplotds%>%filter(ITTFL %in% c("Y"))
      if (input$saffl==TRUE) tmpplotds<-tmpplotds%>%filter(SAFFL %in% c("Y"))
      if (input$sex %in% unique(adsl$SEX)) tmpplotds<-tmpplotds%>%filter(SEX %in% c(input$sex))
      tmpplotds<-tmpplotds%>%filter(AGE>=input$age[1] & AGE<=input$age[2])
      
      tmpplotds2 <- cbind(tmpplotds[input$plotvariables2],tmpplotds[input$plotvariables22])
      names(tmpplotds2) <- c("x","y")
      
      tmpplotds2 %>% 
        e_charts(x=x) %>% 
        e_scatter(y,legend = FALSE) %>% 
        e_datazoom(
          x_index=0,
          toolbox = FALSE
        ) %>%
        e_datazoom(
          y_index=0,
          toolbox = FALSE
        ) %>%
        e_tooltip(formatter = htmlwidgets::JS("
          function(params){
            return('x: ' + params.value[0] + '<br />y: ' + params.value[1])
          }
        ")
        )%>%
        #e_tooltip()%>%
        e_x_axis(splitLine = list(show = FALSE))%>%
        e_color(c('#DDA0DD' , '#eeeeee', '#59c4e6', '#edafda')) %>% 
        e_toolbox_feature(feature = "saveAsImage") %>% 
        e_toolbox_feature(feature = "dataZoom") %>% 
        e_toolbox_feature(feature = "dataView") %>% 
        e_toolbox_feature(feature = "restore") 
    }
    
  })
  
  #scatter chart
  output$plot2 <- renderEcharts4r({
    runplotds2()
  })
  
  runplotds3 <- reactive({
    
    if (!input$plotvariables3=="" & !input$plotvariables32==""){
      tmpplotds <- get(tolower(input$datasets3))
      #filter data
      if (input$ittfl==TRUE) tmpplotds<-tmpplotds%>%filter(ITTFL %in% c("Y"))
      if (input$saffl==TRUE) tmpplotds<-tmpplotds%>%filter(SAFFL %in% c("Y"))
      if (input$sex %in% unique(adsl$SEX)) tmpplotds<-tmpplotds%>%filter(SEX %in% c(input$sex))
      tmpplotds<-tmpplotds%>%filter(AGE>=input$age[1] & AGE<=input$age[2])
      
      tmpplotds2 <- cbind(tmpplotds[input$plotvariables3],tmpplotds[input$plotvariables32])
      names(tmpplotds2) <- c("x","y")
      
      tmpplotds2 %>% 
        e_charts(x=x) %>% 
        e_line(y,legend = FALSE) %>% 
        e_datazoom(
          x_index=0,
          toolbox = FALSE
        ) %>%
        e_datazoom(
          y_index=0,
          toolbox = FALSE
        ) %>%
        e_tooltip(trigger = "axis")%>%
        e_x_axis(splitLine = list(show = FALSE))%>%
        e_color(c('#DDA0DD' , '#eeeeee', '#59c4e6', '#edafda')) %>% 
        e_toolbox_feature(feature = "saveAsImage") %>% 
        e_toolbox_feature(feature = "dataZoom") %>% 
        e_toolbox_feature(feature = "dataView") %>% 
        e_toolbox_feature(feature = "restore") 
    }
    
  })
  
  #line chart
  output$plot3 <- renderEcharts4r({
    runplotds3()
  })
  
  runplotds4 <- reactive({
    
    if (!input$plotvariables4==""){
      tmpplotds <- get(tolower(input$datasets4))
      #filter data
      if (input$ittfl==TRUE) tmpplotds<-tmpplotds%>%filter(ITTFL %in% c("Y"))
      if (input$saffl==TRUE) tmpplotds<-tmpplotds%>%filter(SAFFL %in% c("Y"))
      if (input$sex %in% unique(adsl$SEX)) tmpplotds<-tmpplotds%>%filter(SEX %in% c(input$sex))
      tmpplotds<-tmpplotds%>%filter(AGE>=input$age[1] & AGE<=input$age[2])

      
      tmpplotds %>% count(get(input$plotvariables4)) %>% arrange(n) %>% 
        e_charts(get(input$plotvariables4)) %>% 
        e_pie(n,legend = FALSE)%>%
        e_tooltip(trigger = "item") %>% 
        e_toolbox_feature(feature = "saveAsImage") %>% 
        e_toolbox_feature(feature = "dataZoom") %>% 
        e_toolbox_feature(feature = "dataView") %>% 
        e_toolbox_feature(feature = "restore") 
      
    }
    
  })
  
  #pie chart
  output$plot4 <- renderEcharts4r({
    runplotds4()
  }) 
  
  
  runplotds5 <- reactive({
   
    #filter data
    if (input$ittfl==TRUE) adrs<-adrs%>%filter(ITTFL %in% c("Y"))
    if (input$saffl==TRUE) adrs<-adrs%>%filter(SAFFL %in% c("Y"))
    if (input$sex %in% unique(adsl$SEX)) adrs<-adrs%>%filter(SEX %in% c(input$sex))
    adrs<-adrs%>%filter(AGE>=input$age[1] & AGE<=input$age[2])
    
	  tmpplotds <- select(mutate(adrs,treat=as.numeric(TRTEDT-TRTSDT+1)),USUBJID,AVALC,AVISITN,ADY,TRTA,treat)

	  tmpplotds2 <- reshape2::dcast(tmpplotds,USUBJID+TRTA+treat+AVISITN~AVALC,value.var="ADY") %>% 
	    mutate(color=ifelse(TRTA=="Placebo","red",ifelse(TRTA=="Xanomeline High Dose","blue",ifelse(TRTA=="Xanomeline Low Dose","yellow",""))))

		tmpplotds2 %>%
		  arrange(TRTA,treat) %>% 
		  e_charts(x = USUBJID) %>%
		  e_bar(treat, legend = FALSE, name = "Treatment duration") %>% 
		  #e_scatter(CR, name = "CR", symbol = "circle", symbol_size = 8) %>%
		  e_scatter(PR, name = "PR", symbol = "diamond", symbol_size = 8) %>%
		  e_scatter(SD, name = "SD", symbol = "roundRect", symbol_size = 8) %>%
		  e_scatter(PD, name = "PD", symbol = "triangle", symbol_size = 8) %>%
		  e_datazoom(
			x_index = 0,
			type = "slider", 
			toolbox = FALSE
		  )%>%
		  e_datazoom(
			y_index = 0,
			type = "slider", 
			toolbox = FALSE
		  )%>%
		  e_tooltip() %>% 
		  e_flip_coords() %>% 
		  e_y_axis(splitLine = list(show = FALSE)) %>% 
		  e_x_axis(show = TRUE)%>%
		  e_add("itemStyle",color)%>%
		  e_color(c('#dda0dd' , '#eeeeee', '#59c4e6', '#edafda')) %>% 
		  e_toolbox_feature(feature = "saveAsImage") %>% 
		  e_toolbox_feature(feature = "dataZoom") %>% 
		  e_toolbox_feature(feature = "dataView") %>% 
		  e_toolbox_feature(feature = "restore") 

  })
  
  #swimmer
  output$plot5 <- renderEcharts4r({
    runplotds5()
  }) 
  
  
  runplotds7 <- reactive({

  #filter data
  if (input$ittfl==TRUE) adtr<-adtr%>%filter(ITTFL %in% c("Y"))
  if (input$saffl==TRUE) adtr<-adtr%>%filter(SAFFL %in% c("Y"))
  if (input$sex %in% unique(adsl$SEX)) adtr<-adtr%>%filter(SEX %in% c(input$sex))
  adtr<-adtr%>%filter(AGE>=input$age[1] & AGE<=input$age[2])
    
	tmpplotds <- adtr %>% filter(PARAMCD=="PCNSD") %>% 
		select(USUBJID,AVAL,TRTA) %>% 
		arrange(USUBJID,AVAL) %>% distinct(USUBJID,.keep_all=TRUE) %>% 
		mutate(color=ifelse(TRTA=="Placebo","red",ifelse(TRTA=="Xanomeline High Dose","blue",ifelse(TRTA=="Xanomeline Low Dose","yellow",""))))
	
	tmpplotds %>%
	  arrange(desc(AVAL)) %>% 
	  e_charts(x = USUBJID) %>%
	  e_bar(AVAL, legend = FALSE, name = "Best change from baseline") %>% 
	  e_datazoom(
		x_index = 0,
		type = "slider", 
		toolbox = FALSE
	  )%>%
	  e_datazoom(
		y_index = 0,
		type = "slider", 
		toolbox = FALSE
	  )%>%
	  e_tooltip() %>% 
	  e_add("itemStyle", color) %>% 
	  e_toolbox_feature(feature = "saveAsImage") %>% 
	  e_toolbox_feature(feature = "dataZoom") %>% 
	  e_toolbox_feature(feature = "dataView") %>% 
	  e_toolbox_feature(feature = "restore") 

  })
  
  #waterfall
  output$plot7 <- renderEcharts4r({
    runplotds7()
  }) 
  

  runplotds8 <- reactive({
    
    #filter data
    if (input$ittfl==TRUE) adtr<-adtr%>%filter(ITTFL %in% c("Y"))
    if (input$saffl==TRUE) adtr<-adtr%>%filter(SAFFL %in% c("Y"))
    if (input$sex %in% unique(adsl$SEX)) adtr<-adtr%>%filter(SEX %in% c(input$sex))
    adtr<-adtr%>%filter(AGE>=input$age[1] & AGE<=input$age[2])
    
    tmpplotds <- adtr %>% filter(PARAMCD=="PCNSD" & !is.na(ADY)) %>% 
  	select(USUBJID,AVAL,AVISITN,TRTA,ADY)
  	ady0 <- tmpplotds %>% arrange(USUBJID) %>% distinct(USUBJID,.keep_all=TRUE) 
  	ady0$ADY=0
  	ady0$AVISITN=0
  	ady0$AVAL=0
  	tmpplotds2 <- rbind(tmpplotds,ady0) %>% 
	  reshape2::dcast(USUBJID+ADY+AVISITN~TRTA,value.var="AVAL") %>% 
	  arrange(USUBJID,ADY)

	  names(tmpplotds2) <- gsub(" ","_",names(tmpplotds2))
	  
  	tmpplotds2 %>%
  	  group_by(USUBJID)%>%
  	  e_charts(x = ADY) %>%
  	  e_line(Placebo, symbol="circle", name = "Change from baseline in Placebo") %>% 
  	  e_line(Xanomeline_High_Dose, symbol="diamond", name = "Change from baseline in Xanomeline_High_Dose") %>% 
  	  e_line(Xanomeline_Low_Dose, symbol="triangle", name = "Change from baseline in Xanomeline_Low_Dose") %>% 
  	  e_datazoom(
  		x_index = 0,
  		type = "slider", 
  		toolbox = FALSE
  	  )%>%
  	  e_datazoom(
  		y_index = 0,
  		type = "slider", 
  		toolbox = FALSE
  	  )%>%
  	  e_tooltip() %>% 
  	  e_color(c('green','red','blue')) %>% 
  	  e_toolbox_feature(feature = "saveAsImage") %>% 
  	  e_toolbox_feature(feature = "dataZoom") %>% 
  	  e_toolbox_feature(feature = "dataView") %>% 
  	  e_toolbox_feature(feature = "restore") 
    
  })
  
  #spider
  output$plot8 <- renderEcharts4r({
    runplotds8()
  }) 
  
  #efficacy table and plot
  runeffds1 <- reactive({
    
    #filter data
    if (input$ittfl==TRUE) adtte<-adtte%>%filter(ITTFL %in% c("Y"))
    if (input$saffl==TRUE) adtte<-adtte%>%filter(SAFFL %in% c("Y"))
    if (input$sex %in% unique(adsl$SEX)) adtte<-adtte%>%filter(SEX %in% c(input$sex))
    adtte<-adtte%>%filter(AGE>=input$age[1] & AGE<=input$age[2])
    str1 <<- input$trtvariablebeff1
    fit <- survfit(as.formula(paste("Surv(AVAL, CNSR)~", str1)),
                   data = adtte)

    #patients with event, censored
    ev <- data.frame(summary(fit)$table)
    #rownames
    x <- data.frame(rownames(ev))
    names(x)='trt'
    
    ev <- ev %>% mutate(censor=records-events)%>% 
      select(records,events,censor)
    
    #median and 95%CI
    d_50 <- data.frame(quantile(fit, percent=c(0.25,0.5,0.75)))
    
    d_50 <- d_50 %>% mutate(ci_95_1=paste(lower.50 , ',', upper.50), r25_75=paste(quantile.25 , ',', quantile.75)) %>% 
      select(quantile.50,ci_95_1,r25_75)
    
    #30, 60, 90
    a <- summary(fit,times=c(30,60,90))
    
    a0 <- do.call(cbind,lapply(list(a$n.risk,a$surv,a$lower,a$upper), data.frame))
    names(a0)=c('risk','survn','low','up')
    a0 <- a0 %>% mutate(id=row_number(),surv2=paste(round(survn,3)*100,"%"),ci=paste("(",round(low,3)*100,"%, ",round(up,3)*100,"%)")) 
    
    
    a10 <- a0 %>% filter(id %in% c(1,4,7)) %>% select(risk,surv2,ci)
    names(a10)=c('risk_30','surv_30','ci_30')
    a20 <- a0 %>% filter(id %in% c(2,5,8)) %>% select(risk,surv2,ci)
    names(a20)=c('risk_60','surv_60','ci_60')
    a30 <- a0 %>% filter(id %in% c(3,6,9)) %>% select(risk,surv2,ci)
    names(a30)=c('risk_90','surv_90','ci_90')
    
    all <- cbind(x,ev,d_50,a10,a20,a30)
    names(all)=c('trt','Number of Patients','Events','Censored','Median','95%CI of median',
                 '25% - 75%','At risk(30 days)','Survival rate(30 days)','95%CI(30 days)',
                 'At risk(60 days)','Survival rate(60 days)','95%CI(60 days)',
                 'At risk(90 days)','Survival rate(90 days)','95%CI(90 days)')
    all2 <- data.frame(t(all)) 
    tmp1 <- data.frame(rownames(all2))
    names(tmp1)='label'
    all3 <- cbind(tmp1,all2) %>% filter(!label=='trt')
    names(all3)=c('Label','Placebo','Xanomeline High Dose','Xanomeline Low Dose')
    all3
    
  })
  
  output$effds1 <- renderTable({
    runeffds1()
    
  })


  runeffplot1 <- reactive({
    
    #filter data
    if (input$ittfl==TRUE) adtte<-adtte%>%filter(ITTFL %in% c("Y"))
    if (input$saffl==TRUE) adtte<-adtte%>%filter(SAFFL %in% c("Y"))
    if (input$sex %in% unique(adsl$SEX)) adtte<-adtte%>%filter(SEX %in% c(input$sex))
    adtte<-adtte%>%filter(AGE>=input$age[1] & AGE<=input$age[2])
    str1 <<- input$trtvariablebeff1
    fit <- survfit(as.formula(paste("Surv(AVAL, CNSR)~", str1)),
                   data = adtte)
    
    ggplot1 <- ggsurvplot(fit,
               pval = TRUE, conf.int = FALSE,
               risk.table = TRUE, # Add risk table
               risk.table.col = "strata", # Change risk table color by groups
               linetype = "strata", # Change line type by groups
               surv.median.line = "hv", # Specify median survival
               ggtheme = theme_gray(),
               xlab = "Time (days)", 
               axes.offset =  T,
               legend.labs = c("Placebo", "Xanomeline High Dose", "Xanomeline Low Dose")
    )
    
    ggplot1
    
  })
  
  output$effplot1 <- renderPlot({
    runeffplot1()
    
  })
  
  #download survaival plot
  output$download1 <- downloadHandler(
    filename = function(){paste("survival", '.png', sep='')},
    content = function(file){
      ggsave(file, plot = print(runeffplot1(), width = 9, height = 6))
    }
  )
  output$download2 <- downloadHandler(
    filename = function(){paste("survival", '.pdf', sep='')},
    content = function(file){
      ggsave(file, plot = print(runeffplot1()), device = 'pdf', width = 9, height = 6)
    }
  )
  
  
 
  # deploy needed, end shiny if close the browser
  
    session$onSessionEnded(function() {
        stopApp()
	})
	
  # deploy end
  
})



