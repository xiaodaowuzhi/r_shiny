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



# build ui.R
# 1. header
header <- 
  dashboardHeader(title ="CStone Data Exploration",
                   disable = FALSE, 
                   titleWidth  = 300,
                   dropdownMenu( type = 'message',
                                 messageItem(
                                   from = "Email me",
                                   message =  "Feedback or comment",
                                   icon = icon("envelope"),
                                   href = "mailto:yuanyaoxian@cstonepharma.com"
                                  ),
                                icon = icon('comment')
                            )
                  )

header$children[[2]]$children[[2]] <- header$children[[2]]$children[[1]]
header$children[[2]]$children[[1]] <- tags$a(tags$img(src='head.png'), target = '_blank')

# 2. siderbar
siderbar <- dashboardSidebar(
  width = 300,
  collapsed = TRUE,
  sidebarMenu(
    id = "sidebar",
    useShinyjs(),
    #first
    menuItem("Data Listing", tabName = 'listing', icon = icon('table'),
             badgeLabel = "v0.1", badgeColor = "green"),

    #second
    menuItem("Summary Table", tabName = 'table', icon = icon('columns'),
             badgeLabel = "v0.1", badgeColor = "green" ),
    
    #third
    menuItem("Plot", tabName = 'plot', icon = icon('chart-bar'),
             badgeLabel = "v0.1", badgeColor = "green" ),
       
    #fourth  
    menuItem("Efficacy", tabName = 'eff', icon = icon('question'),
             badgeLabel = "developing", badgeColor = "red" ),
    
	  #fifth		 
    menuItem("About", tabName = 'about', icon = icon('book-reader'),
             badgeLabel = "v0.1", badgeColor = "green" )
  ) 
)

# 3. body
body <- dashboardBody(
  useShinyjs(),
  tags$script(HTML("$('body').addClass('sidebar-mini');")),
  tabItems(
    # 3.1 Main dashboard
    tabItem( tabName = "listing",
             fluidRow(
               tabBox(
                 title = NULL,
                 # The id lets us use input$tabset1 on the server to find the current tab
                 id = "tabset1",
                 width=NULL,
                 height=NULL,
                 tabPanel("Datasets",value = "tabset11",  
                          div(style="display: inline-block;vertical-align:top; width: 150px;",selectInput("datasetl1", "Select Data:",filex2,selected="adsl")),
                          div(style="display: inline-block;vertical-align:top; width: 950px;",selectInput("variable", "Select Variable:","All variables",multiple=TRUE,selected="All variables")),
                          DTOutput("dtadsl")),
                 tabPanel("Import",value = "tabset12",
                          div(style="display: inline-block;vertical-align:top; width: 400px;",fileInput("importfile", "Import File",
                                        accept = c(
                                          ".csv",
                                          ".xlsx",
                                          ".xls",
                                          ".sas7bdat",
                                          ".xpt",
                                          ".rda")
                          )),
                          DTOutput("importds"))
               )
             )
    ),
    tabItem( tabName = "table",
             
             fluidRow(
               tabBox(
                 title = NULL,
                 # The id lets us use input$tabset2 on the server to find the current tab
                 id = "tabset2", 
                 width=NULL,
                 height=NULL,
                 #height = "250px",
                 tabPanel("Subject level - ADSL",value = "tabset21",
                          div(style="display: inline-block;vertical-align:top; width: 250px;",selectInput("trtvariable", "Select Treatment Variable:",list("TRT01A" = "TRT01A", 
                                                                                                                                                           "TRT01P" = "TRT01P"),selected="TRT01A")),
                          div(style="display: inline-block;vertical-align:top; width: 250px;",selectInput("variable2", "Select Variables:",names(adsl),multiple=TRUE)),
                          div(gt_output("summaryds1"),align = "left",style="vertical-align:left;")),
                 
                 tabPanel("Adverse event",value = "tabset22", 
                          
                          div(tableOutput("summaryds2"),align = "left",style="vertical-align:left;")),
                 
                 tabPanel("Basic data structure",value = "tabset23", 
                          div(style="display: inline-block;vertical-align:top; width: 250px;",selectInput("dsbds", "Select Dataset:",filex2,selected="advs")),
                          div(style="display: inline-block;vertical-align:top; width: 250px;",selectInput("trtvariablebds", "Select Treatment Variable:",list("TRTA" = "TRTA", 
                                                                                                                                                              "TRTP" = "TRTP"),selected="TRTA")),
                          div(style="display: inline-block;vertical-align:top; width: 250px;",selectInput("avalvariablebds", "Select Variable:",list("AVAL" = "AVAL", 
                                                                                                                                                     "CHG" = "CHG"),selected="AVAL")),
                          div(tableOutput("summaryds3"),align = "left",style="vertical-align:left;"))
               )
               
             )
             
    ),
    tabItem( tabName = "plot",
             
             fluidRow(
               tabBox(
                 title = NULL,
                 # The id lets us use input$tabset2 on the server to find the current tab
                 id = "tabset3", 
                 width=NULL,
                 height=NULL,
                 #height = "250px",
                 tabPanel("Basic Charts",value = "tabset31",
                          fluidRow(style='margin: 0px;',
                                   shinydashboardPlus::box(title = "Bar", status = "warning", solidHeader = FALSE, collapsible = TRUE, width = 6, closable = TRUE,                         
                                           div(style="display: inline-block;vertical-align:top; width: 150px;",selectInput("datasets1", "Select Data:",filex2,selected="adsl")),
                                           div(style="display: inline-block;vertical-align:top; width: 150px;",selectInput("plotvariables1", "Select Variable:","")),		
                                           echarts4rOutput("plot1")),
                                   shinydashboardPlus::box(title = "Pie", status = "warning", solidHeader = FALSE, collapsible = TRUE, width = 6,  closable = TRUE,                          
                                           div(style="display: inline-block;vertical-align:top; width: 150px;",selectInput("datasets4", "Select Data:",filex2,selected="adsl")),
                                           div(style="display: inline-block;vertical-align:top; width: 150px;",selectInput("plotvariables4", "Select Variable:","")),		
                                           echarts4rOutput("plot4"))),
                          fluidRow(style='margin: 0px;',
                                   shinydashboardPlus::box(title = "Line", status = "warning", solidHeader = FALSE, collapsible = TRUE, width = 6,  closable = TRUE,                          
                                           div(style="display: inline-block;vertical-align:top; width: 150px;",selectInput("datasets3", "Select Data:",filex2,selected="adsl")),
                                           div(style="display: inline-block;vertical-align:top; width: 150px;",selectInput("plotvariables3", "Select X Variable:","")),	
                                           div(style="display: inline-block;vertical-align:top; width: 150px;",selectInput("plotvariables32", "Select Y Variable:","")),		
                                           echarts4rOutput("plot3")),
                                   shinydashboardPlus::box(title = "Scatter", status = "warning", solidHeader = FALSE, collapsible = TRUE, width = 6,  closable = TRUE,                          
                                           div(style="display: inline-block;vertical-align:top; width: 150px;",selectInput("datasets2", "Select Data:",filex2,selected="adsl")),
                                           div(style="display: inline-block;vertical-align:top; width: 150px;",selectInput("plotvariables2", "Select X Variable:","")),	
                                           div(style="display: inline-block;vertical-align:top; width: 150px;",selectInput("plotvariables22", "Select Y Variable:","")),	
                                           echarts4rOutput("plot2")))
                          
                 ),
                 tabPanel("Oncology Charts",value = "tabset32",
                          fluidRow(style='margin: 0px;',
                                   shinydashboardPlus::box(title = "Swimmer", status = "warning", solidHeader = FALSE, collapsible = TRUE, width = 9, closable = TRUE,                 
                                           echarts4rOutput("plot5"))),
                                   #shinydashboardPlus::box(title = "KM", status = "warning", solidHeader = FALSE, collapsible = TRUE, width = 6,  closable = TRUE,                          
                                           #echarts4rOutput("plot6"))),
                          fluidRow(style='margin: 0px;',
                                   shinydashboardPlus::box(title = "Waterfall", status = "warning", solidHeader = FALSE, collapsible = TRUE, width = 6,  closable = TRUE,                          
                                           echarts4rOutput("plot7")),
                                   shinydashboardPlus::box(title = "Spider", status = "warning", solidHeader = FALSE, collapsible = TRUE, width = 6,  closable = TRUE,                          
                                           echarts4rOutput("plot8")))
                          
                 )
                 
                 
               )
               
             )
             
    ),
    tabItem(tabName = "eff",div(id="tabset4",
        fluidRow(style='margin: 0px;',
            shinydashboardPlus::box(title = "Efficacy Table", status = "warning", solidHeader = FALSE, collapsible = TRUE, width = 6,  closable = TRUE, 
              div(style="display: inline-block;vertical-align:top; width: 250px;",selectInput("trtvariablebeff1", "Select Treatment Variable:",list("TRTA" = "TRTA", "TRTP" = "TRTP"),selected="TRTA")),
              div(tableOutput("effds1"),align = "center",style="vertical-align:center;")),
            shinydashboardPlus::box(title = "KM Plot", status = "warning", solidHeader = FALSE, collapsible = TRUE, width = 6,  closable = TRUE, 
                                    dropdownMenu = boxDropdown(icon=shiny::icon("download"),
                                                               boxDropdownItem(tags$style(type="text/css",
                                                                                          "#download1, #download1:active  {
                                                                                          background-color:rgba(0,0,0,0);
                                                                                          border-style: none;
                                                                                          }
                                                                                         "),
                                                                               downloadButton("download1", "Download png")),
                                                               boxDropdownItem(tags$style(type="text/css",
                                                                                          "#download2, #download1:active  {
                                                                                          background-color:rgba(0,0,0,0);
                                                                                          border-style: none;
                                                                                          }
                                                                                         "),
                                                                               downloadButton("download2", "Download pdf"))
                                    ),
              div(plotOutput("effplot1")))
            ))
    ),
    tabItem( tabName = "about", fluidRow(
      
      accordion(
        id = "accordion1",
        width = 8,
        accordionItem(
          title = "About study data",
          status = "danger",
          collapsed = TRUE,
          solidHeader = FALSE,
          tags$div(style="display: inline-block;vertical-align:top; font-size: 18px; font-family: Arial, Helvetica, sans-serif;",
          "This is a pilot study where comes from the website cdisc, you can also download the data from the ",
          tags$a(href="https://bitbucket.cdisc.org/projects/CED/repos/sdtm-adam-pilot-project/browse/updated-pilot-submission-package/900172/m5/datasets/cdiscpilot01/analysis/adam/datasets", "link",target="_blank"),
          ", specilly adtr and adrs were dummy data")
        ),
        accordionItem(
          title = "Packages used",
          status = "warning",
          collapsed = TRUE,
          solidHeader = FALSE,
          tags$div(style="display: inline-block;vertical-align:top; font-size: 18px; font-family: Arial, Helvetica, sans-serif;",
                   "Many packages were used in the shiny app, thanks to these developers, but not limited to the following..."),
          userList(
            userListItem(
              image = "shiny.png", 
              title = "Shiny", 
              subtitle = NULL
            ),
            userListItem(
              image = "plus.png", 
              title = "Shinydashboardplus", 
              subtitle = NULL
            ),
            userListItem(
              image = "tidyr.png", 
              title = "tidyr", 
              subtitle = NULL
            ),
			userListItem(
              image = "gtsummary.png", 
              title = "gtsummary", 
              subtitle = NULL
            ),
			userListItem(
              image = "arsenal.png", 
              title = "arsenal", 
              subtitle = NULL
            ),
            userListItem(
              image = "ggplot2.png", 
              title = "ggplot2", 
              subtitle = NULL
            ),
            userListItem(
              image = "echarts4r.png", 
              title = "echarts4r", 
              subtitle = NULL
            )
          )
        ),
        accordionItem(
          
          title = "About me",
          status = "info",
          collapsed = TRUE,
          solidHeader = FALSE,
          userBox(
            title = userDescription(
              title = tags$a(href="https://www.linkedin.com/in/yaoxian-yuan-1b0b13194/", "Yaoxian Yuan",target="_blank"),
              subtitle = "Statistical Programmer",
              image = "hj.jfif",
              backgroundImage = "background.jpeg"),
            width = 8,
            status = "teal",
            closable = FALSE,
            collapsible = FALSE,
            NULL,
            footer = tags$div(style="display: inline-block;vertical-align:top; font-size: 18px; font-family: Arial, Helvetica, sans-serif;",
                              tags$a(href="https://www.linkedin.com/in/yaoxian-yuan-1b0b13194/", "Yaoxian Yuan",target="_blank")," is the statistical progrmmer from CStone, he has five years experience in the industry and interted in data visualization")
              
          )
          
        )   
      )
      
    ))

    #tab end 
  )
  
)

#right siderbar
controlbar <-   dashboardControlbar(
  shinyjs::useShinyjs(),
  id = "controlbar",
  width=300,
  controlbarItem(
    "right_bar",
    div(style="display: inline-block;vertical-align:top; width: 40px;",HTML("<br>")),
    div(style="display: inline-block;vertical-align:top; width: 100px;",checkboxInput("ittfl", "ITTFL", FALSE)),
    div(style="display: inline-block;vertical-align:top; width: 40px;",HTML("<br>")),
    div(style="display: inline-block;vertical-align:top; width: 100px;",checkboxInput("saffl", "SAFFL", FALSE)),
    
    div(style="display: inline-block;vertical-align:top; width: 40px;",HTML("<br>")),
    div(style="display: inline-block;vertical-align:top; width: 220px;",selectInput("sex","SEX:", choices = NULL,width = "220px")),
  
    div(style="display: inline-block;vertical-align:top; width: 40px;",HTML("<br>")),
    div(style="display: inline-block;vertical-align:top; width: 220px;",sliderInput("age", "Age:", min = 1, max = 100, value = c(1,100))),
    
    div(style="display: inline-block;vertical-align:top; width: 40px;",HTML("<br>")),
    div(style="display: inline-block;vertical-align:top; width: 220px;",downloadButton('download',"Download the data"),style = "color: black; margin-left: 5px;"),
  
    div(style="display: inline-block;vertical-align:top; width: 40px;",HTML("<br>")),
    div(style="display: inline-block;vertical-align:top; width: 40px;",HTML("<br>")),
    div(style="display: inline-block;vertical-align:top; width: 220px;",actionButton('reset_filter',paste0("Reset filter data",stringi::stri_dup(intToUtf8(160), 5)),icon = icon("cog", lib = "glyphicon")),style="color: black; margin-left: 50px;")
    )
    
    #selectInput("sex","SEX:", choices = NULL,width = "300px"),
    #sliderInput("age", "Age:", min = 1, max = 100, value = c(1,100)),
    #div(downloadButton('download',"Download the data"),style = "color: black; margin-left: 15px;"))
 
)
footer <- dashboardFooter(left="Developed by Yaoxian Yuan", right="Copyright 2021, All Rights Reserved.")
# all
ui <- shinydashboardPlus::dashboardPage(options = list(sidebarExpandOnHover = TRUE),skin = "green",header, siderbar, body, controlbar,footer)



