library(shiny)
library(shinydashboard)
library(plotly)
library(devtools)
library(tidyverse)
library(DT) 
library(ggplot2)
library(formattable)
library(matrixStats)
library(reshape2)
library(dashboardthemes)
library(shinythemes)
library(shinyWidgets)
library(shinyjs)
library(shinycssloaders)
library(hitandrun)
source('functions3.R')

convertMenuItem <- function(mi,tabName) {
  mi$children[[1]]$attribs['data-toggle']="tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  #if(length(mi$attribs$class)>0 && mi$attribs$class=="treeview"){
   # mi$attribs$class=NULL
  #}
  mi
}



ui <- {
  dashboardPage( title="Quantitative BR Analysis",
  dashboardHeader(title="Decision analysis for benefit-risk", titleWidth=600),
  dashboardSidebar(
    sidebarMenu(id="tabs",
                menuItem("Introduction, Data and Preferences",tabName="intro"),
                convertMenuItem(menuItem("Data Source", tabName = "Data",radioButtons("source",
                  "Select source of data",
                  choices = c(
                    "Example dataset" = "example",
                    "Upload dataset" = "upload"
                  )
                ),
                conditionalPanel(
                  condition = "input.source == 'example'",
                  helpText("This is a fabricated dataset")
                ),
                conditionalPanel(
                  condition = "input.source == 'upload'",
                  fileInput(
                    "upload_data",
                    "Please upload your data",
                    multiple = FALSE,
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv"
                    )
                  ),
                  helpText("File should be in .csv format")
                ),
                actionButton(
                  "use",
                  "Use dataset"
                )),"Data"),
                convertMenuItem(menuItem( "Range and Utility functions", tabName = "rangeing", 
                  radioButtons(
                    "rangetyp","Select what range to use",choices = c("Configured range" = "configr","Input custom range" = "customr")),
                  actionButton("applyran", "Define Range")),"rangeing"),
                convertMenuItem(menuItem( "Weights", tabName = "weighting", 
                                          radioButtons(
                                            "weightyp","Select Weighting Type",choices = c("Swing Weighting" = "weightimp")),actionButton("defwts", "Define Weights")),"weighting"),
                convertMenuItem(menuItem( "MCDA", tabName = "mcdas", actionButton("applym", "Apply")),"mcdas"),
                convertMenuItem(menuItem( "Probabilistic MCDA", tabName = "pmcdas", 
                numericInput("a", 
                             "'a' for Prior Beta(a,b) (Default: 1)",
                             min = 0,
                             max = 100000,
                             value = 1, 
                             step = 0.001 ), 
                numericInput("b",
                             "'b' for Prior Beta(a,b) (Default: 1)",
                             min = 0,
                             max = 100000, 
                             value = 1,
                             step = 0.001), 
                numericInput("alpha", 
                             "'alpha' for Prior Gamma(alpha,beta) (Default: 1)",
                             min = 0,
                             max = 100000,
                             value = 0.001, 
                             step = 0.001 ), 
                numericInput("beta",
                             "'beta' for Prior Gamma(alpha,beta) (Default: 1)",
                             min = 0,
                             max = 100000, 
                             value = 0.001,
                             step = 0.001), 
                numericInput("Mu0", 
                             "'Mu0' for Prior Mu(Mu0,Tau) (Default: 1)",
                             min = -100000,
                             max = 100000,
                             value = 0, 
                             step = 0.001 ), 
                numericInput("Tau",
                             "'Tau' for Prior Mu(Mu0,Tau) (Default: 1)",
                             min = 0,
                             max = 100000, 
                             value = 1000,
                             step = 0.001), 
                actionButton("applyp","Apply")),"pmcdas"),
                convertMenuItem(
                  menuItem( "SMAA", tabName = "smaas", 
                  numericInput("a", 
                  "'a' for Prior Beta(a,b) (Default: 1)",
                  min = 0,
                  max = 100000,
                  value = 1, 
                  step = 0.001 ), 
                  numericInput("b",
                               "'b' for Prior Beta(a,b) (Default: 1)",
                               min = 0,
                               max = 100000, 
                               value = 0.001,
                               step = 0.001), 
                  numericInput("beta",
                               "'beta' for Prior Gamma(alpha,beta) (Default: 1)",
                               min = 0,
                               max = 100000, 
                               value = 0.001,
                               step = 0.001), 
                  numericInput("Mu0", 
                               "'Mu0' for Prior Mu(Mu0,Tau) (Default: 1)",
                               min = -100000,
                               max = 100000,
                               value = 0, 
                               step = 0.001 ), 
                  numericInput("Tau",
                               "'Tau' for Prior Mu(Mu0,Tau) (Default: 1)",
                               min = 0,
                               max = 100000, 
                               value = 1000,
                               step = 0.001), 
                  actionButton("applysm","Apply")),"smaas"),menuItem("Generate Report",tabName="report"))),
  dashboardBody(
    
    tags$head(tags$style(HTML("pre { white-space: pre-wrap; word-break: keep-all; }"))),
    #fluidRow(tabBox(width="100%", title="Arranged Data",withSpinner(DT::dataTableOutput("tab_dataset"), type = 5, color = "#34495e"),selected=TRUE),tabBox(width="100%", title="Value Functions",plotlyOutput("tab_plot00"),selected=TRUE)),
    tabItems(
      tabItem(tabName="intro",fluidRow(tabBox(title="",width='100%',height = "250px",id="methods",tabPanel("Introduction",verbatimTextOutput("introtext"),height = "400px", width = "400px"),tabPanel("MCDAimage",imageOutput("mcda1")),tabPanel("pMCDAimage",imageOutput("pmcda1")),tabPanel("SMAAimage",imageOutput("smaa1")), selected=TRUE))),
      tabItem(tabName = "Data",fluidRow(box(width='100%',uiOutput("mainmenu"),status="primary"),box(width='100%',withSpinner(DT::dataTableOutput("tab_dataset"), type = 5, color = "#34495e"),status="primary"))),
      tabItem(tabName = "rangeing",fluidRow(box(width='100%',uiOutput("setting_panel1"),status="primary"),box(width='100%',plotlyOutput("tab_plot00"),status="primary"))),
      tabItem(tabName = "weighting",fluidRow(box(width='100%',uiOutput("setting_panel2"),status="primary"))),
      tabItem(tabName ="mcdas",fluidRow(box(width='100%',plotlyOutput("tab_plot01"),collapsible = TRUE,status="primary",collapsed = TRUE),box(width='100%',plotlyOutput("tab_plot0"),collapsible = TRUE,status="primary",collapsed = TRUE),box(width='100%',plotlyOutput("tab_plot02"),collapsible = TRUE,status="primary",collapsed = TRUE))), 
      tabItem(tabName ="pmcdas",fluidRow(box(width='100%',plotlyOutput("tab_plot"),collapsible = TRUE,status="primary",collapsed = TRUE),box(width='100%',plotlyOutput("tab_plot2"),collapsible = TRUE,status="primary",collapsed = TRUE),box(width='100%',plotlyOutput("tab_plot3"),collapsible = TRUE,status="primary",collapsed = TRUE))), 
      tabItem(tabName ="smaas",fluidRow(box(width='100%',plotlyOutput("tab_plot4"),collapsible = TRUE,status="primary",collapsed = TRUE),box(width='100%',plotlyOutput("tab_plot5"),collapsible = TRUE,status="primary",collapsed = TRUE),box(width='100%',plotlyOutput("tab_plot6"),collapsible = TRUE,status="primary",collapsed = TRUE))),
      tabItem(tabName="report",fluidRow(box(status = "success", solidHeader = TRUE,collapsible = TRUE, width = '100%',downloadButton("report_br", "Generate report") ) ) ) )
      ))
}  


server <-  function(input, output, session){
  
  output$mainmenu <- renderMenu({
        conditionalPanel(
          condition = "input.use",
          br(),
          pickerInput(
            "alternative",
            "Select column containing alternatives",
            choices = NULL
          ),
          pickerInput(
            "criterion_max",
            "Benefit criteria",
            choices = NULL,
            multiple = TRUE,
            options = list(
              "live-search" = TRUE,
              "actions-box" = TRUE
            )
          ),
          pickerInput(
            "criterion_min",
            "Risk criteria",
            choices = NULL,
            multiple = TRUE,
            options = list(
              "live-search" = TRUE,
              "actions-box" = TRUE
            )
          ),
          pickerInput(
            "N",
            "Select column containing number of subjects",
            choices = NULL,
            multiple = TRUE,
            options = list(
              "live-search" = TRUE,
              "actions-box" = TRUE
            )
          ),
          pickerInput(
            "variance",
            "Select columns containing variance(s)",
            choices = NULL,
            multiple = TRUE,
            options = list(
              "live-search" = TRUE,
              "actions-box" = TRUE
            )
          ),
          pickerInput(
            "binvar",
            "Select criteria which are event counts",
            choices = NULL,
            multiple = TRUE,
            options = list(
              "live-search" = TRUE,
              "actions-box" = TRUE
            )
          ),
          pickerInput(
            "contvar",
            "Select criteria with continuous scores",
            choices = NULL,
            multiple = TRUE,
            options = list(
              "live-search" = TRUE,
              "actions-box" = TRUE
            )
          ),
          pickerInput(
            "lowbetter",
            "Select criteria where lower scores are better",
            choices = NULL,
            multiple = TRUE,
            options = list(
              "live-search" = TRUE,
              "actions-box" = TRUE
            )
          ),
          pickerInput(
            "topalt",
            "Select most important criterion",
            choices = NULL
          ),
          actionButton(
            "arrange",
            "Arrange dataset"
          )
        )
  })
  
  output$weighting <- renderMenu({
    menuItem("Weights",tabName='defwts',uiOutput("setting_panel2"))
    
  })
  
  rawdata <- eventReactive(input$use, {
    if (input$source == "example") {
      read_csv("./antidepressants.csv")
    } else if (input$source == "upload") {
      read_csv(input$upload_data$datapath)
    }
  })
  
  observeEvent(input$use, {
    showModal(
      modalDialog(
        title = strong("Dataset input format"),
        "Input Dataset must contain:", 
        br(),
        "Column containing names of treatment alternatives to compare",
        br(),
        "Column(s) for each benefit and risk criterion with values equal to the sum of events (for proportions and rates) or score (for continuous endpoints) across subjects",
        br(),
        "Column(s) for each benefit and risk criterion for the number of subjects (or total follow-up time for rates) in data",
        br(),
        "Column(s) for each benefit and risk criterion with the variance of the mean",
        size = "m",
        easyClose = TRUE, 
        fade = TRUE
      )
    )
    
    updatePickerInput(
      session = session,
      inputId = "alternative",
      choices = names(rawdata())
    )
    
    updatePickerInput(
      session = session,
      inputId = "N",
      choices = names(rawdata())
    )
    
    updatePickerInput(
      session = session,
      inputId = "variance",
      choices = names(rawdata())
    )
    
    updatePickerInput(
      session = session,
      inputId = "criterion_max",
      choices = names(rawdata())
    )
    
    updatePickerInput(
      session = session,
      inputId = "criterion_min",
      choices = names(rawdata())
    )
    
    updatePickerInput(
      session = session,
      inputId = "binvar",
      choices = names(rawdata())
    )
    
    updatePickerInput(
      session = session,
      inputId = "contvar",
      choices = names(rawdata())
    )
    
    updatePickerInput(
      session = session,
      inputId = "lowbetter",
      choices = names(rawdata())
    )
    
    
    updatePickerInput(
      session = session,
      inputId = "topalt",
      choices = names(rawdata())
    )
    
  })
  
  observe({
    toggleState(id = "arrange", condition = !is.null(input$alternative) & {
      !is.null(input$criterion_max) | !is.null(input$criterion_min)
    })
  })
  
  observeEvent(input$arrange, {
    showModal(
      modalDialog(
        title = strong("Dataset is set!"),
        "Please have a look at the arranged dataset, does it look alright?", 
        br(),
        "If so, please continue to 'Range and Utility' item on the sidebar to define the range of plausible values for the criteria and the utility functions.",
        size = "m",
        easyClose = TRUE, 
        fade = TRUE
      )
    )
  })
  
  dataset <- eventReactive(input$arrange, {
    cost <- formattable::style(
      "background-color" = csscolor("darkred"),
      color = "white",
      display = "block",
      "border-radius" = "4px",
      "padding" = "0 4px"
    )
    
    benefit <- formattable::style(
      "background-color" = csscolor("seagreen"),
      color = "white",
      display = "block",
      "border-radius" = "4px",
      "padding" = "0 4px"
    )
    
    if (!is.null(input$criterion_max) & !is.null(input$criterion_min)) {
      restab1 <- rawdata() %>%
        select(one_of(input$alternative, input$criterion_max, input$criterion_min, input$N, input$variance)) %>% 
        formattable(
          list(area(col = isolate(input$criterion_max)) ~ formatter("span", style = benefit),
               area(col = isolate(input$criterion_min)) ~ formatter("span", style = cost))
        )
    } else if  (is.null(input$criterion_min)) {
      restab1 <- rawdata() %>%
        select(one_of(input$alternative, input$criterion_max, input$criterion_min, input$N)) %>% 
        formattable(
          list(area(col = isolate(input$criterion_max)) ~ formatter("span", style = benefit))
        )
    } else if (is.null(input$criterion_max)) {
      restab1 <- rawdata() %>%
        select(one_of(input$alternative, input$criterion_max, input$criterion_min, input$N)) %>% 
        formattable(
          list(area(col = isolate(input$criterion_min)) ~ formatter("span", style = cost))
        )
    }
    return(restab1)
  })
  
  
  observeEvent(input$applyran, {
    output$setting_panel1 <- renderUI({
      tagList(
        conditionalPanel(
          condition = "input.rangetyp == 'configr'",
          helpText("The configured range uses the minimum and maximum, across treatment alternatives, of 95% confidence intervals on each criterion as the least and most plausible values for this criterion. This serves as the domain for the utility functions")), 
        conditionalPanel(
          condition = "input.rangetyp == 'customr'",
          map(
            c(input$criterion_max, input$criterion_min),
            ~ numericInput(
              inputId = paste0("bestval_", .x), label = paste("Most desirable plausible value for", .x),
              min = 0, max = 100000, value=0,step = 0.000001
            )
          ),
          map(
            c(input$criterion_max, input$criterion_min),
            ~ numericInput(
              inputId = paste0("worstval_", .x), label = paste("Least desirable plausible value for", .x),
              min = 0, max = 100000, value=0,step = 0.000001
            )
          ),
          helpText("Please input plausible values")
        ),
        actionButton(
          "set",
          "Set best / worst values"
        ),
        conditionalPanel(
          condition = "input.set",
          br(), 
          helpText("The default utility function for all criteria is a linear function. To change it to a piecewise linear function (with two pieces) move the slider below to determine the change point of the function where utility value is 0.5."),
        map(
          c(input$criterion_max, input$criterion_min),
          ~ sliderInput(
            inputId = paste0("pvf1_", .x), label = paste("Pieces for piecewise linear utility function for", .x),
            min = 0, max = 1, value = 0.5, step = 0.000001
          )
        ),
        actionButton("applypvf1", "Apply")
      ),
      conditionalPanel(
        condition = "input.applypvf1",
        br(), 
        helpText("The utility function is now set to have two pieces. To change it to a four piece linear function move the sliders below to determine change point where utility value is 0.25 (left slider) and 0.75 (right slider)."),
        map(
          c(input$criterion_max, input$criterion_min),
          ~ sliderInput(
            inputId = paste0("pvf2_", .x), label = paste("Pieces for piecewise linear utility function for", .x),
            min = 0, max = 1, value = c(0,1), step=0.000001
          )
        ),
        actionButton("applypvf2", "Apply")
      )
      )
    })
  })
  
  observeEvent(input$defwts, {
    is_topval <-  function(x) {
      as.logical(x==input$topalt)
    }
    output$setting_panel2 <- renderUI({
      tagList(
        conditionalPanel(
          condition = "input.weightyp == 'weightimp'",
          helpText(paste("You've indicated that improving", input$topalt, "from it's lowest to highest plausible value is the most important (i.e. it has 100% importance). Now indicate the relative importance (in %) to this improvement of each other criterion's improvement using the sliders below.",sep=" ")), 
          map_if(
            c(input$criterion_max, input$criterion_min),is_topval,
            ~ sliderInput(
              inputId = paste0("weight_", .x), label = paste("Weight for", .x),
              min = 100,
              max = 100,
              value = c(100,100)
            ), .else = ~ sliderInput(
              inputId = paste0("weight_", .x), label = paste("Weight for", .x),
              min = 1,
              max = 100,
              value = c(1,100)
            ) )),
        actionButton("applywts", "Apply")
      )
    })
  })
  
  observeEvent(input$set, {
    allattr <- c(input$criterion_max, input$criterion_min)
    V <- dataset() %>%
      as.data.frame() %>%
      `rownames<-`(.[, input$alternative]) %>%
      select(one_of(input$variance)) %>%
      as.matrix()
    
    d <- dataset() %>%
      as.data.frame() %>%
      `rownames<-`(.[, input$alternative]) %>%
      select(one_of(input$criterion_max, input$criterion_min)) %>% as.matrix()
    
    Nvector <- dataset() %>%
      as.data.frame() %>%
      `rownames<-`(.[, input$alternative]) %>%
      select(one_of(input$N)) %>%
      as.matrix()
    
    dval = d / Nvector
    
    contvars <- as.numeric(c(input$criterion_max,input$criterion_min) %in% input$contvar)
    
    lowisgood <- as.numeric(c(input$criterion_max,input$criterion_min) %in% input$lowbetter)
    
    lowcival = as.numeric(apply(dval - 1.96*sqrt(V),2,min)) 
    highcival = as.numeric(apply(dval + 1.96*sqrt(V),2,max))
    for (i in 1:length(allattr)) {
      if(contvars[i] == 0) lowcival[i] = max(0,lowcival[i])
      if(input$rangetyp == "customr") {
        besworval <- c(input[[paste0("bestval_", allattr[i])]],input[[paste0("worstval_", allattr[i])]])
        if (besworval[1]>besworval[2]) {
          defpiece = besworval[2] + 0.5*(besworval[1]-besworval[2])
        }
        else if (besworval[1]<besworval[2]) {
          defpiece = besworval[1] + 0.5*(besworval[2]-besworval[1])
        }
        updateSliderInput(
          session = session,
          inputId = paste0("pvf1_", allattr[i]), label = paste("Pieces for piecewise linear utility function for", allattr[i]),
          min = min(besworval),
          max = max(besworval),
          value = defpiece, step = 0.000001 )
      } else {
          defpiece = lowcival[i] + 0.5*(highcival[i]-lowcival[i])
          updateSliderInput(
          session = session,
          inputId = paste0("pvf1_", allattr[i]), label = paste("Pieces for piecewise linear utility function for", allattr[i]),
          min = round(lowcival[i],4),
          max = round(highcival[i],4),
          value = defpiece, step=0.000001)
      }
    }
  })

  
  observeEvent(input$applypvf1, {
    allattr <- c(input$criterion_max, input$criterion_min)
    V <- dataset() %>%
      as.data.frame() %>%
      `rownames<-`(.[, input$alternative]) %>%
      select(one_of(input$variance)) %>%
      as.matrix()
    
    d <- dataset() %>%
      as.data.frame() %>%
      `rownames<-`(.[, input$alternative]) %>%
      select(one_of(input$criterion_max, input$criterion_min)) %>% as.matrix()
    
    Nvector <- dataset() %>%
      as.data.frame() %>%
      `rownames<-`(.[, input$alternative]) %>%
      select(one_of(input$N)) %>%
      as.matrix()
    
    dval = d / Nvector
    
    contvars <- as.numeric(c(input$criterion_max,input$criterion_min) %in% input$contvar)
    
    lowisgood <- as.numeric(c(input$criterion_max,input$criterion_min) %in% input$lowbetter)
    
    lowcival = as.numeric(apply(dval - 1.96*sqrt(V),2,min)) 
    highcival = as.numeric(apply(dval + 1.96*sqrt(V),2,max))
    for (i in 1:length(allattr)) {
      if(contvars[i] == 0) lowcival[i] = max(0,lowcival[i])
      midval <- input[[paste0("pvf1_", allattr[i])]]
      if(input$rangetyp == "customr") {
        besworval <- c(input[[paste0("bestval_", allattr[i])]],input[[paste0("worstval_", allattr[i])]])
        if (besworval[1]>besworval[2]) {
          defpiece1 = besworval[2] + 0.5*(midval-besworval[2])
          defpiece2 = midval + 0.5*(besworval[1]-midval)
        }
        else if (besworval[1]<besworval[2]) {
          defpiece1 = besworval[1] + 0.5*(midval-besworval[1])
          defpiece2 = midval + 0.5*(besworval[2]-midval)
        }
        updateSliderInput(
          session = session,
          inputId = paste0("pvf2_", allattr[i]), label = paste("Pieces for piecewise linear utility function for", allattr[i]),
          min = min(besworval),
          max = max(besworval),
          value = c(defpiece1,defpiece2), step = 0.000001)
      } else {
        defpiece1 = lowcival[i] + 0.5*(midval-lowcival[i])
        defpiece2 = midval + 0.5*(highcival[i]-midval)
        updateSliderInput(
          session = session,
          inputId = paste0("pvf2_", allattr[i]), label = paste("Pieces for piecewise linear utility function for", allattr[i]),
          min = round(lowcival[i],4),
          max = round(highcival[i],4),
          value = c(round(defpiece1,4),round(defpiece2,4)),  step=0.000001)
      }
    }
  })
  
  
  observeEvent(input$applypvf2, {
    showModal(
      modalDialog(
        title = strong("Ranges and utility functions are set!"),
        "Please have a look at the plots for utility functions.", 
        br(),
        "If you're ready, please continue to 'Weights' item on the sidebar to define the weights for each criterion.",
        size = "m",
        easyClose = TRUE, 
        fade = TRUE
      )
    )
  })
  
  
 observeEvent(input$tabs, {
   req(input$tabs)
   if (input$tabs == "intro") {
     updateTabItems(session,"tabs","intro")
   } else if (input$tabs == "mcdas") {
     updateTabItems(session,"tabs","mcdas")
   } else if (input$tabs == "pmcdas") {
     updateTabItems(session,"tabs","pmcdas")
   } else if (input$tabs == "smaas") {
      updateTabItems(session,"tabs","smaas")
   }
 })
  
  
  res0 <- eventReactive(input$applypvf2, {
    
    if(input$rangetyp == "customr") {
      maxvec <- map_dbl(
        c(input$criterion_max,input$criterion_min),
        ~ isolate(input[[paste0("bestval_", .x)]])
      )
      minvec <- map_dbl(
        c(input$criterion_max,input$criterion_min),
        ~ input[[paste0("worstval_", .x)]]
      )
      
    } else {
      allattr <- c(input$criterion_max, input$criterion_min)
      maxvec <- minvec <- rep(0,length(allattr))
      V <- dataset() %>%
        as.data.frame() %>%
        `rownames<-`(.[, input$alternative]) %>%
        select(one_of(input$variance)) %>%
        as.matrix()
      
      d <- dataset() %>%
        as.data.frame() %>%
        `rownames<-`(.[, input$alternative]) %>%
        select(one_of(input$criterion_max, input$criterion_min)) %>% as.matrix()
      
      Nvector <- dataset() %>%
        as.data.frame() %>%
        `rownames<-`(.[, input$alternative]) %>%
        select(one_of(input$N)) %>%
        as.matrix()
      
      dval = d / Nvector
      
      contvars <- as.numeric(c(input$criterion_max,input$criterion_min) %in% input$contvar)
      
      lowisgood <- as.numeric(c(input$criterion_max,input$criterion_min) %in% input$lowbetter)
      
      lowcival = as.numeric(apply(dval - 1.96*sqrt(V),2,min)) 
      highcival = as.numeric(apply(dval + 1.96*sqrt(V),2,max))
      for (i in 1:length(allattr)) {
        if(contvars[i] == 0) lowcival[i] = max(0,lowcival[i])
        if(lowisgood[i] == 1) {
          maxvec[i] = lowcival[i]
          minvec[i] = highcival[i]
        } else { 
          maxvec[i] = highcival[i]
          minvec[i] = lowcival[i]
        }
      }
    }
    
    
    
    piecelow <- map_dbl(
      c(input$criterion_max,input$criterion_min),
      ~ isolate(input[[paste0("pvf2_", .x)]][1])
    )
    
    piecemid <- map_dbl(
      c(input$criterion_max,input$criterion_min),
      ~ isolate(input[[paste0("pvf1_", .x)]])
    )
    
    piecehigh <- map_dbl(
      c(input$criterion_max,input$criterion_min),
      ~ isolate(input[[paste0("pvf2_", .x)]][2])
    )
    
    midptone <- 0.25 + as.numeric(maxvec < minvec)*0.5
    midpttwo <- 0.5
    midpttre <- 0.25 + as.numeric(minvec < maxvec)*0.5
    
    data0<-as.data.frame(rbind(cbind(maxvec,rep(1,length(maxvec)),c(input$criterion_max,input$criterion_min)),cbind(minvec,rep(0,length(minvec)),c(input$criterion_max,input$criterion_min)),cbind(piecelow,midptone,c(input$criterion_max,input$criterion_min)),cbind(piecemid,midpttwo,c(input$criterion_max,input$criterion_min)),cbind(piecehigh,midpttre,c(input$criterion_max,input$criterion_min))))
    
    res0 <- data0 %>% `colnames<-`(c("Score","Utility","criterion")) %>% mutate(criterion = as.character(criterion)) %>% mutate(Score = as.numeric(as.character(Score))) %>% mutate(Utility = as.numeric(as.character(Utility)))
    
  } )
  
  
  observeEvent(input$applywts, {
    showModal(
      modalDialog(
        title = strong("Weights are set!"),
        "Go to the analysis items on sidebar to run analysis.", 
        br(),
        "Input weights are normalized to a scale between 0-1 and applied to utilities to obtain weighted utility score on each enpoint. These are added to get Net Utility.",
        size = "m",
        easyClose = TRUE, 
        fade = TRUE
      )
    )
  })
  
  
  observe({
    toggleState(
      id = "applym",
      condition = !is.null(dataset())
    )
  })
  
  observeEvent(input$applym, {
    showModal(
      modalDialog(
        title = strong("MCDA analysis is run!"),
        "Expand the tab boxes to see plots", 
        size = "m",
        easyClose = TRUE, 
        fade = TRUE
      )
    )
  })
  
  
  res1 <- eventReactive(input$applym, {
      w <- map(
        c(input$criterion_max, input$criterion_min),
        ~ isolate(input[[paste0("weight_", .x)]])
      )
      
      c = mergeConstraints(lowerRatioConstraint(length(w),1,2,100/(w[[2]][2])),upperRatioConstraint(length(w),1,2,100/(w[[2]][1])))
      for(j in 3:length(w)) { c = mergeConstraints(c,lowerRatioConstraint(length(w),1,j,100/(w[[j]][2])),upperRatioConstraint(length(w),1,j,100/(w[[j]][1])) )}
      c = mergeConstraints(c,simplexConstraints(length(w)))
      w = hitandrun(c)
      
      w1 = colMeans(w)
      
      Nvector <- dataset() %>%
        as.data.frame() %>%
        `rownames<-`(.[, input$alternative]) %>%
        select(one_of(input$N)) %>%
        as.matrix()
      
      Variance <- dataset() %>%
        as.data.frame() %>%
        `rownames<-`(.[, input$alternative]) %>%
        select(one_of(input$variance)) %>%
        as.matrix()
      
      contvars <- as.numeric(c(input$criterion_max,input$criterion_min) %in% input$contvar) 
      
      if(input$rangetyp == "customr") {
        maxvec <- map_dbl(
          c(input$criterion_max,input$criterion_min),
          ~ isolate(input[[paste0("bestval_", .x)]])
        )
        minvec <- map_dbl(
          c(input$criterion_max,input$criterion_min),
          ~ input[[paste0("worstval_", .x)]]
        )
        
      } else {
        allattr <- c(input$criterion_max, input$criterion_min)
        maxvec <- minvec <- rep(0,length(allattr))
        
        d <- dataset() %>%
          as.data.frame() %>%
          `rownames<-`(.[, input$alternative]) %>%
          select(one_of(input$criterion_max, input$criterion_min)) %>% as.matrix()
        
        dval = d / Nvector
        
        lowisgood <- as.numeric(c(input$criterion_max,input$criterion_min) %in% input$lowbetter)
        
        lowcival = as.numeric(apply(dval - 1.96*sqrt(Variance),2,min)) 
        highcival = as.numeric(apply(dval + 1.96*sqrt(Variance),2,max))
        for (i in 1:length(allattr)) {
          if(contvars[i] == 0) lowcival[i] = max(0,lowcival[i])
          if(lowisgood[i] == 1) {
            maxvec[i] = lowcival[i]
            minvec[i] = highcival[i]
          } else { 
            maxvec[i] = highcival[i]
            minvec[i] = lowcival[i]
          }
        }
      }
      
      piecelow <- map_dbl(
        c(input$criterion_max,input$criterion_min),
        ~ isolate(input[[paste0("pvf2_", .x)]][1])
      )
      
      piecemid <- map_dbl(
        c(input$criterion_max,input$criterion_min),
        ~ isolate(input[[paste0("pvf1_", .x)]])
      )
      
      piecehigh <- map_dbl(
        c(input$criterion_max,input$criterion_min),
        ~ isolate(input[[paste0("pvf2_", .x)]][2])
      )
      
      topvar = as.numeric(c(input$criterion_max, input$criterion_min)==input$topalt)
      
      
      res1 <- dataset() %>%
        as.data.frame() %>%
        `rownames<-`(.[, input$alternative]) %>%
        select(one_of(input$criterion_max, input$criterion_min)) %>%
        as.matrix() %>%
        MCDA(Nvector=Nvector, weights = w1/sum(w1), most = maxvec,least = minvec, piece1 = piecelow, piece2 = piecemid, piece3 = piecehigh, topvar=topvar, variance = Variance)
      
    })
  
  observe({
    toggleState(
      id = "applyp",
      condition = !is.null(dataset())
    )
  })
  
  observeEvent(input$applyp, {
    showModal(
      modalDialog(
        title = strong("Probabilistic MCDA analysis is running!"),
        "Patience is a virtue, give it a minute and expand the tab boxes to see plots", 
        size = "m",
        easyClose = TRUE, 
        fade = TRUE
      )
    )
  })
  
  res2 <- eventReactive(input$applyp, {

      w <- map(
        c(input$criterion_max, input$criterion_min),
        ~ input[[paste0("weight_", .x)]]
      )
      
      c = mergeConstraints(lowerRatioConstraint(length(w),1,2,100/(w[[2]][2])),upperRatioConstraint(length(w),1,2,100/(w[[2]][1])))
      for(j in 3:length(w)) { c = mergeConstraints(c,lowerRatioConstraint(length(w),1,j,100/(w[[j]][2])),upperRatioConstraint(length(w),1,j,100/(w[[j]][1])) )}
      c = mergeConstraints(c,simplexConstraints(length(w)))
      w = hitandrun(c)
      
      w1 = colMeans(w)
      
      Nvector <- dataset() %>%
        as.data.frame() %>%
        select(one_of(input$N)) %>%
        as.matrix()
      
      Variance <- dataset() %>%
        as.data.frame() %>%
        `rownames<-`(.[, input$alternative]) %>%
        select(one_of(input$variance)) %>%
        as.matrix()
      
      contvars <- as.numeric(c(input$criterion_max,input$criterion_min) %in% input$contvar) 
      
      binvars <- as.numeric(c(input$criterion_max,input$criterion_min) %in% input$binvar)
      
      
      if(input$rangetyp == "customr") {
        maxvec <- map_dbl(
          c(input$criterion_max,input$criterion_min),
          ~ isolate(input[[paste0("bestval_", .x)]])
        )
        minvec <- map_dbl(
          c(input$criterion_max,input$criterion_min),
          ~ input[[paste0("worstval_", .x)]]
        )
        
      } else {
        allattr <- c(input$criterion_max, input$criterion_min)
        maxvec <- minvec <- rep(0,length(allattr))
        
        d <- dataset() %>%
          as.data.frame() %>%
          `rownames<-`(.[, input$alternative]) %>%
          select(one_of(input$criterion_max, input$criterion_min)) %>% as.matrix()
        
        dval = d / Nvector
        
        lowisgood <- as.numeric(c(input$criterion_max,input$criterion_min) %in% input$lowbetter)
        
        lowcival = as.numeric(apply(dval - 1.96*sqrt(Variance),2,min)) 
        highcival = as.numeric(apply(dval + 1.96*sqrt(Variance),2,max))
        for (i in 1:length(allattr)) {
          if(contvars[i] == 0) lowcival[i] = max(0,lowcival[i])
          if(lowisgood[i] == 1) {
            maxvec[i] = lowcival[i]
            minvec[i] = highcival[i]
          } else { 
            maxvec[i] = highcival[i]
            minvec[i] = lowcival[i]
          }
        }
      }
      
      piecelow <- map_dbl(
        c(input$criterion_max,input$criterion_min),
        ~ isolate(input[[paste0("pvf2_", .x)]][1])
      )
      
      piecemid <- map_dbl(
        c(input$criterion_max,input$criterion_min),
        ~ isolate(input[[paste0("pvf1_", .x)]])
      )
      
      piecehigh <- map_dbl(
        c(input$criterion_max,input$criterion_min),
        ~ isolate(input[[paste0("pvf2_", .x)]][2])
      )
      
      
      res2 <- dataset() %>%
        as.data.frame() %>%
        select(one_of(input$criterion_max, input$criterion_min)) %>%
        as.matrix() %>%
        pMCDA(Nvector=Nvector, Ntrt = length(pull(dataset()[, input$alternative])), Nendpts = length(c(input$criterion_max,input$criterion_min)), a_in = as.numeric(input$a), b_in = as.numeric(input$b), alpha_in = as.numeric(input$alpha), beta_in = as.numeric(input$beta), mu0_in = input$Mu0 , tau_in = input$Tau , weights = w1/sum(w1), most = maxvec,least = minvec, piece1 = piecelow, piece2 = piecemid, piece3 = piecehigh, contvars = contvars, binvars = binvars, variance = Variance) %>%
        as_tibble() %>%
        `colnames<-`(c(pull(dataset()[, input$alternative])))
  })
      
  
  observe({
    toggleState(
      id = "applysm",
      condition = !is.null(dataset())
    )
  })
  
  
  observeEvent(input$applysm, {
    showModal(
      modalDialog(
        title = strong("Stochastic Multi Criteria analysis is running!"),
        "Patience is a virtue, give it a minute and expand the tab boxes to see plots", 
        size = "m",
        easyClose = TRUE, 
        fade = TRUE
      )
    )
  })
  
  
  res3 <- eventReactive(input$applysm, {
    w <- map(
      c(input$criterion_max, input$criterion_min),
      ~ input[[paste0("weight_", .x)]]
    )
    
    
    Nvector <- dataset() %>%
      as.data.frame() %>%
      select(one_of(input$N)) %>%
      as.matrix()
    
    Variance <- dataset() %>%
      as.data.frame() %>%
      `rownames<-`(.[, input$alternative]) %>%
      select(one_of(input$variance)) %>%
      as.matrix()
    
    contvars <- as.numeric(c(input$criterion_max,input$criterion_min) %in% input$contvar) 
    
    binvars <- as.numeric(c(input$criterion_max,input$criterion_min) %in% input$binvar)
    
    if(input$rangetyp == "customr") {
      maxvec <- map_dbl(
        c(input$criterion_max,input$criterion_min),
        ~ isolate(input[[paste0("bestval_", .x)]])
      )
      minvec <- map_dbl(
        c(input$criterion_max,input$criterion_min),
        ~ input[[paste0("worstval_", .x)]]
      )
      
    } else {
      allattr <- c(input$criterion_max, input$criterion_min)
      maxvec <- minvec <- rep(0,length(allattr))
      
      d <- dataset() %>%
        as.data.frame() %>%
        `rownames<-`(.[, input$alternative]) %>%
        select(one_of(input$criterion_max, input$criterion_min)) %>% as.matrix()
      
      dval = d / Nvector
      
      lowisgood <- as.numeric(c(input$criterion_max,input$criterion_min) %in% input$lowbetter)
      
      lowcival = as.numeric(apply(dval - 1.96*sqrt(Variance),2,min)) 
      highcival = as.numeric(apply(dval + 1.96*sqrt(Variance),2,max))
      for (i in 1:length(allattr)) {
        if(contvars[i] == 0) lowcival[i] = max(0,lowcival[i])
        if(lowisgood[i] == 1) {
          maxvec[i] = lowcival[i]
          minvec[i] = highcival[i]
        } else { 
          maxvec[i] = highcival[i]
          minvec[i] = lowcival[i]
        }
      }
    }
    piecelow <- map_dbl(
      c(input$criterion_max,input$criterion_min),
      ~ isolate(input[[paste0("pvf2_", .x)]][1])
    )
    
    piecemid <- map_dbl(
      c(input$criterion_max,input$criterion_min),
      ~ isolate(input[[paste0("pvf1_", .x)]])
    )
    
    piecehigh <- map_dbl(
      c(input$criterion_max,input$criterion_min),
      ~ isolate(input[[paste0("pvf2_", .x)]][2])
    )
    
    res3 <- dataset() %>%
      as.data.frame() %>%
      select(one_of(input$criterion_max, input$criterion_min)) %>%
      as.matrix() %>%
      SMAA(Nvector=Nvector, Ntrt = length(pull(dataset()[, input$alternative])), Nendpts = length(c(input$criterion_max,input$criterion_min)), a_in = as.numeric(input$a), b_in = as.numeric(input$b), alpha_in = as.numeric(input$alpha), beta_in = as.numeric(input$beta), mu0_in = input$Mu0 , tau_in = input$Tau , most = maxvec,least = minvec, piece1 = piecelow, piece2 = piecemid, piece3 = piecehigh, contvars = contvars, binvars = binvars, variance = Variance, weights = w) %>%
      as_tibble() %>%
      `colnames<-`(c(pull(dataset()[, input$alternative])))
    })
  
  output$introtext<- renderPrint({
    cat(paste0("Hi. This is a Shiny application for quantitative benefit-risk analysis. To see a pictorial representation of the methods available here click on the tabs above. For a detailed overview, check out this website - mcda.drugis.org.

Begin by uplodaing the data and defining relevant fields using the 'Data Source' Item on the sidebar. Once uploaded you can view the Data on the 'Arranged Data' tab.", collapse = " "))
  })
  
  output$tab_dataset <- DT::renderDataTable({
    df1 <- dataset() %>% select(one_of(input$alternative,c(input$criterion_max,input$criterion_min)[1],input$N[1],input$variance[1]))
    for (i in 2:length(input$N)) {
      df1 <- bind_cols(df1, dataset() %>% select(one_of(c(input$criterion_max,input$criterion_min)[i],input$N[i],input$variance[i])) )
    }
    df1 %>% as.datatable(
        rownames = FALSE,
        caption = "Columns with green colour define criteria which are benefits, whereas columns with red colour define criteria which are risks.",
        style = "bootstrap",
        extensions = c("Scroller", "Buttons"),
        options = list(
          dom = "Brt",
          autoWidth = FALSE,
          scrollX = TRUE,
          deferRender = TRUE,
          scrollY = 300,
          scroller = TRUE,
          buttons =
            list(
              list(
                extend = "copy"
              ),
              list(
                extend = "collection",
                buttons = c("csv", "excel"),
                text = "Download"
              )
            )
        )
      )
  }#,
  #server = FALSE
  )
  
  output$tab_plot00 <- renderPlotly({
    plotdata = res0()
    p00<-ggplot(data=plotdata, aes(x=Score, y=Utility, group=criterion)) + geom_line() + geom_point() + facet_wrap(. ~ criterion, scales="free_x") + labs(title = "Utility functions", x = "Score on criterion Scale", y = "Utility") 
    ggplotly(p00)
  }
  )
  
  output$mcda1 <- renderImage({
    filename <- normalizePath(file.path("./images", paste('mcda0','.jpg', sep='')))
    list(src = filename,
         alt = paste("MCDA Primer"))
    
  }, deleteFile = FALSE)
  
  output$tab_plot0 <- renderPlotly({
    #refdata = res() %>% select(c(pull(dataset()[, input$alternative]))[1]) %>% as.numeric()
    #req(res$res1)
    plotdatalist = res1()
    plotdata = plotdatalist$data1 %>% as_tibble() %>% mutate_all(funs((. - c(plotdatalist$data1[,1])))) %>% mutate(criterion = c(input$criterion_max, input$criterion_min)) %>% reshape2::melt(id.var = c('criterion'), variable.name = 'Alternative')
    p0<-ggplot(data=plotdata, aes(x=criterion, y=value, fill=value < 0)) + scale_fill_manual(values = c("green", "red")) + geom_col(width=0.5) + facet_grid(. ~ Alternative) + coord_flip() + geom_vline(aes(xintercept=0, color="grey"), linetype="dashed") + labs(title = "Difference in Utilitys", x = "criterion", y = "Weighted Difference in Utility") + theme(legend.position = "none")
    ggplotly(p0)
  }
  )
  
  output$tab_plot01 <- renderPlotly({
    plotdatalist = res1()
    plotdata = plotdatalist$data1 %>% as_tibble() %>% mutate(criterion = c(input$criterion_max, input$criterion_min)) %>% reshape2::melt(id.var = c('criterion'), variable.name = 'Alternative') 
    p01<-ggplot(data=plotdata, aes(x=criterion, y=value)) + geom_col(width=0.5) +  facet_grid(. ~ Alternative) + labs(title = "Utility by Alternative", x = "criterion", y = "Utility") + theme(legend.position = "none") + coord_flip()
    ggplotly(p01)
  }
  )
  
  output$tab_plot02 <- renderPlotly({
    plotdatalist = res1()
    plotdata1 = plotdatalist$data2 %>% as_tibble() %>% `colnames<-`(c(pull(dataset()[, input$alternative]))) %>% mutate(Weight = c(seq(0.1,1,by=0.1))) %>% reshape2::melt(id.var = c('Weight'), variable.name = 'Alternative')
    plotdata2 = plotdatalist$data3 %>% as_tibble() %>% `colnames<-`(c(pull(dataset()[, input$alternative]))) %>% mutate(Weight = c(seq(0.1,1,by=0.1))) %>% reshape2::melt(id.var = c('Weight'), variable.name = 'Alternative') %>% rename(Variance = value) %>% left_join(plotdata1, by = c("Weight", "Alternative")) %>% mutate(Upper = value + 1.96*Variance) %>% mutate(Lower = value - 1.96*Variance)
    p02<-ggplot(data=plotdata2, aes(x=Weight, y=value, color=Alternative)) + geom_point() + geom_line() 
    p02<- p02 + geom_ribbon(aes(ymin=plotdata2$Lower, ymax=plotdata2$Upper), linetype=2, alpha=0.1) + labs(title = "Utility vs Weight for most important criterion", x = "Weight", y = "Net Utility")
    ggplotly(p02)
  }
  )
  
  output$pmcda1 <- renderImage({
    filename <- normalizePath(file.path("./images", paste('pmcda','.jpg', sep='')))
    list(src = filename,
         alt = paste("Probabilistic MCDA Primer"))
    
  }, deleteFile = FALSE)
  
  output$tab_plot <- renderPlotly({
    plotdata = res2() %>% stack() %>% `colnames<-`(c("Utility", "Alternative"))
    p<-ggplot(data=plotdata, aes(x=Utility, fill=Alternative)) + geom_density(alpha=0.4) + labs(title = "Distribution of Benefit-Risk Score", x = "Overall Benefit- Risk Score", y = "Density", fill = "Alternative")
    ggplotly(p)
  }
  )
  
  
  output$tab_plot2 <- renderPlotly({
    plotdata = res2() %>% as.matrix() %>% rowRanks() %>% as.tibble() %>% `colnames<-`(c(pull(dataset()[, input$alternative]))) %>% stack() %>% `colnames<-`(c("Rank", "Alternative")) %>% mutate(Rank = length(c(pull(dataset()[, input$alternative]))) - Rank + 1) %>% mutate(Rank = as.character(Rank)) %>% group_by(Rank, Alternative) %>% tally() %>% mutate(Rank.Probability = n/100000)
    p2<-ggplot(data=plotdata) + geom_bar(aes(y = Rank.Probability, x = Alternative, fill = Rank), stat="identity", width = 0.5) + ggtitle("Probability of Ranking") 
    ggplotly(p2)
  }
  )
  
  
  output$tab_plot3 <- renderPlotly({
    plotdata = res2() %>% mutate_all(funs((. - c(pull(res2()[,1]))))) %>% stack() %>% `colnames<-`(c("Difference", "Alternative"))
    p3<-ggplot(data=plotdata, aes(x=Difference)) + geom_density(alpha=0.4) + facet_grid(Alternative ~ .) + geom_vline(aes(xintercept=0, color="grey"), linetype="dashed") + labs(title = "Distribution of difference in Benefit-Risk Score", x = "Overall Benefit- Risk Score Difference", y = "Density") + theme(legend.position = "none")
    ggplotly(p3)
  }
  )
  
  output$smaa1 <- renderImage({
    filename <- normalizePath(file.path("./images", paste('smaa1','.jpg', sep='')))
    list(src = filename,
         alt = paste("SMAA Primer"))
    
  }, deleteFile = FALSE)
  
  output$tab_plot4 <- renderPlotly({
    plotdata = res3() %>% stack() %>% `colnames<-`(c("Utility", "Alternative"))
    p<-ggplot(data=plotdata, aes(x=Utility, fill=Alternative)) + geom_density(alpha=0.4) + labs(title = "Distribution of Benefit-Risk Score", x = "Overall Benefit- Risk Score", y = "Density", fill = "Alternative")
    ggplotly(p)
  }
  )
  
  
  output$tab_plot5 <- renderPlotly({
    plotdata = res3() %>% as.matrix() %>% rowRanks() %>% as.tibble() %>% `colnames<-`(c(pull(dataset()[, input$alternative]))) %>% stack() %>% `colnames<-`(c("Rank", "Alternative")) %>% mutate(Rank = length(c(pull(dataset()[, input$alternative]))) - Rank + 1) %>% mutate(Rank = as.character(Rank)) %>% group_by(Rank, Alternative) %>% tally() %>% mutate(Rank.Probability = n/100000)
    p2<-ggplot(data=plotdata) + geom_bar(aes(y = Rank.Probability, x = Alternative, fill = Rank), stat="identity", width = 0.5) + ggtitle("Probability of Ranking") 
    ggplotly(p2)
  }
  )
  
  
  output$tab_plot6 <- renderPlotly({
    plotdata = res3() %>% mutate_all(funs((. - c(pull(res3()[,1]))))) %>% stack() %>% `colnames<-`(c("Difference", "Alternative"))
    p3<-ggplot(data=plotdata, aes(x=Difference)) + geom_density(alpha=0.4) + facet_grid(Alternative ~ .) + geom_vline(aes(xintercept=0, color="grey"), linetype="dashed") + labs(title = "Distribution of difference in Benefit-Risk Score", x = "Overall Benefit- Risk Score Difference", y = "Density") + theme(legend.position = "none")
    ggplotly(p3)
  }
  )
  
  output$report_br<- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report_br.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report_br.Rmd")
      file.copy("report_br.Rmd", tempReport, overwrite = TRUE)
      
      w <- map(
        c(input$criterion_max, input$criterion_min),
        ~ isolate(input[[paste0("weight_", .x)]])
      )
      
      c = mergeConstraints(lowerRatioConstraint(length(w),1,2,100/(w[[2]][2])),upperRatioConstraint(length(w),1,2,100/(w[[2]][1])))
      for(j in 3:length(w)) { c = mergeConstraints(c,lowerRatioConstraint(length(w),1,j,100/(w[[j]][2])),upperRatioConstraint(length(w),1,j,100/(w[[j]][1])) )}
      c = mergeConstraints(c,simplexConstraints(length(w)))
      w = hitandrun(c)
      
      w1 = round(colMeans(w),3)
      
      # Set up parameters to pass to Rmd document
      params <- list(
        datasetd = dataset(),
        criterion_maxd = input$criterion_max,
        criterion_mind = input$criterion_min,
        Nd = input$N,
        varianced = input$variance,
        alternatived = input$alternative,
        res0d = res0(),
        res1d = res1(),
        res2d = res2(),
        res3d = res3(),
        weightsd = w1
      )
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
}

shinyApp(ui = ui, server = server)
