library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinyjs)
library(shinycssloaders)
library(DT)
library(ggplot2)
library(RColorBrewer)
library(ggrepel)
library(ggpubr)
library(plotly)
library(readr)
library(sass)
library(httr2)
library(openxlsx)
library(dplyr)
library(tidyr)

source("AppFunctions.R")

header <- dashboardHeader(title = span("CovidTGI", style = "color: white; font-size: 24px"))

sidebar <- dashboardSidebar(
  sidebarMenu(
    pickerInput("country","Country",choices=NULL, options = list(size=10, style="picker")),
    pickerInput("region","Region/State", choices=NULL, options = list(size=10, style = "picker"))
  )
)

body <- dashboardBody(
  useShinyjs(),
  fluidRow(
    tabBox(
      width=12,
      id = "appInfo",
      tabPanel("CLADE INFO", 
               fluidRow(
                 box(width=12, title="Clade prevalence", status="primary",solidHeader = T,collapsible = T,
                     sliderTextInput("rangePrevalence",label = NULL,choices=colnames(global.clade.prevalences)[-1],
                                     selected=colnames(global.clade.prevalences)[c(2,ncol(global.clade.prevalences))]),
                     downloadButton("downCladePrevalencePlot","Download plot (.png)"),
                     downloadButton("downCladePrevalenceData","Download data (.xlsx)"),
                     uiOutput("plotCladePrevalences") %>% withSpinner()
                 )),
               fluidRow(
                 box(width=12, title="Clade mutation rates", status="primary",solidHeader = T,collapsible = T,
                     fluidRow(column(width=4,
                      pickerInput("clades","Select one or more clades: ", choices=global.clade.prevalences$Clade, multiple = T, 
                                 options = list(`live-search`=TRUE, size=10, style = "picker"))
                     ),
                     column(width=6,
                      hidden(pickerInput(width="40%","topkMutations","Top frequent mutations to show: ",c(10,15,20,25,30,35,40,45,50),
                                  options = list(size=20, style = "picker")))
                     )),
                     hidden(sliderTextInput("rangeCladeMutationRates",label = NULL,choices=colnames(global.clade.prevalences)[-1],
                                     selected=colnames(global.clade.prevalences)[c(2,ncol(global.clade.prevalences))])),
                     hidden(downloadButton("downCladeMutRatesPlot","Download plot (.png)")),
                     hidden(downloadButton("downCladeMutRatesData","Download data (.xlsx)")),
                     uiOutput("plotCladeMutationRates") %>% withSpinner()
                )),
               fluidRow(
                 box(width=12, title="Related information", status="primary",solidHeader = T,collapsible = T,
                     hidden(radioButtons("externalClade","Search on:",c("NetMe","Pubmed"))),
                     hidden(selectInput("netmeSourceClade","Literature source",c("Full-text articles","Abstracts"))),
                     hidden(selectInput("netmePapersClade","Papers to use",c("10","20","50","100"))),
                     uiOutput("extLinkClade")
               ))
               ),
      tabPanel("MUTATION INFO",
               fluidRow(
                box(width=12, title="Mutations", status="primary",solidHeader = T,collapsible = T,
                     pickerInput(width="50%","mutations","Select one or more mutations:", choices=NULL, multiple = T, options = list(`live-search`=TRUE, size=10, style = "picker"))
                )),
               fluidRow(
                box(width=12,title="Temporal rates",status="primary",solidHeader = T,collapsible = T,
                    hidden(downloadButton("downRatePlot","Download plot (.png)")),
                    hidden(downloadButton("downRateData","Download data (.xlsx)")),
                    uiOutput("plotRate") %>% withSpinner()
                )),
               fluidRow(
                 box(width=12,title="Clade distribution",status="primary",solidHeader = T,collapsible = T,
                    # hidden(pickerInput(width="40%","topkCladeDistr","Top frequent clades to show: ",c(10,15,20,25,30,35,40,45,50),
                    #              options = list(size=20, style = "picker"))),
                    hidden(downloadButton("downCladeDistributionPlot","Download plot (.png)")),
                    hidden(downloadButton("downCladeDistributionData","Download data (.xlsx)")),
                    uiOutput("plotCladeDistribution") %>% withSpinner()
               )),
               fluidRow(
                 box(width=12,title="Clade-specific rates",status="primary",solidHeader = T,collapsible = T,
                    # hidden(pickerInput(width="40%","topkCladeRates","Top frequent clades to show: ",c(10,15,20,25,30,35,40,45,50),
                    #              options = list(size=20, style = "picker"))),
                    hidden(downloadButton("downCladeRatesPlot","Download plot (.png)")),
                    hidden(downloadButton("downCladeRatesData","Download data (.xlsx)")),
                    uiOutput("plotCladeRates") %>% withSpinner()
                 )),
               fluidRow(
                 box(width=12,title="Correlations",status="primary",solidHeader = T,collapsible = T,
                     box(solidHeader=T, width=5, hidden(downloadButton("downCorrTab", "Download table (.xlsx)")),
                         DTOutput("tableCorr") %>% withSpinner()),
                     box(solidHeader=T, width=7, hidden(downloadButton("downCorrPlot", "Download plot (.png)")),
                         hidden(downloadButton("downCorrPlotData", "Download plot data (.xlsx)")),
                         hidden(checkboxGroupInput("plotCorrOpt", "", c("Use clades","Show regression line"),selected=c("Show regression line"),inline=T)),
                         uiOutput("plotCorr") %>% withSpinner(),
                         uiOutput("plotHeatmap") %>% withSpinner())
               )),
               fluidRow(
                 box(width=12, title="Related information", status="primary",solidHeader = T,collapsible = T,
                     hidden(radioButtons("external","Search on:",c("NetMe","Pubmed"))),
                     hidden(selectInput("netmeSource","Literature source",c("Full-text articles","Abstracts"))),
                     hidden(selectInput("netmePapers","Papers to use",c("10","20","50","100"))),
                     uiOutput("extLink")
               ))
      ),
    ),
  ),
  fluidRow(
    tags$head(tags$style(sass(sass_file("www/bootswatch-lumen.scss")))),
    tags$head(tags$style(HTML('.box {-webkit-box-shadow: none; -moz-box-shadow: none;box-shadow: none; font-size: 16px;}'))),
    tags$head(tags$style(HTML(".main-sidebar { font-size: 18px;}"))),
    tags$head(tags$style(HTML(".selectize-input { font-size: 16px; line-height: 18px;} .selectize-dropdown { font-size: 16px; line-height: 18px; }"))),
    tags$head(tags$style(HTML(".dropdown-menu ul li:nth-child(n) a { color: black !important; font-size: 16px;}"))),
    tags$head(tags$style(HTML(".picker {font-size: 16px; color: black; background-color: #EEEEEE;}"))),
    tags$head(tags$style(HTML(".nav-tabs {font-size: 16px; font-weight: bold;}")))
  )
)

dashboardPage(header,sidebar,body,title = "CovidTGI")
