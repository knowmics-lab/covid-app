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

source("AppFunctions.R")

header <- dashboardHeader(title = span("CovidTGIApp", style = "color: white; font-size: 25px"))

sidebar <- dashboardSidebar(
  sidebarMenu(
    pickerInput("country","Country",choices=NULL, options = list(size=10, style="picker")),
    pickerInput("region","Region/State", choices=NULL, options = list(size=10, style = "picker")),
    pickerInput("mutations","Mutations", choices=NULL, multiple = T, options = list(`live-search`=TRUE, size=10, style = "picker")),
    hidden(radioButtons("external","Search on:",c("NetMe","Pubmed"))),
    hidden(selectInput("netmeSource","Literature source",c("Full-text articles","Abstracts"))),
    hidden(selectInput("netmePapers","Papers to use",c("10","20","50","100"))),
    uiOutput("extLink")
  )
)

body <- dashboardBody(
  useShinyjs(),
  fluidRow(
    box(width=12,title="Mutation rates",status="primary",solidHeader = T,collapsible = T,
            hidden(downloadButton("downRatePlot","Download plot (.png)")),
            hidden(downloadButton("downRateData","Download data (.csv)")),
            uiOutput("plotRate") %>% withSpinner()
    ),
    box(width=12,title="Clade frequencies",status="primary",solidHeader = T,collapsible = T,
        hidden(downloadButton("downCladePlot","Download plot (.png)")),
        hidden(downloadButton("downCladeData","Download data (.csv)")),
        uiOutput("plotClade") %>% withSpinner()
    ),
    box(width=12,title="Correlations",status="primary",solidHeader = T,collapsible = T,
        box(solidHeader=T, width=5, hidden(downloadButton("downCorrTab", "Download table (.csv)")),
            DTOutput("tableCorr") %>% withSpinner()),
        box(solidHeader=T, width=7, hidden(downloadButton("downCorrPlot", "Download plot (.png)")),
            hidden(downloadButton("downCorrPlotData", "Download plot data (.csv)")),
            hidden(checkboxGroupInput("plotCorrOpt", "", c("Use clades","Show regression line"),selected=c("Show regression line"),inline=T)),
            uiOutput("plotCorr") %>% withSpinner(),
            uiOutput("plotHeatmap") %>% withSpinner())
    ),
    tags$head(tags$style(sass(sass_file("www/bootswatch-lumen.scss")))),
    tags$head(tags$style(HTML('.box{-webkit-box-shadow: none; -moz-box-shadow: none;box-shadow: none; font-size: 18px;}'))),
    tags$head(tags$style(HTML(".main-sidebar { font-size: 20px;}"))),
    tags$head(tags$style(HTML(".selectize-input { font-size: 18px; line-height: 20px;} .selectize-dropdown { font-size: 18px; line-height: 20px; }"))),
    tags$head(tags$style(HTML(".dropdown-menu ul li:nth-child(n) a { color: black !important; font-size: 18px;}"))),
    tags$head(tags$style(HTML(".picker {font-size: 16px; color: black;}")))
  )
)

dashboardPage(header,sidebar,body,title = "CovidTGIApp")
