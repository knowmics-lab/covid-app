library(shiny)
library(shinyjs)
library(shinycssloaders)
library(DT)
library(ggplot2)
library(RColorBrewer)
library(ggrepel)
library(ggpubr)

source("AppFunctions.R")

fluidPage(
  theme = bslib::bs_theme(bootswatch = "cerulean"),
  tags$style(HTML(".checkbox-inline {margin-right: 20px}")),
  tabsetPanel(
      tabPanel("Temporal rate analysis",
          shinyjs::useShinyjs(),
          fluidRow(
            column(2,selectizeInput("countryRate","Country", choices=NULL)),
            column(2,selectizeInput("regionRate","Region/State", choices=NULL)),
            column(2,selectizeInput("mutationRate","Mutations", choices=NULL, multiple = T))
          ),
          fluidRow(
            column(2,downloadButton("downRatePlot","Download temporal plot (.png)",style = 'margin-bottom:16px')),
            column(2,downloadButton("downRateData","Download temporal plot data (.csv)",style = 'margin-bottom:16px')),
            column(2,downloadButton("downCladePlot","Download pie plot (.png)",style = 'margin-bottom:16px')),
            column(2,downloadButton("downCladeData","Download pie plot data (.csv)",style = 'margin-bottom:16px'))
          ),
          fluidRow(
            column(12,plotOutput("plotRate",height="700px") %>% withSpinner())
          ),
          fluidRow(
            column(12,plotOutput("plotClade",height="700px") %>% withSpinner())
          )
      ),
      tabPanel("Correlation analysis",
          fluidRow(
            column(2,selectizeInput("countryCorr","Country", choices=NULL)),
            column(2,selectizeInput("regionCorr","Region/State", choices=NULL)),
            column(2,selectizeInput("cladeCorr","Clade", choices=NULL)),
            column(2,selectizeInput("mutationCorr","Filter by mutations", choices=NULL, multiple = T, options = list(maxItems = 2)))
            ),
          fluidRow(
            column(6,downloadButton("downCorrTab", "Download correlation table (.csv)",style = 'margin-bottom:16px')),
            column(2,downloadButton("downCorrPlot", "Download correlation plot (.png)",style = 'margin-bottom:16px')),
            column(4,downloadButton("downCorrPlotData", "Download correlation plot data (.csv)",style = 'margin-bottom:16px'))
            ),
          fluidRow(
            column(6,DTOutput("tableCorr") %>% withSpinner()),
            column(6,
              fluidRow(column(12,style = "text-align: center;",
                              checkboxGroupInput("plotCorrOpt", "", c("Use clades","Show regression line"),
                                                    selected=c("Show regression line") ,inline=T))),
              fluidRow(column(12,plotOutput("plotCorr",height="700px") %>% withSpinner())))
          )
      )
  )
)
