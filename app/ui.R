library(shiny)
library(shinyjs)
library(tidyverse)
library(psych)
library(DT)
library(ggplot2)
library(ggpubr)
library(ggrepel)
library(radiant.data)
library(scales)
library(data.table)
library(shinycssloaders)

source("AppFunctions.R")

fluidPage(
  theme = bslib::bs_theme(bootswatch = "cerulean"),
  tags$style(HTML(".checkbox-inline {margin-right: 20px}")),
  tabsetPanel(
    tabPanel(
      "Temporal analysis",
      shinyjs::useShinyjs(),
      fluidRow(
        column(
          2, 
          selectizeInput(
            "mutationTemp", "Select one or more mutations", 
            choices = NULL, multiple = TRUE
          )
        ),
        column(2, downloadButton("downTempPlot", "Download temporal plot (.png)", style = "margin-top:32px")),
        column(3, downloadButton("downTempData", "Download temporal plot data (.csv)", style = "margin-top:32px")),
        column(2, downloadButton("downCladePlot", "Download pie plot (.png)", style = "margin-top:32px")),
        column(3, downloadButton("downCladeData", "Download pie plot data (.csv)", style = "margin-top:32px"))
      ),
      fluidRow(
        column(6, plotOutput("plotTemp", height = "700px") %>% withSpinner()),
        column(6, plotOutput("plotClade", height = "350px") %>% withSpinner())
      )
    ),
    tabPanel(
      "Correlation analysis",
      fluidRow(
        column(2, selectInput("cladeCorr", "Select a clade",
          choices = c("All", as.character(sort(unique(mutation.data$clade)))), selected = "All"
        )),
        column(2, selectizeInput("mutationCorr", "Filter by mutations", choices = NULL, multiple = T, options = list(maxItems = 2))),
        column(3, downloadButton("downCorrTab", "Download correlation table (.csv)", style = "margin-top:32px")),
        column(2, downloadButton("downCorrPlot", "Download correlation plot (.png)", style = "margin-top:32px")),
        column(3, downloadButton("downCorrPlotData", "Download correlation plot data (.csv)", style = "margin-top:32px"))
      ),
      fluidRow(
        column(6, DTOutput("tableCorr") %>% withSpinner()),
        column(
          6,
          fluidRow(column(12,
            style = "text-align: center;",
            checkboxGroupInput("plotCorrOpt", "", c("Use clades", "Show regression line"),
              selected = c("Show regression line"), inline = T
            )
          )),
          fluidRow(
            column(
              12, 
              plotOutput("plotCorr", height = "700px") %>% withSpinner()
            )
          )
        )
      )
    )
  )
)
