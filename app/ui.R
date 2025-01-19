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
library(rintrojs)

source("AppFunctions.R")

header <- dashboardHeader(title = span("CovidTGI", style = "color: white; font-size: 24px"))

sidebar <- dashboardSidebar(
  introjsUI(),
  sidebarMenu(
    introBox(
      pickerInput("country","Country",choices=NULL, options = list(size=10, style="picker")),
      pickerInput("region","Region/State", choices=NULL, options = list(size=10, style = "picker")),
      data.step = 1,
      data.intro = "Select the country and region/US state in which to analyze Sars-Cov-2 data"
    ),
    div(style = "margin-top: 25px;"),
    introBox(
      htmlOutput("statsCountry", class="htmlStats"),
      data.step = 2,
      data.intro = "This is the number of samples collected in our data and the number of infected reported for the selected geographical area."
    ),
    actionButton("help","How to use",style = "margin-top:25px; margin-left:15px;")
  )
)

body <- dashboardBody(
  useShinyjs(),
  fluidRow(
    tabBox(
      width=12,
      id = "appInfo",
      selected = "cladeTab",
      tabPanel("CLADE INFO", value="cladeTab",
               fluidRow(
                 introBox(
                   box(width=12, title="Clade prevalence", status="primary",solidHeader = T,collapsible = T,
                     introBox(
                       sliderTextInput("rangePrevalence",label = NULL,choices=colnames(global.clade.prevalences)[-1],
                                     selected=colnames(global.clade.prevalences)[c(2,ncol(global.clade.prevalences))]),
                       data.step = 4,
                       data.intro = "Use this slider to set the time interval in which to show rates."
                     ),
                     downloadButton("downCladePrevalencePlot","Download plot (.png)"),
                     downloadButton("downCladePrevalenceData","Download data (.xlsx)"),
                     uiOutput("plotCladePrevalences") %>% withSpinner()
                   ),
                   data.step = 3,
                   data.intro = "Frequency rate of clades in the selected geographical area."
                 ),
               ),
               fluidRow(
                 introBox(
                   box(width=12, title="Clade observed mutation frequencies", status="primary",solidHeader = T,collapsible = T,
                     fluidRow(column(width=4,
                      introBox(
                        pickerInput("clades","Select one or more clades: ", choices=global.clade.prevalences$Clade, multiple = T, 
                                 options = list(`live-search`=TRUE, size=10, style = "picker")),
                        data.step = 6,
                        data.intro = "Use this picker input to select the clades to show."
                      )
                     ),
                     column(width=6,
                     introBox(
                       hidden(pickerInput(width="40%","topkMutations","Top frequent mutations to show: ",c(10,15,20,25,30,35,40,45,50),
                                  options = list(size=20, style = "picker"))),
                       data.step = 7,
                       data.intro = "Use this picker input to select the maximum number of most frequent mutations to show."
                     )
                     )),
                     introBox(
                       hidden(sliderTextInput("rangeCladeMutationRates",label = NULL,choices=colnames(global.clade.prevalences)[-1],
                                     selected=colnames(global.clade.prevalences)[c(2,ncol(global.clade.prevalences))])),
                       data.step = 8,
                       data.intro = "Use this slider to set the time interval in which to show the rates."
                     ),
                     hidden(downloadButton("downCladeMutRatesPlot","Download plot (.png)")),
                     hidden(downloadButton("downCladeMutRatesData","Download data (.xlsx)")),
                     uiOutput("plotCladeMutationRates") %>% withSpinner()
                   ),
                   data.step = 5,
                   data.intro = "Most frequent mutations found in one or more clades for the selected geographical area."
                 ),
               ),
               fluidRow(
                 introBox(
                   box(width=12, title="Related information", status="primary",solidHeader = T,collapsible = T,
                     hidden(radioButtons("externalClade","Search on:",c("NetMe","Pubmed"))),
                     hidden(selectInput("netmeSourceClade","Literature source",c("Full-text articles","Abstracts"))),
                     hidden(selectInput("netmePapersClade","Papers to use",c("10","20","50","100"))),
                     uiOutput("extLinkClade")),
                   data.step = 9,
                   data.intro = "Search for related information about selected clades on NetMe tool or Pubmed database."
                 )
               )
            ),
      tabPanel("MUTATION INFO", value="mutationTab",
               fluidRow(
                introBox(
                  box(width=12, title="Mutations", status="primary",solidHeader = T,collapsible = T,
                     pickerInput(width="50%","mutations","Select one or more mutations:", choices=NULL, multiple = T, 
                                 options = list(`live-search`=TRUE, size=10, style = "picker"))),
                     # pickerInput(width="50%","mutations","Select one or more mutations:", choices=global.mutation.rates$Mutation, multiple = T, 
                     #             selected = c("E:S55F","E:T11A"), options = list(`live-search`=TRUE, size=10, style = "picker"))),
                  data.step = 10,
                  data.intro = "Select one or more mutations observed in samples of the selected geographic area."
                )
               ),
               fluidRow(
                introBox(
                  box(width=12,title="Temporal rates",status="primary",solidHeader = T,collapsible = T,
                    hidden(downloadButton("downRatePlot","Download plot (.png)")),
                    hidden(downloadButton("downRateData","Download data (.xlsx)")),
                    uiOutput("plotRate") %>% withSpinner()),
                  data.step = 11,
                  data.intro = "Monthly rates of the chosen mutations in the selected geographical area."
                )
               ),
               fluidRow(
                 introBox(
                   box(width=12,title="Clade distribution",status="primary",solidHeader = T,collapsible = T,
                     # hidden(pickerInput(width="40%","topkCladeDistr","Top frequent clades to show: ",c(10,15,20,25,30,35,40,45,50),
                     #              options = list(size=20, style = "picker"))),
                     hidden(downloadButton("downCladeDistributionPlot","Download plot (.png)")),
                     hidden(downloadButton("downCladeDistributionData","Download data (.xlsx)")),
                     uiOutput("plotCladeDistribution") %>% withSpinner()),
                   data.step = 12,
                   data.intro = "Percentage of samples within each clade having the chosen mutations in the selected geographical area."
                 )
               ),
               fluidRow(
                 introBox(
                   box(width=12,title="Clade-specific rates",status="primary",solidHeader = T,collapsible = T,
                     # hidden(pickerInput(width="40%","topkCladeRates","Top frequent clades to show: ",c(10,15,20,25,30,35,40,45,50),
                     #              options = list(size=20, style = "picker"))),
                     hidden(downloadButton("downCladeRatesPlot","Download plot (.png)")),
                     hidden(downloadButton("downCladeRatesData","Download data (.xlsx)")),
                     uiOutput("plotCladeRates") %>% withSpinner()),
                   data.step = 13,
                   data.intro = "Frequency rates of chosen mutations within each clade in the selected geographical area."
                 )
               ),
               fluidRow(
                 introBox(
                   box(width=12,title="Correlations",status="primary",solidHeader = T,collapsible = T,
                     box(solidHeader=T, width=5, hidden(downloadButton("downCorrTab", "Download table (.xlsx)")),
                         introBox(
                           DTOutput("tableCorr") %>% withSpinner(),
                           data.step = 15,
                           data.intro = "Click on a row of the table to select a pair of correlated mutations."
                         )
                     ),
                     box(solidHeader=T, width=7, hidden(downloadButton("downCorrPlot", "Download plot (.png)")),
                         hidden(downloadButton("downCorrPlotData", "Download plot data (.xlsx)")),
                         introBox(
                           hidden(checkboxGroupInput("plotCorrOpt", "", c("Use clades","Show regression line"),selected=c("Show regression line"),inline=T)),
                           data.step = 17,
                           data.intro = "Select 'Use clades' to show scatter plot of clade-specific rates of selected mutations and 'Show regression line' to show the regression line of the scatter plot."
                         ),
                         introBox(
                           uiOutput("plotCorr") %>% withSpinner(),
                           data.step = 16,
                           data.intro = "Scatter plot of monthly rates of selected mutations."
                         ),
                         introBox(
                           uiOutput("plotHeatmap") %>% withSpinner(),
                           data.step = 18,
                           data.intro = "Number of samples in which the correlated mutations are jointly present or absent for the clade in which the correlation has been observed."
                         )
                     )
                   ),
                   data.step = 14,
                   data.intro = "List of all significant pairwise global or clade-specific rate correlations involving the chosen mutations in the selected geographical area."
                 )
               ),
               fluidRow(
                 introBox(
                   box(width=12, title="Related information", status="primary",solidHeader = T,collapsible = T,
                     hidden(radioButtons("external","Search on:",c("NetMe","Pubmed"))),
                     hidden(selectInput("netmeSource","Literature source",c("Full-text articles","Abstracts"))),
                     hidden(selectInput("netmePapers","Papers to use",c("10","20","50","100"))),
                     uiOutput("extLink")),
                   data.step = 19,
                   data.intro = "Search for related information about selected mutations on NetMe tool or Pubmed database."
                 )
               )
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
    tags$head(tags$style(HTML(".nav-tabs {font-size: 16px; font-weight: bold;}"))),
    tags$head(tags$style(HTML("htmlStats {margin-top: 100px;}")))
  )
)

dashboardPage(header,sidebar,body,title = "CovidTGI")
