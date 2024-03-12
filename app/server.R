library(shiny)
library(shinyjs)
library(shinycssloaders)
library(DT)
library(ggplot2)
library(RColorBrewer)
library(ggrepel)
library(ggpubr)

source("AppFunctions.R")

function(input, output, session) {
  
  updateSelectizeInput(session,"countryRate",
                       choices=unique(c("All",metadata$Country)), 
                       server=T)
  
  updateSelectizeInput(session,"countryCorr",
                       choices=unique(c("All",metadata$Country)), 
                       server=T)
  
  observeEvent(input$countryRate, {
    list.regions <- metadata[metadata$Country==input$countryRate,"Region"]
    updateSelectizeInput(session, "regionRate",
                         choices=unique(c("All",list.regions)), server=TRUE)
  })
  
  observeEvent(input$countryCorr, {
    list.regions <- metadata[metadata$Country==input$countryCorr,"Region"]
    updateSelectizeInput(session, "regionCorr",
                         choices=unique(c("All",list.regions)), server=TRUE)
  })
  
  mutation.rates <- reactive({
    if(input$countryRate!="" && input$regionRate!="")
      readRDS(paste0("Data/Rates/",input$countryRate,"_",input$regionRate,".rds"))
  })
  
  clade.corr <- reactive({
    if(input$countryCorr!="" && input$regionCorr!="" && input$cladeCorr!="")
      readRDS(paste0("Data/Correlations/",input$countryCorr,"_",input$regionCorr,"_",input$cladeCorr,".rds"))
  })
  
  observeEvent(input$countryRate,{
    if(input$countryRate!="" && input$regionRate!="")
      updateSelectizeInput(session,"mutationRate",choices=mutation.rates()$Mutation,server=T)
  })
  
  observeEvent(input$regionRate,{
    if(input$countryRate!="" && input$regionRate!="")
      updateSelectizeInput(session,"mutationRate",choices=mutation.rates()$Mutation,server=T)
  })
  
  observeEvent(input$countryCorr,{
    if(input$countryCorr!="" && input$regionCorr!="")
    {
      list.clades <- metadata[metadata$Country==input$countryCorr & metadata$Region==input$regionCorr,"Clade"]
      updateSelectizeInput(session,"cladeCorr",choices=unique(c("All",list.clades)),server=T)
    }
  })
  
  observeEvent(input$regionCorr,{
    if(input$countryCorr!="" && input$regionCorr!="")
    {
      list.clades <- metadata[metadata$Country==input$countryCorr & metadata$Region==input$regionCorr,"Clade"]
      updateSelectizeInput(session,"cladeCorr",choices=unique(c("All",list.clades)),server=T)
    }
  })
  
  observeEvent(input$cladeCorr, {
    if(input$cladeCorr!="")
      updateSelectizeInput(session,"mutationCorr",selected="",choices=sort(unique(c(clade.corr()$`Mutation 1`,clade.corr()$`Mutation 2`))), server=TRUE)
  })
  
  output$plotRate <- renderPlot({
    if(length(input$mutationRate)>0) {
      shinyjs::enable('downRatePlot')
      shinyjs::enable('downRateData')
      make.temporal.plot(mutation.rates(),input$mutationRate)
    } else {
      shinyjs::disable('downRatePlot')
      shinyjs::disable('downRateData')
    }}, res = 96)
  
  output$plotClade <- renderPlot({
    if(length(input$mutationRate)>0) {
      shinyjs::enable('downCladePlot')
      shinyjs::enable('downCladeData')
      make.clade.plot(mutation.rates(),input$mutationRate)
    } else {
      shinyjs::disable('downCladePlot')
      shinyjs::disable('downCladeData')
    }}, height=function(){
      if(length(input$mutationRate)>0)
        500*ceiling(length(input$mutationRate)/3)
      else
        "auto"
      }, res = 96)

  output$tableCorr <- DT::renderDT({
    if(length(input$mutationCorr)==0)
      head(clade.corr(),100000)
    else if(length(input$mutationCorr)==1)
      clade.corr()[clade.corr()$`Mutation 1` %in% input$mutationCorr | clade.corr()$`Mutation 2` %in% input$mutationCorr,]
    else
      clade.corr()[(clade.corr()$`Mutation 1` %in% input$mutationCorr & clade.corr()$`Mutation 2` %in% input$mutationCorr) |
                     (clade.corr()$`Mutation 2` %in% input$mutationCorr & clade.corr()$`Mutation 1` %in% input$mutationCorr),]
    }, options = list(dom = "ltp", pageLength = 20), rownames=F, selection="single")

  output$plotCorr <- renderPlot({
    row <- input$tableCorr_rows_selected
    if(!is.null(row))
    {
      shinyjs::enable('downCorrPlot')
      shinyjs::enable('downCorrPlotData')
      shinyjs::enable('plotCorrOpt')
      if(length(input$mutationCorr)==0)
        vals <- clade.corr()[row,c(1,2)]
      else if(length(input$mutationCorr)==1)
        vals <- (clade.corr()[clade.corr()$`Mutation 1` %in% input$mutationCorr | clade.corr()$`Mutation 2` %in% input$mutationCorr,])[row,c(1,2)]
      else
        vals <- (clade.corr()[(clade.corr()$`Mutation 1` %in% input$mutationCorr & clade.corr()$`Mutation 2` %in% input$mutationCorr) |
                              (clade.corr()$`Mutation 2` %in% input$mutationCorr & clade.corr()$`Mutation 1` %in% input$mutationCorr),])[row,c(1,2)]
      temp.rates <- readRDS(paste0("Data/Rates/",input$countryCorr,"_",input$regionCorr,".rds"))
      temp.rates <- temp.rates[temp.rates$Mutation==vals[1,1] | temp.rates$Mutation==vals[1,2],]
      make.correlation.plot(temp.rates,input$plotCorrOpt)
    } else {
      shinyjs::disable('downCorrPlot')
      shinyjs::disable('downCorrPlotData')
      shinyjs::disable('plotCorrOpt')
    }
  })
  
  output$downCorrTab <- downloadHandler(
    filename = function() {
      if(length(input$mutationCorr)==0)
        paste0(input$countryCorr,"_",input$regionCorr,"_",input$cladeCorr,"_signCorr.csv")
      else
        paste0(input$countryCorr,"_",input$regionCorr,"_",input$cladeCorr,"_",paste0(input$mutationCorr,collapse="_"),"_signCorr.csv")
    },
    content = function(file) {
      if(length(input$mutationCorr)==0)
        write_csv(clade.corr(), file)
      else if(length(input$mutationCorr)==1)
        write_csv(clade.corr()[clade.corr()$`Mutation 1` %in% input$mutationCorr | clade.corr()$`Mutation 2` %in% input$mutationCorr,], file)
      else
        write_csv(clade.corr()[(clade.corr()$`Mutation 1` %in% input$mutationCorr & clade.corr()$`Mutation 2` %in% input$mutationCorr) |
                               (clade.corr()$`Mutation 2` %in% input$mutationCorr & clade.corr()$`Mutation 1` %in% input$mutationCorr),], file)
    },
    contentType = "text/csv"
  )

  output$downRatePlot <- downloadHandler(
    filename = function() {
      paste0(input$countryRate,"_",input$regionRate,"_",paste0(input$mutationRate,collapse="_"),"_temporalPlot.png")
    },
    content = function(file) {
      ggsave(file,make.temporal.plot(mutation.rates(),input$mutationRate),width=4500,height=3000,units="px")
    },
    contentType = "text/png"
  )

  output$downRateData <- downloadHandler(
    filename = function() {
      paste0(input$countryRate,"_",input$regionRate,"_",paste0(input$mutationRate,collapse="_"),"_temporalRates.csv")
    },
    content = function(file) {
      sub.mutation.rates <- mutation.rates()[mutation.rates()$Mutation %in% input$mutationRate,!grepl("Rate",names(mutation.rates()))]
      write_csv(sub.mutation.rates,file)
    },
    contentType = "text/csv"
  )

  output$downCladePlot <- downloadHandler(
    filename = function() {
      paste0(input$countryRate,"_",input$regionRate,"_",paste0(input$mutationRate,collapse="_"),"_cladePie.png")
    },
    content = function(file) {
      ggsave(file,make.clade.plot(mutation.rates(),input$mutationRate),width=6000,height=3000,units="px",bg = "white")
    },
    contentType = "text/png"
  )

  output$downCladeData <- downloadHandler(
    filename = function() {
      paste0(input$countryRate,"_",input$regionRate,"_",paste0(input$mutationRate,collapse="_"),"_cladeRates.csv")
    },
    content = function(file) {
      sub.mutation.rates <- mutation.rates()[mutation.rates()$Mutation %in% input$mutationRate,grepl("Rate|Mutation",names(mutation.rates()))]
      write_csv(sub.mutation.rates,file)
    },
    contentType = "text/csv"
  )

  output$downCorrPlot <- downloadHandler(
    filename = function() {
      row <- input$tableCorr_rows_selected
      if(length(input$mutationCorr)==0)
        vals <- clade.corr()[row,c(1,2)]
      else if(length(input$mutationCorr)==1)
        vals <- (clade.corr()[clade.corr()$`Mutation 1` %in% input$mutationCorr | clade.corr()$`Mutation 2` %in% input$mutationCorr,])[row,c(1,2)]
      else
        vals <- (clade.corr()[(clade.corr()$`Mutation 1` %in% input$mutationCorr & clade.corr()$`Mutation 2` %in% input$mutationCorr) |
                                (clade.corr()$`Mutation 2` %in% input$mutationCorr & clade.corr()$`Mutation 1` %in% input$mutationCorr),])[row,c(1,2)]
      paste0(vals[1,1],"_",vals[1,2],"_correlationPlot.png")
    },
    content = function(file) {
      row <- input$tableCorr_rows_selected
      if(length(input$mutationCorr)==0)
        vals <- clade.corr()[row,c(1,2)]
      else if(length(input$mutationCorr)==1)
        vals <- (clade.corr()[clade.corr()$`Mutation 1` %in% input$mutationCorr | clade.corr()$`Mutation 2` %in% input$mutationCorr,])[row,c(1,2)]
      else
        vals <- (clade.corr()[(clade.corr()$`Mutation 1` %in% input$mutationCorr & clade.corr()$`Mutation 2` %in% input$mutationCorr) |
                              (clade.corr()$`Mutation 2` %in% input$mutationCorr & clade.corr()$`Mutation 1` %in% input$mutationCorr),])[row,c(1,2)]
      temp.rates <- readRDS(paste0("Data/Rates/",input$countryCorr,"_",input$regionCorr,".rds"))
      temp.rates <- temp.rates[temp.rates$Mutation==vals[1,1] | temp.rates$Mutation==vals[1,2],]
      ggsave(file,make.correlation.plot(temp.rates,input$plotCorrOpt),width=4500,height=3000,units="px")
    },
    contentType = "text/png"
  )

  output$downCorrPlotData <- downloadHandler(
    filename = function() {
      row <- input$tableCorr_rows_selected
      if(length(input$mutationCorr)==0)
        vals <- clade.corr()[row,c(1,2)]
      else if(length(input$mutationCorr)==1)
        vals <- (clade.corr()[clade.corr()$`Mutation 1` %in% input$mutationCorr | clade.corr()$`Mutation 2` %in% input$mutationCorr,])[row,c(1,2)]
      else
        vals <- (clade.corr()[(clade.corr()$`Mutation 1` %in% input$mutationCorr & clade.corr()$`Mutation 2` %in% input$mutationCorr) |
                                (clade.corr()$`Mutation 2` %in% input$mutationCorr & clade.corr()$`Mutation 1` %in% input$mutationCorr),])[row,c(1,2)]
      paste0(vals[1,1],"_",vals[1,2],"_correlationPlotData.csv")
    },
    content = function(file) {
      row <- input$tableCorr_rows_selected
      if(length(input$mutationCorr)==0)
        vals <- clade.corr()[row,c(1,2)]
      else if(length(input$mutationCorr)==1)
        vals <- (clade.corr()[clade.corr()$`Mutation 1` %in% input$mutationCorr | clade.corr()$`Mutation 2` %in% input$mutationCorr,])[row,c(1,2)]
      else
        vals <- (clade.corr()[(clade.corr()$`Mutation 1` %in% input$mutationCorr & clade.corr()$`Mutation 2` %in% input$mutationCorr) |
                                (clade.corr()$`Mutation 2` %in% input$mutationCorr & clade.corr()$`Mutation 1` %in% input$mutationCorr),])[row,c(1,2)]
      temp.rates <- readRDS(paste0("Data/Rates/",input$countryCorr,"_",input$regionCorr,".rds"))
      temp.rates <- temp.rates[temp.rates$Mutation==vals[1,1] | temp.rates$Mutation==vals[1,2],]
      if("Use clades" %in% input$plotCorrOpt)
      {
        temp.rates <- temp.rates[,grepl("_relRate|Mutation",names(temp.rates))]
        names(temp.rates) <- gsub("_relRate","",names(temp.rates))
      }
      else
        temp.rates <- temp.rates[,!grepl("Rate",names(temp.rates))]
      write_csv(temp.rates,file)
    },
    contentType = "text/csv"
  )
  
}
