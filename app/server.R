function(input, output, session) {
  
  #thematic::thematic_shiny()
  
  observe(
    if(length(input$mutations)>0) {
      shinyjs::show('external')
      shinyjs::show('runSearch')
    } else {
      shinyjs::hide('external')
      shinyjs::hide('runSearch')
      shinyjs::hide('netmeSource')
      shinyjs::hide('netmePapers')
    }
  )
  
  observeEvent(input$external,{
    if(input$external=="Pubmed") {
      shinyjs::hide('netmeSource')
      shinyjs::hide('netmePapers')
    } else {
      shinyjs::show('netmeSource')
      shinyjs::show('netmePapers')
    }
  })
  
  observeEvent(input$runSearch,{
    query <- unlist(strsplit(input$mutations,":"))
    query <- query[seq(2,length(query),2)]
    search.type <- "full-text"
    if(input$netmeSource=="Abstracts")
      search.type <- "abstract"
    if(input$external=="Pubmed") {
      browseURL(paste0("https://pubmed.ncbi.nlm.nih.gov/?term=",paste0(query,collapse="+")))
    } else {
      req <- request("http://netme.click:8092/send_data")
      req <- req_body_json(req,list(queryMode="pubmed",input=paste0(query,collapse=" "),networkName=paste0(paste0(query,collapse=" ")," network"),searchOn="terms",
                                       searchType=search.type,papersNumber=input$netmePapers,sortType="relevance"))
      resp <- req_perform(req)
      browseURL(paste0("https://netme.click/#/results/",resp_body_json(resp)$query_id))
    }
  })
  
  updateSelectizeInput(session,"country", choices=unique(metadata$Country), server=T)

  observeEvent(input$country, {
    list.regions <- as.character(metadata[metadata$Country==input$country,"Region"])
    updateSelectizeInput(session, "region", choices=unique(c("All",list.regions)), server=TRUE)
  })

  mutation.rates <- eventReactive(list(input$country,input$region),{
    if(any(metadata$Country==input$country & metadata$Region==input$region))
      readRDS(paste0("Data/Rates/",input$country,"_",input$region,".rds"))
  },ignoreInit = T)

  clade.corr <- eventReactive(list(input$country,input$region),{
    if(any(metadata$Country==input$country & metadata$Region==input$region))
      readRDS(paste0("Data/Correlations/",input$country,"_",input$region,".rds"))
  }, ignoreInit = T)

  observeEvent(list(input$country,input$region),{
    freezeReactiveValue(input,"mutations")
    if(any(metadata$Country==input$country & metadata$Region==input$region))
      updateSelectizeInput(session,"mutations",choices=mutation.rates()$Mutation,server=T)
  })
  
  output$plotRate <- renderUI({
    if(length(input$mutations)>0) {
      shinyjs::show('downRatePlot')
      shinyjs::show('downRateData')
    } else {
      shinyjs::hide('downRatePlot')
      shinyjs::hide('downRateData')
    }
    if(length(input$mutations)>0) {
      plot.rate <- make.temporal.plot(mutation.rates(),input$mutations)
      plot.rate <- ggplotly(plot.rate, tooltip = "text", height=600) %>% config(displayModeBar = FALSE) %>%
        layout(hoverlabel = list(font=list(size=17)))
      box(width=12, height=600, solidHeader = T, renderPlotly(plot.rate))
    }
    })

  output$plotClade <- renderUI({
    if(length(input$mutations)>0) {
      shinyjs::show('downCladePlot')
      shinyjs::show('downCladeData')
    } else {
      shinyjs::hide('downCladePlot')
      shinyjs::hide('downCladeData')
    }
    if(length(input$mutations)>0) {
      clade.plots <- make.clade.interactive.plot(mutation.rates(),input$mutations)
      lapply(input$mutations, function(mut) {
        box(width=4, solidHeader=T, renderPlotly(clade.plots[[mut]]))
      })
    }
  })

  output$tableCorr <- DT::renderDT({
    corr.table <- clade.corr()[clade.corr()$`Mutation 1` %in% input$mutations | clade.corr()$`Mutation 2` %in% input$mutations,]
    corr.table <- corr.table[,1:6]
    corr.table <- datatable(corr.table, selection="single", rownames=F, filter="top", options = list(
      dom = "tp", pageLength = 25,
      rowCallback = JS(
        "function(row, data) {",
        "$('td:eq(4)', row).html(data[4].toExponential(2));",
        "$('td:eq(5)', row).html(data[5].toExponential(2));",
        "}")
    )
    )
    if(length(input$mutations)>0) {
      if(nrow(corr.table$x$data)>0) {
        shinyjs::show('downCorrTab')
      } else {
        shinyjs::hide('downCorrTab')
      }
      return(corr.table)
    } else {
      shinyjs::hide('downCorrTab')
      return(NULL)
    }
  })

  output$plotCorr <- renderUI({
    if(length(input$mutations)>0)
    {
      row <- input$tableCorr_rows_selected
      if(!is.null(row)) {
        shinyjs::show('downCorrPlot')
        shinyjs::show('downCorrPlotData')
        shinyjs::show('plotCorrOpt')
      } else {
        shinyjs::hide('downCorrPlot')
        shinyjs::hide('downCorrPlotData')
        shinyjs::hide('plotCorrOpt')
      }
      if(!is.null(row)) {
        vals <- (clade.corr()[clade.corr()$`Mutation 1` %in% input$mutations | clade.corr()$`Mutation 2` %in% input$mutations,])[row,c("Mutation 1","Mutation 2")]
        temp.rates <- mutation.rates()[mutation.rates()$Mutation==vals[1,1] | mutation.rates()$Mutation==vals[1,2],,drop=F]
        plot.corr <- make.correlation.interactive.plot(temp.rates,input$plotCorrOpt)
        box(width=12, height=600, solidHeader = T, renderPlotly(plot.corr))
      }
    } else {
      shinyjs::hide('downCorrPlot')
      shinyjs::hide('downCorrPlotData')
      shinyjs::hide('plotCorrOpt')
    }
  })
  
  output$plotHeatmap <- renderUI({
    if(length(input$mutations)>0)
    {
      row <- input$tableCorr_rows_selected
      if(!is.null(row)) {
        vals <- (clade.corr()[clade.corr()$`Mutation 1` %in% input$mutations | clade.corr()$`Mutation 2` %in% input$mutations,])[row,c("Mutation 1","Mutation 2","Cont00","Cont01","Cont10","Cont11")]
        heatmap.plot <- make.heatmap.plot(vals[1,1],vals[1,2],as.numeric(vals[1,-c(1,2)]))
        box(width=7, height=400, solidHeader = T, renderPlotly(heatmap.plot))
      }
    }
  })

  output$downCorrTab <- downloadHandler(
    filename = function() {
      paste0(input$country,"_",input$region,"_",paste0(input$mutations,collapse="_"),"_signCorr.csv")
    },
    content = function(file) {
      write_csv(clade.corr()[clade.corr()$`Mutation 1` %in% input$mutations | clade.corr()$`Mutation 2` %in% input$mutations,], file)
    },
    contentType = "text/csv"
  )

  output$downRatePlot <- downloadHandler(
    filename = function() {
      paste0(input$country,"_",input$region,"_",paste0(input$mutations,collapse="_"),"_temporalPlot.png")
    },
    content = function(file) {
      ggsave(file,make.temporal.plot(mutation.rates(),input$mutations),width=4500,height=3000,units="px")
    },
    contentType = "text/png"
  )

  output$downRateData <- downloadHandler(
    filename = function() {
      paste0(input$country,"_",input$region,"_",paste0(input$mutations,collapse="_"),"_temporalRates.csv")
    },
    content = function(file) {
      sub.mutation.rates <- mutation.rates()[mutation.rates()$Mutation %in% input$mutations,!grepl("Rate",names(mutation.rates()))]
      write_csv(sub.mutation.rates,file)
    },
    contentType = "text/csv"
  )

  output$downCladePlot <- downloadHandler(
    filename = function() {
      paste0(input$country,"_",input$region,"_",paste0(input$mutations,collapse="_"),"_cladePie.png")
    },
    content = function(file) {
      ggsave(file,make.clade.plot(mutation.rates(),input$mutations),width=6000,height=3000,units="px",bg = "white")
    },
    contentType = "text/png"
  )

  output$downCladeData <- downloadHandler(
    filename = function() {
      paste0(input$country,"_",input$region,"_",paste0(input$mutations,collapse="_"),"_cladeRates.csv")
    },
    content = function(file) {
      sub.mutation.rates <- mutation.rates()[mutation.rates()$Mutation %in% input$mutations,grepl("Rate|Mutation",names(mutation.rates()))]
      write_csv(sub.mutation.rates,file)
    },
    contentType = "text/csv"
  )

  output$downCorrPlot <- downloadHandler(
    filename = function() {
      row <- input$tableCorr_rows_selected
      vals <- (clade.corr()[clade.corr()$`Mutation 1` %in% input$mutations | clade.corr()$`Mutation 2` %in% input$mutations,])[row,c("Mutation 1","Mutation 2")]
      paste0(vals[1,1],"_",vals[1,2],"_correlationPlot.png")
    },
    content = function(file) {
      row <- input$tableCorr_rows_selected
      vals <- (clade.corr()[clade.corr()$`Mutation 1` %in% input$mutations | clade.corr()$`Mutation 2` %in% input$mutations,])[row,c("Mutation 1","Mutation 2")]
      temp.rates <- mutation.rates()[mutation.rates()$Mutation==vals[1,1] | mutation.rates()$Mutation==vals[1,2],,drop=F]
      ggsave(file,make.correlation.plot(temp.rates,input$plotCorrOpt),width=4500,height=3000,units="px")
    },
    contentType = "text/png"
  )

  output$downCorrPlotData <- downloadHandler(
    filename = function() {
      row <- input$tableCorr_rows_selected
      vals <- (clade.corr()[clade.corr()$`Mutation 1` %in% input$mutations | clade.corr()$`Mutation 2` %in% input$mutations,])[row,c("Mutation 1","Mutation 2")]
      paste0(vals[1,1],"_",vals[1,2],"_correlationPlotData.csv")
    },
    content = function(file) {
      row <- input$tableCorr_rows_selected
      vals <- (clade.corr()[clade.corr()$`Mutation 1` %in% input$mutations | clade.corr()$`Mutation 2` %in% input$mutations,])[row,c("Mutation 1","Mutation 2")]
      temp.rates <- mutation.rates()[mutation.rates()$Mutation==vals[1,1] | mutation.rates()$Mutation==vals[1,2],,drop=F]
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

