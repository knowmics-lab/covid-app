function(input, output, session) {
  
  #thematic::thematic_shiny()
  
  observeEvent(input$help,{
    updatePickerInput(session,"country", choices=unique(metadata$Country),
                                 choicesOpt = list(content = c("All",sapply(unique(metadata$Country)[-1],build.country.icon))))
    updatePickerInput(session,"region", choices=unique(c("All")))
    updatePickerInput(session,"mutations",choices=global.mutation.rates$Mutation, selected=global.mutation.rates$Mutation[c(1,2)])
    updatePickerInput(session,"clades",choices=global.clade.prevalences$Clade, selected=global.clade.prevalences$Clade[c(1,2)])
    introjs(session, options = list("nextLabel"="Next", "prevLabel"="Previous", "skipLabel"="Close"),
                     events = list(onbeforechange = I("
        if (this._currentStep < 9) {
          $('a[data-value=\"mutationTab\"]').removeClass('active');
          $('a[data-value=\"cladeTab\"]').addClass('active');
          $('a[data-value=\"cladeTab\"]').trigger('click');
        } else {
          $('a[data-value=\"cladeTab\"]').removeClass('active');
          $('a[data-value=\"mutationTab\"]').addClass('active');
          $('a[data-value=\"mutationTab\"]').trigger('click');
        }
        ")))
  }
  )
  
  observe({
    if(length(input$mutations)>0) {
      shinyjs::show('external')
      if(input$external=="NetMe") {
        shinyjs::show('netmeSource')
        shinyjs::show('netmePapers')
      }
    } else {
      shinyjs::hide('external')
      shinyjs::hide('netmeSource')
      shinyjs::hide('netmePapers')
    }
  })
  
  observe({
    if(length(input$clades)>0) {
      shinyjs::show('externalClade')
      if(input$externalClade=="NetMe") {
        shinyjs::show('netmeSourceClade')
        shinyjs::show('netmePapersClade')
      }
    } else {
      shinyjs::hide('externalClade')
      shinyjs::hide('netmeSourceClade')
      shinyjs::hide('netmePapersClade')
    }
  })
  
  observeEvent(input$external,{
    if(length(input$mutations)>0) {
    if(input$external=="Pubmed") {
      shinyjs::hide('netmeSource')
      shinyjs::hide('netmePapers')
    } else {
      shinyjs::show('netmeSource')
      shinyjs::show('netmePapers')
    }
    }
  })
  
  observeEvent(input$externalClade,{
    if(length(input$clades)>0) {
      if(input$externalClade=="Pubmed") {
        shinyjs::hide('netmeSourceClade')
        shinyjs::hide('netmePapersClade')
      } else {
        shinyjs::show('netmeSourceClade')
        shinyjs::show('netmePapersClade')
      }
    }
  })
  
  observeEvent(
    eventExpr = {
      input$mutations
      input$external
    },
    handlerExpr = {
    if(length(input$mutations)>0)
    {
      query <- unlist(strsplit(input$mutations,":"))
      query <- c("sars-cov-2",query[seq(2,length(query),2)])
      search.type <- "full-text"
      if(input$netmeSource=="Abstracts")
        search.type <- "abstract"
      if(input$external=="Pubmed") {
        link <- paste0("https://pubmed.ncbi.nlm.nih.gov/?term=",paste0(query,collapse="+"))
      } else {
        req <- request("http://netme.click:8092/send_data")
        req <- req_body_json(req,list(queryMode="pubmed",input=paste0(query,collapse=" "),networkName=paste0(paste0(query,collapse=" ")," network"),searchOn="terms",
                                       searchType=search.type,papersNumber=input$netmePapers,sortType="relevance"))
        resp <- req_perform(req)
        link <- paste0("https://netme.click/#/results/",resp_body_json(resp)$query_id)
      }
      url <- a(p("Run search", class = "btn btn-default action-button" , style = "fontweight:600; margin-left:20px;"), 
               target = "_blank", href = link)
      output$extLink <- renderUI({ tagList(url) })
    } else {
      output$extLink <- renderUI("")
    }
  })
  
  observeEvent(
    eventExpr = {
      input$clades
      input$externalClade
    },
    handlerExpr = {
      if(length(input$clades)>0)
      {
        query <- c("sars-cov-2",input$clades)
        search.type <- "full-text"
        if(input$netmeSourceClade=="Abstracts")
          search.type <- "abstract"
        if(input$externalClade=="Pubmed") {
          link <- paste0("https://pubmed.ncbi.nlm.nih.gov/?term=",paste0(query,collapse="+"))
        } else {
          req <- request("http://netme.click:8092/send_data")
          req <- req_body_json(req,list(queryMode="pubmed",input=paste0(query,collapse=" "),networkName=paste0(paste0(query,collapse=" ")," network"),searchOn="terms",
                                        searchType=search.type,papersNumber=input$netmePapersClade,sortType="relevance"))
          resp <- req_perform(req)
          link <- paste0("https://netme.click/#/results/",resp_body_json(resp)$query_id)
        }
        url <- a(p("Run search", class = "btn btn-default action-button" , style = "fontweight:600; margin-left:20px;"), 
                 target = "_blank", href = link)
        output$extLinkClade <- renderUI({ tagList(url) })
      } else {
        output$extLinkClade <- renderUI("")
      }
  })
  
  updatePickerInput(session, "country", choices=unique(metadata$Country),
                    choicesOpt = list(content = c("All",sapply(unique(metadata$Country)[-1],build.country.icon))))
  
  observeEvent(input$country, {
    if(input$country=="All") {
      statText <- "&nbsp &nbsp GLOBAL DATA<br/>"
    } else {
      statText <- paste0("&nbsp &nbsp ",toupper(input$country),"<br/>")
    }
    statText <- paste0(statText,"&nbsp &nbsp Samples: ")
    if(!is.null(clade.prevalences())) {
      if(ncol(clade.prevalences())==1) {
        statText <- paste0(statText,"not available<br/>")
      }
      else {
        statText <- paste0(statText,format(sum(clade.prevalences()[,-1],na.rm = T),big.mark=","),"<br/>")
      }
    } else {
      statText <- paste0(statText,format(sum(global.clade.prevalences[,-1],na.rm = T),big.mark=","),"<br/>")
    }
    statText <- paste0(statText,"&nbsp &nbsp Infected: ")
    if(!is.null(clade.prevalences())) {
      if(paste0(input$country,"_",input$region) %in% colnames(population.data)) {
        statText <- paste0(statText,format(sum(population.data[,paste0(input$country,"_",input$region)],na.rm = T),big.mark=","))
      } else {
        statText <- paste0(statText,"not available")
      }
    } else {
      statText <- paste0(statText,format(sum(population.data[,"All_All"],na.rm = T),big.mark=","))
    }
    output$statsCountry <- renderUI({HTML(statText)})
    list.regions <- as.character(metadata[metadata$Country==input$country,"Region"])
    updatePickerInput(session,"region", choices=unique(c("All",list.regions)))
    updatePickerInput(session,"mutations",choices=mutation.rates()$Mutation,selected = input$mutations)
    updatePickerInput(session,"clades",choices=clade.prevalences()$Clade,selected = input$clades)
    if(!is.null(clade.prevalences()))
    {
      if(ncol(clade.prevalences())<3) {
        shinyjs::hide('rangePrevalence')
        shinyjs::hide('rangeCladeMutationRates')
      } else {
        shinyjs::show('rangePrevalence')
        shinyjs::show('rangeCladeMutationRates')
        updateSliderTextInput(session,"rangePrevalence",choices=colnames(clade.prevalences())[-1],
                          selected=colnames(clade.prevalences())[c(2,ncol(clade.prevalences()))])
        updateSliderTextInput(session,"rangeCladeMutationRates",choices=colnames(clade.prevalences())[-1],
                          selected=colnames(clade.prevalences())[c(2,ncol(clade.prevalences()))])
      }
    }
  })

  observeEvent(input$region,{
    if(input$country=="All") {
      statText <- "&nbsp &nbsp GLOBAL DATA<br/>"
    } else {
      statText <- paste0("&nbsp &nbsp ",toupper(paste0(input$country," - ",input$region)),"<br/>")
    }
    statText <- paste0(statText,"&nbsp &nbsp Samples: ")
    if(!is.null(clade.prevalences())) {
      if(ncol(clade.prevalences())==1) {
        statText <- paste0(statText,"not available<br/>")
      }
      else {
        statText <- paste0(statText,format(sum(clade.prevalences()[,-1],na.rm = T),big.mark=","),"<br/>")
      }
    } else {
      statText <- paste0(statText,format(sum(global.clade.prevalences[,-1],na.rm = T),big.mark=","),"<br/>")
    }
    statText <- paste0(statText,"&nbsp &nbsp Infected: ")
    if(!is.null(clade.prevalences())) {
      if(paste0(input$country,"_",input$region) %in% colnames(population.data)) {
        statText <- paste0(statText,format(sum(population.data[,paste0(input$country,"_",input$region)],na.rm = T),big.mark=","))
      } else {
        statText <- paste0(statText,"not available")
      }
    } else {
      statText <- paste0(statText,format(sum(population.data[,"All_All"],na.rm = T),big.mark=","))
    }
    output$statsCountry <- renderUI({HTML(statText)})
    updatePickerInput(session,"clades",choices=clade.prevalences()$Clade,selected=input$clades)
    updatePickerInput(session,"mutations",choices=mutation.rates()$Mutation,selected=input$mutations)
    updateSliderTextInput(session,"rangePrevalence",choices=colnames(clade.prevalences())[-1],
                          selected=colnames(clade.prevalences())[c(2,ncol(clade.prevalences()))])
    updateSliderTextInput(session,"rangeCladeMutationRates",choices=colnames(clade.prevalences())[-1],
                          selected=colnames(clade.prevalences())[c(2,ncol(clade.prevalences()))])
  })

  observeEvent(input$mutations, {
    updatePickerInput(session,"mutations",choices = unique(c(input$mutations,mutation.rates()$Mutation)),
                      selected = input$mutations)
  })
  
  clade.prevalences <- eventReactive(list(input$country,input$region),{
    if(any(metadata$Country==input$country & metadata$Region==input$region)) {
      if(input$country=="All" && input$region=="All") {
        global.clade.prevalences
      } else {
        readRDS(paste0("Data/CladePrevalences/",input$country,"_",input$region,".rds"))
      }
    }
  })
  
  clade.mutation.rates <- eventReactive(list(input$country,input$region),{
    if(any(metadata$Country==input$country & metadata$Region==input$region)) {
      if(input$country=="All" && input$region=="All") {
        global.clade.mutation.rates
      } else {
        readRDS(paste0("Data/CladeMutationRates/",input$country,"_",input$region,".rds"))
      }
    }
  })
  
  mutation.rates <- eventReactive(list(input$country,input$region),{
    if(any(metadata$Country==input$country & metadata$Region==input$region)) {
      if(input$country=="All" && input$region=="All") {
        global.mutation.rates
      } else {
        readRDS(paste0("Data/Rates/",input$country,"_",input$region,".rds"))
      }
    }
  })

  clade.corr <- eventReactive(list(input$country,input$region),{
    if(any(metadata$Country==input$country & metadata$Region==input$region)) {
      if(input$country=="All" && input$region=="All") {
        global.clade.corr
      } else {
        readRDS(paste0("Data/Correlations/",input$country,"_",input$region,".rds"))
      }
    }
  })
  
  output$plotCladePrevalences <- renderUI({
    if(!is.null(clade.prevalences()) && length(input$rangePrevalence)>0) {
      shinyjs::show('downCladePrevalencePlot')
      shinyjs::show('downCladePrevalenceData')
    } else {
      shinyjs::hide('downCladePrevalencePlot')
      shinyjs::hide('downCladePrevalenceData')
    }
    if(!is.null(clade.prevalences()) && length(input$rangePrevalence)>0)
    {
      ranges <- input$rangePrevalence
      if(ranges[1]!=ranges[2])
      {
        start.interval <- which(colnames(clade.prevalences())==ranges[1])
        end.interval <- which(colnames(clade.prevalences())==ranges[2])
        if(length(start.interval)>0 && length(end.interval)>0)
        {
          #print(paste0(start.interval,"\t",end.interval))
          plot.data <- compute.prevalences(clade.prevalences(),start.interval,end.interval)
          prevalence.plot <- make.prevalence.plot(plot.data)
          prevalence.plot <- ggplotly(prevalence.plot, tooltip = c("label","Clade","Percentage"), height=600) %>% 
            config(displayModeBar = FALSE) %>%
            layout(hoverlabel = list(font=list(size=17)), legend = list(title = list(text = "")))
          box(width=12, height=600, solidHeader = T, renderPlotly(prevalence.plot))
        }
      } else {
        shinyjs::hide('downCladePrevalencePlot')
        shinyjs::hide('downCladePrevalenceData')
      }
    }
  })
  
  output$plotCladeMutationRates <- renderUI({
    if(length(input$clades)>0 && length(input$rangeCladeMutationRates)>0 && !is.null(clade.prevalences())) {
      shinyjs::show('downCladeMutRatesPlot')
      shinyjs::show('downCladeMutRatesData')
      shinyjs::show('topkMutations')
      shinyjs::show('rangeCladeMutationRates')
    } else {
      shinyjs::hide('downCladeMutRatesPlot')
      shinyjs::hide('downCladeMutRatesData')
      shinyjs::hide('topkMutations')
      shinyjs::hide('rangeCladeMutationRates')
    }
    if(length(input$clades)>0 && length(input$rangeCladeMutationRates)>0 && !is.null(clade.prevalences())) 
    {
      ranges <- input$rangeCladeMutationRates
      if(ranges[1]!=ranges[2])
      {
        if(ncol(clade.mutation.rates())==3) {
          start.interval <- colnames(clade.mutation.rates())[3]
          end.interval <- start.interval
        } else {
          start.interval <- which(colnames(clade.mutation.rates())==ranges[1])
          end.interval <- which(colnames(clade.mutation.rates())==ranges[2])
        }
        if(length(start.interval)>0 && length(end.interval)>0)
        {
          if(any(input$clades %in% clade.prevalences()$Clade))
          {
            plot.data <- compute.clade.frequent.mut(clade.mutation.rates(),clade.prevalences(),input$clades,start.interval,end.interval)
            clade.mut.rates.plots <- make.clade.frequent.mut.plot(plot.data,as.numeric(input$topkMutations))
            clade.mut.rates.plots <- lapply(clade.mut.rates.plots,function(bar.plot){
              ggplotly(bar.plot, tooltip = "text", height=600) %>% config(displayModeBar = FALSE) %>%
                layout(hoverlabel = list(font=list(size=17)))
            })
            lapply(input$clades, function(clade) {
              box(width=6, height=600, solidHeader=T, renderPlotly(clade.mut.rates.plots[[clade]]))
            })
          }
        }
      } else {
        shinyjs::hide('downCladeMutRatesPlot')
        shinyjs::hide('downCladeMutRatesData')
        shinyjs::hide('topkMutations')
        shinyjs::hide('rangeCladeMutationRates')
      }
    }
  })
  
  output$plotRate <- renderUI({
    if(length(input$mutations)>0) {
      shinyjs::show('downRatePlot')
      shinyjs::show('downRateData')
    } else {
      shinyjs::hide('downRatePlot')
      shinyjs::hide('downRateData')
    }
    #print(length(!grepl("Rate",names(mutation.rates()))))
    if(length(input$mutations)>0 && length(!grepl("Rate",names(mutation.rates())))>0) 
    {
      if(any(input$mutations %in% mutation.rates()$Mutation))
      {
        plot.rate <- make.temporal.plot(mutation.rates(),input$mutations)
        plot.rate <- ggplotly(plot.rate, tooltip = "text", height=600) %>% config(displayModeBar = FALSE) %>%
          layout(hoverlabel = list(font=list(size=17)))
        box(width=12, height=600, solidHeader = T, renderPlotly(plot.rate))
      }
    }
    })

  output$plotCladeDistribution <- renderUI({
    if(length(input$mutations)>0 && !is.null(mutation.rates())) {
      shinyjs::show('downCladeDistributionPlot')
      shinyjs::show('downCladeDistributionData')
      #shinyjs::show('topkCladeDistr')
    } else {
      shinyjs::hide('downCladeDistributionPlot')
      shinyjs::hide('downCladeDistributionData')
      #shinyjs::hide('topkCladeDistr')
    }
    if(length(input$mutations)>0 && !is.null(mutation.rates())) 
    {
      if(any(input$mutations %in% mutation.rates()$Mutation))
      {
        plot.data <- compute.mut.frequencies(mutation.rates(),input$mutations,"_absRate")
        #clade.distribution.plots <- make.mut.frequency.plot(plot.data,as.numeric(input$topkCladeDistr))
        clade.distribution.plots <- make.mut.frequency.plot(plot.data)
        clade.distribution.plots <- lapply(clade.distribution.plots,function(plot){
          ggplotly(plot, tooltip = "text", height=600) %>% config(displayModeBar = FALSE) %>%
            layout(hoverlabel = list(font=list(size=17)))
        })
        lapply(input$mutations, function(mut) {
          box(width=6, height=600, solidHeader=T, renderPlotly(clade.distribution.plots[[mut]]))
        })
      }
    }
  })
  
  output$plotCladeRates <- renderUI({
    if(length(input$mutations)>0 && !is.null(mutation.rates())) {
      shinyjs::show('downCladeRatesPlot')
      shinyjs::show('downCladeRatesData')
      #shinyjs::show('topkCladeRates')
    } else {
      shinyjs::hide('downCladeRatesPlot')
      shinyjs::hide('downCladeRatesData')
      #shinyjs::hide('topkCladeRates')
    }
    if(length(input$mutations)>0 && !is.null(mutation.rates())) 
    {
      if(any(input$mutations %in% mutation.rates()$Mutation))
      {
        plot.data <- compute.mut.frequencies(mutation.rates(),input$mutations,"_relRate")
        #clade.rates.plots <- make.mut.frequency.plot(plot.data,as.numeric(input$topkCladeRates))
        clade.rates.plots <- make.mut.frequency.plot(plot.data)
        clade.rates.plots <- lapply(clade.rates.plots,function(plot){
          ggplotly(plot, tooltip = "text", height=600) %>% config(displayModeBar = FALSE) %>%
            layout(hoverlabel = list(font=list(size=17)))
        })
        lapply(input$mutations, function(mut) {
          box(width=6, height=600, solidHeader=T, renderPlotly(clade.rates.plots[[mut]]))
        })
      }
    }
  })

  output$tableCorr <- DT::renderDT({
    corr.table <- clade.corr()[clade.corr()$`Mutation 1` %in% input$mutations | clade.corr()$`Mutation 2` %in% input$mutations,]
    corr.table <- corr.table[,c("Clade","Mutation 1","Mutation 2","Correlation","FDR")]
    corr.table <- datatable(corr.table, selection = list(mode="single", selected = 1), rownames=F, filter="top", options = list(
      dom = "tp", pageLength = 25,
      rowCallback = JS(
        "function(row, data) {",
        "$('td:eq(4)', row).html(data[4].toExponential(2));",
        "}")
    )
    )
    if(!is.null(mutation.rates()) && length(input$mutations)>0) {
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
        sub.clade.corr <- clade.corr()[clade.corr()$`Mutation 1` %in% input$mutations | clade.corr()$`Mutation 2` %in% input$mutations,]
        vals <- sub.clade.corr[row,c("Mutation 1","Mutation 2")]
        if(is.na(vals[1,1])) {
          shinyjs::hide('downCorrPlot')
          shinyjs::hide('downCorrPlotData')
          shinyjs::hide('plotCorrOpt')
        } else {
          temp.rates <- mutation.rates()[mutation.rates()$Mutation==vals[1,1] | mutation.rates()$Mutation==vals[1,2],,drop=F]
          plot.corr <- make.correlation.interactive.plot(temp.rates,input$plotCorrOpt)
          box(width=12, height=600, solidHeader = T, renderPlotly(plot.corr))
        }
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
        if(!is.na(vals[1,1])) {
          heatmap.plot <- make.heatmap.plot(vals[1,1],vals[1,2],as.numeric(vals[1,-c(1,2)]))
          box(width=7, height=400, solidHeader = T, renderPlotly(heatmap.plot))
        }
      }
    }
  })

  output$downCorrTab <- downloadHandler(
    filename = function() {
      paste0(input$country,"_",input$region,"_",paste0(input$mutations,collapse="_"),"_corr.xlsx")
    },
    content = function(file) {
      wb <- createWorkbook()
      output.file <- paste0(input$country,"_",input$region,"_",paste0(input$mutations,collapse="_"))
      output.file <- gsub(":",".",output.file)
      addWorksheet(wb,output.file)
      data <- clade.corr()[clade.corr()$`Mutation 1` %in% input$mutations | clade.corr()$`Mutation 2` %in% input$mutations,]
      names(data)[grep("Cont",names(data))] <- c("Absent-Absent","Absent-Present","Present-Absent","Present-Present")
      writeDataTable(wb, 1, data, startRow = 1, startCol = 1)
      saveWorkbook(wb,file)
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )

  output$downCladePrevalencePlot <- downloadHandler(
    filename = function() {
      paste0(input$country,"_",input$region,"_",paste0(input$rangePrevalence,collapse="_"),"_cladePrevalencePlot.png")
    },
    content = function(file) {
      ranges <- input$rangePrevalence
      start.interval <- which(colnames(clade.prevalences())==ranges[1])
      end.interval <- which(colnames(clade.prevalences())==ranges[2])
      plot.data <- compute.prevalences(clade.prevalences(),start.interval,end.interval)
      ggsave(file,make.prevalence.plot(plot.data),width=6000,height=2000,units="px")
    },
    contentType = "text/png"
  )
  
  output$downCladePrevalenceData <- downloadHandler(
    filename = function() {
      paste0(input$country,"_",input$region,"_",paste0(input$rangePrevalence,collapse="_"),"_cladePrevalences.xlsx")
    },
    content = function(file) {
      ranges <- input$rangePrevalence
      start.interval <- which(colnames(clade.prevalences())==ranges[1])
      end.interval <- which(colnames(clade.prevalences())==ranges[2])
      prevalence.data <- compute.prevalences(clade.prevalences(),start.interval,end.interval)
      wb <- createWorkbook()
      output.file <- paste0(paste0(input$rangePrevalence,collapse="_"))
      addWorksheet(wb,output.file)
      writeDataTable(wb, 1, prevalence.data, startRow = 1, startCol = 1)
      saveWorkbook(wb,file)
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
  
  output$downCladeMutRatesPlot <- downloadHandler(
    filename = function() {
      paste0(input$country,"_",input$region,"_",paste0(input$clades,collapse="_"),"_",paste0(input$rangeCladeMutationRates,collapse="_"),paste0("_top",input$topk),"_cladeMutRatesPlot.png")
    },
    content = function(file) {
      ranges <- input$rangeCladeMutationRates
      if(ncol(clade.mutation.rates())==3) {
        start.interval <- colnames(clade.mutation.rates())[3]
        end.interval <- start.interval
      } else {
        start.interval <- which(colnames(clade.mutation.rates())==ranges[1])
        end.interval <- which(colnames(clade.mutation.rates())==ranges[2])
      }
      plot.data <- compute.clade.frequent.mut(clade.mutation.rates(),clade.prevalences(),input$clades,start.interval,end.interval)
      clade.mut.rates.plots <- make.clade.frequent.mut.plot(plot.data,as.numeric(input$topkMutations))
      nrow <- ceiling(length(clade.mut.rates.plots)/2)
      ncol <- min(length(clade.mut.rates.plots),2)
      final.plot <- ggarrange(plotlist = clade.mut.rates.plots, nrow = nrow, ncol = ncol)
      ggsave(file,final.plot,width=6000,height=2000*nrow,units="px")
    },
    contentType = "text/png"
  )
  
  output$downCladeMutRatesData <- downloadHandler(
    filename = function() {
      paste0(input$country,"_",input$region,"_",paste0(input$clades,collapse="_"),"_",paste0(input$rangeCladeMutationRates,collapse="_"),"_cladeMutRatesPlot.xlsx")
    },
    content = function(file) {
      ranges <- input$rangeCladeMutationRates
      if(ncol(clade.mutation.rates())==3) {
        start.interval <- colnames(clade.mutation.rates())[3]
        end.interval <- start.interval
      } else {
        start.interval <- which(colnames(clade.mutation.rates())==ranges[1])
        end.interval <- which(colnames(clade.mutation.rates())==ranges[2])
      }
      plot.data <- compute.clade.frequent.mut(clade.mutation.rates(),clade.prevalences(),input$clades,start.interval,end.interval)
      wb <- createWorkbook()
      clade.list <- unique(plot.data$Clade)
      for(clade in clade.list)
      {
        sub.plot.data <- plot.data[plot.data$Clade==clade,]
        addWorksheet(wb,clade)
        writeDataTable(wb, clade, sub.plot.data, startRow = 1, startCol = 1)
      }
      saveWorkbook(wb,file)
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
  
  output$downRatePlot <- downloadHandler(
    filename = function() {
      paste0(input$country,"_",input$region,"_",paste0(input$mutations,collapse="_"),"_temporalPlot.png")
    },
    content = function(file) {
      ggsave(file,make.temporal.plot(mutation.rates(),input$mutations),width=6000,height=2000,units="px")
    },
    contentType = "text/png"
  )

  output$downRateData <- downloadHandler(
    filename = function() {
      paste0(input$country,"_",input$region,"_",paste0(input$mutations,collapse="_"),"_temporalRates.xlsx")
    },
    content = function(file) {
      sub.mutation.rates <- mutation.rates()[mutation.rates()$Mutation %in% input$mutations,!grepl("Rate",names(mutation.rates()))]
      wb <- createWorkbook()
      output.file <- paste0(input$country,"_",input$region,"_",paste0(input$mutations,collapse="_"))
      output.file <- gsub(":",".",output.file)
      addWorksheet(wb,output.file)
      writeDataTable(wb, 1, sub.mutation.rates, startRow = 1, startCol = 1)
      saveWorkbook(wb,file)
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )

  output$downCladeDistributionPlot <- downloadHandler(
    filename = function() {
      paste0(input$country,"_",input$region,"_",paste0(input$mutations,collapse="_"),"_cladeDistribution.png")
    },
    content = function(file) {
      plot.data <- compute.mut.frequencies(mutation.rates(),input$mutations,"_absRate")
      clade.distribution.plots <- make.mut.frequency.plot(plot.data)
      #clade.distribution.plots <- make.mut.frequency.plot(plot.data,as.numeric(input$topkCladeDistr))
      #nrow <- ceiling(length(clade.distribution.plots)/2)
      #ncol <- min(length(clade.distribution.plots),2)
      for(i in 1:length(clade.distribution.plots))
      {
        clade.distribution.plots[[i]] <- clade.distribution.plots[[i]] +
          theme(axis.text.x=element_text(size=16, angle=90, color="black", vjust = 0.5),
                axis.text.y=element_text(size=16, color="black"))
      }
      final.plot <- ggarrange(plotlist = clade.distribution.plots, nrow = length(clade.distribution.plots), ncol = 1)
      ggsave(file,final.plot,width=6000,height=2000*length(clade.distribution.plots),units="px")
    },
    contentType = "text/png"
  )

  output$downCladeDistributionData <- downloadHandler(
    filename = function() {
      paste0(input$country,"_",input$region,"_",paste0(input$mutations,collapse="_"),"_cladeDistribution.xlsx")
    },
    content = function(file) {
      plot.data <- compute.mut.frequencies(mutation.rates(),input$mutations,"_absRate")
      wb <- createWorkbook()
      mutation.list <- unique(plot.data$Mutation)
      for(mutation in mutation.list)
      {
        sub.plot.data <- plot.data[plot.data$Mutation==mutation,]
        addWorksheet(wb,gsub(":",".",mutation))
        writeDataTable(wb,gsub(":",".",mutation), sub.plot.data, startRow = 1, startCol = 1)
      }
      saveWorkbook(wb,file)
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )

  output$downCladeRatesPlot <- downloadHandler(
    filename = function() {
      paste0(input$country,"_",input$region,"_",paste0(input$mutations,collapse="_"),"_cladeRates.png")
    },
    content = function(file) {
      plot.data <- compute.mut.frequencies(mutation.rates(),input$mutations,"_relRate")
      clade.rates.plots <- make.mut.frequency.plot(plot.data)
      #clade.rates.plots <- make.mut.frequency.plot(plot.data,as.numeric(input$topkCladeRates))
      # nrow <- ceiling(length(clade.rates.plots)/2)
      # ncol <- min(length(clade.rates.plots),2)
      for(i in 1:length(clade.rates.plots))
      {
        clade.rates.plots[[i]] <- clade.rates.plots[[i]] +
          theme(axis.text.x=element_text(size=16, angle=90, color="black", vjust = 0.5),
                axis.text.y=element_text(size=16, color="black"))
      }
      final.plot <- ggarrange(plotlist = clade.rates.plots, nrow = length(clade.rates.plots), ncol = 1)
      ggsave(file,final.plot,width=6000,height=2000*length(clade.rates.plots),units="px")
    },
    contentType = "text/png"
  )
  
  output$downCladeRatesData <- downloadHandler(
    filename = function() {
      paste0(input$country,"_",input$region,"_",paste0(input$mutations,collapse="_"),"_cladeRates.xlsx")
    },
    content = function(file) {
      plot.data <- compute.mut.frequencies(mutation.rates(),input$mutations,"_relRate")
      wb <- createWorkbook()
      mutation.list <- unique(plot.data$Mutation)
      for(mutation in mutation.list)
      {
        sub.plot.data <- plot.data[plot.data$Mutation==mutation,]
        addWorksheet(wb,gsub(":",".",mutation))
        writeDataTable(wb, gsub(":",".",mutation), sub.plot.data, startRow = 1, startCol = 1)
      }
      saveWorkbook(wb,file)
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
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
      paste0(vals[1,1],"_",vals[1,2],"_correlationPlotData.xlsx")
    },
    content = function(file) {
      row <- input$tableCorr_rows_selected
      vals <- (clade.corr()[clade.corr()$`Mutation 1` %in% input$mutations | clade.corr()$`Mutation 2` %in% input$mutations,])[row,c("Mutation 1","Mutation 2")]
      wb <- createWorkbook()
      output.file <- paste0(vals[1,1],"_",vals[1,2])
      output.file <- gsub(":",".",output.file)
      addWorksheet(wb,output.file)
      temp.rates <- mutation.rates()[mutation.rates()$Mutation==vals[1,1] | mutation.rates()$Mutation==vals[1,2],,drop=F]
      if("Use clades" %in% input$plotCorrOpt)
      {
        temp.rates <- temp.rates[,grepl("_relRate|Mutation",names(temp.rates))]
        names(temp.rates) <- gsub("_relRate","",names(temp.rates))
      }
      else
        temp.rates <- temp.rates[,!grepl("Rate|error",names(temp.rates))]
      writeDataTable(wb, 1, temp.rates, startRow = 1, startCol = 1)
      saveWorkbook(wb,file)
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )

}

