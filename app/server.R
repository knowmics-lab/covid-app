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

source("AppFunctions.R")

function(input, output, session) {
  clade.corr.table <- reactive({
    clade.correlations[clade.correlations$Clade == input$cladeCorr, -1]
  })

  temporal.mutation.set.rates <- reactive({
    if (length(input$mutationTemp) > 0) {
      shinyjs::enable("downTempPlot")
      shinyjs::enable("downTempData")
      compute.temporal.mutation.rates(temporal.rates, input$mutationTemp)
    } else {
      shinyjs::disable("downTempPlot")
      shinyjs::disable("downTempData")
    }
  })

  pie.plot.data <- reactive({
    if (length(input$mutationTemp) > 0) {
      shinyjs::enable("downCladePlot")
      shinyjs::enable("downCladeData")
      compute.pie.mutation.rates(clade.rates, input$mutationTemp)
    } else {
      shinyjs::disable("downCladePlot")
      shinyjs::disable("downCladeData")
    }
  })

  temporal.pair.rates <- reactive({
    row <- input$tableCorr_rows_selected
    if (!is.null(row)) {
      shinyjs::enable("downCorrPlot")
      shinyjs::enable("downCorrPlotData")
      if (length(input$mutationCorr) == 0) {
        vals <- clade.corr.table()[row, c(1, 2)]
      } else if (length(input$mutationCorr) == 1) {
        vals <- (clade.corr.table()[clade.corr.table()$`Mutation 1` %in% input$mutationCorr | clade.corr.table()$`Mutation 2` %in% input$mutationCorr, ])[row, c(1, 2)]
      } else {
        vals <- (clade.corr.table()[(clade.corr.table()$`Mutation 1` %in% input$mutationCorr & clade.corr.table()$`Mutation 2` %in% input$mutationCorr) |
          (clade.corr.table()$`Mutation 2` %in% input$mutationCorr & clade.corr.table()$`Mutation 1` %in% input$mutationCorr), ])[row, c(1, 2)]
      }
      compute.temporal.pair.rates(mutation.data, vals[1, 1], vals[1, 2], input$plotCorrOpt)
    } else {
      shinyjs::disable("downCorrPlot")
      shinyjs::disable("downCorrPlotData")
    }
  })

  updateSelectizeInput(session, "mutationTemp",
    choices = colnames(mutation.data)[-(1:3)], server = T
  )

  observeEvent(input$cladeCorr, {
    updateSelectizeInput(session, "mutationCorr",
      selected = "",
      choices = sort(unique(c(clade.corr.table()$`Mutation 1`, clade.corr.table()$`Mutation 2`))), server = TRUE
    )
  })

  output$plotTemp <- renderPlot(
    {
      if (length(input$mutationTemp) > 0) {
        shinyjs::enable("downTempPlot")
        shinyjs::enable("downTempData")
        make.temporal.plot(temporal.mutation.set.rates(), input$mutationTemp)
      } else {
        shinyjs::disable("downTempPlot")
        shinyjs::disable("downTempData")
      }
    },
    res = 96
  )

  output$plotClade <- renderPlot(
    {
      if (length(input$mutationTemp) > 0) {
        shinyjs::enable("downCladePlot")
        shinyjs::enable("downCladeData")
        make.clade.plot(pie.plot.data(), input$mutationTemp)
      } else {
        shinyjs::disable("downCladePlot")
        shinyjs::disable("downCladeData")
      }
    },
    height = function() {
      if (length(input$mutationTemp) > 0) {
        400 * ceiling(length(input$mutationTemp) / 2)
      } else {
        "auto"
      }
    },
    res = 96
  )

  output$plotCorr <- renderPlot({
    row <- input$tableCorr_rows_selected
    if (!is.null(row)) {
      shinyjs::enable("downCorrPlot")
      shinyjs::enable("downCorrPlotData")
      shinyjs::enable("plotCorrOpt")
      make.correlation.plot(temporal.pair.rates(), input$plotCorrOpt)
    } else {
      shinyjs::disable("downCorrPlot")
      shinyjs::disable("downCorrPlotData")
      shinyjs::disable("plotCorrOpt")
    }
  })

  output$tableCorr <- DT::renderDT(
    {
      if (length(input$mutationCorr) == 0) {
        clade.corr.table()
      } else if (length(input$mutationCorr) == 1) {
        clade.corr.table()[clade.corr.table()$`Mutation 1` %in% input$mutationCorr | clade.corr.table()$`Mutation 2` %in% input$mutationCorr, ]
      } else {
        clade.corr.table()[(clade.corr.table()$`Mutation 1` %in% input$mutationCorr & clade.corr.table()$`Mutation 2` %in% input$mutationCorr) |
          (clade.corr.table()$`Mutation 2` %in% input$mutationCorr & clade.corr.table()$`Mutation 1` %in% input$mutationCorr), ]
      }
    },
    options = list(dom = "ltp", pageLength = 20),
    rownames = F,
    selection = "single"
  )

  output$downCorrTab <- downloadHandler(
    filename = function() {
      if (length(input$mutationCorr) == 0) {
        paste0(input$cladeCorr, "_signCorr.csv")
      } else {
        paste0(input$cladeCorr, "_", paste0(input$mutationCorr, collapse = "_"), "_signCorr.csv")
      }
    },
    content = function(file) {
      if (length(input$mutationCorr) == 0) {
        write_csv(clade.corr.table(), file)
      } else if (length(input$mutationCorr) == 1) {
        write_csv(clade.corr.table()[clade.corr.table()$`Mutation 1` %in% input$mutationCorr | clade.corr.table()$`Mutation 2` %in% input$mutationCorr, ], file)
      } else {
        write_csv(clade.corr.table()[(clade.corr.table()$`Mutation 1` %in% input$mutationCorr & clade.corr.table()$`Mutation 2` %in% input$mutationCorr) |
          (clade.corr.table()$`Mutation 2` %in% input$mutationCorr & clade.corr.table()$`Mutation 1` %in% input$mutationCorr), ], file)
      }
    },
    contentType = "text/csv"
  )

  output$downTempPlot <- downloadHandler(
    filename = function() {
      paste0(paste0(input$mutationTemp, collapse = "_"), "_temporalPlot.png")
    },
    content = function(file) {
      ggsave(file, temporal.plot(), width = 4500, height = 3000, units = "px")
    },
    contentType = "text/png"
  )

  output$downTempData <- downloadHandler(
    filename = function() {
      paste0(paste0(input$mutationTemp, collapse = "_"), "_temporalRates.csv")
    },
    content = function(file) {
      final.data <- temporal.mutation.set.rates()
      final.data$rate <- paste0(final.data$rate, "%")
      write_csv(final.data, file)
    },
    contentType = "text/csv"
  )

  output$downCladePlot <- downloadHandler(
    filename = function() {
      paste0(paste0(input$mutationTemp, collapse = "_"), "_cladePie.png")
    },
    content = function(file) {
      ggsave(file, make.clade.plot(pie.plot.data(), input$mutationTemp), width = 4500, height = 3000, units = "px", bg = "white")
    },
    contentType = "text/png"
  )

  output$downCladeData <- downloadHandler(
    filename = function() {
      paste0(paste0(input$mutationTemp, collapse = "_"), "_cladeRates.csv")
    },
    content = function(file) {
      write_csv(pie.plot.data(), file)
    },
    contentType = "text/csv"
  )

  output$downCorrPlot <- downloadHandler(
    filename = function() {
      mutation.pair <- names(temporal.pair.rates())
      paste0(paste0(mutation.pair, collapse = "_"), "_correlationPlot.png")
    },
    content = function(file) {
      ggsave(file, make.correlation.plot(temporal.pair.rates(), input$plotCorrOpt), width = 4500, height = 3000, units = "px")
    },
    contentType = "text/png"
  )

  output$downCorrPlotData <- downloadHandler(
    filename = function() {
      mutation.pair <- names(temporal.pair.rates())
      paste0(paste0(mutation.pair, collapse = "_"), "_correlationPlotData.csv")
    },
    content = function(file) {
      final.data <- temporal.pair.rates()
      final.data[, 2] <- paste0(final.data[, 2], "%")
      final.data[, 3] <- paste0(final.data[, 3], "%")
      write_csv(final.data, file)
    },
    contentType = "text/csv"
  )
}
