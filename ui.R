library(shiny)
library(readxl)
library(drc)
library(multcomp)
library(xtable)


ui = fluidPage(
  titlePanel("Uploading Files"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose data File',
                accept=c('text/csv',
                         'text/comma-separated-values,text/plain',
                         '.xlsx'))
    ),
    mainPanel(
      tableOutput(outputId = 'originalTable'),
      plotOutput('plot',width = "50%"),
      downloadButton(outputId = "down", label = "Download the plot")
    )
  )
)

