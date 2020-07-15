library(shiny)

shinyUI(fluidPage(
  titlePanel(title="TITLE"),
  sidebarLayout(
    sidebarPanel(plotOutput("plot"),
      sliderInput("slider","Number of obs.:",1,100,50)),
    mainPanel()
  )))