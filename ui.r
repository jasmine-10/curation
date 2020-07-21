library(shiny)

shinyUI(fluidPage(
  titlePanel(title="GENE EXPRESSION SIGNATURES OF SEPSIS"),
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(condition="input.tabselected==2",
                       selectInput("geneselected", label="Select a gene of interest:",choices=genes[,1], selected="A1BG"),
                       plotOutput("plot"))),
    mainPanel(
      tabsetPanel(
        id="tabselected",
        tabPanel("About",value=1,textOutput('text1')),
        tabPanel("Data table",value=2,dataTableOutput('table')),
        tabPanel("By gene", value=3, textOutput('text2'))
    )
  ))))