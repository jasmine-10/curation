library(shiny)
library(shinydashboard)

bars<-table(genes$Genes)

shinyServer(function(input,output){
  
  #side panel bar graph
  output$plot<-renderPlot({
    barplot(bars)
  })
  
  #main panel table
  output$table<-renderDataTable(genes)
  
  output$text1<-renderText("About the dashboard here.")
  
  output$text2<-renderText("By gene")
  
  

})