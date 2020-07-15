library(shiny)
library(shinydashboard)

bars<-table(iris$Sepal.Length)

shinyServer(function(input,output){
  
  #side panel bar graph
  output$plot<-renderPlot({
    barplot(bars)
  })
  

})