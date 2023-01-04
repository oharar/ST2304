library(shiny)

# Define UI for miles per gallon app ----
ui <- pageWithSidebar(
  
  # App title ----
  headerPanel("Land or Sea?"),
  
  # Sidebar panel for inputs ----
  sidebarPanel(
    
    # Input: Type in sample size ----
    numericInput("Sample_size", "Sample size:", 10, min = 1, max = 100),
    verbatimTextOutput("value"),
    
    # Input: Selector for number of samples ----
    sliderInput("Samples", "Number of samples", value=1, min=1, max=10),
    
    actionButton("goButton", "Go!")

  ),
  
  # Main panel for displaying outputs ----
  mainPanel(
    # This is the dynamic UI for the plots
    plotOutput("plots")
  )
  
)

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  
  # Insert the right number of plot output objects into the web page
  output$plots <- renderPlot({
    input$goButton
      if(input$Samples > 1){
      par(mfrow=c(2,5))
        x <- rbinom(input$Sample_size, 1, 0.4) # draw n samples
      for(i in 1:input$Samples){
        x <- rbind(x, rbinom(input$Sample_size, 1, 0.4)) # draw n samples
      }
      for(i in 1:input$Samples){
      hist(x=x[i,], main=paste("Sample", i, sep=""),
           xlab = "", ylab= "Frequency", axes=F, col=3)
      axis(1, at=c(0,1), labels=c("Sea", "Land"))
      axis(2, las=1)}}else{
      x <- rbinom(input$Sample_size, 1, 0.4) # draw n samples
      hist(x=x, main=paste("Sample", 1, sep=""),           
      xlab = "", ylab= "Frequency", axes=F, col=3)
      axis(1, at=c(0,1), labels=c("Sea", "Land"))
      axis(2, las=1)}
    })
  
  
  
}

shinyApp(ui, server)