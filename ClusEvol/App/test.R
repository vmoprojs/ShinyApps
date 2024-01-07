## Only run this example in interactive R sessions
if (interactive()) {
  ui <- fluidPage(
    sidebarPanel(
      selectInput("plotType", "Plot Type",
                  c(Scatter = "scatter", Histogram = "hist")
      ),
      # Only show this panel if the plot type is a histogram
      conditionalPanel(
        condition = "input.plotType == 'hist'",
        selectInput(
          "breaks", "Breaks",
          c("Sturges", "Scott", "Freedman-Diaconis", "[Custom]" = "custom")
        ),
        # Only show this panel if Custom is selected
        conditionalPanel(
          condition = "input.breaks == 'custom'",
          sliderInput("breakCount", "Break Count", min = 1, max = 50, value = 10)
        )
      )
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
  
  server <- function(input, output) {
    x <- rnorm(100)
    y <- rnorm(100)
    
    output$plot <- renderPlot({
      if (input$plotType == "scatter") {
        plot(x, y)
      } else {
        breaks <- input$breaks
        if (breaks == "custom") {
          breaks <- input$breakCount
        }
        
        hist(x, breaks = breaks)
      }
    })
  }
  
  shinyApp(ui, server)
}





data(khanmiss)
khan.expr <- khanmiss[-1, -(1:2)]
##
## First example
##
if(exists(".Random.seed")) rm(.Random.seed)
khan.imputed <- impute.knn(as.matrix(khan.expr))

khan.imputed <- impute.knn(as.matrix(khan.expr[,1:2]))$data
str(khan.imputed)

dim(khan.expr)

temp1 <- khan.imputed
temp1 <- as.numeric(khan.expr[,1])
temp1 <- temp1[which(!is.na(temp1))]
matrix(temp1,ncol=1)

kmodel <- pam(dist(scale(temp1)),2)
kmodel$clustering




getwd()
datos <- rio::import("data/pwt1001.xlsx",which = "Data")
if(input$logscale){
  datos[,5:ncol(datos)] <- apply(datos[,5:ncol(datos)],2,log)
}