source("app.R")
# Use a fluid Bootstrap layout
fluidPage(    
  
  # Give the page a title
  titlePanel(title=div(img(src="flacsologo.png",height=50, width=50),"Trade Network Centrality: A sensitivity analysis toolkit")),
  hr(),
  helpText("This application is a beta version (under development) of the study of centrality indicators of the trade network. The values displayed are the results of the investigation: A sensitivity measurement of the international trade network in the period 1992-2015"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(width = 3,
      helpText("Choose the centrality measure you want to analyze"),
      selectInput("indicator", "Indicator:", 
                  choices=indicador),
      checkboxInput("checkdata", "Show figure values", FALSE),
      hr(),
      helpText("Choose the country(s) to plot:"),
      
      selectInput("countryA", "Country 1:", 
                  choices=pais),
      
      selectInput("countryB", "Country 2:", 
                  choices=pais),
      
      sliderInput("time", "Time Window", 
                  min=1992, max=2015, value=c(1992,2015)),
      
      wellPanel(
        selectInput("year1", "Select the comparison years for the Wilcoxon test by pairs of years", 
                    choices=anio),
        selectInput("year2", "", 
                    choices=anio),
        tags$small(paste0(
          "Note: The comparison between years is carried out ",
          " through simulations of the centrality indicator of each country. ",
          " 1000 simulations were carried out per year.",
          " Source: World Bank.",
          " Autor:",
              "\nVíctor Morales-Oñate"
        ))
      )
      
    ),
    
    
    # Create a spot for the barplot
    mainPanel(
      
      # img(src='flacsologo.png', align = "right", height=42, width=32),
      plotOutput("indicatorPlotA"),
      
      # if( input$checkdata == TRUE)
      # {
        h4("Table"),
        tableOutput("summary"),
      # }
      
      
      
      tabsetPanel(id="tabspanel", type = "tabs",
                  tabPanel(title = "Wilcox Country 1",
                           verbatimTextOutput(outputId = "pA"))),
      
      
      tabsetPanel(id="tabspanel", type = "tabs",
                  tabPanel(title = "Wilcox Country 2",
                           verbatimTextOutput(outputId = "pB")))
    )
  )
  ,
  el <- div(HTML("<h3>Technical Note</h3> <br>
<ul>
<li>Eigen centrality: This type of measurement is based on the idea that the centrality of the neighbors is inherited to the target node</li>
<li>Elasticity: It assumes a marginal propensity to import, as well as a certain commercial structure given by the network</li>
<ul>
<li>Elasticity i: Average effect of a 1% increase in the income of country <i>i</i> on a country in the rest of the world.</li>
<li>Elasticity j: Average effect of a 1% increase in the income of a country from the rest of the world on country <i>j</i>.</li>
</ul>
</ul>
                 ")),
  cat(as.character(el))
  
)