source("app.R")
# Use a fluid Bootstrap layout
fluidPage(    
  
  # Give the page a title
  titlePanel(title=div(img(src="flacsologo.png",height=32, width=22),"Centralidad en la red de comercio")),
  hr(),
  helpText("El presente aplicativo es una versión beta (en desarrollo) del estudio de
           indicadores de centralidad de la red de comercio. 
           Los valores desplegados son los resultados de la investigación: 
           Una medición de sensibilidad de la red de comercio internacional en el período 1992-2015"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(width = 3,
      helpText("Elija el indicador de centralidad que desea analizar"),
      selectInput("indicator", "Indicador:", 
                  choices=indicador),
      checkboxInput("checkdata", "Mostrar valores del gráfico", FALSE),
      hr(),
      helpText("Elija el(los) países a graficar:"),
      
      selectInput("countryA", "País 1:", 
                  choices=pais),
      
      selectInput("countryB", "País 2:", 
                  choices=pais),
      
      sliderInput("time", "Ventana de tiempo", 
                  min=1992, max=2015, value=c(1992,2015)),
      
      wellPanel(
        selectInput("year1", "Selecciona los años de comparación para el test de Wilcoxon por parejas de años", 
                    choices=anio),
        selectInput("year2", "", 
                    choices=anio),
        tags$small(paste0(
          "Nota: La comparación entre años es realizada",
          " mediante simulaciones del indicador de",
          " centralidad de cada país. Se realizaron 1000 simulaciones por año.",
          " Fuente de datos: Banco Mundial.",
          " Autores:",
          "   \nPérez-Ovierdo Wilson,", 
              "\nJohn Cajas-Guijarro y ",
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
        h4("Tabla de valores"),
        tableOutput("summary"),
      # }
      
      
      
      tabsetPanel(id="tabspanel", type = "tabs",
                  tabPanel(title = "Wilcox País 1",
                           verbatimTextOutput(outputId = "pA"))),
      
      
      tabsetPanel(id="tabspanel", type = "tabs",
                  tabPanel(title = "Wilcox País 2",
                           verbatimTextOutput(outputId = "pB")))
    )
  )
  ,
  el <- div(HTML("<h3>Nota técnica</h3> <br>
<ul>
<li>Eigen: centralidad de Vector propio o <i>pagerank</i>. Este tipo de medida se basa en la idea de que la centralidad de los vecinos le es heredada al nodo objetivo</li>
<li>Elasticidad: Supone una propensión marginal a importar, así como una determinada estructura comercial dada por la red</li>
<ul>
<li>Elasticidad i: medida de la centralidad de cada país en el comercio mundial. Así, un país es más <i>central</i> si aumenta su elasticidad.</li>
<li>Elasticidad j: dependencia de un país frente a un shock del resto de países.</li>
</ul>
</ul>
                 ")),
  cat(as.character(el))
  
)