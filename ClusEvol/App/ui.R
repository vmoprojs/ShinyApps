source("app.R")

dataset <- NA
# Use a fluid Bootstrap layout
fluidPage(    
  
  # Give the page a title
  titlePanel(title=div(img(src="soliLogo.jpg",height=100, width=100),"Cluster Evolution Analytics")),
  titlePanel(title=div("Country macroeconomic profiles")),
  # Generate a row with a sidebar
  sidebarPanel( width = 3,
                
    # condition = "input.tabs=='Results'",
      helpText("Chose the category of variables that you wish to analyse"),
      
    selectInput("varType", "Select type of variable selection",
                c(Group = "group", Individual = "individual"),selected = "individual"
    ),
      
      
      
    
    # Only show this panel if the plot type is a histogram
    conditionalPanel(
      condition = "input.varType == 'group'",
      selectInput("indicator", "Variables category:", choices=indicador,selected = "Real GDP, employment and population levels",multiple=FALSE),
    ),
    
    # Only show this panel if the plot type is a histogram
    conditionalPanel(
      condition = "input.varType == 'individual'",
      selectInput("indicator1", "Variables:", choices=indIndiv,selected = "hc",multiple=TRUE),
    ),
    
    
      selectInput("ctry", "Country to be analyzed:", choices=ctcode,selected = "ECU"),
      
      hr(),
      helpText("Choose the number of groups:"),
      numericInput("obs", "N. Groups", 5, min = 1, max = 50),
      
      helpText("Choose the base year:"),
      selectInput("yrs1", "Base Year", choices = yr,selected  = yr[length(yr)]),
      
      helpText("Choose the years range for the analysis:"),
      selectInput("min", "Minimum", choices = yr,selected = 1990),
      # selectInput("max", "Maximo", choices = ""),
      
      helpText("Chose display year (cannot be less than the max year):"),
      selectInput("yrs", "Year to display", choices = yr,selected = yr[length(yr)]),
      
      checkboxInput("checkres1", "Hide base year", FALSE),
    checkboxInput("logscale", "Log scale", FALSE)
      # checkboxInput("checkres", "Veamos", TRUE)
    ),
    
    # Create a spot for the barplot
    mainPanel(
      width = 8,
      tabsetPanel(
        type = "tabs",
        tabPanel("Results",
                 highchartOutput("networkPlot", width = 1500, height = 700),
                 # tabPanel("Summary",  verbatimTextOutput("summary"))
                 h3("Table for base year:"),
                 dataTableOutput('summary'),
                 h3("Summary statistics input data:"),
                 verbatimTextOutput(outputId='sumdata')
                 
                 # plotOutput("indicatorPlotB"),
                 
        ),
        tabPanel("#ClustersInTime",
                 h3("Summary Table for groups:"),
                 dataTableOutput('ClusSummary')
                 ,
                 downloadButton('downLoadFilter1',"Data download")
                 # ,
                 # verbatimTextOutput("Raw"),
                 # DT::dataTableOutput('CT')
                 
        ),
        tabPanel("AllGroupings",
                 h3("Table with groups for all years:"),
                 dataTableOutput('tabAll'),
                 downloadButton('downLoadFilter',"Data download")
                 # ,
                 # verbatimTextOutput("Raw"),
                 # DT::dataTableOutput('ex1')
                 
        ),
        tabPanel("Info",
                 el <- div(HTML("<h3>Notes</h3> <br>
                            
                                <br>
                              
                                <h4>Data source:</h4>	
                                <li>https://doi.org/10.34894/QT5BCC</li>
                                
                                <h4>Real GDP, employment and population levels</h4>	
                                <li>rgdpe	Expenditure-side real GDP at chained PPPs (in mil. 2011US$)</li>
                                <li>rgdpo	Output-side real GDP at chained PPPs (in mil. 2011US$)</li>
                                <li>pop	Population (in millions)</li>
                                <li>emp	Number of persons engaged (in millions)</li>
                                <li>avh	Average annual hours worked by persons engaged</li>
                                <li>hc	Human capital index, based on years of schooling and returns to education; see Human capital in PWT9.</li>
                                
                                <h4>Current price GDP, capital and TFP</h4>	
                                <li>ccon	Real consumption of households and government, at current PPPs (in mil. 2011US$)</li>
                                <li>cda	Real domestic absorption, (real consumption plus investment), at current PPPs (in mil. 2011US$)</li>
                                <li>cgdpe	Expenditure-side real GDP at current PPPs (in mil. 2011US$)</li>
                                <li>cgdpo	Output-side real GDP at current PPPs (in mil. 2011US$)</li>
                                <li>cn	Capital stock at current PPPs (in mil. 2011US$)</li>
                                <li>ck	Capital services levels at current PPPs (USA=1)</li>
                                <li>ctfp	TFP level at current PPPs (USA=1)</li>
                                <li>cwtfp	Welfare-relevant TFP levels at current PPPs (USA=1)</li>
                                
                                <h4>National accounts-based variables	</h4>
                                <li>rgdpna	Real GDP at constant 2011 national prices (in mil. 2011US$)</li>
                                <li>rconna	Real consumption at constant 2011 national prices (in mil. 2011US$)</li>
                                <li>rdana	Real domestic absorption at constant 2011 national prices (in mil. 2011US$)</li>
                                <li>rnna	Capital stock at constant 2011 national prices (in mil. 2011US$)</li>
                                <li>rkna	Capital services at constant 2011 national prices (2011=1)</li>
                                <li>rtfpna	TFP at constant national prices (2011=1)</li>
                                <li>rwtfpna	Welfare-relevant TFP at constant national prices (2011=1)</li>
                                <li>labsh	Share of labour compensation in GDP at current national prices</li>
                                <li>irr	Real internal rate of return</li>
                                <li>delta	Average depreciation rate of the capital stock</li>
                                
                                <h4>Exchange rates and GDP price levels	</h4>
                                <li>xr	Exchange rate, national currency/USD (market+estimated)</li>
                                <li>pl_con	Price level of CCON (PPP/XR), price level of USA GDPo in 2011=1</li>
                                <li>pl_da	Price level of CDA (PPP/XR), price level of USA GDPo in 2011=1</li>
                                <li>pl_gdpo	Price level of CGDPo (PPP/XR),  price level of USA GDPo in 2011=1</li>
                                
                                <h4>Data information variables</h4>
                                <li>i_cig	0/1/2: relative price data for consumption, investment and government is extrapolated (0), benchmark (1) or interpolated (2)</li>
                                <li>i_xm	0/1/2: relative price data for exports and imports is extrapolated (0), benchmark (1) or interpolated (2)</li>
                                <li>i_xr	0/1: the exchange rate is market-based (0) or estimated (1)</li>
                                <li>i_outlier	0/1: the observation on pl_gdpe or pl_gdpo is not an outlier (0) or an outlier (1)</li>
                                <li>i_irr	0/1/2/3: the observation for irr is not an outlier (0), may be biased due to a low capital share (1), hit the lower bound of 1 percent (2), or is an outlier (3)</li>
                                <li>cor_exp	Correlation between expenditure shares of the country and the US (benchmark observations only)</li>
                                <li>statcap	Statistical capacity indicator (source: World Bank, developing countries only)</li>
                                
                                <h4>Shares in CGDPo	</h4>
                                <li>csh_c	Share of household consumption at current PPPs</li>
                                <li>csh_i	Share of gross capital formation at current PPPs</li>
                                <li>csh_g	Share of government consumption at current PPPs</li>
                                <li>csh_x	Share of merchandise exports at current PPPs</li>
                                <li>csh_m	Share of merchandise imports at current PPPs</li>
                                <li>csh_r	Share of residual trade and GDP statistical discrepancy at current PPPs</li>
                                
                                <h4>Price levels, expenditure categories and capital	</h4>
                                <li>pl_c	Price level of household consumption,  price level of USA GDPo in 2011=1</li>
                                <li>pl_i	Price level of capital formation,  price level of USA GDPo in 2011=1</li>
                                <li>pl_g	Price level of government consumption,  price level of USA GDPo in 2011=1</li>
                                <li>pl_x	Price level of exports, price level of USA GDPo in 2011=1</li>
                                <li>pl_m	Price level of imports, price level of USA GDPo in 2011=1</li>
                                <li>pl_n	Price level of the capital stock, price level of USA in 2011=1</li>
                                <li>pl_k	Price level of the capital services, price level of USA=1</li>
                                ")),
                 cat(as.character(el))
                 
                 ),
        id = "tabs"
      )
    )
  
    
  )


