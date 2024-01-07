source("app.R")

options(shiny.maxRequestSize=50*1024^2)
# options(repos = BiocInstaller::biocinstallRepos())
getOption("repos")
# Define a server for the Shiny app
function(input, output,session) {
  
  #####################################
  ###### READ IN / GET DATA ###########
  #####################################
  
  df_shiny <- reactive({
    datos <- rio::import("data/pwt1001.xlsx",which = "Data")
    
    
    
    target.vars = c("rgdpe","rgdpo","pop","emp","avh","hc")
    
    if(input$indicator =="Real GDP, employment and population levels")
    {
      # Real GDP, employment and population levels
      target.vars <-  c("rgdpe","rgdpo","pop","emp","avh","hc")
    }
    if(input$indicator =="Current price GDP, capital and TFP")
    {
      # Current price GDP, capital and TFP
      target.vars <- c("ccon","cda","cgdpe","cgdpo","cn","ck","ctfp","cwtfp")
    }
    if(input$indicator =="National accounts-based variables")
    {
      # National accounts-based variables:
      target.vars <- c("rgdpna","rconna","rdana","rnna","rkna","rtfpna",
                       "rwtfpna","labsh","irr","delta")
    }
    if(input$indicator =="Exchange rates and GDP price levels")
    {
      # Exchange rates and GDP price levels:
      target.vars <- c("xr","pl_con","pl_da","pl_gdpo")
    }
    if(input$indicator =="Data information variables")
    {
      # Data information variables:
      target.vars <- c("i_cig","i_xm","i_xr","i_outlier","i_irr","cor_exp","statcap")
    }
    if(input$indicator =="Shares in CGDPo")
    {
      # Shares in CGDPo:
      target.vars <- c("csh_c","csh_i","csh_g","csh_x","csh_m","csh_r")
    }
    if(input$indicator =="Price levels, expenditure categories and capital")
    {
      # Price levels, expenditure categories and capital
      target.vars <- c("pl_c","pl_i","pl_g","pl_x","pl_m","pl_n","pl_k")
    }
    
    if(input$varType =="individual")
    {
      # Price levels, expenditure categories and capital
      target.vars <- input$indicator1
    }
    
    
    
    
    
    # mostrar <- input$yrs
    yrs.base <- input$yrs1
    pais <- input$ctry #Country to be analyzed
    refdatos <- datos[datos$year==yrs.base,c("countrycode","year",target.vars)]
    
    if(input$logscale & length(target.vars)>1){
      refdatos[,3:ncol(refdatos)] <- apply(refdatos[,3:ncol(refdatos)],2,log)
    }
    if(input$logscale & length(target.vars)==1){
      refdatos[,3:ncol(refdatos)] <- log(refdatos[,3:ncol(refdatos)])
    }


    ECbase <- refdatos[refdatos$countrycode==pais,c("countrycode","year",target.vars)]

    
    
    results <- list()
    ng <- input$obs#number of groups
    yrs <- unique(datos$year)
    na.sol <- NULL # NA and NROW
    init <- which(yrs==input$min) #initiation year
    contador <- 1
    
    Ksol = NULL
    for (j in  init:length(yrs))
    {
      refdatos <- datos[datos$year==yrs[j],c("countrycode","year",target.vars)]
      
      if(input$logscale & length(target.vars)>1){
        refdatos[,3:ncol(refdatos)] <- apply(refdatos[,3:ncol(refdatos)],2,log)
      }
      if(input$logscale & length(target.vars)==1){
        refdatos[,3:ncol(refdatos)] <- log(refdatos[,3:ncol(refdatos)])
      }
      
      
      refdatos[refdatos$countrycode==pais,] <- ECbase #replace ECU values
      na.sol <- rbind(na.sol,c(sum(complete.cases(as.matrix(refdatos[,-(1:2)]))),nrow(as.matrix(refdatos[,-(1:2)]))))
      if(length(target.vars)>1)
      {
        temp1 <- impute.knn(as.matrix(refdatos[,-(1:2)]))$data
        rownames(temp1) <- refdatos[,"countrycode"]
      }else{
        posauxNA <- which(!is.na(refdatos[,target.vars]))
        refdatos <- refdatos[posauxNA,]
        temp1 <- matrix(refdatos[,target.vars],ncol = 1)
        rownames(temp1) <- refdatos[,"countrycode"]
        # cat("temp1:--- ",(temp1),"\n")
      }
      
      

      # normalize data:
      auxK = cbind(kmodel$clustering,yrs[j])
      Ksol = rbind(Ksol,auxK )
      
      # Find Ecuador's group and data

      gbase <- list()
      for(i in 1:ng)
      {
        # i = 1
        gbase[[i]] <- as.vector(refdatos[,1][kmodel$cluster==i])
      }
      
      nbelong <- grep(pais,gbase)# number of group which "ECU" belongs to
      
      Cnamesbase <- datos$country[match(gbase[[nbelong]],datos$countrycode)]#Country names in EC group
      Cdatabase <- refdatos[match(gbase[[nbelong]],refdatos$countrycode),-(1:2)]# Data in EC group
      temp1.aux <- temp1[match(gbase[[nbelong]],rownames(temp1)),]# Data in EC group
 
      if(length(target.vars)>1)
      {
        
        solbase <- data.frame(countrycode = rownames(temp1.aux),country = Cnamesbase,Cdatabase)
      }else{
        # temp1.aux <- temp1[match(gbase[[nbelong]],names(temp1)),]# Data in EC group
        solbase <- data.frame(countrycode = names(temp1.aux),country = Cnamesbase,Cdatabase)
        
      }
      # cat("Control 140\n")
      # ST: cleaning outliers inside group
      spe.norm.aux <- decostand(temp1.aux, "normalize")
      
      # cat("Control 141\n")
      if(length(target.vars)>1){
        centers <- spe.norm.aux[which(kmodel$medoids[nbelong]==rownames(spe.norm.aux)) ,]
        # cat("Control 141",str(spe.norm.aux),"\n")
        distances <- sqrt(rowSums((spe.norm.aux - (centers))^2))
        outliers <- order(distances, decreasing=T)[1:round(.2*nrow(solbase))]
        nnn <- rownames(spe.norm.aux[-outliers,]) #names of members of ecuador group
        if(!pais%in%nnn) {nnn =  c(pais,nnn)}
        # outliers
        g.fin <- solbase[solbase$countrycode%in%nnn,]
      }else{
        # outliers TO_DO: clear outliers with one variable
        nnn <- rownames(spe.norm.aux) #names of members of ecuador group
        g.fin <- solbase[solbase$countrycode%in%nnn,]
        names(g.fin)[3] <- target.vars# para cambiar el nombre de los valors al de la variable input
      }
      
      
      
      
      # END: cleaning outliers inside group
      # g.fin = solbase # delete when outlieres are taken into account
      g.fin$anio = yrs[j]

      # print(yrs[j])
      results[[contador]] <- g.fin
      contador <- contador+1
    }
    
    Ksol = data.frame(  countrycode = rownames(Ksol), Cat = Ksol[,1],year =  Ksol[,2])
    # cat("\n results:--- ",str(results),"\n")
    # cat("Ksol:--- ",str(Ksol),"\n")
    names(results) <- yrs[init:length(yrs)]
    mapdata <- get_data_from_map(download_map_data1("custom/world-palestine-highres"))
    # load("data/mapdata.RData")
    # mapdata <- get_data_from_map(mapdata)
    
    # cat("results:---",(results),"\n")
    sumdat <- summary(datos[,target.vars])
    return(list(datos = datos,target.vars=target.vars,
                results = results,mapdata = mapdata,ECk = nbelong, Clus = Ksol,sumdat=sumdat))
    
  })
  
  
  output$summary <- renderDataTable({
    if(!input$checkres1)
    {
    J = (df_shiny()$results[input$yrs][[1]])
    
      # cat("----",str(J),"\n")
      datatable(J,rownames = F,class = 'cell-border stripe',filter = "top",
                options = list(columnDefs = list(list(className = 'dt-center', targets = 2:ncol(J)-1))))%>%
        formatRound(df_shiny()$target.vars,2)
      
      }
      
  })
  
  
  output$sumdata <- renderPrint({
    if(!input$checkres1)
    {
     df_shiny()$sumdat
    }
    
  })
  
  output$tabAll <- renderDataTable({
    
    # if(input$checkres)
    {
      
      J = bind_rows(df_shiny()$results, .id = "year")
      J = datatable(J,rownames = F,class = 'cell-border stripe',filter = "top",
                options = list(columnDefs = 
                      list(list(className = 'dt-center', targets = 2:ncol(J)-1))))%>%
        formatRound(df_shiny()$target.vars,2)
      
      J
      
    }
    })
  
  losDatos <- reactive( #All groupings
    {
      J = bind_rows(df_shiny()$results, .id = "year")
      # J = datatable(J,rownames = F,class = 'cell-border stripe',filter = "top",
      #               options = list(columnDefs =
      #                                list(list(className = 'dt-center', targets = 2:ncol(J)-1))))%>%
      #   formatRound(df_shiny()$target.vars,2)
      return(J)
    }
  )
  
  CT <- reactive( #Clusters in time
    {
      df2 = table(df_shiny()$Clus$year,df_shiny()$Clus$Cat)
      df2 = as.data.frame(as.data.frame.matrix(df2))
      df2$year = rownames(df2)
      
      
      return(df2)
      
    }
  )
  
  
  output$downLoadFilter <- downloadHandler(
    
    # J = bind_rows(df_shiny()$results, .id = "year"),
    
    
    
    filename = function() {
      paste('AllGroupings-', Sys.Date(), '.csv', sep = '')
    },
    content = function(file){
      write.csv(losDatos(),file)
    }
  )
  
  output$downLoadFilter1 <- downloadHandler(
    
    filename = function() {
      paste('ClustersTime-', Sys.Date(), '.csv', sep = '')
    },
    content = function(file){
      write.csv(CT(),file)
    }
  )
  
  output$networkPlot <- renderHighchart (
    {
      
      if(!is.null(df_shiny()))
      {
        #write all R-code inside this
        
        pais = dataset[dataset$countrycode==input$ctry,2]
        
        mapdata = df_shiny()$mapdata
        df1 = data.frame ( isoMap = mapdata$`iso-a3`)
        df2 = df_shiny()$Clus
        df2 = df2[which(df2$year == input$yrs),]
        names(df2) = c("isoMap","Cat","year")
        dfVal = merge(df1,df2, by.x = "isoMap",all.x = TRUE)
        dfVal$Cat[is.na(dfVal$Cat)] = 0 # Se asigna 0 a los NA
        
        nnp1 = dfVal$Cat[dfVal$isoMap == input$ctry]
        
        aa = sort(unique(dfVal$Cat))
        ccol = c(aa,aa[length(aa)]+1)
        # ccol = c(0,aa)
        
        stops <- data.frame(
          name = c(aa),
          from = c(ccol[-length(ccol)]),
          to = c(ccol[-1])
          ,stringsAsFactors = FALSE
        )
        
        stops <- list_parse(stops)
        
        data_ecuador <- mapdata %>% 
          select(PROVINCIA = `iso-a3`) %>% arrange(PROVINCIA)%>%
          mutate(X = 1:213,Cat = dfVal$Cat)
        
        hcmap1("custom/world-palestine-highres", data = data_ecuador, value = "Cat",
              joinBy = c("iso-a3", "PROVINCIA"),
              dataLabels = list(enabled = TRUE, format = '{point.name}'),
              borderColor = "lightblue", borderWidth = 0.5,
              tooltip = list(valueDecimals = 2, valuePrefix = "Clus Nº ")
        ) %>%
          hc_mapNavigation(enabled = TRUE) %>%
          hc_title(text = paste(pais[1]," is in cluster: ",nnp1,sep = ""),
                   align = "center", style = list(color = "#000000", fontWeight = "bold"))%>%
          hc_colorAxis(dataClasses = stops,dataClassColor = "category")%>%
          hc_legend(layout = "horizontal", align = "bottom", valueDecimals = 2)  
        
      }
    
  }) # end networkPlot 
  
  output$ClusSummary <- renderDataTable ({
    
    #write all R-code inside this
    df2 = table(df_shiny()$Clus$year,df_shiny()$Clus$Cat)
    df2 = as.data.frame(as.data.frame.matrix(df2))
    df2$year = rownames(df2)

    J = df2
    datatable(J,rownames = F,class = 'cell-border stripe',filter = "top",
              options = list(columnDefs = list(list(className = 'dt-center', targets = 2:ncol(J)-1))))%>%
      formatRound(colnames(df2),0)
    })
  # End R-session when browser closed
  session$onSessionEnded(stopApp)
}
