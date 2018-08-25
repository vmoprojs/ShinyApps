source("app.R")
# Define a server for the Shiny app
function(input, output) {
  
  # Fill in the spot we created for a plot
  output$indicatorPlotA <- renderPlot({

    # Render a barplot

    fil2 <- base$indicator==input$indicator &  base$country==input$countryA
    fil3 <- replicas.sd$indicator==input$indicator &  replicas.sd$country==input$countryA
    
    fil2B <- base$indicator==input$indicator &  base$country==input$countryB
    fil3B <- replicas.sd$indicator==input$indicator &  replicas.sd$country==input$countryB

    val <- log(base[fil2,"value"])
    
    sup <- val+1*exp(replicas.sd[fil3,"value"])
    inft <- val-1*exp(replicas.sd[fil3,"value"])
    
    anioA <- (base[fil2,"year"])
    
    
    valB <- log(base[fil2B,"value"])
    anioB <- (base[fil2B,"year"])
    supB <- valB+1*exp(replicas.sd[fil3B,"value"])
    inftB <- valB-1*exp(replicas.sd[fil3B,"value"])
    
    
    dat.aux <- data.frame(inferior = inft,mean = val,superior = sup)
    dat.auxB <- data.frame(inferior = inftB,mean = valB,superior = supB)
    # print(valB)

    yrange <- range(c(as.vector(dat.aux),as.vector(dat.auxB)))
    # nn <- as.character(paises[match(pais,input$country),"Nombre"])
    # if(length(anio)!=length(val))
    # {
    #   if(length(anio)>length(val)) 
    #   {
    #     dem <- rep(0,length(anio)>length(val))
    #   }
    # }
    
    
    plot(anioA,val,t = "l",main ="",ylim = yrange,xlim = range(input$time),
         xlab = "Años", ylab = input$indicator, col = "blue")
    lines(anioA,sup, col = "blue",lty = 2)
    lines(anioA,inft, col = "blue",lty = 2)
    
    if(length(anioA)==length(anioB))
    {
      lines(anioB,valB,t = "l",ylim = yrange, col = "red")
      lines(anioB,supB, col = "red",lty = 2)
      lines(anioB,inftB, col = "red",lty = 2)
      legend("topleft", legend=c(paste(input$countryA), paste(input$countryB)),
             col=c("blue", "red"), lty=1, cex=0.8, bty = "n")
    }else
    {
      lines(anioB,valB,t = "l",ylim = yrange, col = "red")
      lines(anioB,supB, col = "red",lty = 2)
      lines(anioB,inftB, col = "red",lty = 2)
      legend("topleft", legend=c(paste(input$countryA), paste(input$countryB)),
             col=c("blue", "red"), lty=1, cex=0.8, bty = "n")
    }
   
    
    
    
    # plot(anio,valB,t = "l",main = input$countryB,ylim = yrange,
    #      xlab = "Años", ylab = input$indicator)
    # lines(anio,supB, col = "blue",lty = 2)
    # lines(anio,inftB, col = "blue",lty = 2)

    # Determine the x values that will go into CI
    x <- anioA
    CI.x.top <- x # x values going forward
    CI.x.bot <- rev(x) # x values backwards
    CI.x <- c(CI.x.top, CI.x.bot) # polygons are drawn clockwise

    # Determine the Y values for CI
    y <- inft
    CI.y.top <- sup # top of CI
    CI.y.bot <- rev(inft) # bottom of CI, but rev Y!
    CI.y <- c(CI.y.top,CI.y.bot) # forward, then backward
    # Add a polygon for the CI
    CI.col <- adjustcolor("blue",alpha.f=0.25) # Pick a pretty CI color
    polygon(CI.x, CI.y, col=CI.col, border=NA) # draw the polygon
    
    if(length(anioA)==length(anioB))
    {
      # Determine the Y values for CI
      y <- inftB
      CI.y.topB <- supB # top of CI
      CI.y.botB <- rev(inftB) # bottom of CI, but rev Y!
      CI.yB <- c(CI.y.topB,CI.y.botB) # forward, then backward
      
      
      
      CI.colB <- adjustcolor("red",alpha.f=0.25) # Pick a pretty CI color
      polygon(CI.x, CI.yB, col=CI.colB, border=NA) # draw the polygon
    }else
    {
      # Determine the x values that will go into CI
      x <- anioB
      CI.x.top <- x # x values going forward
      CI.x.bot <- rev(x) # x values backwards
      CI.x <- c(CI.x.top, CI.x.bot) # polygons are drawn clockwise
      # Determine the Y values for CI
      y <- inftB
      CI.y.topB <- supB # top of CI
      CI.y.botB <- rev(inftB) # bottom of CI, but rev Y!
      CI.yB <- c(CI.y.topB,CI.y.botB) # forward, then backward
      
      
      
      CI.colB <- adjustcolor("red",alpha.f=0.25) # Pick a pretty CI color
      polygon(CI.x, CI.yB, col=CI.colB, border=NA) # draw the polygon
    }
    

  })
  
  output$pA <- renderPrint({
    fil11 <- replicas$indicator==input$indicator & replicas$year==input$year1 & replicas$country==input$countryA
    fil22 <- replicas$indicator==input$indicator & replicas$year==input$year2 & replicas$country==input$countryA
    if(sum(fil11)!=0 & sum(fil22)!=0)
    {
      xx <- exp(as.numeric(replicas[fil11,1:1000]))
      yy <- exp(as.numeric(replicas[fil22,1:1000]))
      xx <- c(xx,yy)
      ff <- rep(c(0,1),each=1000)
      # validate(
      #   need(length(xx)!=length(ff), "Please select a data set")
      # )
      dat <- data.frame(Valores = xx, Año = ff)
      wilcox.test( Valores~Año,paired = TRUE,data = dat)
    }
    
  })
  
  output$pB <- renderPrint({
    fil11 <- replicas$indicator==input$indicator & replicas$year==input$year1 & replicas$country==input$countryB
    fil22 <- replicas$indicator==input$indicator & replicas$year==input$year2 & replicas$country==input$countryB
    if(sum(fil11)!=0 & sum(fil22)!=0)
    {
      xx <- exp(as.numeric(replicas[fil11,1:1000]))
      yy <- exp(as.numeric(replicas[fil22,1:1000]))
      xx <- c(xx,yy)
      ff <- rep(c(0,1),each=1000)
      # validate(
      #   need(length(xx)!=length(ff), "Please select a data set")
      # )
      dat <- data.frame(Valores = xx, Año = ff)
      wilcox.test( Valores~Año,paired = TRUE,data = dat)
    }
    
  })
  
  output$summary <- renderTable({
    
    fil2 <- base$indicator==input$indicator &  base$country==input$countryA
    fil3 <- replicas.sd$indicator==input$indicator &  replicas.sd$country==input$countryA
    
    fil2B <- base$indicator==input$indicator &  base$country==input$countryB
    fil3B <- replicas.sd$indicator==input$indicator &  replicas.sd$country==input$countryB
    
    val <- log(base[fil2,"value"])
    anio <- (base[fil2,"year"])
    sup <- val+1*exp(replicas.sd[fil3,"value"])
    inft <- val-1*exp(replicas.sd[fil3,"value"])
    
    
    valB <- log(base[fil2B,"value"])
    # anio <- (base[fil2B,"year"])
    supB <- valB+1*exp(replicas.sd[fil3B,"value"])
    inftB <- valB-1*exp(replicas.sd[fil3B,"value"])
    
    # Muts <- mydata()
    # summary(Muts)
    # validate(
    #   need(length(inft)!=length(inftB), "Please select a data set")
    # )
    if(length(val)==length(valB))
    {
      # print("XXXXXXX")
      dat.tab <- data.frame(InfP1 = inft, VaP1 = val, SupP1 = sup,
                            InfP2 = inftB, VaP2 = valB, SupP2 = supB)
      rownames(dat.tab) <- anio
    }
    if(length(val)!=length(valB))
    {
      # print("YYYYYY")
      dat.tabA <- data.frame(Inferior = inft, Valor = val, Superior = sup)
      dat.tabA$Pais <- input$countryA
      
      dat.tabB <- data.frame(Inferior = inftB, Valor = valB, Superior = supB)
      dat.tabB$Pais <- input$countryB
      dat.tab <- rbind(dat.tabA,dat.tabB)
    }
    
   
    if( input$checkdata == TRUE)
    {
      print(dat.tab,digits = 3)
    }
    
    
  })
  
}