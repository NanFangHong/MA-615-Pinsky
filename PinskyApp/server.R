library(shiny)
library(shinydashboard)
library(tidyverse)
library(quantmod)
library(data.table)
library(grid)
library(gridExtra)
library(cluster)
library(Rtsne)
library(DT)
#source('https://s3.us-east-2.amazonaws.com/nanfang/intraday-data.R')

tidyHits <-
  fread(
    'https://s3.us-east-2.amazonaws.com/nanfang/TidyHits.csv',
    header = F,
    sep = ','
  ) %>% as.tibble() %>% rename(
    Year = V1,
    Hour = V2,
    BotTop = V3,
    Hit = V4,
    Symbol = V5
  )
companyProfile <-
  fread(
    'https://s3.us-east-2.amazonaws.com/nanfang/companyProfile.csv',
    header = T,
    sep = ','
  ) %>% as.tibble()

shinyServer(function(input, output) {
  
  output$realtime_plot <- renderPlot({
    Symbol = input$symbol_conclusion
    Medoid = input$medoid_conclusion
    

    f.get.google.intraday <- function(symbol, freq, period) {
      base.url <- 'http://www.google.com/finance/getprices?'
      options.url <- paste('i=', freq, '&p=', period, '&f=d,o,h,l,c,v&df=cpct&q=', symbol, sep = '')
      full.url <- paste(base.url, options.url, sep = '')
      
      data <- read.csv(full.url, skip = 7, header = FALSE, stringsAsFactors = FALSE)
      
      starting.times.idx <- which(substring(data$V1, 1, 1) == 'a')
      ending.seconds.idx <- c(starting.times.idx[-1] - 1, nrow(data))
      r.str.idx.use <- paste(starting.times.idx, ':', ending.seconds.idx, sep = '')
      
      starting.times <- as.numeric(substring(data[starting.times.idx, 1], 2))
      
      data[starting.times.idx, 1] <- 0
      clean.idx <- do.call(c, lapply(seq(1, length(r.str.idx.use)),
                                     function(i) {
                                       starting.times[i] + freq * as.numeric(data[eval(parse(text = r.str.idx.use[i])), 1])
                                     })
      )
      data.xts <- xts(data[,-1], as.POSIXct(clean.idx, origin = '1970-01-01', tz = 'GMT'))
      
      indexTZ(data.xts) <- 'America/New_York'
      colnames(data.xts) <- c('Open', 'High', 'Low', 'Close', 'Volume')
      
      data.xts
    }
    

    ticker <- Symbol
    
    #High
    
    intraday_3d <- f.get.google.intraday(ticker, 60 * 30, '3d') 
    
    symbol.intraday <- intraday_3d %>% as.tibble()
    symbol.intraday.filter <- symbol.intraday %>% mutate(Hour = index(intraday_3d), Minute = hms::as.hms(index(intraday_3d))) %>% filter(lubridate::date(Hour) == lubridate::today()) %>% filter(Minute > hms::as.hms("10:15:00"))
    i <- 1
    while (nrow(symbol.intraday.filter) == 0 ){
      symbol.intraday.filter <- symbol.intraday %>% mutate(Hour = index(intraday_3d)) %>% filter(lubridate::date(Hour) == (lubridate::today() - lubridate::days(i)))
      i <- i + 1
    }
    symbol.intraday <- symbol.intraday.filter
    symbol.intraday <- symbol.intraday[3:nrow(symbol.intraday),]
    Now_Price <- tail(symbol.intraday, n =1)$Close %>% as.double()
    Now_Hour <- nrow(symbol.intraday)
    Rest_of_Hours <- 12 - Now_Hour
    
    
    sd.High <- sqrt(var(intraday_3d$High) * Rest_of_Hours) 
    High.So_far_Price <- max(symbol.intraday$High)
    High.So_far_Hour <- which.max(symbol.intraday$High)  
    Adjusted_High_So_Far_P <- pnorm(High.So_far_Price, mean = Now_Price, sd = sd.High)
    Adjusted_Sum_of_rest_of_PMF <- 1 - Adjusted_High_So_Far_P 
    quote <- tidyHits %>% filter(Symbol == Medoid)
    TotalHit <-
      quote %>% filter(BotTop == 'High') %>% summarise(TotalCase = sum(Hit)) %>% as.integer()
    quoteHigh <-
      quote %>% filter(BotTop == 'High') %>% group_by(Hour) %>% summarise(HighHit = sum(Hit)) %>% mutate(HighHitPMF = HighHit / TotalHit)
    Adjusted_PMF_High <- quoteHigh %>% pull(HighHitPMF)
    Adjusted_PMF_High[1:Now_Hour] <- 0
    Adjusted_PMF_High[High.So_far_Hour] <- Adjusted_High_So_Far_P
    Sum_of_rest_of_PMF <- sum(Adjusted_PMF_High[(High.So_far_Hour + 1):length(Adjusted_PMF_High)])
    if (Sum_of_rest_of_PMF == 0){
      Adjusted_PMF_High[High.So_far_Hour] <- 1
    }else {
      Adjusted_PMF_High[(High.So_far_Hour + 1):length(Adjusted_PMF_High)] <- (Adjusted_Sum_of_rest_of_PMF * Adjusted_PMF_High[(High.So_far_Hour + 1):length(Adjusted_PMF_High)]) / Sum_of_rest_of_PMF
    }
    
    
    #Low
    sd.Low <- sqrt(var(intraday_3d$Low) * Rest_of_Hours) 
    Low.So_far_Price <- min(symbol.intraday$Low)
    Low.So_far_Hour <- which.min(symbol.intraday$Low)  
    Adjusted_Low_So_Far_P <- 1 - pnorm(Low.So_far_Price, mean = Now_Price, sd = sd.Low)
    Adjusted_Sum_of_rest_of_PMF <- 1 - Adjusted_Low_So_Far_P 
    TotalHit <-
      quote %>% filter(BotTop == 'Low') %>% summarise(TotalCase = sum(Hit)) %>% as.integer()
    quoteLow <-
      quote %>% filter(BotTop == 'Low') %>% group_by(Hour) %>% summarise(LowHit = sum(Hit)) %>% mutate(LowHitPMF = LowHit / TotalHit)
    Adjusted_PMF_Low <- quoteLow %>% pull(LowHitPMF)
    Adjusted_PMF_Low[1:Now_Hour] <- 0
    Adjusted_PMF_Low[Low.So_far_Hour] <- Adjusted_Low_So_Far_P
    Sum_of_rest_of_PMF <- sum(Adjusted_PMF_Low[(Low.So_far_Hour + 1):length(Adjusted_PMF_Low)])
    if (Sum_of_rest_of_PMF == 0){
      Adjusted_PMF_Low[Low.So_far_Hour] <- 1
    }else {
      Adjusted_PMF_Low[(Low.So_far_Hour + 1):length(Adjusted_PMF_Low)] <- (Adjusted_Sum_of_rest_of_PMF * Adjusted_PMF_Low[(Low.So_far_Hour + 1):length(Adjusted_PMF_Low)]) / Sum_of_rest_of_PMF
    }
    
    
    
    
    
    
    if (nrow(symbol.intraday) < 12){
      symbol.intraday.addNA <- symbol.intraday %>% select(High, Low) %>% mutate(Hour = c(1:nrow(symbol.intraday)))
      for (i in (nrow(symbol.intraday)+1):12){
        print(i)
        symbol.intraday.addNA <- add_row(symbol.intraday.addNA, High = c(NA), Low = c(NA), Hour = c(i))
      }
      symbol.intraday.addNA <- symbol.intraday.addNA %>% mutate(PMF = Adjusted_PMF_High)
    }else {
      symbol.intraday.addNA <- symbol.intraday %>% select(High, Low) %>% mutate(PMF = Adjusted_PMF_High, Hour = c(1:12))
    }
    
    
    
    
    ymin_High = min(symbol.intraday.addNA$High, na.rm=TRUE)
    ymax_High = max(symbol.intraday.addNA$High, na.rm=TRUE)
    p1 <- ggplot(symbol.intraday.addNA, aes(Hour))  + 
      geom_rect(aes(xmin = 1 - 0.5, xmax = 1 + 0.5, ymin = ymin_High, ymax = ymax_High), alpha = log(Adjusted_PMF_High[1]+1)*0.3) + 
      geom_rect(aes(xmin = 2 - 0.5, xmax = 2 + 0.5, ymin = ymin_High, ymax = ymax_High), alpha = log(Adjusted_PMF_High[2]+1)*0.3)+ 
      geom_rect(aes(xmin = 3 - 0.5, xmax = 3 + 0.5, ymin = ymin_High, ymax = ymax_High), alpha = log(Adjusted_PMF_High[3]+1)*0.3)+ 
      geom_rect(aes(xmin = 4 - 0.5, xmax = 4 + 0.5, ymin = ymin_High, ymax = ymax_High), alpha = log(Adjusted_PMF_High[4]+1)*0.3)+ 
      geom_rect(aes(xmin = 5 - 0.5, xmax = 5 + 0.5, ymin = ymin_High, ymax = ymax_High), alpha = log(Adjusted_PMF_High[5]+1)*0.3)+ 
      geom_rect(aes(xmin = 6 - 0.5, xmax = 6 + 0.5, ymin = ymin_High, ymax = ymax_High), alpha = log(Adjusted_PMF_High[6]+1)*0.3)+ 
      geom_rect(aes(xmin = 7 - 0.5, xmax = 7 + 0.5, ymin = ymin_High, ymax = ymax_High), alpha = log(Adjusted_PMF_High[7]+1)*0.3)+ 
      geom_rect(aes(xmin = 8 - 0.5, xmax = 8 + 0.5, ymin = ymin_High, ymax = ymax_High), alpha = log(Adjusted_PMF_High[8]+1)*0.3)+ 
      geom_rect(aes(xmin = 9 - 0.5, xmax = 9 + 0.5, ymin = ymin_High, ymax = ymax_High), alpha = log(Adjusted_PMF_High[9]+1)*0.3)+ 
      geom_rect(aes(xmin = 10 - 0.5, xmax = 10 + 0.5, ymin = ymin_High, ymax = ymax_High), alpha = log(Adjusted_PMF_High[10]+1)*0.3)+ 
      geom_rect(aes(xmin = 11 - 0.5, xmax = 11 + 0.5, ymin = ymin_High, ymax = ymax_High), alpha = log(Adjusted_PMF_High[11]+1)*0.3)+ 
      geom_rect(aes(xmin = 12 - 0.5, xmax = 12 + 0.5, ymin = ymin_High, ymax = ymax_High), alpha = log(Adjusted_PMF_High[12]+1)*0.3)+
      geom_point(aes(y = High), size = 5, colour = "deepskyblue") + ggtitle(Symbol) + 
      labs(x = 'Every 30-mins intervals from 10:00 A.M.',
           y = 'Current Price and Predicted High Hits')
    
    ymin_Low = min(symbol.intraday.addNA$Low, na.rm=TRUE)
    ymax_Low = max(symbol.intraday.addNA$Low, na.rm=TRUE)
    p2 <- ggplot(symbol.intraday.addNA, aes(Hour))  + 
      geom_rect(aes(xmin = 1 - 0.5, xmax = 1 + 0.5, ymin = ymin_Low, ymax = ymax_Low), alpha = log(Adjusted_PMF_Low[1]+1)*0.3) + 
      geom_rect(aes(xmin = 2 - 0.5, xmax = 2 + 0.5, ymin = ymin_Low, ymax = ymax_Low), alpha = log(Adjusted_PMF_Low[2]+1)*0.3)+ 
      geom_rect(aes(xmin = 3 - 0.5, xmax = 3 + 0.5, ymin = ymin_Low, ymax = ymax_Low), alpha = log(Adjusted_PMF_Low[3]+1)*0.3)+ 
      geom_rect(aes(xmin = 4 - 0.5, xmax = 4 + 0.5, ymin = ymin_Low, ymax = ymax_Low), alpha = log(Adjusted_PMF_Low[4]+1)*0.3)+ 
      geom_rect(aes(xmin = 5 - 0.5, xmax = 5 + 0.5, ymin = ymin_Low, ymax = ymax_Low), alpha = log(Adjusted_PMF_Low[5]+1)*0.3)+ 
      geom_rect(aes(xmin = 6 - 0.5, xmax = 6 + 0.5, ymin = ymin_Low, ymax = ymax_Low), alpha = log(Adjusted_PMF_Low[6]+1)*0.3)+ 
      geom_rect(aes(xmin = 7 - 0.5, xmax = 7 + 0.5, ymin = ymin_Low, ymax = ymax_Low), alpha = log(Adjusted_PMF_Low[7]+1)*0.3)+ 
      geom_rect(aes(xmin = 8 - 0.5, xmax = 8 + 0.5, ymin = ymin_Low, ymax = ymax_Low), alpha = log(Adjusted_PMF_Low[8]+1)*0.3)+ 
      geom_rect(aes(xmin = 9 - 0.5, xmax = 9 + 0.5, ymin = ymin_Low, ymax = ymax_Low), alpha = log(Adjusted_PMF_Low[9]+1)*0.3)+ 
      geom_rect(aes(xmin = 10 - 0.5, xmax = 10 + 0.5, ymin = ymin_Low, ymax = ymax_Low), alpha = log(Adjusted_PMF_Low[10]+1)*0.3)+ 
      geom_rect(aes(xmin = 11 - 0.5, xmax = 11 + 0.5, ymin = ymin_Low, ymax = ymax_Low), alpha = log(Adjusted_PMF_Low[11]+1)*0.3)+ 
      geom_rect(aes(xmin = 12 - 0.5, xmax = 12 + 0.5, ymin = ymin_Low, ymax = ymax_Low), alpha = log(Adjusted_PMF_Low[12]+1)*0.3)+
      geom_point(aes(y = Low), size = 5, colour = "deepskyblue") + ggtitle(Symbol) + 
      labs(x = 'Every 30-mins intervals from 10:00 A.M.',
           y = 'Current Price and Predicted Low Hits')
    
    
    grid.arrange(p1, p2, ncol = 1)
    

  })
  
  output$relation_plot <- renderPlot({
    print(dataInput)
  })
  
  output$table <- DT::renderDataTable(DT::datatable({
    data <- companyProfile
    if (input$table_continent != "All") {
      print(1)
      data <- data %>% filter(Continent == input$table_continent)
      print(2)
      print(data)
    }
    if (input$table_country != "All") {
      data <- data %>% filter(Country == input$table_country)
    }
    if (input$table_sector != "All") {
      data <- data %>% filter(Sector == input$table_sector)
    }
    if (input$table_industry != "All") {
      data <- data %>% filter(Industry == input$table_indutry)
    }
    print(data)
    data
  }, options = list(scrollX = TRUE)))
  
  output$histogram_plot <- renderPlot({
    quote <-
      tidyHits %>% filter(Symbol == input$symbol &
                            Year >= input$range[1] &
                            Year <= input$range[2])
    TotalHit <-
      quote %>% filter(BotTop == 'High') %>% summarise(TotalCase = sum(Hit)) %>% as.integer()
    quoteHigh <-
      quote %>% filter(BotTop == 'High') %>% group_by(Hour) %>% summarise(HighHit = sum(Hit)) %>% mutate(HighHitPMF = HighHit / TotalHit)
    quoteLow <-
      quote %>% filter(BotTop == 'Low') %>% group_by(Hour) %>% summarise(LowHit = sum(Hit)) %>% mutate(LowHitPMF = LowHit / TotalHit)
    quoteHigh_minus_Low <-
      quoteHigh %>% left_join(quoteLow, by = c("Hour")) %>% mutate(High_minus_Low = HighHit - LowHit) %>% select(Hour, High_minus_Low)
    p1 <- ggplot(quoteHigh, aes(Hour, HighHit)) + geom_col()
    p2 <- ggplot(quoteLow, aes(Hour, LowHit)) + geom_col()
    p3 <-
      ggplot(quoteHigh_minus_Low, aes(Hour, High_minus_Low)) + geom_col()
    grid.arrange(p1, p2, p3, ncol = 1)
  })
  
  output$Bhat_explanation <- renderUI({
    HTML(
      "
      <h3>Intraday Stock Behavior and Top/Bottom Hits Distribution</h3><br/>
      A stock price will hit the top <strong>(High hits)</strong> and the bottom <strong>(Low hits)</strong> during a day. 
      If we divide a day into twelve <strong>30-min</strong> intervals, and through historical data
      we count the High hits and Low hits falling in corresponding intervals,
      we can get High hits distribution and Low hits distribution. <br/><br/>
      If we let those distributions of different stocks cluster by themselves, 
      there are some chances that we get a cluster where members are <strong>proportionally unusual</strong>.
      If that is the case, we can dig out the story behind this cluster, and design a trading strategy. 
      <h3>Measuring Dissimilarity Using Bhattacharyya Distance in Cluster Analysis</h3><br/>"
    )
  })
  
  output$PAM_explanation <- renderUI({
    HTML(
      "
      
      <br/><h3>Clustering Analysis Using PAM</h3>
      <br/>Now that the distance matrix has been calculated, it is time to select an algorithm for clustering. While many algorithms that can handle a custom distance matrix exist, <strong>partitioning around medoids (PAM)</strong> will be used here.
      
      <br/><br/>Partitioning around medoids is an iterative clustering procedure with the following steps:
      
      <br/><br/>1.Choose k random entities to become the medoids:
      <br/><br/>2.Assign every entity to its closest medoid (using our custom distance matrix in this case)
      <br/><br/>3.For each cluster, identify the observation that would yield the lowest average distance if it were to be re-assigned as the medoid. If so, make this observation the new medoid.
      <br/><br/>4.If at least one medoid has changed, return to step 2. Otherwise, end the algorithm."
    )
  })
  
  output$Silhouette_explanation <- renderUI({
    HTML(
      "<br/><h3>Evaluate Clustering Result Using Silhouette Width</h3><br/>A variety of metrics exist to help optimize the number of clusters to be extracted in a cluster analysis. We will use <strong>silhouette width</strong>, an internal validation metric which is an aggregated measure of how similar an observation is to its own cluster compared its closest neighboring cluster. The metric can range from -1 to 1, where <strong>higher values are better</strong>."
    )
  })
  
  output$tSNE_explanation <- renderUI({
    HTML(
      "<br/><h3>Visualization Through Reducing Dimensions</h3><br/>One way to visualize many variables in a lower dimensional space is with <strong>t-distributed stochastic neighborhood embedding (t-SNE)</strong>. This method is a dimension reduction technique that tries to preserve local structure so as to make clusters visible in a 2D or 3D visualization. While it typically utilizes Euclidean distance, it has the ability to handle a custom distance metric like the one we created above."
    )
  })
  
  pam_Reactive <- eventReactive({
    input$clusterNumber
    input$range_cluster
    input$HighLow
  }, {
    filename <-
      paste(
        'https://s3.us-east-2.amazonaws.com/nanfang/PinskyDisMat/start%3D',
        input$range_cluster[1],
        '%26end%3D',
        input$range_cluster[2],
        '%26HighLow%3D',
        input$HighLow,
        '.csv',
        sep = ''
      )
    DisMat <- fread(filename, sep = ',') %>% as.matrix()
    rownames(DisMat) <- colnames(DisMat)
    set.seed(2018)
    pam_fit <- pam(x = DisMat,
                   diss = TRUE,
                   k = input$clusterNumber)
    list(DisMat = DisMat, pam_fit = pam_fit)
  })
  
  
  output$cluster_plot <- renderPlot({
    DisMat <- pam_Reactive()$DisMat
    pam_fit <- pam_Reactive()$pam_fit
    tsne_obj <-
      Rtsne(
        DisMat,
        is_distance = TRUE,
        perplexity = nrow(DisMat) %% input$clusterNumber
      )
    tsne_data <- tsne_obj$Y %>%
      data.frame() %>%
      setNames(c("X", "Y")) %>%
      mutate(cluster = factor(pam_fit$clustering),
             name = names(pam_fit$clustering))
    
    #
    #plot(pam_fit)
    
    ggplot(aes(x = X, y = Y), data = tsne_data) +
      geom_point(aes(color = cluster)) +
      labs(x = 't-SNE X (Visualization Purpose Only)',
           y = 't-SNE Y (Visualization Purpose Only)') +
      ggtitle(
        paste(
          'k =',
          input$clusterNumber,
          'Histogram Type =',
          input$HighLow,
          ' Range: ',
          input$range_cluster[1],
          ' to ',
          input$range_cluster[2],
          sep = ' '
        )
      )
    
    
  })
  
  output$symbol_in_cluster <- renderText({
    pam_fit <- pam_Reactive()$pam_fit
    clustering <- pam_fit$clustering %>% as.matrix()
    clustering <-
      tibble(Symbol = rownames(clustering), Cluster = clustering[, 1])
    symbolList <- clustering %>% filter(Cluster == input$clusterChoice) %>% pull(Symbol) %>% paste(collapse = ';')
    paste('Medoid: ', pam_fit$medoids[input$clusterChoice], '  Members in cluster: ', symbolList, sep = '')
  })
  
  
  output$total_continent_plot <- renderPlot({
    pam_fit <- pam_Reactive()$pam_fit
    clustering <- pam_fit$clustering %>% as.matrix()
    clustering <-
      tibble(Symbol = rownames(clustering), Cluster = clustering[, 1])
    
    p1 <-
      companyProfile %>% count(Continent) %>% ggplot(aes(x = "", y = n, fill =
                                                           Continent)) +
      geom_bar(width = 1, stat = "identity") + coord_polar("y", start =
                                                             0) +
      scale_fill_manual(
        values = c(
          "Asia" = "tomato",
          "Austrialia and South Pacific" = "chocolate",
          "Central America" = "yellowgreen",
          "Europe" = "limegreen",
          "Middle East" = "cyan3",
          "North America" = "deepskyblue",
          "South America" = "slateblue"
        )
      ) + ggtitle("Total Proportion") + 
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "bottom"
      )
    g_legend <- function(a.gplot) {
      tmp <- ggplot_gtable(ggplot_build(a.gplot))
      leg <-
        which(sapply(tmp$grobs, function(x)
          x$name) == "guide-box")
      legend <- tmp$grobs[[leg]]
      return(legend)
    }
    mylegend <- g_legend(p1)
    
    p2 <-
      clustering %>% filter(Cluster == input$clusterChoice) %>% inner_join(companyProfile, by = c("Symbol")) %>% count(Continent) %>% ggplot(aes(x =
                                                                                                                                                   "", y = n, fill = Continent)) +
      geom_bar(width = 1, stat = "identity") + coord_polar("y", start =
                                                             0) +
      scale_fill_manual(
        values = c(
          "Asia" = "tomato",
          "Austrialia and South Pacific" = "chocolate",
          "Central America" = "yellogreen",
          "Europe" = "limegreen",
          "Middle East" = "cyan3",
          "North America" = "deepskyblue",
          "South America" = "slateblue"
        )
      ) + ggtitle(paste("Proportion in cluster ", input$clusterChoice, sep = '')) + 
      theme(axis.title.x = element_blank(), axis.title.y = element_blank()) 
    
    p3 <-
      grid.arrange(
        arrangeGrob(
          p1 + theme(legend.position = "none"),
          p2 + theme(legend.position = "none"),
          nrow = 1
        ),
        mylegend,
        nrow = 2,
        heights = c(3, 2)
      )
    
    p3
  })
  
  output$total_sector_plot <- renderPlot({
    pam_fit <- pam_Reactive()$pam_fit
    clustering <- pam_fit$clustering %>% as.matrix()
    clustering <-
      tibble(Symbol = rownames(clustering), Cluster = clustering[, 1])
    
    p1 <-
      companyProfile %>% count(Sector) %>% ggplot(aes(x = "", y = n, fill = Sector)) +
      geom_bar(width = 1, stat = "identity") + coord_polar("y", start =
                                                             0) +
      scale_fill_manual(
        values = c(
          "Basic Industries" = "tomato",
          "Capital Goods" = "chocolate",
          "Consumer Durables" = "yellowgreen",
          "Consumer Non-Durables" = "chartreuse3",
          "Consumer Services" = "limegreen",
          "Energy" = "springgreen3",
          "Finance" = "cyan3",
          "Health Care" = "deepskyblue",
          "Miscellaneous" = "slateblue",
          "Public Utilities" = "darkorchid",
          "Technology" = "orchid",
          "Transportation" = "hotpink"
        )
      ) + ggtitle("Total Proportion") + 
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "bottom"
      )
    g_legend <- function(a.gplot) {
      tmp <- ggplot_gtable(ggplot_build(a.gplot))
      leg <-
        which(sapply(tmp$grobs, function(x)
          x$name) == "guide-box")
      legend <- tmp$grobs[[leg]]
      return(legend)
    }
    mylegend <- g_legend(p1)
    
    p2 <-
      clustering %>% filter(Cluster == input$clusterChoice) %>% inner_join(companyProfile, by = c("Symbol")) %>% count(Sector) %>% ggplot(aes(x =
                                                                                                                                                "", y = n, fill = Sector)) +
      geom_bar(width = 1, stat = "identity") + coord_polar("y", start =
                                                             0) +
      scale_fill_manual(
        values = c(
          "Basic Industries" = "tomato",
          "Capital Goods" = "chocolate",
          "Consumer Durables" = "yellowgreen",
          "Consumer Non-Durables" = "chartreuse3",
          "Consumer Services" = "limegreen",
          "Energy" = "springgreen3",
          "Finance" = "cyan3",
          "Health Care" = "deepskyblue",
          "Miscellaneous" = "slateblue",
          "Public Utilities" = "darkorchid",
          "Technology" = "orchid",
          "Transportation" = "hotpink"
        )
      ) + ggtitle(paste("Proportion in cluster ", input$clusterChoice, sep = '')) +
      theme(axis.title.x = element_blank(), axis.title.y = element_blank())
    
    p3 <-
      grid.arrange(
        arrangeGrob(
          p1 + theme(legend.position = "none"),
          p2 + theme(legend.position = "none"),
          nrow = 1
        ),
        mylegend,
        nrow = 2,
        heights = c(3, 2)
      )
    
    p3
  })
  
  output$all_cluster_plot <- renderPlot({
    # Read:https://s3.us-east-2.amazonaws.com/nanfang/PinskyDisMat/start%3D1991%26end%3D1996%26HighLow%3DHigh-Low.csv
    filename <-
      paste(
        'https://s3.us-east-2.amazonaws.com/nanfang/PinskyDisMat/start%3D',
        input$range_all_cluster[1],
        '%26end%3D',
        input$range_all_cluster[2],
        '%26HighLow%3D',
        input$HighLow_all_cluster,
        '.csv',
        sep = ''
      )
    #print(filename)
    #filename <- 'https://s3.us-east-2.amazonaws.com/nanfang/PinskyDisMat/start%3D1991%26end%3D1996%26HighLow%3DHigh-Low.csv'
    DisMat <- fread(filename, sep = ',') %>% as.matrix()
    rownames(DisMat) <- colnames(DisMat)
    set.seed(2018)
    
    avg_width <-
      tibble(
        k = 1:input$clusterMaxNumber,
        avg_width = rep(0, times = input$clusterMaxNumber)
      )
    for (i in 1:input$clusterMaxNumber) {
      try({
        pam_fit <- pam(x = DisMat,
                       diss = TRUE,
                       k = i)
        avg_width[i, 2] <- pam_fit$silinfo$avg.width
      }, TRUE)
    }
    
    ggplot(aes(x = k, y = avg_width), data = avg_width) +
      geom_point() +
      labs(x = 'Cluster Number k (invalid if more than data points)',
           y = 'Average Silhouette Width (the higher the better)') +
      ggtitle(
        paste(
          'Maximum k =',
          input$clusterMaxNumber,
          'Histogram Type =',
          input$HighLow_all_cluster,
          ' Range: ',
          input$range_all_cluster[1],
          ' to ',
          input$range_all_cluster[2],
          sep = ' '
        )
      )
    
  })
  
  
})
