library(tidyverse)
library(quantmod)
library(data.table)
library(grid)
library(gridExtra)
library(cluster)
library(Rtsne)

tidyHits <- fread('./vector.csv', header = F, sep = ',') %>% as.tibble() %>% rename(Year = V1, Hour = V2, BotTop = V3, Hit = V4, Symbol = V5)

BhattacharyyaDistance <- function(PMF1, PMF2){
  return(- log(sum(sapply(1:length(PMF1), function(i) sqrt(PMF1[i] * PMF2[i])))))
}

DissimilarityMatrice <- function(cluster_quote, HighLow = 'High'){
  cluster_symbol <- cluster_quote %>% select(Symbol) %>% unique() %>% pull(Symbol)
  length <- length(cluster_symbol)
  DisMat = matrix(0, nrow = length, ncol = length)
  dimnames(DisMat) <- list(cluster_symbol, cluster_symbol)
  for (sym_i in cluster_symbol) {
    sym_i_index <- which(colnames(DisMat) == sym_i)
    filtered_quote_i <- cluster_quote %>% filter(Symbol == sym_i & BotTop == HighLow)
    TotalHit_i <- filtered_quote_i %>% summarise(TotalCase = sum(Hit)) %>% as.integer()
    HighLowPMF_i <- filtered_quote_i %>% group_by(Hour) %>% summarise(HighLowHit = sum(Hit)) %>% mutate(HighLowHitPMF = HighLowHit / TotalHit_i) %>% pull(HighLowHitPMF)
    
    for (sym_j in cluster_symbol){
      sym_j_index <- which(colnames(DisMat) == sym_j)
      if (sym_j_index == sym_i_index) {DisMat[sym_i, sym_j] <- 0}
      else if (sym_j_index < sym_i_index) {DisMat[sym_i, sym_j] <- DisMat[sym_j, sym_i]}
      else {
        filtered_quote_j <- cluster_quote %>% filter(Symbol == sym_j & BotTop == HighLow)
        TotalHit_j <- filtered_quote_j %>% summarise(TotalCase = sum(Hit)) %>% as.integer()
        HighLowPMF_j <- filtered_quote_j %>% group_by(Hour) %>% summarise(HighLowHit = sum(Hit)) %>% mutate(HighLowHitPMF = HighLowHit / TotalHit_j) %>% pull(HighLowHitPMF)
        DisMat[sym_i, sym_j] <- BhattacharyyaDistance(HighLowPMF_i, HighLowPMF_j)
      }
      
    }
  }
  return(DisMat)
}

DissimilarityMatrice_High_minus_Low <- function(cluster_quote){
  cluster_symbol <- cluster_quote %>% select(Symbol) %>% unique() %>% pull(Symbol)
  length <- length(cluster_symbol)
  DisMat = matrix(0, nrow = length, ncol = length)
  dimnames(DisMat) <- list(cluster_symbol, cluster_symbol)
  for (sym_i in cluster_symbol) {
    sym_i_index <- which(colnames(DisMat) == sym_i)
    filtered_quote_i <- cluster_quote %>% filter(Symbol == sym_i & BotTop == 'High')
    TotalHit_i <- filtered_quote_i %>% summarise(TotalCase = sum(Hit)) %>% as.integer()
    HighPMF_i <- filtered_quote_i %>% group_by(Hour) %>% summarise(HighLowHit = sum(Hit)) %>% mutate(HighLowHitPMF = HighLowHit / TotalHit_i) %>% pull(HighLowHitPMF)
    filtered_quote_i <- cluster_quote %>% filter(Symbol == sym_i & BotTop == 'Low')
    LowPMF_i <- filtered_quote_i %>% group_by(Hour) %>% summarise(HighLowHit = sum(Hit)) %>% mutate(HighLowHitPMF = HighLowHit / TotalHit_i) %>% pull(HighLowHitPMF)
    High_minus_Low_PMF_i <- HighPMF_i - LowPMF_i
    for (sym_j in cluster_symbol){
      sym_j_index <- which(colnames(DisMat) == sym_j)
      if (sym_j_index == sym_i_index) {DisMat[sym_i, sym_j] <- 0}
      else if (sym_j_index < sym_i_index) {DisMat[sym_i, sym_j] <- DisMat[sym_j, sym_i]}
      else {
        filtered_quote_j <- cluster_quote %>% filter(Symbol == sym_j & BotTop == 'High')
        TotalHit_j <- filtered_quote_j %>% summarise(TotalCase = sum(Hit)) %>% as.integer()
        HighPMF_j <- filtered_quote_j %>% group_by(Hour) %>% summarise(HighLowHit = sum(Hit)) %>% mutate(HighLowHitPMF = HighLowHit / TotalHit_j) %>% pull(HighLowHitPMF)
        filtered_quote_j <- cluster_quote %>% filter(Symbol == sym_j & BotTop == 'Low')
        LowPMF_j <- filtered_quote_j %>% group_by(Hour) %>% summarise(HighLowHit = sum(Hit)) %>% mutate(HighLowHitPMF = HighLowHit / TotalHit_j) %>% pull(HighLowHitPMF)
        High_minus_Low_PMF_j <- HighPMF_j - LowPMF_j
        DisMat[sym_i, sym_j] <- sum(abs(High_minus_Low_PMF_j - High_minus_Low_PMF_i))
        
      }
      
    }
  }
  return(DisMat)
}

for (start_year in c(1991:2016)){
  for (end_year in c(start_year:2016)){
    filtered_symbol <- tidyHits %>% filter(Year >= start_year & Year <= end_year) %>% 
      group_by(Symbol) %>% select(Year) %>% unique() %>% tally() %>% 
      filter(n == end_year - start_year + 1) %>% pull(Symbol)
    cluster_quote <- tidyHits %>% filter(Year >= start_year & Year <= end_year & Symbol %in% filtered_symbol)
    for (HighLow in c('High', 'Low', 'High-Low')){
      
      if (HighLow == 'High-Low') {
        DisMat <- DissimilarityMatrice_High_minus_Low(cluster_quote)
      }else{
        DisMat <- DissimilarityMatrice(cluster_quote, HighLow)
      }
      filename <- paste('./PinskyDisMat/start=', start_year, '&end=', end_year, '&HighLow=', HighLow, '.csv', sep = '')
      print(filename)
      DisMat %>% write.table(file=filename, 
                  sep=',', 
                  row.names=F, 
                  col.names=T)
    }
  }
}

# Read:
# filename <- paste('./PinskyDisMat/start=', input$range_cluster[1], '&end=', input$range_cluster[2], '&HighLow=', input$HighLow, '.csv', sep = '')
# DisMat <- fread(filename, sep = ',') %>% as.matrix()
# rownames(DisMat2) <- colnames(DisMat2)

