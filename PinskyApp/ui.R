library(shiny)
library(shinydashboard)
library(tidyverse)
library(alphavantager)
library(data.table)
companyProfile <-
  fread(
    'https://s3.us-east-2.amazonaws.com/nanfang/companyProfile.csv',
    header = T,
    sep = ','
  ) %>% as.tibble()


dashboardPage(
  dashboardHeader(title = 'Pinsky Conjecture'),
  dashboardSidebar(sidebarMenu(
    menuItem(
      "Part 1 Introduction",
      tabName = "part1",
      icon = icon("signal"),
      menuSubItem("Conclusion", tabName = "realtime"),
      menuSubItem("Histogram", tabName = "Histogram")
    ),
    menuItem(
      "Part 2 Clustering",
      tabName = "part2",
      icon = icon("certificate"),
      menuSubItem("k-clusters", tabName = "cluster"),
      menuSubItem("Selecting Best k", tabName = "cluster_all")
    )
  )),
  dashboardBody(tabItems(
    tabItem(tabName = "realtime",
            fluidRow(
              box(
                width = 8,
                title = "Realtime Stock Price Analysis",
                status = "primary",
                conditionalPanel(condition = "$('html').hasClass('shiny-busy')",
                                 div(
                                   img(src = 'https://icons8.com/preloaders/svg-preview.php?preloader_id=30'),
                                   style = "text-align: center;"
                                 )),
                plotOutput("realtime_plot")
              ),
              box(
                width = 4,
                title = "Control Panel",
                status = "success",
                textInput("symbol_conclusion", "Member of Your Cluster", placeholder = 'PEP', value = 'PEP'),
                textInput("medoid_conclusion", "Medoid of Your Cluster", placeholder = 'PEP', value = 'PEP')
              ),
              box(
                width = 4,
                title = "Intraday Trading Strategy",
                status = "danger",
                "Now Look at the diagram. 
                Blue points are past High or Low hits during each 30 minutes intervals today. 
                A realtime adjusted probability mass function (PMF) was generated. 
                The darker the more likely it would become today's Highest or Lowest hit. 
                So, your strategy is simple. Do intraday option! 
                'Call' on darker High PMF time interval.
                'Put' on darker Low PMF time interval.
                Good luck with your profolio!"
              ),
              box(
                title = "Introduction",
                status = "warning",
                width = 8,
                htmlOutput("Bhat_explanation"),
                withMathJax(),
                #replace \ with \\ to run in this scenario
                helpText(
                  'Bhattacharyya Distance of two discrete distributions:
                    $$ D = -ln(\\sum_{x \\in B}\\sqrt{p(x)q(x)}) $$
                    where p(x) and q(x) are Probability Mass Function (PMF).'
                ),
                htmlOutput("PAM_explanation"),
                htmlOutput("Silhouette_explanation"),
                htmlOutput("tSNE_explanation")
              )
            )),
    tabItem(tabName = "Histogram",
            
            fluidRow(
              column(
                width = 6,
                box(
                  title = "Histogram",
                  status = "primary",
                  width = NULL,
                  conditionalPanel(condition = "$('html').hasClass('shiny-busy')",
                                   div(
                                     img(src = 'https://icons8.com/preloaders/svg-preview.php?preloader_id=30'),
                                     style = "text-align: center;"
                                   )),
                  
                  
                  plotOutput("histogram_plot", height = "600px")
                )
                
              ),
              column(
                width = 6,
                box(
                  title = "Control Panel",
                  status = "warning",
                  width = NULL,
                  sliderInput(
                    "range",
                    "Range:",
                    min = 1991,
                    max = 2016,
                    value = c(1999, 2013)
                  ),
                  textInput(
                    "symbol",
                    "Stock Symbol",
                    value = 'A',
                    placeholder = 'A'
                  )
                  
                ),
                box(
                  title = "Company Profiles",
                  status = "danger",
                  width = NULL,
                  fluidRow(
                    column(
                      3,
                      selectInput(
                        inputId = "table_continent",
                        label = "Continent:",
                        choices = c("All",
                                    unique(as.character(
                                      companyProfile$Continent
                                    ))),
                        selected = "All"
                      )
                    ),
                    column(
                      3,
                      selectInput(
                        inputId = "table_country",
                        label = "Country:",
                        choices = c("All",
                                    unique(as.character(
                                      companyProfile$Country
                                    ))),
                        selected = "All"
                      )
                    ),
                    column(
                      3,
                      selectInput(
                        inputId = "table_sector",
                        label = "Sector:",
                        choices = c("All",
                                    unique(as.character(
                                      companyProfile$Sector
                                    ))),
                        selected = "All"
                      )
                    ),
                    column(
                      3,
                      selectInput(
                        inputId = "table_industry",
                        label = "Industry:",
                        choices = c("All",
                                    unique(as.character(
                                      companyProfile$Industry
                                    ))),
                        selected = "All"
                      )
                    )
                  ),
                  conditionalPanel(condition = "$('html').hasClass('shiny-busy')",
                                   div(
                                     img(src = 'https://icons8.com/preloaders/svg-preview.php?preloader_id=30'),
                                     style = "text-align: center;"
                                   )),
                  # Create a new row for the table.
                  
                  DT::dataTableOutput("table")
                  
                )
              )
            )),
    tabItem(tabName = "cluster",
            fluidRow(
              column(
                width = 6,
                box(
                  width = NULL,
                  title = "Cluster Plot",
                  status = "primary",
                  conditionalPanel(condition = "$('html').hasClass('shiny-busy')",
                                   div(
                                     img(src = 'https://icons8.com/preloaders/svg-preview.php?preloader_id=30'),
                                     style = "text-align: center;"
                                   )),
                  plotOutput("cluster_plot")
                ),
                box(
                  width = NULL,
                  title = "Control Panel for Clustering",
                  status = "success",
                  sliderInput(
                    "range_cluster",
                    "Range:",
                    min = 1991,
                    max = 2016,
                    value = c(1996, 1999)
                  ),
                  sliderInput(
                    "clusterNumber",
                    label = "Cluster Number",
                    min = 2,
                    max = 50,
                    value = 3
                  ),
                  selectInput(
                    "HighLow",
                    "Histogram Type:",
                    choices = c('High', 'Low', 'High-Low'),
                    width = '25%'
                  )
                  
                ),
                box(
                  width = NULL,
                  title = "Control Panel for Unusual Cluster Analysis",
                  status = "success",
                  sliderInput(
                    "clusterChoice",
                    label = "Which of Cluster? (invalid if more than total cluster number)",
                    min = 1,
                    max = 50,
                    value = 1
                  ),
                  "Stock Symbol in This Cluster:",
                  verbatimTextOutput("symbol_in_cluster")
                )
              ),
              column(
                width = 6,
                
                box(
                  title = "Unusual Cluster by Region",
                  status = "success",
                  width = NULL,
                  conditionalPanel(condition = "$('html').hasClass('shiny-busy')",
                                   div(
                                     img(src = 'https://icons8.com/preloaders/svg-preview.php?preloader_id=30'),
                                     style = "text-align: center;"
                                   )),
                  plotOutput("total_continent_plot")
                ),
                box(
                  title = "Unusual Cluster by Sector",
                  status = "success",
                  width = NULL,
                  conditionalPanel(condition = "$('html').hasClass('shiny-busy')",
                                   div(
                                     img(src = 'https://icons8.com/preloaders/svg-preview.php?preloader_id=30'),
                                     style = "text-align: center;"
                                   )),
                  plotOutput("total_sector_plot")
                ),
                box(
                  title = "Now Discover Your Own Profolio",
                  status = "primary",
                  width = NULL,
                  "Select one cluster that you think behaves unusual due to either region or sector reason, 
                  which means its pie chart on right side is very different from the left. 
                  Then choose a member of your selected cluster. Lastly, input the member symbol and medoid on the first page.
                  A trading strategy will be automatically generated for you. "
                )
              )
              
            )),
    tabItem(tabName = "cluster_all",
            fluidRow(
              column(
                width = 6,
                box(
                  width = NULL,
                  title = "Selecting Best Number of Cluster",
                  status = "primary",
                  conditionalPanel(condition = "$('html').hasClass('shiny-busy')",
                                   div(
                                     img(src = 'https://icons8.com/preloaders/svg-preview.php?preloader_id=30'),
                                     style = "text-align: center;"
                                   ))
                  ,
                  plotOutput("all_cluster_plot")
                )
              ),
              column(
                width = 6,
                box(
                  width = NULL,
                  title = "Control Panel",
                  status = "success",
                  sliderInput(
                    "range_all_cluster",
                    "Range:",
                    min = 1991,
                    max = 2016,
                    value = c(1996, 1999)
                  ),
                  sliderInput(
                    "clusterMaxNumber",
                    label = "Cluster Number from 1 to",
                    min = 1,
                    max = 50,
                    value = 1
                  ),
                  selectInput(
                    "HighLow_all_cluster",
                    "Histogram Type:",
                    choices = c('High', 'Low', 'High-Low')
                  )
                  
                )
              )
              
            ))
  ))
)
