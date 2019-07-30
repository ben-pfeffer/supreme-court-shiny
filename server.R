library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(shinycssloaders)

# load justice by year dataset (simplified from big data set)
justice.by.year <- readr::read_csv('small_dataset.csv') %>%
  mutate(MQrank = as.factor(MQrank)) %>%
  mutate(liberal.vote.pct = round(liberal.vote.pct, 4))

# load full dataset. Remove 'Interstate Relations' cases
df <- readr::read_csv('full_dataset.csv') %>% 
  mutate(direction = direction - 1) %>%
  filter(!is.na(direction)) %>%
  filter(!is.na(issueArea)) %>%
  filter(!issueArea == 'Interstate Relations') %>%
  mutate(liberal.vote.pct = round(liberal.vote.pct, 4))
# interstate relations decisions aren't classified on the liberal/conservative scale, 
# so they are removed

# fit the logistic regression model to predict justice liberal vote probability 
LR.fit <- glm(direction ~ MQscore * issueArea, data = df, family ='binomial')

# load cached data set of issue Area trends over time. 
# could be run from code, but processing takes too long
master.list <- readr::read_csv('full-court-outcome-predictions.csv') %>% 
  rename('Actual Outcomes' = Observed.Liberal.Vote.Percentage,
         'Voting History Prediction' = Historical.Vote.Simulation,
         'Model Prediction' = Model.Simulation)

# Load custom functions

# Calculate Justice's Liberal Vote Percentage Actual and from Model
calculateObservedVsPredicted <- function(just, df) {
  
  x <- df %>% filter(justiceName == just) %>% 
    group_by(issueArea) %>% 
    summarize(lib.count = sum(direction, na.rm = TRUE),
              case.count = n()) %>%
    mutate(observed.liberal.pct = lib.count / case.count) %>%
    select(issueArea, observed.liberal.pct, case.count) %>%
    rename('Actual Career Percentage' = observed.liberal.pct)
  
  # now find average MQ score for justice
  avgMQ <- df %>% filter(justiceName == just) %>%
    summarize(mean(MQscore, na.rm = TRUE)) %>% 
    unlist()
  
  # get predictions for each issueArea from model
  a <- predict(LR.fit, newdata = data.frame(MQscore = rep(avgMQ, nrow(x[,1])),
                                            issueArea = x$issueArea),
               se.fit = TRUE, type = 'response')
  
  x$'Model Prediction' = round(a$fit, 7)
  x <- x[,c(1,3,2,4)]
  
  return(x)
}
# Models the probabilities the court as a whole votes in a liberal direction in a given year
getPredictionsForYear <- function(year) {
  
  cases.for.year <- df %>% filter(term == year)
  cases.before.year <- df %>% filter(term <= year) 
  
  # for each justice on the bench in a given year
  justices.for.year <- unique(cases.for.year$justiceName)
  all.areas <- unique(df$issueArea)
  
  # find career voting history in previous cases, and LR model predicted percentages
  calcs.for.year <- lapply(justices.for.year, 
                           FUN = calculateObservedVsPredicted, 
                           df = cases.before.year)
  
  predictions.for.year <- data.frame()
  
  # bind justice names to data calculations
  for(i in 1:length(calcs.for.year)) {
    temp <- data.frame(justice = justices.for.year[i], term = year, calcs.for.year[i])
    predictions.for.year <- bind_rows(predictions.for.year, temp)
  }
  
  predictions.for.year <- select(predictions.for.year, -case.count)
  
  # if historical vote percentage is 100 or 0, use model preciction instead
  predictions.for.year$Actual.Career.Percentage <- 
    ifelse(predictions.for.year$Actual.Career.Percentage == 0,
           predictions.for.year$Model.Prediction, 
           ifelse(predictions.for.year$Actual.Career.Percentage == 1,
                  predictions.for.year$Model.Prediction,
                  predictions.for.year$Actual.Career.Percentage))
  
  return(predictions.for.year)
}
# Find the actual percentage of the time in a given year
# that the court as a whole voted liberal for each issueArea
calculateCourtPercentages <- function(year, df) {
  x <- df %>% select(caseId, term, issueArea, direction, majority) %>% 
    filter(term == year) %>%
    distinct(caseId, .keep_all = TRUE) %>%
    mutate(lib.majority = ifelse(majority == 2, 
                                 direction, 
                                 ifelse(direction == 1, 0, 1))) %>%
    group_by(issueArea) %>% 
    summarize(term = year,
              lib.majority = sum(direction, na.rm = TRUE),
              case.count = n()) %>%
    mutate(Observed.Liberal.Vote.Percentage = lib.majority / case.count) %>%
    select(-lib.majority) 
}
# Monte Carlo simulation of many court votes in a given year for all issue Areas
runSimulatedVotes <- function(df, full.df) {
  issueAreas <- as.character(unique(df$issueArea))
  num.justices <- length(unique(df$justice))
  output.df <- data.frame()
  term <- unique(df$term)
  
  B <- 10000
  for(i in 1:length(issueAreas)) {
    # filter only 1 issue area at a time 
    x <- df %>% filter(issueArea == issueAreas[i])
    # calculate a long-run court outcome average though Monte-Carlo sampling from judges historical vote patterns
    y <- sapply(x$Actual.Career.Percentage, 
                FUN = rbinom, size = 1, n = B)
    q <-  ifelse(rowSums(y)>num.justices/2, 1, 0) %>% sum() / B
    # calculate long-run court outcome average through Monte-Carlo sampling each judge from the logistic regression model
    z <- sapply(x$Model.Prediction, 
                FUN = rbinom, size = 1, n = B)
    j <- ifelse(rowSums(z)>num.justices/2, 1, 0) %>% sum() / B
    
    #add a row to the output data frame
    row <- data.frame(term = term, 
                      issueArea = issueAreas[i], 
                      Historical.Vote.Simulation = q, 
                      Model.Simulation = j)
    
    output.df <- bind_rows(output.df, row)
  }
  
  # add actual court percentages for the year to the output
  x <- calculateCourtPercentages(term, full.df)
  output.df <- left_join(output.df, x, by = c('issueArea', 'term'))

  return(output.df)
}

# Shiny Server Call
shinyServer(function(input, output, session) {
  
  # build tables for both data frames
  getDataset <- reactive({
    if(input$dataset == 'Justice by Case'){
      dataset <- df
    } else {
      dataset <- justice.by.year
    }
    if(!input$justiceName == 'None'){
       dataset <- dataset %>% filter(justiceName == input$justiceName)
    } 
    if(!input$year.table == 'None') {
      dataset <- dataset %>% filter(term == input$year.table)
    }
    dataset
  })
  output$table <- renderDT(
    DT::datatable(data = getDataset(), 
                  options = list(scrollX = TRUE))
  )
  
  # build data for Clustered MQ score graph with user-specified number of clusters
  getClusterData <- reactive({
    cluster.set <- justice.by.year %>% select(justiceName, term, MQscore, MQrank) %>% na.omit()
    clusters <- kmeans(cluster.set[,3:4], centers = input$num.clusters, iter.max = 25, algorithm = 'MacQueen')
    cluster.set$cluster = as.factor(clusters$cluster)
    cluster.by.year <- justice.by.year %>% left_join(cluster.set, by = c('justiceName', 'term', 'MQscore', 'MQrank')) %>%
      select(term, justiceName, MQscore, MQrank, cluster)
  })

  # build plot for MQ score graph
  output$MQscore.graph <- renderPlot({
    
    clusterData <- getClusterData()
    
    if(!input$onlyMedian) {
      g <-  ggplot(data = filter(clusterData, MQrank == 5)) + 
        geom_line(aes(x = term, y = MQscore)) +
        coord_cartesian(ylim = c(-8, 5)) + 
        geom_hline(yintercept = 0) + 
        labs(title = 'Martin Quinn Score of the Median Supreme Court Justice', 
             x = 'Year', y = 'Martin Quinn Score')
    } else {
      g <-  ggplot(data = clusterData,
                   aes(x = term, y = MQscore, group = justiceName)) + 
        coord_cartesian(ylim = c(-8, 5)) + 
        geom_hline(yintercept = 0) + 
        theme(legend.position = 'none') + 
        labs(title = 'Martin Quinn Scores of All Supreme Court Justices', 
             x = 'Year', y = 'Martin Quinn Score')
      
      if(input$clustering) {
        g <- g + geom_line(aes(color = cluster, 
                               size = MQrank, alpha = MQrank)) +
          scale_color_brewer(palette="Dark2")
      } else {
        g <- g + geom_line(aes(color = justiceName, group = justiceName, 
                               size = MQrank, alpha = MQrank))
      }
    
      if(input$median){
        g <- g + scale_size_manual(values = c(.5, .5, .5, .5, 1.5, .5, .5, .5, .5)) + 
          scale_alpha_manual(values = c(.5, .5, .5, .5, 1, .5, .5, .5, .5))
      } else {
        g <- g + scale_size_manual(values = c(.5, .5, .5, .5, .5, .5, .5, .5, .5)) + 
          scale_alpha_manual(values = c(.7, .7, .7, .7, .7, .7, .7, .7, .7))
      }
    }
    g
  })
  
  # build plot for the logistic regression graph
  output$LR.graph <- renderPlot({
    g <- ggplot(data = df, aes(x = MQscore, y = direction)) +
      labs(x = 'Martin Quinn Score', y = 'Percent of Time Voting Liberal') +
      geom_jitter(alpha = 0.03)
    
    if(input$areas) {
      g <- g +
        stat_smooth(method = 'glm', method.args = list(family = 'binomial'),
                           size = 1, se = FALSE, aes(color = issueArea))
    }
    
    if(input$overall) {
      g <- g + stat_smooth(method = 'glm', method.args = list(family = 'binomial'),
                           size = 2.5)
    }
  
    g
  })
  
  # find justice vote percentages for user-specified justice
  run.observed.v.predicted <- reactive({
    x <- calculateObservedVsPredicted(input$just.name, df)
    x[3] <- round(x[3], 4)
    x[4] <- round(x[4], 4)
    x
      
  })
  # build data table for justice observed and model predicted liberal vote probabilities
  output$justice.table <- renderDT(
    run.observed.v.predicted()
  )
  # build plot for justice observed and model predicted liberal vote probabilities
  build.justice.graph <- reactive({
    observed.v.predicted <- run.observed.v.predicted()
    observed.v.predicted$case.count <- ifelse(is.na(observed.v.predicted$case.count),
                                              0, observed.v.predicted$case.count)
    observed.v.predicted$issueArea <- paste0(observed.v.predicted$issueArea, ' (', 
                                             observed.v.predicted$case.count, ' cases)')
    
    ggplot(data = reshape2::melt(select(observed.v.predicted, issueArea,
                                        'Actual Career Percentage', 
                                        'Model Prediction')),
           aes(x = variable, y = value, fill = variable, width = .8)) +
      geom_bar(stat = 'identity', position = 'dodge') + 
      facet_wrap(~issueArea) + coord_flip() +
      theme(legend.position = 'none') + 
      labs(title = paste0('Liberal Vote Percentages for ', input$just.name),
           x = '', y = '')
  })
  
  output$justice.graph <- renderPlot({
    print(build.justice.graph())
  })
    
  
  # get custom prediction for justice issueArea and MQ score
  get.custom.prediction <- reactive({
    x <- predict(LR.fit, newdata = data.frame(MQscore = input$MQslider,
                                         issueArea = input$MQissueArea),
            se.fit = FALSE, type = 'response')
    paste0('This imaginary justice with a Martin Quinn score of ', 
           input$MQslider, ' is predicted to cast a liberal vote in ', 
           round(x*100 , 2), '% of ', input$MQissueArea, ' cases.')
  })
  output$custom.prediction <- renderText({
    get.custom.prediction()
  })
  
  # get court decision predicitons for user specified year
  getYearData <- reactive({
    x <- runSimulatedVotes(getPredictionsForYear(input$year), df) %>% 
      rename('Actual Outcomes' = Observed.Liberal.Vote.Percentage,
             'Voting History Prediction' = Historical.Vote.Simulation,
             'Model Prediction' = Model.Simulation) 
    x[6] <- round(x[6], 4)
    x <- x[,c(1, 2, 5, 3, 4, 6)]
  })
  output$year.table <- renderDT(
    datatable(data = getYearData(),
              options = list(scrollX = TRUE))
  )
  # plot specific years predictions in faceted grid
  build.year.graph <- reactive({
    specific.year.predictions <- getYearData() 
    specific.year.predictions$case.count <- ifelse(is.na(specific.year.predictions$case.count), 
                                                   0, specific.year.predictions$case.count)
    specific.year.predictions$issueArea <- paste0(specific.year.predictions$issueArea, ' (', 
                                                  specific.year.predictions$case.count, ' cases)')
    
    ggplot(data =reshape2::melt(select(specific.year.predictions, issueArea, 
                                       'Voting History Prediction',
                                       'Model Prediction',
                                       'Actual Outcomes')),
           aes(x = variable, y = value, fill = variable, width = .8)) +
      geom_bar(stat = 'identity', position = 'dodge') + 
      facet_wrap(~issueArea) + coord_flip() +
      theme(legend.position = 'none') + 
      labs(title = paste0('Predicted and Actual Liberal Outcome Percentages for ', 
                          specific.year.predictions$term[1]), 
           x = '', y = '')
  })
  output$court.year.predictions <- renderPlot({
    build.year.graph()
  })
  
  # get court's liberal vote trend and predictions for whole data set range
  getTrend <- reactive({
    dat <- master.list %>% filter(issueArea == input$issueArea) %>% 
      select(-case.count, -issueArea) %>% 
      reshape2::melt(id.vars = 'term')
    
  })
  # build plot for trend over whole data set range
  output$trend.lines <- renderPlotly({
    dat <- getTrend()
    ggplotly(ggplot(data = dat) + 
      geom_smooth(aes(x = term, y = value, color = variable), size = .75) + 
      geom_point(aes(x = term, y = value, color = variable), size = .5) + 
      labs(x = 'Year', y = 'Liberal Outcome Percentage') )
  })
  
  # code for all download buttons
  output$justDownGraph <- downloadHandler(
    filename = function() {paste0(input$just.name, 'Graph.pdf') },
    content = function(file) {
      ggsave(file, plot = build.justice.graph(), device = "pdf")
    }
  )
  output$justDownData <- downloadHandler(
    filename = function() {paste0(input$just.name, 'Data.csv')},
    content = function(file) {
      readr::write_csv(run.observed.v.predicted(), file)
    }
  )
  output$yearDownData <- downloadHandler(
    filename = function(){paste0(input$year, 'CourtPredictionsData.csv')},
    content = function(file) {
      readr::write_csv(getYearData(), file)
    }
   )
  output$yearDownGraph <- downloadHandler(
    filename = function() {paste0(input$year, 'CourtPredictionsGraph.pdf')},
    content = function(file) {
      ggsave(file, plot = , device = 'pdf')
    }
  )
  output$datasetDownload <- downloadHandler(
    filename = function() {paste0(input$dataset, '.csv')},
    content = function(file) {
      readr::write_csv(getDataset(), file)
    }
  )
})



       