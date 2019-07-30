library(shiny)
library(ggplot2)
library(shinydashboard)
library(DT)
library(plotly)
library(shinycssloaders)

#create list of all justices to select from
justice.name.list <- c("HHBurton", "RHJackson", "WODouglas","FFrankfurter",
                       "SFReed", "HLBlack", "WBRutledge", "FMurphy", 
                       "FMVinson","TCClark","SMinton","EWarren","JHarlan2",
                       "WJBrennan", "CEWhittaker","PStewart","BRWhite",
                       "AJGoldberg","AFortas","TMarshall","WEBurger",
                       "HABlackmun","LFPowell","WHRehnquist","JPStevens",
                       "SDOConnor","AScalia","AMKennedy","DHSouter",
                       "CThomas","RBGinsburg","SGBreyer","JGRoberts",
                       "SAAlito","SSotomayor","EKagan","NMGorsuch" )

issueArea.list <- c('Economic Activity', 'Criminal Procedure','Civil Rights',
                    'Judicial Power', 'Unions',   'Due Process', 'Federal Taxation',
                    'First Amendment', 'Privacy', 'Federalism', 'Attorneys',
                    'Miscellaneous', 'Private Action')

dashboardPage(
  dashboardHeader(title = 'Supreme Court'),
  
  # Side Bar Content
  dashboardSidebar(
    sidebarMenu(
      menuItem('Intro', tabName = 'intro'),
      menuItem('Data', tabName = 'tables'),
      menuItem("The Court's Ideologies", tabName = 'MQgraph'),
      menuItem('Logistic Regression', tabName = 'issueArea'),
      menuItem('Justice Analysis', tabName = 'justice'),
      menuItem('Court Analysis', tabName = 'court'),
      menuItem('Trends Over Time', tabName = 'trend')
    )
  ),
  
  # Body Content
  dashboardBody(
    tabItems(
      tabItem(tabName = 'intro',
              box(h1("An Exploration of Supreme Court Data"),
                  h4('by Ben Pfeffer'), width = 12),
              box(h2('Welcome!'), 
                  "In this project we will explore the Supreme Court from a liberal-conservative perspective. We will begin by looking at the popular Martin-Quinn score that attempts to quantify a justice's ideology. We build a model that quantifies a justice's probability of casting a liberal vote given their Martin Quinn score We then try to improve on this model by incorporating information on the case's issue area. From there we build a second model that simulates the votes of all justices on the court for a given year based on their likelihoods of casting a liberal vote. We use this model to predict court outcomes based on its judges ideologies for a given issue area and year. Finally, we will look at how the court's decision making has changed over time for different issue areas as the various justices have entered and exited the court. I hope you find it enjoyable and illuminating!",
                  width = 12),
              box(h2('Data Sources'), 
                  HTML("The bulk of the data used for this project comes from the Supreme Court Database. It can be freely downloaded <a href = http://scdb.wustl.edu/data.php> here</a>.<br><br> 
                  The Martin-Quinn score data came from the Martin Quinn Scores website. Data is available to download <a href = https://mqscores.lsa.umich.edu/measures.php> here</a>. If you'd like to read up on the details of how these scores are calculated and what they represent, the paper is available <a href = https://mqscores.lsa.umich.edu/media/pa02.pdf> here</a>. For the purposes of this app you don't need to know all the details. I'll explain the broader points as we go."), width = 12),
              box(HTML("If this is your first time using this app I recommend you traverse the app in order, as concepts build on each other as we go. Let's get started! Click the <b>Data</b> tab to move forward."), width = 12)
              
              ),
      tabItem(tabName = 'tables',
              fluidRow(
                box(h3("There are two datasets used in this analysis."), 
                    HTML("The <b>Justice by Year</b> data set contains one row for each justice each year. The <b>Justice by Case</b> data set contains one row for each justice for each court case. The analyses in the rest of this app are based on these two data sets. You can explore them both below. Explanations for each variable are at the bottom of the page. For the code I used to transform the source data sets into these, see the <i>build-data-set.Rmd</i> file."), width = 12),
                box(selectInput('dataset', 
                                label = 'Select Data Set', choices = c('Justice by Year',
                                                                       'Justice by Case'))),
                box(
                    selectizeInput('justiceName',
                                   label = 'Filter by Justice',
                                   choices = c('None', justice.name.list),
                                   multiple = FALSE, 
                                   selected = 'None'),
                    selectizeInput('year.table', 
                                   label = 'Filter by Year',
                                   choices = c('None', 1946:2017),
                                   selected = 'None')
                ),
                box(DTOutput('table'), width = 12),
                box(downloadButton('datasetDownload', 'Download Data Set')),
                conditionalPanel(condition = "input.dataset.indexOf('Justice by Year')",
                                 box(h3('Variable Explanations'),
HTML("<b>caseId</b> - the case number from the Supreme Court Database<br>
                                          <b>term</b> - the year the court handed down its decision<br>
                                          <b>issueArea</b> - one of 14 different categories defined by the Supreme Court Database. We will explore these in more depth later<br>
                                          <b>majVotes</b> - the number of justices voting in the majority<br>
                                          <b>minVotes</b> - the number of justices voting in the dissent<br>
                                          <b>justiceName</b> - the justice's first initial and last name<br>
                                          <b>direction</b> - whether the justice cast a conservative or liberal vote. 0 is a conservative vote and 1 is a liberal vote, as determined by the Supreme Court Database. These generally align with the popular understandings of the terms. For details of exactly how decisions are classified <a href = 'http://scdb.wustl.edu/documentation.php?var=decisionDirection'>see here</a><br>
                                          <b>majority</b> - whether the justice voted with the majority. 2 is a majority vote and 1 is a dissenting vote <br>
                                          <b>MQscore</b> - the justice's Martin Quinn score. These scores don't mean anything in an absolute sense. The justice's relative positions and the distances between scores are what matter. Positive scores are more conservative, and negative scores are more liberal, but a score of 0 does not automatically represent the exact political center. However, the further a justice's score is from 0, the further that justice's ideology is from the political center. <br>
                                          <b>MQrank</b> - the justice's relative rank on the court for the year, as determined by <b>MQscore</b>. A rank of 1 means this is the most liberal member of the court for that year (lowest MQscore). A rank of 9 means this is the most conservative justice for the year (highest MQscore). <br>
                                          <b>MQswing.total</b> - the number of swing votes the justice cast in that year. In this project, I  define a swing vote as only occuring in a case decided by 1 vote (usually that means a 5-4 decision). I assign the swing vote to the the most conservative member of a liberal majority, or the most liberal member of a conservative majority, as measured by <b>MQscore</b> <br>
                                          <b>case.load</b> - the number of votes the justice cast for the year<br>
                                          <b>liberal.vote.total</b> the number of liberal votes a justice cast for the year<br>
                                          <b>liberal.vote.pct</b> - the percentage of a justice's votes for the year that were liberal<br>"), width = 12)),
                conditionalPanel(condition = ('input.dataset.indexOf("Justice by Case")'),
                                 box(h3('Variable Explanations'),
HTML("<b>term</b> - the year the court handed down its decision<br>
                                          <b>justiceName</b> - the justice's first initial and last name<br>
                                          <b>MQscore</b> - the justice's Martin Quinn score. These scores don't mean anything in an absolute sense. The justice's relative positions and the distances between scores are what matter. Positive scores are more conservative, and negative scores are more liberal, but a score of 0 does not automatically represent the exact political center. However, the further a justice's score is from 0, the further that justice's ideology is from the political center. <br>
                                          <b>MQrank</b> - the justice's relative rank on the court for the year, as determined by <b>MQscore</b>. A rank of 1 means this is the most liberal member of the court for that year (lowest MQscore). A rank of 9 means this is the most conservative justice for the year (highest MQscore). <br>
                                          <b>MQswing.total</b> - the number of swing votes the justice cast in that year. In this project, I  define a swing vote as only occuring in a case decided by 1 vote (usually that means a 5-4 decision). I assign the swing vote to the the most conservative member of a liberal majority, or the most liberal member of a conservative majority, as measured by <b>MQscore</b> <br>
                                          <b>case.load</b> - the number of votes the justice cast for the year<br>
                                          <b>liberal.vote.total</b> the number of liberal votes a justice cast for the year<br>
                                          <b>liberal.vote.pct</b> - the percentage of a justice's votes for the year that were liberal<br>"), width = 12))
                )
              ),
      
      tabItem(tabName = 'MQgraph',
              fluidRow(
                box(h3("The Court's Ideologies"),
                    HTML("The chart below shows the Martin Quinn score of the median justice from 1946 to 2017. The exact number of the Martin Quinn score does not mean much on it's own. The relative position or distance between two points is what matters. Higher Martin Quinn scores are more conservative, and lower scores are more liberal. The 0 line does not have any special significance (it does not automatically note the center), but is included as a useful reference. Pay attention to when and how the Martin Quinn scores move higher and lower, but don't put too much stock in whether they happen to move from one side of the 0 line to the other. The magnitude of the change is much more informative than the sign in front of the number.<br><br>
                    The median justice occupies a very important place on the court. There are four justices more liberal and four justices more conservative than the median justice. For this reason, in close decisions - those decided by only one vote - the median justice often determines the outcome of the case. As the median justice becomes more conservative, the court is more likely to swing conservative. When the median justice is more liberal, the court is more likely to swing liberal. <br><br>
                         "), width = 12),
               box(checkboxInput('onlyMedian', 
                                          h5('Show All Justices'))),
               conditionalPanel(condition = 'input.onlyMedian', 
                                             box(checkboxInput("median", 
                                                           h5("Emphasize Median Justice"),
                                                           value = TRUE),
                                             checkboxInput('clustering', 
                                                           h5('Cluster Similar Justices')),
                                             conditionalPanel(condition = 'input.clustering', 
                                                              sliderInput("num.clusters", 
                                                                          "Number of Clusters",
                                                                          min = 2, max = 8, 
                                                                          value = 3, 
                                                                          step = 1,
                                                                          ticks = FALSE)
                                                              )
                                             )
                            ),
               
                            
               box(plotOutput('MQscore.graph'), width = 12),
               conditionalPanel(condition = 'input.clustering',
                                box("Above we have run a clustering algorithm to sort similar justices together. The algorithm running here clusters justices with similar Martin Quinn scores and ranks. That is, justices with similar ideologies (MQscore) and who occupy similar relative positions on the court (MQrank) are grouped together. Clustering the court into 3 groups could be thought of as separating it into 'liberals,' 'centrists,' and 'conservatives.'", width = 12)),
               conditionalPanel(condition = 'input.median',
                                             conditionalPanel(condition = 'input.onlyMedian',
                                box("By making the line for the median justice bolder we can see how the position has changed hands over time. This can happen because the previous median justice retired, a new justice was brough in, or because of ideological drift over time.", width = 12))),
               conditionalPanel(condition = 'input.onlyMedian',
                                box(HTML("There is a trend here that many justices follow: a justice is likey to become more liberal over time, as indicated by the downward slope of most of the lines. There are certainly some who become more conservative or remain about the same, but the majority undergo some liberalization as they age.<br><br>
                                         In popular culture there is a meme that young people start out as liberal and become more conservative as they age. This is clearly not the case on the Supreme Court."), width = 12))
               
              )
      ),
      tabItem(tabName = 'issueArea',
              fluidPage(
                box(h3('Likelihood of Voting Liberal'), 
                    HTML("In this project we are interested predicting whether a justice will cast a liberal or a conservative vote depending on their Martin Quinn score. In the chart below each point represents a vote cast by a Supreme Court justice. Points in the top row are liberal votes. Points in the bottom row are conservative votes. The far left side of the chart gives us all the votes by the most liberal justice in our data set. The far right side gives us all the votes by the most conservative. As we move from left to right the justices become more conservative - and more likely to cast a conservative vote."), width = 12),
                box(checkboxInput('overall', h4('Show Overall Average')),
                    checkboxInput('areas', h4('Break into Issue Areas'))),
                
                box(plotOutput('LR.graph'), width = 12),
                conditionalPanel(condition = 'input.areas', 
                                 box(HTML("One limitation of the Martin Quinn score is that it is one-dimensional. For this reason our model also takes a case's issue area into consideration. By breaking out our predictions by issue area we get a fuller picture of voting patterns. Some issue areas like 'The First Amendment' are more divisive than the average. In these issue areas the liberals are more likely than average to vote liberal and the conservatives are more likely than average to vote conservative. Other issue areas are less divisive, such as 'Judicial Power' and 'Federalism' - the probability of a liberal vote does not change as drastically with a change in MQ score. <br><br>
                          Still others reverse the trend. In 'Federal Taxation' cases conservative justices are more likely to cast a liberal vote (that is, pro-United States, anti-taxpayer) than the liberal justices. This was a surprising finding for me. One potential explanation could lie in the fact that this model does not take year into account, so maybe past conservatives voted differently than today's conservatives. Conservatives and liberals might have realigned around this issue sometime since 1946 (when this data set begins). I have not yet explored whether this holds up to the data. It would make for an interesting future investigation."), width = 12)),
                conditionalPanel(condition = 'input.overall', 
                                 box("To predict the probability that a justice casts a liberal vote given their Martin Quinn ideology, we run a logistic regression model. The model learns from past voting records and Martin Quinn scores in order to make predictions about future justices. It returns a value between 0 and 1. In this case, that value can be thought of as the probability (according to our model) that a justice with that Martin Quinn score will cast a liberal vote. We can see that a liberal justice with a Martin Quinn score of -5 has around an 80% chance of casting a liberal vote, while a conservative justice with a score of almost 5 has about a 25% chance of casting a liberal vote.", width = 12))
                )
              ),
      tabItem(tabName = 'justice',
              fluidRow(
                box(h3('Test how the model works'),
                    sliderInput('MQslider', 'Select MQ score', min = -7.5, 
                                max = 4.5, value = 0, step = 0.1),
                    selectizeInput('MQissueArea', label = 'Select Issue Area',
                                   choices = issueArea.list,
                                   selected = 'Economic Activity',
                                   multiple = FALSE)),
                box(h3(textOutput('custom.prediction'))),
                box(h3("How does this model do in real life?"),
                    "To make an informal assessment of how accurately our model predicts vote directions, let's compare the percentage of votes the model predicts for a given justice with the actual percentage of liberal votes a justice has cast in their career. Because Martin Quinn scores change over a justice's career, we take our chose justice's career average score to input into the model. This analysis can also give us insight into any 'quirky' issue areas a justice may have. For example, Justice Roberts seems to vote conservative more often than expected in Union cases, and liberal more often than expected in First Amendment cases. Try it out on some other justices. The graphs and datasets are available to download at the bottom of the page.", width = 12),
                box(h3('Try it on a real justice'),
                    selectizeInput('just.name',
                                   label = 'Select your Favorite (or your Least Favorite) Justice',
                                   choices = justice.name.list, 
                                   selected = 'JGRoberts',
                                   multiple = FALSE),
                plotOutput('justice.graph'), width = 12),
                box(DTOutput('justice.table'), width = 12),
                box(downloadButton('justDownGraph', 'Download Graph'),
                    downloadButton('justDownData', 'Download Data'))
              )
      ),
      tabItem(tabName = 'court',
              fluidRow(
                box(h3('Predicting Court Outcomes'),
                    HTML("We can use the predictions we just developed for each justice in last slide to predict the court's vote as a whole. We do this through simulation. First we pick a year to simulate. Then for each justice on the court that year we get predictions on their liberal vote probabilities in two ways: by querying the model and by looking at their career voting history through the year selected (no looking into the future). These two methods give us a probability that a justice will cast a liberal vote in a given year and issue area. We put these probabilities through a random number generator to create a simulated vote. For instance, if a justice has a 75% liberal vote probability, the random number generator will return 1 75% of the time and 0 25% of the time. After simulating a vote for each justice on the bench in that year (each with their own liberal vote probabilities), we add up the votes and see if the court reached a liberal or conservative majority. This process is repeated many times to find a long-run average for both model based and vote-history based simulations. We then compare these court outcome predictions to the actual court decisions for that year. <br><br> 
                         Notice that the predictions are not as close to reality as the previous model. I'd like to suggest a couple of possible reasons for this. First, pay attention to the number of cases in an issue area. It is hard to assess the model's accuracy if an issue area has only a few cases in a given year. Don't discount the model in those areas. Being off by 1 case either way can make a large impact on percentage. Second, the model assumes all the justice's votes are <i>independent</i> of one another. In this context that means that one justice voting liberal or conservative in a case has no effect on the odds of the other justices voting liberal or conservative in the same case. The Supreme Court votes 9-0 or 8-1 far more often than they would if justice's votes were actually independent. If a conservative justice casts a liberal vote the liberal vote probabilities for all the other justices should be adjusted higher in that case, and vice versa. The justice's votes are certainly not independent in reality. One could program a model that takes vote interdependence into consideration and probably achieve superior results."),width = 12),
                  box(sliderInput('year', label = 'Select aYear', 
                                  min = 1946, max = 2017, step = 1, 
                                  value = 2000, ticks = TRUE,
                                  sep = ''), 
                      width = 12),
                  box(plotOutput('court.year.predictions'), width = 12),
                  box(DTOutput('year.table'), width = 12),
                box(downloadButton('yearDownGraph', 'Download Graph'),
                    downloadButton('yearDownData', 'Download Data'))
                )),
      tabItem(tabName = 'trend',
              fluidRow(
                box(h3("Trends over Time"),
                    "We can use the model predictions for each year to graph the liberal vote probability over time. This also allows us to see which prediction technique (model-based or vote-history based) is working better. For the Economic Activity issue area, the 'Voting History Prediction' seems to have been pretty accurate since around 1980 (the blue and red lines are tracking closely). Note, however, that for one individual year the prediction has not always been close (many of the blue points on the graph are not close to the red line). This is likely an issue of small sample size. The actual results in one given year may not be close to their actual long run average. Check out how the actual percentages over time compare with the models in different issue areas. In what areas is has the Supreme Court become more conservative or liberal over time?", width = 12),
                box(selectizeInput('issueArea', choices = issueArea.list,
                                   label = 'Select an Issue Area',
                                 selected = '', multiple = FALSE)),
                box(plotlyOutput('trend.lines'), width = 12)
              )
      )
    )
  )
)

