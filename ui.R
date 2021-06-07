library(shiny)
library(png)
library(shinyBS)
library(shinyDND)
library(shinyjs)
library(ggplot2)
library(dplyr)
library(shinydashboard)
library(simstudy)
library(lubridate)
library(shinyalert)
library(shinyWidgets)

convertMenuItem <- function(mi,tabName) {
  mi$children[[1]]$attribs['data-toggle']="tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  if(length(mi$attribs$class)>0 && mi$attribs$class=="treeview"){
    mi$attribs$class=NULL
  }
  mi
}

shinyUI(list(
        dashboardPage(
          skin = "black", 
          dashboardHeader(title = "ANCOVA",
                          titleWidth = 250,
                          tags$li(
                            class = "dropdown",
                            actionLink("info",
                                       icon("info"))
                          ),
                          tags$li(
                            class = "dropdown",
                            tags$a(target = "_blank", 
                                   icon("comments"),
                                   href = "https://pennstate.qualtrics.com/jfe/form/SV_7TLIkFtJEJ7fEPz?appName=ANCOVA"
                            )
                          ),
                          tags$li(
                            class = "dropdown",
                            tags$a(icon("home"),
                                   href = 'https://shinyapps.science.psu.edu/'
                            )
                            )
                          ),
          #adding pages to sidebar
          dashboardSidebar(
            width = 220,
            sidebarMenu(id = "pages",
              menuItem(text = "Overview",
                       tabName = "instruction", 
                       icon = icon("dashboard")
              ),
              menuItem(text = "Prerequisites", 
                       tabName = "prereq", 
                       icon = icon("book")
              ),
              menuItem(text = "Explore",
                       tabName = "explore", 
                       icon = icon("wpexplorer")
              ),
              menuItem(text = "Game", 
                       tabName = "qqq", 
                       icon = icon("gamepad")
              ),
              menuItem(text = "References",
                       tabName = "refs",
                       icon = icon("leanpub")
              )
            ),
            tags$div(
              class = "sidebar-logo", 
              boastUtils::sidebarFooter()
            )
          ),
          dashboardBody(
            tabItems(
              tabItem(tabName = "instruction",
                      h1("ANCOVA"),
                      h2("About:"),
                      h4('This app introduces the concept of ANCOVA focusing on 
                         interpreting interaction plots.'),
                      br(),
                      h2('Instructions:'),
                      h4(
                        tags$li('Click Go button to enter the explore page. Use 
                                the dropdown menu to select a dataset.')
                        ),
                      h4(
                        tags$li('Use the radio buttons to select different variables 
                                and see the changes in the interaction plot. Or 
                                use slider bars to change the parameters. ')
                        ),
                      h4(
                        tags$li('After working with the explore section, you can 
                                start the matching game to test your understanding 
                                of the concepts. Click "start" to set the timer 
                                and start the game. You can use "i" button for 
                                instruction and "?" for hints.')
                        ),
                      div(style = "text-align: center",
                          bsButton(inputId = "go",
                                   label = "Prerequisites",
                                   icon = icon("book"),
                                   style = "danger",
                                   size = "large")
                          ),
                      br(),
                      h2('Acknowledgements:'),
                      h4("This app was developed and coded by Luxin Wang and
                         modified by Zhiruo Wang and Lydia Bednarczyk.")
                      ),
      #Adding prerequisites page
              tabItem(tabName = "prereq",
                      h2('Prerequisites'),
                      h3('What is ANCOVA:'),
                      h4('ANCOVA is the analysis of variance with continuous 
                         variables added in. The information below will explain 
                         the difference between ANOVA, Regression, and ANCOVA.'),
                      br(),
                      fluidRow(
                        column(
                          width = 2,
                          offset = 0,
                          h4("X: Categorical", tags$br(),"Y: Continuous")
                        ),
                        column(
                          width = 2,
                          offset = 0,
                          h4("------->")
                          
                        ),
                        column(
                          width = 6,
                          offset = 0,
                          h4("ANOVA is used for comparing three or more group means.",
                             tags$br(),
                             "Different groups are different levels of categorical 
                             variables, and group means are calculated from continuous 
                             variables.", 
                             tags$br(), 
                             "Example: Are the average scores 
                             of three STAT 200 sections significantly different 
                             from each other?")
                        )
                      ),
                      fluidRow(
                        column(
                          width = 2,
                          offset = 0,
                          h4("X: Continuous", tags$br(),"Y: Continuous")
                        ),
                        column(
                          width = 2,
                          offset = 0,
                          h4("------->")
                          
                        ),
                        column(
                          width = 6,
                          offset = 0,
                          h4("Regression is used for determining the relationship 
                             between two continuous variables.",
                             tags$br(),
                             "One dependent variable (Y) can also be affected by 
                             multiple independent variables (X).", 
                             tags$br(), 
                             "Example: How will crime rate be impacted by population 
                             density, unemployment rate, and income?")
                        )
                      ),
                      fluidRow(
                        column(
                          width = 2,
                          offset = 0,
                          h4("X: Categorical & Continuous", tags$br(),"Y: Continuous")
                        ),
                        column(
                          width = 2,
                          offset = 0,
                          h4("------->")
                          
                        ),
                        column(
                          width = 6,
                          offset = 0,
                          h4("ANCOVA is adding continuous variables onto ANOVA 
                             analysis, which is called covariate.",
                             tags$br(),
                             "Significant differences between group means, and 
                             significant relationships between continuous variables 
                             are both analyzed.", 
                             tags$br(), 
                             "Example: Who makes the most money? Will gender or 
                             years after graduation influence the income?")
                        )
                      ),
                      
                      h3('Diagnostic Plots:'),
                      fluidRow(column(11,offset=2, img(src='plot.png',width=550),style='margin-top:-1em')),
                      h4('Model checking is a critical part of an analysis. You need to understand the diagnostic plot s like these four:',br(),
                         tags$li('The Residuals vs Fitted plot checks linear pattern of residuals. If the liner model is correct, you should expect a roughly horizontal line.'), br(),
                         tags$li('The Normal Q-Q plot checks normality. If the normality assumption is true, you should expect the dots roughly follow a straight line.'),br(),
                         tags$li('The Scale-Location plot checks for equal spread of residual the residuals. If the equal variance assumption is true, you should expect a roughly horizontal line with the dots showing equal spread.'),br(),
                         tags$li('The Residual vs Leverage plot checks for influential outliers. Outliers with high leverage will appear outside the dashed line range.')),
                      
                      div(style = "text-align: center",
                          actionButton("start","Go to the overview",icon("bolt"),style='padding:10px; font-size:100%',class="circle grow"))
              ),
              
              
              
              
                        
                        
                        # tabItem(tabName='box',
                        #         br(),
                        #         fluidRow(
                        #           column(3, bsButton('ano',HTML('<b> X </b> :Categorical <br/>  Y: Continuous'),type = 'toggle')),
                        #           column(1, 
                        #                  conditionalPanel("input.ano != 0",
                        #                                   img(src='line1.png',width=250,style='margin-top:6em;margin-left:6.5em'))
                        #           ),
                        #           column(5, offset = 2,
                        #                  conditionalPanel("input.ano != 0",
                        #                                   tags$a(uiOutput('box1'),href='http://shiny.science.psu.edu/auc39/ANOVA/',target="_blank"))
                        #           )
                        #         ),br(),br(),
                        #         fluidRow(
                        #           column(3, bsButton('regression' ,HTML('<b> X </b>:Continuous <br/>  Y: Continuous'),type = 'toggle')),
                        #           column(1, 
                        #                  conditionalPanel("input.regression != 0",
                        #                                   img(src='line2.png',width=250,style='margin-top:6em;margin-left:6.5em'))
                        #           ),
                        #           column(5,offset=2,
                        #                  conditionalPanel("input.regression != 0",
                        #                                   tags$a(uiOutput('box2'),href='http://tjmcintyre.shinyapps.io/AssumptionsApp/',target="_blank"))
                        #           )
                        #         ),br(),br(),
                        #         fluidRow(
                        #           column(3, bsButton('anc',HTML('<b> X </b>:Categorical & Continuous <br/>  Y: Continuous'),type = 'toggle')),
                        #           column(1, 
                        #                  conditionalPanel("input.anc != 0",
                        #                                   img(src='line3.png',width=250,style='margin-top:6em;margin-left:6.5em'))
                        #           ),
                        #           column(5,offset=2,
                        #                  conditionalPanel("input.anc != 0",
                        #                                   uiOutput('box3'))
                        #           )
                        #         ),br(),
                        #         fluidRow(
                        #           column(3,offset=4,actionButton("go2","Go to the overview",icon("bolt"),style='padding:10px; font-size:100%',class="circle grow"))
                        #         )
                        #         
                        #         
                        # ),
                        
                        
                       
                        
                        
                        
                        tabItem(tabName ="exploring",
                                # div(style="display: inline-block;vertical-align:top;",
                                #     tags$a(href='https://shinyapps.science.psu.edu/',tags$img(src='homebut.PNG', width = 19))
                                # ),
                                # div(style="display: inline-block;vertical-align:top;",
                                #     circleButton("infobut",icon = icon("info"), status = "myClass",size = "xs")
                                # ),
                                h3('ANCOVA Interaction Plot'),
                                sidebarLayout(
                                  sidebarPanel(
                                    
                                    selectInput('menu1','Select the Data',c('Otter','Diet','Random')),
                                    conditionalPanel("input.menu1=='Diet'",style='font-size:14px;',
                                                     tags$style(HTML(".radio-inline {margin-right: 8%;}")),
                                                     radioButtons('select_conti', 'Select Continous Variable',inline=TRUE, choices =c('Age','Height','Pre-diet Weight'), selected = 'Age'),
                                                     radioButtons('select_covar', 'Select Covariance',inline=TRUE,choices =c('Gender','Diet'), selected = 'Gender')
                                    ),
                                    
                                    conditionalPanel("input.menu1=='Random'",
                                                     sliderInput('slope1','Change the slope of Line A',-5,5,0,step=1),
                                                     sliderInput('slope2','Change the slope of Line B',-5,5,0,step=1),
                                                     sliderInput('inter1','Change the intersection of Line A',-5,5,0,step=1),
                                                     sliderInput('inter2','Change the intersection of Line B',-5,5,0,step=1),
                                                     sliderInput('sample','Change the sample size',100,800,100,step=50)
                                                     
                                                     
                                                     
                                    ),
                                    fluidRow(uiOutput('p'),align = "left"),
                                    
                                    conditionalPanel("input.menu1!='Random'",
                                                     downloadButton("downloadData", "Download the Dataset")
                                    )
                                  ),
                                  
                                  mainPanel(
                                    plotOutput('plot_gg'),
                                    bsPopover('plot_gg', 'Notice', 'Different lines represent different values of covariate. Remember intersection does not imply significant interaction.', placement = "bottom", trigger = "hover", options = NULL),
                                    tags$b(verbatimTextOutput('analysis1')),
                                    bsPopover('analysis1', 'ANOVA Table', 'Pay attention to the last column. Small p-value indicates siginificant influence or interaction.', placement = "top", trigger = "hover", options = NULL),
                                    
                                    div(style = "text-align: center",
                                        actionButton("game","go gaming!",icon("bolt"),class="circle grow"))
                                  )
                                )
                                
                                
                        ),
                        
                        
                        
                        tabItem(tabName='game',
                                useShinyalert(),
                                
                                
                                fluidRow(column(5,numericInput('seconds','Select the time limit (second)',value=60,min=60,max=300,step=120),
                                                bsPopover('seconds', 'Timer', 'Type in the time limit (in seconds) you want for this game. Click "Start" to start the timer and play the game', placement = "right", trigger = "hover", options = NULL),
                                                fluidRow(column(2,actionButton('start_timer','Start',style='padding:5px; font-size:90%')),
                                                         #column(2,actionButton('set','Set Timer',style='padding:5px; font-size:90%')),
                                                         column(2,actionButton('reset','Reset',style='padding:5px; font-size:90%')),
                                                         # column(1, tags$a(href='https://shinyapps.science.psu.edu/',tags$img(src='homebut.PNG', width = 30))),
                                                         # column(1, bsButton('bq1', '',icon = icon('info',class = "iconq fa-fw"),type = 'toggle', class = 'butt',style='padding:20px'),
                                                         #        div(id = "plot-container1",
                                                         #            conditionalPanel("input.bq1 != 0",
                                                         #                             tags$img(src = "INS.png",
                                                         #                                      id = "ins"))
                                                         #        )
                                                         # ),
                                                         # 
                                                         column(1, bsButton('bq2', '',icon = icon('question',class = "iconq fa-fw"),type = 'toggle', class = 'butt',style='padding:20px'),
                                                                div(id = "plot-container2",
                                                                    conditionalPanel("input.bq2 != 0",
                                                                                     tags$img(src = "STAT.png",
                                                                                              id = "hint"))
                                                                )
                                                         )
                                                         
                                                        
                                                         ), br(),
                                                
                                                fluidRow(
                                                  valueBoxOutput("scoreBox"),
                                                  valueBoxOutput('percentBox'),
                                                  valueBoxOutput('timeBox')
                                                  
                                                  
                                                  
                                                )
                                ),
                                column(3,offset=4,textOutput("timeleft"))),
                                
                                fluidRow(column(2, offset=1,uiOutput('a'),align='right',style='margin-top:-1em;'),
                                         column(2,offset=2,uiOutput('b'),align='right',style='margin-top:-1em;'),
                                         column(2,offset=2,uiOutput('c'),align='left',style='margin-top:-1em;')),
                                
                                fluidRow(column(4,uiOutput('plot1')),
                                         column(4,uiOutput('plot2')),
                                         column(4,uiOutput('plot3'))
                                ),
                                
                                hr(),
                                fluidRow(column(4,uiOutput('table1'),style='margin-top:-2em;'),
                                         column(4,uiOutput('table2'),style='margin-top:-2em;'),
                                         column(4,uiOutput('table3'),style='margin-top:-2em;')
                                ),
                                
                                
                                fluidRow(column(3,offset=1,style='padding:20px;margin-top:-2em;font-size: 15px',fluidRow(radioButtons('radio1','',c('A','B','C'),selected='',inline=TRUE),fluidRow(uiOutput('answer1'),style='margin-top:-1em;'))),
                                         column(3,offset=1,style='padding:20px;margin-top:-2em;font-size: 15px',fluidRow(radioButtons('radio2','',c('A','B','C'),selected='',inline=TRUE),fluidRow(uiOutput('answer2'),style='margin-top:-1em;'))),
                                         column(3,offset=1,style='padding:20px;margin-top:-2em;font-size: 15px',fluidRow(radioButtons('radio3','',c('A','B','C'),selected='',inline=TRUE),fluidRow(uiOutput('answer3'),style='margin-top:-1em;')))
                                ),
                                
                                fluidRow(
                                  #column(4, uiOutput('correctC')),
                                  column(1,offset = 3,actionButton("submitA", "Submit Answer",style='padding:5px; font-size:110%;margin-top:-2em;')),
                                  column(1,offset = 3,actionButton("new","New>>",style='padding:5px; font-size:110%;margin-top:-2em;', disabled = FALSE))
                                )
                                
                                
                                
                        )#closing for game section
                        
                        
                        
                      )
                      
                      
                      )                  
                        )))