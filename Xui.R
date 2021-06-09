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
library(boastUtils)

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
                       tabName = "exploring", 
                       icon = icon("wpexplorer")
              ),
              menuItem(text = "Game", 
                       tabName = "game", 
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
                         modified by Zhiruo Wang and Lydia Bednarczyk."),
                      uiOutput('ack2')
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
                          h4("ANCOVA is used by adding continuous variables onto ANOVA 
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
                      fluidRow(
                        column(
                          width = 11,
                          offset = 2,
                          tags$figure(
                            align = "center",
                            tags$img(
                              src = "plot.png",
                              width = 550,
                              alt = "Picture of diagnostic plots"
                            ),
                            tags$figcaption("Image of four diagnostic plots.")
                          )
                          ),
                      h4('Model checking is a critical part of an analysis. You 
                         need to understand these four diagnostic plots:',
                         br(),
                         br(),
                         tags$li('The Residuals vs Fitted plot checks the linear 
                         pattern of residuals. If the linear model is correct, you 
                                 should expect a roughly horizontal line.'), 
                         br(),
                         tags$li('The Normal Q-Q plot checks normality. If the 
                                 normality assumption is true, you should expect 
                                 the dots to roughly follow a straight line.'),
                         br(),
                         tags$li('The Scale-Location plot checks for equal spread 
                                 of the residuals. If the equal variance assumption 
                                 is true, you should expect a roughly horizontal 
                                 line with the dots showing equal spread.'),
                         br(),
                         tags$li('The Residual vs Leverage plot checks for influential 
                                 outliers. Outliers with high leverage will appear 
                                 outside the dashed line range.')
                         ),
                      div(style = "text-align: center",
                         bsButton(inputId = "start",
                                  label = "Explore!",
                                  icon = icon("bolt"),
                                  style = "danger",
                                  size = "large")
                      )
                      )
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
                                h2('ANCOVA Interaction Plot'),
                                h4("First, choose a dataset to explore. Then, 
                                   adjust the inputs in order to see how they affect 
                                   the outcome. Use the p-value to determine if 
                                   there is a statistically significant interaction
                                   between variables in the datasets."),
                                sidebarLayout(
                                  sidebarPanel(
                                    selectInput(
                                      inputId = 'menu1',
                                      label = 'Select the Data',
                                      choices = c('Otter',
                                                  'Diet',
                                                  'Random')
                                      ),
                                    conditionalPanel(
                                      condition = "input.menu1 == 'Diet'",
                                      radioButtons(
                                        inputId = 'select_conti', 
                                        label = 'Select Continous Variable',
                                        inline = TRUE, 
                                        choices = c('Age',
                                                    'Height',
                                                    'Pre-diet Weight'), 
                                        selected = 'Age'
                                        ),
                                      radioButtons(
                                        inputId = 'select_covar', 
                                        label = 'Select Covariance',
                                        inline = TRUE,
                                        choices = c('Gender',
                                                    'Diet'), 
                                        selected = 'Gender'
                                        )
                                    ),
                                    conditionalPanel(
                                      condition = "input.menu1 == 'Random'",
                                      sliderInput(
                                        inputId = 'slope1',
                                        label = 'Change the slope of Line A',
                                        min = -5,
                                        max = 5,
                                        value = 0,
                                        step = 1)
                                      ,
                                      sliderInput(
                                        inputId = 'slope2',
                                        label = 'Change the slope of Line B',
                                        min = -5,
                                        max = 5,
                                        value = 0,
                                        step = 1
                                        ),
                                      sliderInput(
                                        inputId = 'inter1',
                                        label = 'Change the intersection of Line A',
                                        min = -5,
                                        max = 5,
                                        value = 0,
                                        step = 1
                                        ),
                                      sliderInput(
                                        inputId = 'inter2',
                                        label = 'Change the intersection of Line B',
                                        min = -5,
                                        max = 5,
                                        value = 0,
                                        step = 1
                                        ),
                                      sliderInput(
                                        inputId = 'sample',
                                        label = 'Change the sample size',
                                        min = 100,
                                        max = 800,
                                        value = 100,
                                        step = 50
                                        )
                                      ),
                                    fluidRow(
                                      uiOutput('p'),
                                      align = "left"),
                                    conditionalPanel(
                                      condition = "input.menu1 != 'Random'",
                                      downloadButton(
                                        outputId = "downloadData", 
                                        label = "Download the Dataset",
                                        icon = shiny::icon("download")
                                        )
                                    )
                                  ),
                                  
                                  mainPanel(
                                    plotOutput('plot_gg'),
                                    bsPopover('plot_gg', 'Notice', 'Different lines represent different values of covariate. Remember intersection does not imply significant interaction.', placement = "bottom", trigger = "hover", options = NULL),
                                    tags$b(verbatimTextOutput('analysis1')),
                                    bsPopover('analysis1', 'ANOVA Table', 'Pay attention to the last column. Small p-value indicates siginificant influence or interaction.', placement = "top", trigger = "hover", options = NULL),
                                    
                                    div(style = "text-align: center",
                                        bsButton(inputId = "game",
                                                 label = "Play!",
                                                 icon = icon("bolt"),
                                                 style = "danger",
                                                 size = "large")
                                    )
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
                                
                                
                                
                        ),
      tabItem(
        tabName = "refs",
        withMathJax(),
        h2("References"),
        p(
          class = "hangingindent",
          "https://educationshinyappteam.github.io/Style_Guide/index.html#organization"
        ),
        p(
          class = "hangingindent",
          "Bailey, E. (2015). shinyBS: Twitter bootstrap components for shiny. 
            (v0.61). [R package]. Available from https://CRAN.R-project.org/package=shinyBS"
        ),
        p(
          class = "hangingindent",
          "Carey, R. (2019). boastUtils: BOAST Utilities. (v0.1.0). [R Package]. 
            Available from https://github.com/EducationShinyAppTeam/boastUtils"
        ),
        p(
          class = "hangingindent",
          "Chang, W. and Borges Ribeio, B. (2018). shinydashboard: Create dashboards 
            with 'Shiny'. (v0.7.1) [R Package]. 
            Available from https://CRAN.R-project.org/package=shinydashboard"
        ),
        p(
          class = "hangingindent",
          "Chang, W., Cheng, J., Allaire, J., Xie, Y., and McPherson, J. (2019). 
            shiny: Web application framework for R. (v1.4.0) [R Package]. 
            Available from https://CRAN.R-project.org/package=shiny"
        ),
        p(
          class = "hangingindent",
          "Perrier, V., Meyer, F., Granjon, D. (2019). shinyWidgets: Custom inputs 
            widgets for shiny. (v0.5.0) [R Package]. Available from 
            https://CRAN.R-project.org/package=shinyWidgets"
        ),
        p(
          class = "hangingindent",
          "Hijmans, Robert J. (2021). raster: Geographic Data Analysis and Modeling. 
            (v3.4-10) [R Package]. Available from https://CRAN.R-project.org/package=raster"
        ),
        br(),
        br(),
        br(),
        boastUtils::copyrightInfo()
      )
      
      
      
      
                        
                        
                        
                      )
                      
                      
                      )                  
                        )))