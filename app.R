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

ui <- list(
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
                p("First, choose a dataset to explore. Then, 
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
                h2("Matching Game"),
                p("Select the letter of the plot that matches each of the outputs 
                   below. Then submit your answers to check if you are correct."),
                fluidRow(
                  column(
                    width = 6,
                    uiOutput("plotCol")
                  ),
                  column(
                    width = 6,
                    offset = 0,
                    uiOutput("outputCol")
                  )
                ),
                br(),
                fluidRow(
                  column(
                    width = 2,
                    p("Feedback:")
                  ),
                  column(
                    width = 1,
                    p("1st Positions:")
                  ),
                  column(
                    width = 1,
                    uiOutput("feedbackP1")
                  ),
                  column(
                    width = 1,
                    p("2nd Positions:")
                  ),
                  column(
                    width = 1,
                    uiOutput("feedbackP2")
                  ),
                  column(
                    width = 1,
                    p("3rd Positions:")
                  ),
                  column(
                    width = 1,
                    uiOutput("feedbackP3")
                  )
                ),
                fluidRow(
                  column(
                    width = 2,
                    offset = 4,
                    bsButton(
                      inputId = "submit",
                      label = "Submit",
                      size = "large",
                      style = "default"
                    )
                  ),
                  column(
                    width = 2,
                    bsButton(
                      inputId = "new",
                      label = "New",
                      size = "large",
                      style = "default"
                    )
                    
                  )
                ),
                  

                
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
  )
  

)




#Server ----

seaotters <- read.csv("Otter.csv",header=T)

diet <- read.csv("Diet.csv",header=T)
diet$Diet<-as.character(diet$Diet)

bank = read.csv("questionbank.csv")
bank = data.frame(lapply(bank, as.character), stringsAsFactors = FALSE)




server <- function(input, output, session) {
  observeEvent(
    eventExpr = input$info,
    handlerExpr = {
      sendSweetAlert(
        session = session,
        title = "Instructions:",
        type = NULL,
        closeOnClickOutside = TRUE,
        text = "Pick a data set and variables to view the interaction plot and the 
      associated ANOVA table."
      )
    })
  output$ack2 <- renderUI((
    h4('Thanks for the data set and code provided by The University of Sheffield 
       (',url,') and Dr.Dylan Childs(',url2,').')
  ))
  
  url <- a("www.sheffield.ac.uk/mash/data", 
           href="https://www.sheffield.ac.uk/mash/data",
           target="_blank")
  url2 <- a("github.com/dzchilds", 
            href="https://github.com/dzchilds",
            target="_blank")
  
  # output$box1<-renderUI(h4('ANOVA is used for comparing three or more group means. 
  #                          Different groups are different levels of categorical variables, and group means are calculated from continuous variables.
  #                          ',br(),br(),'EX. Are the average score of three STAT 200 sections significantly different from each other?'))
  # 
  # output$box2<-renderUI(h4('Regression is used for determining the relationship between two continuous variables. One dependent variable (Y) can also be affected by multiple independent variables (X).  
  #                          ',br(),br(),'EX. How will crime rate be impacted by population density, unemployment rate, and income.'))
  # 
  # output$box3<-renderUI(h4('ANCOVA is adding continuous variables onto ANOVA analysis, which is called covariate. 
  #                          Significant different between group means and significant relationship between continuous variables will both be analyzed.
  #                          ',br(),br(),'EX. Who makes the most money? Will gender or years after graduation influence the income? '))
  
  ####button###
  
  observeEvent(
    eventExpr = input$go,
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "pages",
        selected = "prereq")
    })
  observeEvent(
    eventExpr = input$start,
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "pages",
        selected = "exploring")
    })
  observeEvent(
    eventExpr = input$game,
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "pages",
        selected = "game")
    })
  
  
  #Download the dataset ----
  
  # Reactive value for selected dataset ----
  datasetInput <- reactive({
    switch(
      EXPR = input$menu1,
      'Otter' = seaotters,
      'Diet' = diet)
  })
  
  # Downloadable csv of selected dataset ----
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$menu1, 
            ".csv", 
            sep = "")
    },
    content = function(con) {
      write.csv(datasetInput(), 
                con)
    }
  )
  
  #Explore page ----
  
  
  ###prep the data###
  otters.model <- lm(Otters ~ Location + Year + Location:Year, 
                     data = seaotters)
  pred.data <- expand.grid(Year = 1992:2003, 
                           Location = c("Lagoon", "Bay"))
  pred.data <- mutate(pred.data, 
                      Otters = predict(otters.model, pred.data))
  
  diet.model<-lm(ab_change ~ gender + Diet + Age + Height + pre.weight + gender:Age 
                 + gender:Height + gender:pre.weight + Diet:Age + Diet:Height + 
                   Diet:pre.weight,
                 data = diet)
  
  diet.model2<-lm(ab_change ~ Age + gender + Age:gender,
                  data=diet)
  pred.data2 <- expand.grid(Age = 16:60, gender = c("M", "F"))
  pred.data2 <- mutate(pred.data2, ab_change = predict(diet.model2, pred.data2))
  
  diet.model3 <- lm(ab_change ~ Height + gender + Height:gender,
                    data=diet)
  pred.data3 <- expand.grid(Height = 141:201, gender = c("M", "F"))
  pred.data3 <- mutate(pred.data3, ab_change = predict(diet.model3, pred.data3))
  
  diet.model4 <- lm(ab_change ~ pre.weight + gender + pre.weight:gender,
                    data=diet)
  pred.data4 <- expand.grid(pre.weight = 58:103, gender = c("M", "F"))
  pred.data4 <- mutate(pred.data4, ab_change = predict(diet.model4, pred.data4))
  
  diet.model5 <- lm(ab_change ~ Age + Diet + Age:Diet,
                    data=diet)
  pred.data5 <- expand.grid(Age = 16:60, Diet = c('1','2','3'))
  pred.data5 <- mutate(pred.data5, ab_change = predict(diet.model5, pred.data5))
  
  diet.model6 <- lm(ab_change ~ Height + Diet + Height:Diet,
                    data=diet)
  pred.data6 <- expand.grid(Height = 141:201, Diet = c('1','2','3'))
  pred.data6 <- mutate(pred.data6, ab_change = predict(diet.model6, pred.data6))
  
  
  diet.model7 <- lm(ab_change ~ pre.weight + Diet + pre.weight:Diet,
                    data = diet)
  pred.data7 <- expand.grid(pre.weight = 58:103, Diet = c('1','2','3'))
  pred.data7 <- mutate(pred.data7, ab_change = predict(diet.model7, pred.data7))
  
  
  ###save random model
  rand<-reactiveValues(rand_mod=NULL)
  
  ###Graph the plot of interaction###
  
  output$plot_gg <- renderPlot(
    if (input$menu1 == 'Otter') {
      ggplot(pred.data, 
             aes(x = Year, 
                 y = Otters, 
                 colour = Location)) +
        geom_line() + 
        geom_point(data = seaotters) + 
        xlab("Year") + 
        ylab("Otters") +
        theme(text = element_text(size = 20),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_blank(), 
              axis.line = element_line(colour = "black"))
    }
    else if (input$menu1 == 'Diet') {
      if (input$select_conti == 'Age' & input$select_covar == 'Gender') {
        ggplot(pred.data2, 
               aes(x = Age, 
                   y = ab_change, 
                   colour = gender)) + 
          geom_line() + 
          geom_point(data = diet) + 
          xlab("Age") + 
          ylab("Decrease in Weight") +
          theme(text = element_text(size=20),
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                panel.background = element_blank(), 
                axis.line = element_line(colour = "black"))
      }
      else if (input$select_conti == 'Height' & input$select_covar == 'Gender') {
        ggplot(pred.data3, 
               aes(x = Height, 
                   y = ab_change, 
                   colour = gender)) + 
          geom_line() + 
          geom_point(data = diet) + 
          xlab("Height") + 
          ylab("Decrease in Weight") +
          theme(text = element_text(size=20),
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                panel.background = element_blank(), 
                axis.line = element_line(colour = "black"))
      }
      else if (input$select_conti == 'Pre-diet Weight' & input$select_covar == 'Gender') {
        ggplot(pred.data4, 
               aes(x = pre.weight, 
                   y = ab_change, 
                   colour = gender)) + 
          geom_line() + 
          geom_point(data = diet) + 
          xlab("Pre-diet Weight") + 
          ylab("Decrease in Weight") +
          theme(text = element_text(size=20),
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                panel.background = element_blank(), 
                axis.line = element_line(colour = "black"))
      }
      else if (input$select_conti == 'Age' & input$select_covar=='Diet') {
        ggplot(pred.data5, 
               aes(x = Age, 
                   y = ab_change, 
                   colour =Diet)) + 
          geom_line() + 
          geom_point(data = diet) + 
          xlab("Age") + 
          ylab("Decrease in Weight") +
          theme(text = element_text(size=20),
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                panel.background = element_blank(), 
                axis.line = element_line(colour = "black"))
      }
      else if (input$select_conti == 'Height' & input$select_covar == 'Diet') {
        ggplot(pred.data6, 
               aes(x = Height, 
                   y = ab_change, 
                   colour = Diet)) + 
          geom_line() + 
          geom_point(data = diet) + 
          xlab("Height") + 
          ylab("Decrease in Weight") +
          theme(text = element_text(size=20),
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                panel.background = element_blank(), 
                axis.line = element_line(colour = "black"))
      }
      else if (input$select_conti == 'Pre-diet Weight' & input$select_covar == 'Diet') {
        ggplot(pred.data7, 
               aes(x = pre.weight, 
                   y = ab_change, 
                   colour = Diet)) + 
          geom_line() + 
          geom_point(data = diet) + 
          xlab("Pre-diet Weight") + 
          ylab("Decrease in Weight") +
          theme(text = element_text(size=20),
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                panel.background = element_blank(), 
                axis.line = element_line(colour = "black"))
      }
    }
    
    else if (input$menu1 == 'Random'){
      
      
      ###create data with label A and B with different slope and intersection
      
      A<-'A'
      B<-'B'
      
      a<-input$inter1
      b<-input$inter2
      
      slope1<-input$slope1
      slope2<-input$slope2
      
      def <- defData(varname = "inter", dist = "nonrandom", formula = a, id = "id")
      
      def<- defData(def,varname = "slope", dist = "nonrandom", formula = slope1, id = "slope")
      def <- defData(def, varname = "X", dist = "uniform", formula = "0;20")
      def <- defData(def, varname = "Y", formula = "inter + X * slope", variance = 11)
      
      def2<- defData(varname = "inter", dist = "nonrandom", formula = b, id = "id")
      
      def2 <- defData(def2,varname = "slope", dist = "nonrandom", formula = slope2, id = "slope")
      def2<- defDataAdd(def2, varname = "X", dist = "uniform", formula = "0;20")
      def2 <- defDataAdd(def2, varname = "Y", formula = "inter + X * slope", variance =11)
      
      
      dt <- genData(input$sample, def)
      dt2<-genData(input$sample,def2)
      
      names(dt2)[1]<-'id'
      
      dt$cov<-'A'
      dt2$cov<-'B'
      
      comb<-rbind(dt,dt2)
      
      
      aov.model<-lm(Y~X+cov+cov:X,data=comb)
      
      
      
      pred.aov <- expand.grid(X =0:20, cov = c("A","B"))
      pred.aov <- mutate(pred.aov, Y = predict(aov.model, pred.aov))
      
      
      ggplot(pred.aov, aes(x = X, y = Y, colour = cov)) + 
        geom_line() + geom_point(data = comb) + 
        xlab("X") + ylab("Y")+theme(text = element_text(size=20),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                    panel.background = element_blank(), axis.line = element_line(colour = "black"))}
    
    
    
  )
  
  
  
  
  ###ANCOVA analysis table###
  output$analysis1<-renderPrint(
    
    
    if (input$menu1=='Otter') {anova(otters.model)}
    else if (input$menu1=='Diet'){
      if (input$select_conti=='Age' & input$select_covar=='Gender'){anova(diet.model2)}
      else if (input$select_conti=='Height' & input$select_covar=='Gender'){anova(diet.model3)}
      else if (input$select_conti=='Pre-diet Weight' & input$select_covar=='Gender'){anova(diet.model4)}
      else if (input$select_conti=='Age' & input$select_covar=='Diet'){anova(diet.model5)}
      else if (input$select_conti=='Height' & input$select_covar=='Diet'){anova(diet.model6)}
      else if (input$select_conti=='Pre-diet Weight' & input$select_covar=='Diet'){anova(diet.model7)}
    }
    else if (input$menu1=='Random'){
      A<-'A'
      B<-'B'
      
      a<-input$inter1
      b<-input$inter2
      
      slope1<-input$slope1
      slope2<-input$slope2
      
      def <- defData(varname = "inter", dist = "nonrandom", formula = a, id = "id")
      
      def<- defData(def,varname = "slope", dist = "nonrandom", formula = slope1, id = "slope")
      def <- defData(def, varname = "X", dist = "uniform", formula = "10;20")
      def <- defData(def, varname = "Y", formula = "inter + X * slope", variance = 11)
      
      def2<- defData(varname = "inter", dist = "nonrandom", formula = b, id = "id")
      
      def2 <- defData(def2,varname = "slope", dist = "nonrandom", formula = slope2, id = "slope")
      def2<- defDataAdd(def2, varname = "X", dist = "uniform", formula = "10;20")
      def2 <- defDataAdd(def2, varname = "Y", formula = "inter + X * slope", variance = 11)
      
      
      dt <- genData(input$sample, def)
      dt2<-genData(input$sample,def2)
      
      names(dt2)[1]<-'id'
      
      dt$cov<-'A'
      dt2$cov<-'B'
      
      comb<-rbind(dt,dt2)
      
      
      aov.model<-lm(Y~X+cov+cov:X,data=comb)
      
      ##testing passing the model
      rand$rand_mod<-anova(aov.model)[3,"Pr(>F)"]
      
      anova(aov.model)
      
      
    }
    
    
  )
  
  
  #####get p values for each interaction 
  
  var<-reactiveValues(p=NULL)
  observe({
    
    # if (is.null(input$menu1)){
    #   return()
    # }
    # 
    # isolate({
    #   var$p<-as.numeric(anova(otters.model)[3,"Pr(>F)"])
    #  
    # })
    
    
    if (input$menu1=='Otter') {var$p<-as.numeric(anova(otters.model)[3,"Pr(>F)"])}
    else if (input$menu1=='Diet'){
      if (input$select_conti=='Age' & input$select_covar=='Gender'){var$p<-as.numeric(anova(diet.model2)[3,"Pr(>F)"])}
      else if (input$select_conti=='Height' & input$select_covar=='Gender'){var$p<-as.numeric(anova(diet.model3)[3,"Pr(>F)"])}
      else if (input$select_conti=='Pre-diet Weight' & input$select_covar=='Gender'){var$p<-as.numeric(anova(diet.model4)[3,"Pr(>F)"])}
      else if (input$select_conti=='Age' & input$select_covar=='Diet'){var$p<-as.numeric(anova(diet.model5)[3,"Pr(>F)"])}
      else if (input$select_conti=='Height' & input$select_covar=='Diet'){var$p<-as.numeric(anova(diet.model6)[3,"Pr(>F)"])}
      else if (input$select_conti=='Pre-diet Weight' & input$select_covar=='Diet'){var$p<-as.numeric(anova(diet.model7)[3,"Pr(>F)"])}
    }
    else if (input$menu1=='Random'){var$p<-as.numeric(rand$rand_mod)}
  })
  
  output$p<-renderUI(
    if (var$p<=0.05){
      h4(strong('P-value for this interaction is',signif(var$p,4),'.' ,br(),'Since the p-value is smaller than 0.05
                (α=0.05), there is a statistically significant interaction between these two variables.'))}
    else {h4(strong('P-value for this interaction is',signif(var$p,4),'.' ,br(),'Since the p-value is greater than 0.05
                    (α=0.05), there is NOT a statistically significant interaction between these two variables.'))}
  )
  
  
  
  
  
  #  Game ----
  numbers <- reactiveValues(strong = c(), moderate = c(), insig = c(), index = c(), question = data.frame())
  
  observeEvent(input$pages,{
    numbers$strong = sample(1:12,1)
    numbers$moderate = sample(13:24,1)
    numbers$insig= sample(25:36,1)
    
    
    numbers$index =c("A","B","C")
    numbers$question = cbind(bank[c(numbers$strong,numbers$moderate,numbers$insig),],numbers$index)
    
  })
  
  observeEvent(input$new,{
    numbers$strong = sample(1:12,1)
    numbers$moderate = sample(13:24,1)
    numbers$insig= sample(25:36,1)
    
    
    numbers$index = c("A","B","C")
    numbers$question = cbind(bank[c(numbers$strong,numbers$moderate,numbers$insig),],numbers$index)
    
    # updateRadioButtons(session, 'radio1','',c('A','B','C'),selected='',inline=TRUE)
    # updateRadioButtons(session,"radio2", '',c('A','B','C'),selected='',inline=TRUE)
    # updateRadioButtons(session, "radio3", '',c('A','B','C'),selected='',inline=TRUE)
  })
  
  
  ## Set up matching columns ----
  output$plotCol <- renderUI({
    plots <- list(
      "1" = img(src = numbers$question[numbers$question[5] == "A",4], 
                width = "35%",
                class = "expandable"),
      "2" = img(src = numbers$question[numbers$question[5] == "B",4], 
                width = "35%", 
                class = "expandable"),
      "3" = img(src = numbers$question[numbers$question[5] == "C",4], 
                width = "35%",
                class = "expandable")
    )
    sortable::rank_list(
      input_id = "rankplots",
      text = "Drag the plots into the same order as the outputs.",
      labels = sample(plots, size = length(plots))
    )
  })
  
  output$outputCol <- renderUI({
    outputs <- list(
      "1" = img(src = numbers$question[numbers$question[5] == "A",3], 
                width = "60%",
                class = "expandable"),
      "2" = img(src = numbers$question[numbers$question[5] == "B",3], 
                width = "60%", 
                class = "expandable"),
      "3" = img(src = numbers$question[numbers$question[5] == "C",3], 
                width = "60%",
                class = "expandable")
    )
    sortable::rank_list(
      input_id = "rankoutputs",
      text = "Drag the outputs into the same order as the plots.",
      labels = sample(outputs, size = length(outputs))
    )
  })
  
  observeEvent(
    eventExpr = input$submit, 
    handlerExpr = {
    matches <- input$rankplots == input$rankoutputs
    
    for(i in 1:3){
      output[[paste0("feedbackP", i)]] <- boastUtils::renderIcon(
        icon = ifelse(
          test = matches[i], 
          yes = "correct", 
          no = "incorrect")
      )
    #   if (attempts$level1 == 1) {
    #     initScore$level1 <- sum(3 * as.integer(matches) - 1)
    #   } 
    #   else {
    #     subqScore$level1 <- sum(3 * as.integer(matches) - 1)
    #   }
    #   if (initScore$level1 == maxScores[1]) {subqScore$level1 <- maxScores[1]}
    # }
    # output$scoreL1 <- renderUI({
    #   paste("Your Score:", ifelse(attempts$level1 == 1,
    #                               initScore$level1,
    #                               subqScore$level1))
    }
    }
  )
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # output$plot1 <- renderUI({
  #   img(src = numbers$question[numbers$question[5] == "A",4], width = "100%", height = "107%", style = "text-align: center")
  # })
  # 
  # # output$table1 <- renderUI({
  # #   img(src = numbers$question[numbers$question[5] == "A",3], width = "100%", height = "100%", style = "text-align: center")
  # # })
  # 
  # output$plot2 <- renderUI({
  #   img(src = numbers$question[numbers$question[5] == "B",4], width = "100%", height = "107%", style = "text-align: center")
  # })
  # 
  # # output$table2 <- renderUI({
  # #   img(src = numbers$question[numbers$question[5] == "B",3], width = "100%", height = "100%", style = "text-align: center")
  # # })
  # 
  # output$plot3 <- renderUI({
  #   img(src = numbers$question[numbers$question[5] == "C",4], width = "100%", height = "107%", style = "text-align: center")
  # })
  # 
  # # output$table3 <- renderUI({
  # #   img(src = numbers$question[numbers$question[5] == "C",3], width = "100%", height = "100%", style = "text-align: center")
  # # })
  # 
  # #######randomize the table######
  # 
  # index2 <- reactiveValues(index2 = 3)
  # 
  # observeEvent(input$new,{index2$index2 <- sample(1:4,1, replace=TRUE, prob=NULL)
  # })
  # 
  # observeEvent(input$reset,{index2$index2 <- sample(1:4,1, replace=TRUE, prob=NULL)
  # })
  # 
  # output$table1<-renderUI({
  #   if (index2$index2==1){img(src = numbers$question[numbers$question[5] == "A",3], width = "105%", height = "105%", style = "text-align: center")}
  #   else if (index2$index2==2){img(src = numbers$question[numbers$question[5] == "B",3], width = "105%", height = "105%", style = "text-align: center")}
  #   else if (index2$index2==3){img(src = numbers$question[numbers$question[5] == "C",3], width = "105%", height = "105%", style = "text-align: center")}
  #   else if (index2$index2==4){img(src = numbers$question[numbers$question[5] == "A",3], width = "105%", height = "105%", style = "text-align: center")}
  # })
  # 
  # output$table2<-renderUI({
  #   if (index2$index2==1){img(src = numbers$question[numbers$question[5] == "B",3], width = "105%", height = "105%", style = "text-align: center")}
  #   else if (index2$index2==2){img(src = numbers$question[numbers$question[5] == "C",3], width = "105%", height = "105%", style = "text-align: center")}
  #   else if (index2$index2==3){img(src = numbers$question[numbers$question[5] == "A",3], width = "105%", height = "105%", style = "text-align: center")}
  #   else if (index2$index2==4){img(src = numbers$question[numbers$question[5] == "C",3], width = "105%", height = "105%", style = "text-align: center")}
  # })
  # 
  # output$table3<-renderUI({
  #   if (index2$index2==1){img(src = numbers$question[numbers$question[5] == "C",3], width = "105%", height = "105%", style = "text-align: center")}
  #   else if (index2$index2==2){img(src = numbers$question[numbers$question[5] == "A",3], width = "105%", height = "105%", style = "text-align: center")}
  #   else if (index2$index2==3){img(src = numbers$question[numbers$question[5] == "B",3], width = "105%", height = "105%", style = "text-align: center")}
  #   else if (index2$index2==4){img(src = numbers$question[numbers$question[5] == "B",3], width = "105%", height = "105%", style = "text-align: center")}
  # })
  # 
  # 
  # 
  # ####letter for the plot
  # output$a<-renderUI(h4('A'))
  # output$b<-renderUI(h4('B'))
  # output$c<-renderUI(h4('C'))
  # #####buttons####
  # 
  # observeEvent(input$submitA,{
  #   updateButton(session,"submitA",disabled = TRUE)
  # })
  # observeEvent(input$new,{
  #   updateButton(session,"submitA",disabled = FALSE)
  # })
  # 
  # observeEvent(input$submitA,{
  #   updateButton(session,"new",disabled = FALSE)
  # })
  # 
  # observeEvent(input$new,{
  #   updateButton(session,"new",disabled = TRUE)
  # })
  # 
  # 
  # observeEvent(input$new, {
  #   reset("radio1")
  # })
  # 
  # 
  # ###################check answers#####
  # 
  # summationC<-reactiveValues(correct1 = c(0), started=FALSE)
  # 
  # observeEvent(input$submitA,{
  #   observeEvent(input$new,{
  #     output$answer1 <- renderUI({
  #       img(src = NULL,width=30)
  #     })
  #   })
  #   observe({
  #     output$answer1 <- renderUI({
  #       if (!is.null(input$radio1)){
  #         if (index2$index2==1 &input$radio1 == 'A'){
  #           img(src = "check.png",width=30)
  #         }
  #         else if (index2$index2==2 &input$radio1 == 'B') {img(src = "check.png",width=30)}
  #         else if (index2$index2==3 &input$radio1 == 'C'){img(src = "check.png",width=30)}
  #         else if (index2$index2==4 &input$radio1 == 'A'){img(src = "check.png",width =30)}
  #         else{
  #           img(src = "cross.png",width=30)
  #         }
  #       }
  #     })
  #   })
  # })
  # 
  # 
  # observeEvent(input$submitA,{
  #   observeEvent(input$new,{
  #     output$answer2 <- renderUI({
  #       img(src = NULL,width=30)
  #     })
  #   })
  #   observe({
  #     output$answer2 <- renderUI({
  #       if (!is.null(input$radio2)){
  #         if (index2$index2==1 &input$radio2 == 'B'){
  #           img(src = "check.png",width=30)
  #           
  #         }
  #         else if (index2$index2==2 &input$radio2 == 'C') {img(src = "check.png",width=30)}
  #         else if (index2$index2==3 &input$radio2 == 'A'){img(src = "check.png",width=30)}
  #         else if (index2$index2==4 &input$radio2 == 'C'){img(src = "check.png",width=30)}
  #         else{
  #           img(src = "cross.png",width=30)
  #         }
  #       }
  #     })
  #   })
  # })
  # 
  # observeEvent(input$submitA,{
  #   observeEvent(input$new,{
  #     output$answer3 <- renderUI({
  #       img(src = NULL,width=30)
  #     })
  #   })
  #   observe({
  #     output$answer3 <- renderUI({
  #       if (!is.null(input$radio3)){
  #         if (index2$index2==1 &input$radio3 == 'C'){
  #           img(src = "check.png",width=30)
  #         }
  #         else if (index2$index2==2 &input$radio3 == 'A') {img(src = "check.png",width=30)}
  #         else if (index2$index2==3 &input$radio3 == 'B'){img(src = "check.png",width=30)}
  #         else if (index2$index2==4 &input$radio3 == 'B'){img(src = "check.png",width=30)}
  #         else{
  #           img(src = "cross.png",width=30);
  #           
  #         }
  #       }
  #     })
  #   })
  # })
  # 
  # 
  # #####count correct answer ########
  # summationC<-reactiveValues(correct1 = c(0),total=c(), started=FALSE)
  # 
  # observeEvent(input$submitA,{
  #   for (i in input$radio1){
  #     summationC$total = c(summationC$total,1)
  #     if (index2$index2==1 &input$radio1 == 'A'){
  #       summationC$correct1 = c(summationC$correct1,1)
  #     }
  #     else if (index2$index2==2 &input$radio1 == 'B') { summationC$correct1 = c(summationC$correct1,1)}
  #     else if (index2$index2==3 &input$radio1 == 'C'){summationC$correct1 = c(summationC$correct1,1)}
  #     else if (index2$index2==4 &input$radio1 == 'A'){summationC$correct1 = c(summationC$correct1,1)}
  #     else{
  #       summationC$correct1 = c(summationC$correct1,0)}
  #     
  #   }
  #   
  #   for (i in input$radio2){
  #     summationC$total = c(summationC$total,1)
  #     if (index2$index2==1 &input$radio2 == 'B'){
  #       
  #       summationC$correct1 = c(summationC$correct1,1)
  #       
  #     }
  #     else if (index2$index2==2 &input$radio2 == 'C') {summationC$correct1 = c(summationC$correct1,1)}
  #     else if (index2$index2==3 &input$radio2 == 'A'){summationC$correct1 = c(summationC$correct1,1)}
  #     else if (index2$index2==4 &input$radio2 == 'C'){summationC$correct1 = c(summationC$correct1,1)}
  #     else{
  #       
  #       summationC$correct1 = c(summationC$correct1,0)}
  #   }
  #   
  #   
  #   for (i in input$radio3){
  #     summationC$total = c(summationC$total,1)
  #     if (index2$index2==1 &input$radio3 == 'C'){
  #       i
  #       summationC$correct1 = c(summationC$correct1,1)
  #     }
  #     else if (index2$index2==2 &input$radio3 == 'A') {summationC$correct1 = c(summationC$correct1,1)}
  #     else if (index2$index2==3 &input$radio3 == 'B'){summationC$correct1 = c(summationC$correct1,1)}
  #     else if (index2$index2==4 &input$radio3 == 'B'){ summationC$correct1 = c(summationC$correct1,1)}
  #     else{
  #       
  #       summationC$correct1 = c(summationC$correct1,0)}
  #   }
  # })
  # 
  # 
  # 
  # output$correctC <- renderPrint({
  #   if (sum(c(summationC$correct1))==0) {cat("You have earned 0 points")}
  #   else{
  #     cat("You have earned",sum(c(summationC$correct1)),'points')}
  # })
  

  
  # ########show up the scoreing panel and popup####
  # 
  # 
  # # observer that invalidates every second. If timer is active, decrease by one.
  # observe({
  #   invalidateLater(1000, session)
  #   isolate({
  #     if(active())
  #     {
  #       timer(timer()-1)
  #       if(timer()<1)
  #       {
  #         active(FALSE)
  #         shinyalert('Count Down Complete','Click to see your score',
  #                    type = "success")
  #         
  #         output$scoreBox <- renderValueBox({
  #           valueBox(
  #             paste0(sum(c(summationC$correct1))), "Totel Score", icon = icon("list"),
  #             color = "purple"
  #           )
  #         })
  #         
  #         output$percentBox <- renderValueBox({
  #           if (sum(c(summationC$correct1))==0){valueBox(
  #             paste0('0%'), "Accuracy", icon = icon("thumbs-up", lib = "glyphicon"),
  #             color = "light-blue"
  #           )}
  #           else{
  #             valueBox(
  #               paste0(round(sum(c(summationC$correct1))/sum(c(summationC$total))*100,digit=1),'%'), "Accuracy", icon = icon("thumbs-up", lib = "glyphicon"),
  #               color = "light-blue"
  #             )}
  #         })
  #         
  #         output$timeBox <- renderValueBox({
  #           valueBox(
  #             paste0(input$seconds,'s'), "Time Used", icon = icon("time",lib = "glyphicon"),
  #             color = "maroon"
  #           )
  #         })
  #         
  #         
  #       }
  #       
  #     }
  #   })
  # })
  # 
  
  
  # observers for actionbuttons
  observeEvent(input$start_timer, {active(TRUE)})
  #observeEvent(input$stop, {active(FALSE)})
  observeEvent(input$start_timer, {timer(input$seconds)})
  observeEvent(input$reset, {timer(input$seconds);active(FALSE)
    output$scoreBox<-NULL; 
    output$percentBox<-NULL;
    output$timeBox<-NULL;
    summationC$correct1 <- c(0); summationC$total=c()
  })
  
  
  ##closing for ui DON'T DELET####  
}

boastUtils::boastApp(ui = ui, server = server)
