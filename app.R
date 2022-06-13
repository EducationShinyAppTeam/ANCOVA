# Load packages ----
library(shiny)
library(shinyBS)
library(ggplot2)
library(dplyr)
library(shinydashboard)
library(simstudy)
library(shinyWidgets)
library(boastUtils)
library(fontawesome)
# library(sortable)

# Set constants ----
maxScore <- 10

# Load data ? ----

# Define UI ----
ui <- list(
  dashboardPage(
    skin = "black",
    ## Header ----
    dashboardHeader(
      title = "ANCOVA",
      titleWidth = 250,
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(class = "dropdown", boastUtils::surveyLink(name = "ANCOVA")),
      tags$li(
        class = "dropdown",
        tags$a(icon("home"), href = 'https://shinyapps.science.psu.edu/')
      )
    ),
    ## Sidebar ----
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "instruction", icon = icon("tachometer-alt")),
        menuItem("Prerequisites", tabName = "prereq", icon = icon("book")),
        menuItem("Explore", tabName = "exploring", icon = icon("wpexplorer")),
        menuItem("Game", tabName = "game", icon = icon("gamepad")),
        menuItem("References", tabName = "refs", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo", 
        boastUtils::sidebarFooter()
      )
    ),
    ## Body ----
    dashboardBody(
      tabItems(
        ### Overview page ----
        tabItem(
          tabName = "instruction",
          h1("Analysis of Covariance (ANCOVA)"),
          p('This app introduces the concept of Analysis of Covariance or ANCOVA
            focusing on interpreting interaction plots.'),
          br(),
          h2('Instructions'),
          tags$ul(
            tags$li('Click the prerequisite button to review the prerequisites 
                    if needed.'),
            tags$li('Navigate to the explore page. Use the dropdown menu to 
                    select a dataset.'),
            tags$li('Use the radio buttons to select different variables 
                    and see the changes in the interaction plot. You can also
                    use the slider bars to change the parameters.'),
            tags$li('After working with the explore section, you can start the
                    matching game to test your understanding of the concepts.'),
            tags$li('Drag the items in each column so that the plots in the
                    first column are in the same order as their corresponding
                    output in the second column.'),
            tags$li('Then, click Submit to check your answers. Once all of your
                    answers are correct, click New for a new set of plots.')  
          ),
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "go",
              label = "Prerequisites",
              icon = icon("book"),
              style = "default",
              size = "large"
            )
          ),
          br(),
          h2('Acknowledgements'),
          p("This app was developed and coded by Luxin Wang and modified by
            Zhiruo Wang, Lydia Bednarczyk, and Phichchaya Sutaporn.",
            br(),
            br(),
            "Cite this app as:",
            br(),
            boastUtils::citeApp(),
            br(),
            br(),
            div(class = "updated", "Last Update: 06/13/2022 by Phichchaya Sutaporn.")
          )
        ),
        ### Prerequisites page ----
        tabItem(
          tabName = "prereq",
          h2('Prerequisites'),
          p("What is ANCOVA? ANCOVA (Analysis of Covariance) is the Analysis of 
            Variance (ANOVA) with at least one continuous variables included in
            the model. The information below will explain highlight the differences
            between ANOVA, Regression, and ANCOVA."),
          br(),
          tags$table(
            tags$thead(
              tags$tr(
                tags$th(scope = "col", width = "120px", "Response Variable (Y)"),
                tags$th(scope = "col", width = "130px", "Explantory Variables (X's)"),
                tags$th(scope = "col", width = "120px", "Type of Analysis"),
                tags$th(scope = "col", "Explanation")
              )
            ),
            tags$tbody(
              tags$tr(
                tags$td("Continuous Response"),
                tags$td("Categorical Factor"),
                tags$td("ANOVA"),
                tags$td("ANOVA is used for comparing three or more group means.",
                        tags$br(),
                        "Different groups are different levels of categorical
                        variables, and group means are calculated from continuous 
                        variables.", 
                        tags$br(), 
                        "Example: Can differences between the average scores of
                        three STAT 200 sections be explained by chance?")
              ),
              tags$tr(
                tags$td("Continuous Response"),
                tags$td("Continuous Predictor"),
                tags$td("Regression"),
                tags$td("Regression is used for determining the relationship 
                        between two continuous variables.",
                        tags$br(),
                        "One dependent variable (Y) can also be affected by 
                        multiple independent variables (X).", 
                        tags$br(), 
                        "Example: How will crime rate be impacted by population 
                        density, unemployment rate, and income?")
              ),
              tags$tr(
                tags$td("Continuous Response"),
                tags$td("Categorical Factor AND Continuous Predictor"),
                tags$td("ANCOVA"),
                tags$td("ANCOVA is used by adding continuous variables onto ANOVA 
                        analysis, which is called covariate.",
                        tags$br(),
                        "Differences between group means and relationships between 
                        continuous variables are both analyzed.", 
                        tags$br(), 
                       "Example: Who makes the most money? Will gender or 
                        years after graduation influence the income?")
              )
            )
          ),
          br(),
          h3('Diagnostic Plots'),
          p('Model checking is a critical part of an analysis. You need to 
            understand these four diagnostic plots. In order to gain a better
            understanding of what properties of the plots satisfy the assumptions,
            look at the',
            a(href = 'https://psu-eberly.shinyapps.io/Assumptions_of_ANOVA/', 
              'Assumptions of ANOVA', class = 'bodylinks'), 'app.'),
          tags$ul(
            tags$li('The Residuals vs Fitted plot checks the linear 
                         pattern of residuals. If the linear model is correct, you 
                                 should expect a roughly horizontal line.'),
            tags$figure(
              align = "center",
              tags$img(
                src = "residualsvsfitted.PNG",
                width = 550,
                alt = "Picture of residuals vs fitted plot"
              ),
              tags$figcaption("Residuals vs fitted plot.")
            ),
            br(),
            tags$li('The Normal Q-Q plot checks normality. If the 
                                 normality assumption is true, you should expect 
                                 the dots to roughly follow a straight line.'),
            tags$figure(
              align = "center",
              tags$img(
                src = "normalqq.PNG",
                width = 550,
                alt = "Picture of normal QQ plot"
              ),
              tags$figcaption("Normal Q-Q plot for standardized residuals
                                       with somewhat heavy tails.")
            ),
            br(),
            tags$li('The Scale-Location plot checks for equal spread 
                                 of the residuals. If the equal variance assumption 
                                 is true, you should expect a roughly horizontal 
                                 line with the dots showing equal spread.'),
            tags$figure(
              align = "center",
              tags$img(
                src = "scalelocation.PNG",
                width = 550,
                alt = "Picture of a scale location plot"
              ),
              tags$figcaption("Scale-location plot with slight fanning.")
            ),
            br(),
            tags$li('The Residual vs Leverage plot checks for influential 
                                 outliers. Outliers with high leverage will appear 
                                 outside the dashed line range.'),
            tags$figure(
              align = "center",
              tags$img(
                src = "residualsvsleverage.PNG",
                width = 550,
                alt = "Picture of residuals vs leverage plot"
              ),
              tags$figcaption("Residuals vs leverage plot.")
            )),
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "start",
              label = "Explore!",
              icon = icon("bolt"),
              style = "default",
              size = "large"
            )
          )
        ),
        ### Explore page ----
        tabItem(
          tabName = "exploring",
          h2('ANCOVA Interaction Plot'),
          p("An important plot to explore for ANCOVA is an interaction plot. That
            is, a plot with which you can explore whether or not there is any type
            of interaction between your categorical factors and your continuous
            predictors."),
          p("To explore interaction plots, you'll first need to choose a dataset
            to explore. Then, adjust the inputs (if applicable) to see how they 
            affect the outcome. See how the p-value relates to the interaction
            plot. When you're done exploring, click the Play button to test your
            knowledge in the matching game."),
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
              )
            ),
            
            mainPanel(
              plotOutput('plot_gg'),
              tags$b(verbatimTextOutput('analysis1')),
            )
          ),
          fluidRow(
            div(style = "text-align: center",
                bsButton(inputId = "game",
                         label = "Play!",
                         icon = icon("bolt"),
                         style = "default",
                         size = "large")
            )
          )
        ),
        tabItem(tabName='game',
                h2("Matching Game"),
                p("Drag the items in each column so that the plots in the first column
                  are in the same order as their corresponding output in the second column. 
                  Then, click Submit to check your answers. Once all of your answers 
                  are correct, click New for a new set of plots."),
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
                    p("1st Position:")
                  ),
                  column(
                    width = 1,
                    uiOutput("feedbackP1")
                  ),
                  column(
                    width = 1,
                    p("2nd Position:")
                  ),
                  column(
                    width = 1,
                    uiOutput("feedbackP2")
                  ),
                  column(
                    width = 1,
                    p("3rd Position:")
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
                      style = "default",
                      #disabled = TRUE
                    )
                  )
                  # ,
                  # column(
                  #   width = 1,
                  #   p("Your score")
                  # ),
                  # column(
                  #   width = 1,
                  #   uiOutput("score")
                  # )
                ),
        ),
        tabItem(
          tabName = "refs",
          withMathJax(),
          h2("References"),
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
            "Chang, W. and Borges, R. B. (2018). shinydashboard: Create dashboards 
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
            "Childs, D. (2016) Otters.csv [dataset]. Available from 
            https://www.sheffield.ac.uk/polopoly_fs/1.570199!/file/stcp-Rdataset-Diet.csv"
          ),
          p(
            class = "hangingindent",
            "de Vries, A. (2020). sortable: Drag-and-Drop in 'shiny' Apps with
            'SortableJS'. (v0.4.4) [R Package]. Available from 
            https://CRAN.R-project.org/package=sortable"
          ),
          p(
            class = "hangingindent",
            "Goldfeld, K. (2020). simstudy: Simulation of Study Data. (v0.2.1) 
            [R package]. Available from https://cran.r-project.org/package=simstudy"
          ),
          p(
            class = "hangingindent",
            "Hijmans, R. J. (2021). raster: Geographic Data Analysis and Modeling. 
            (v3.4-10) [R Package]. Available from https://CRAN.R-project.org/package=raster"
          ),
          p(
            class = "hangingindent",
            "Iannone, R. (2021). fontawesome: Easily work with 'Font Awesome' Icons.
            (v0.2.1) [R Package]. Available from https://CRAN.R-project.org/package=fontawesome"
          ),
          p(
            class = "hangingindent",
            "Perrier, V., Meyer, F., Granjon, D. (2019). shinyWidgets: Custom in
            puts widgets for shiny. (v0.5.0) [R Package]. Available from 
            https://CRAN.R-project.org/package=shinyWidgets"
          ),
          p(
            class = "hangingindent",
            "The University of Sheffield (n.d.) Diet.csv [dataset]. Available from 
            https://www.sheffield.ac.uk/polopoly_fs/1.570199!/file/stcp-Rdataset-Diet.csv"
          ),
          p(
            class = "hangingindent",
            "Wickham, H. (2016). ggplot2: Elegant Graphics for Data Analysis. 
            Springer-Verlag New York. R package version tidyverse. Available from 
            https://ggplot2.tidyverse.org/"
          ),
          p(
            class = "hangingindent",
            "Wickham, H., François, R., Henry, L., and Müller, K. (2021) dplyr: 
            A Grammar of Data Manipulation. (v1.0.6) [R package] Available from 
            https://cran.r-project.org/package=dplyr"
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



seaotters <- read.csv("Otter.csv",header=T)

diet <- read.csv("Diet.csv",header=T)
diet$Diet<-as.character(diet$Diet)

bank = read.csv("questionbank.csv")
bank = data.frame(lapply(bank, as.character), stringsAsFactors = FALSE)

#Server ----
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
  
  attempts <- reactiveValues(
    numattempts = 0)
  gamescore <- reactiveVal(0)
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
  rand <- reactiveValues(rand_mod=NULL)
  
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
          ylab("Decrease in Weight (kg)") +
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
          xlab("Height (cm)") + 
          ylab("Decrease in Weight (kg)") +
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
          xlab("Pre-diet Weight (kg)") + 
          ylab("Decrease in Weight (kg)") +
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
          xlab("Age (year)") + 
          ylab("Decrease in Weight (kg)") +
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
          xlab("Height (cm)") + 
          ylab("Decrease in Weight (kg)") +
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
          xlab("Pre-diet Weight (kg)") + 
          ylab("Decrease in Weight (kg)") +
          theme(text = element_text(size=20),
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                panel.background = element_blank(), 
                axis.line = element_line(colour = "black"))
      }
    }
    
    else if (input$menu1 == 'Random'){
      ###create data with label A and B with different slope and intersection
      
      A <- 'A'
      B <- 'B'
      
      a <- input$inter1
      b <- input$inter2
      
      slope1 <- input$slope1
      slope2 <- input$slope2
      
      def <- defData(varname = "inter", 
                     dist = "nonrandom",
                     formula = a, 
                     id = "id")
      def<- defData(dtDefs = def,
                    varname = "slope", 
                    dist = "nonrandom", 
                    formula = slope1, 
                    id = "slope")
      def <- defData(dtDefs = def, 
                     varname = "X", 
                     dist = "uniform", 
                     formula = "0;20")
      def <- defData(dtDefs = def, 
                     varname = "Y", 
                     formula = "inter + X * slope", 
                     variance = 11)
      
      def2<- defData(varname = "inter", 
                     dist = "nonrandom", 
                     formula = b, 
                     id = "id")
      def2 <- defData(dtDefs = def2,
                      varname = "slope",
                      dist = "nonrandom", 
                      formula = slope2, 
                      id = "slope")
      def2<- defData(dtDefs = def2, 
                     varname = "X", 
                     dist = "uniform", 
                     formula = "0;20")
      def2 <- defData(dtDefs = def2, 
                      varname = "Y", 
                      formula = "inter + X * slope", 
                      variance = 11)
      
      dt <- genData(input$sample, def)
      dt2 <- genData(input$sample,def2)
      
      names(dt2)[1] <- 'id'
      
      dt$cov <- 'A'
      dt2$cov <- 'B'
      
      comb <- rbind(dt,dt2)
      
      aov.model <- lm(Y~X+cov+cov:X, data=comb)
      
      pred.aov <- expand.grid(X = 0:20, 
                              cov = c("A","B"))
      pred.aov <- mutate(pred.aov, 
                         Y = predict(aov.model, pred.aov))
      
      ggplot(pred.aov, 
             aes(x = X, 
                 y = Y, 
                 colour = cov)) +
        geom_line() + 
        geom_point(data = comb) + 
        xlab("X") + 
        ylab("Y") +
        theme(text = element_text(size=20),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_blank(), 
              axis.line = element_line(colour = "black"))
    }
    )
  
  ###ANCOVA analysis table###
  output$analysis1 <- renderPrint(
    if (input$menu1 == 'Otter') {
      anova(otters.model)
      }
    else if (input$menu1 == 'Diet') {
      if (input$select_conti == 'Age' & input$select_covar == 'Gender'){
        anova(diet.model2)
        }
      else if (input$select_conti == 'Height' & input$select_covar == 'Gender'){
        anova(diet.model3)
        }
      else if (input$select_conti == 'Pre-diet Weight' & input$select_covar == 'Gender'){
        anova(diet.model4)
        }
      else if (input$select_conti == 'Age' & input$select_covar == 'Diet'){
        anova(diet.model5)
        }
      else if (input$select_conti == 'Height' & input$select_covar == 'Diet'){
        anova(diet.model6)
        }
      else if (input$select_conti == 'Pre-diet Weight' & input$select_covar == 'Diet'){
        anova(diet.model7)
        }
    }
    else if (input$menu1 == 'Random'){
      A <- 'A'
      B <- 'B'
      
      a <- input$inter1
      b <- input$inter2
      
      slope1 <- input$slope1
      slope2 <- input$slope2
      
      def <- defData(varname = "inter", 
                     dist = "nonrandom", 
                     formula = a, 
                     id = "id")
      def <- defData(def,
                     varname = "slope", 
                     dist = "nonrandom", 
                     formula = slope1, 
                     id = "slope")
      def <- defData(def, 
                     varname = "X", 
                     dist = "uniform", 
                     formula = "10;20")
      def <- defData(def, 
                     varname = "Y", 
                     formula = "inter + X * slope", 
                     variance = 11)
      
      def2 <- defData(varname = "inter", 
                      dist = "nonrandom", 
                      formula = b, 
                      id = "id")
      def2 <- defData(def2,
                      varname = "slope", 
                      dist = "nonrandom", 
                      formula = slope2, 
                      id = "slope")
      def2 <- defData(def2, 
                      varname = "X", 
                      dist = "uniform", 
                      formula = "10;20")
      def2 <- defData(def2, 
                      varname = "Y", 
                      formula = "inter + X * slope", 
                      variance = 11)
      
      dt <- genData(input$sample, def)
      dt2 <- genData(input$sample, def2)
      
      names(dt2)[1] <- 'id'
      
      dt$cov <- 'A'
      dt2$cov <- 'B'
      
      comb <- rbind(dt,dt2)
      
      aov.model <- lm(Y~X+cov+cov:X, data=comb)
      
      ##testing passing the model
      rand$rand_mod <- anova(aov.model)[3, "Pr(>F)"]
      
      anova(aov.model)
    }
    )

  #####get p values for each interaction 
  
  var <- reactiveValues(p = 1)
  observe({
    if (input$menu1 == 'Otter') {
      var$p <- as.numeric(anova(otters.model)[3,"Pr(>F)"])
      }
    else if (input$menu1 == 'Diet'){
      if (input$select_conti == 'Age' & input$select_covar == 'Gender'){
        var$p <- as.numeric(anova(diet.model2)[3,"Pr(>F)"])
        }
      else if (input$select_conti == 'Height' & input$select_covar == 'Gender'){
        var$p <- as.numeric(anova(diet.model3)[3,"Pr(>F)"])
        }
      else if (input$select_conti == 'Pre-diet Weight' & input$select_covar == 'Gender'){
        var$p <- as.numeric(anova(diet.model4)[3,"Pr(>F)"])
        }
      else if (input$select_conti == 'Age' & input$select_covar == 'Diet'){
        var$p <- as.numeric(anova(diet.model5)[3,"Pr(>F)"])
        }
      else if (input$select_conti == 'Height' & input$select_covar == 'Diet'){
        var$p <- as.numeric(anova(diet.model6)[3,"Pr(>F)"])
        }
      else if (input$select_conti == 'Pre-diet Weight' & input$select_covar == 'Diet'){
        var$p <- as.numeric(anova(diet.model7)[3,"Pr(>F)"])
        }
    }
    else if (input$menu1 == 'Random'){
      var$p <- as.numeric(rand$rand_mod)
      }
  })
  
  output$p <- renderUI(
    if (length(var$p) < 1) {
    }
    else if (var$p <= 0.01){
      p(strong('P-value for this interaction is about', 
                signif(var$p,1), 
                '.' , 
                br(),
                'Since the p-value is very small, the model without the interaction 
               term provides a poor explanation of the data.'))
      }
    else if (var$p >= 0.2) {
      (strong('P-value for this interaction is about', 
                    signif(var$p,1),
                    '.' ,
                    br(),
                    'Since the p-value is very large, the model without the interaction 
              term provides a reasonable explanation of the data.'))
    }
    else {
      strong('P-value for this interaction is about', 
             signif(var$p,1),
             '.')
    }
  )
  
  #  Game ----
  numbers <- reactiveValues(strong = c(), 
                            moderate = c(), 
                            insig = c(), 
                            index = c(), 
                            question = data.frame())
  
  observeEvent(
    eventExpr = input$pages,
    handlerExpr = {
      numbers$strong = sample(1:12,1)
      numbers$moderate = sample(13:24,1)
      numbers$insig = sample(25:36,1)
      numbers$index = c("A","B","C")
      numbers$question = cbind(bank[c(numbers$strong,numbers$moderate,numbers$insig),],
                               numbers$index)
      })
       
  observeEvent(
    eventExpr = input$new,
    handlerExpr = {
      numbers$strong = sample(1:12,1)
      numbers$moderate = sample(13:24,1)
      numbers$insig = sample(25:36,1)
      numbers$index = c("A","B","C")
      numbers$question = cbind(bank[c(numbers$strong,numbers$moderate,numbers$insig),],
                               numbers$index)
    }
  )
    
  
  
  ## Set up matching columns ----
  output$plotCol <- renderUI({
    plots <- list(
      "1" = img(src = numbers$question[numbers$question[7] == "A",4], 
                width = "35%",
                class = "expandable",
                alt = numbers$question[numbers$question[7] == "A",5]),
      "2" = img(src = numbers$question[numbers$question[7] == "B",4], 
                width = "35%", 
                class = "expandable",
                alt = numbers$question[numbers$question[7] == "B",5]),
      "3" = img(src = numbers$question[numbers$question[7] == "C",4], 
                width = "35%",
                class = "expandable",
                alt = numbers$question[numbers$question[7] == "C",5]
                )
    )
    sortable::rank_list(
      input_id = "rankplots",
      text = "Drag the plots into the same order as the outputs.",
      labels = sample(plots, 
                      size = length(plots))
    )
  })
  
  output$outputCol <- renderUI({
    outputs <- list(
      "1" = img(src = numbers$question[numbers$question[7] == "A",3], 
                width = "60%",
                class = "expandable",
                alt = numbers$question[numbers$question[7] == "A",6]),
      "2" = img(src = numbers$question[numbers$question[7] == "B",3], 
                width = "60%", 
                class = "expandable",
                alt = numbers$question[numbers$question[7] == "A",6]),
      "3" = img(src = numbers$question[numbers$question[7] == "C",3], 
                width = "60%",
                class = "expandable",
                alt = numbers$question[numbers$question[7] == "A",6])
    )
    sortable::rank_list(
      input_id = "rankoutputs",
      text = "Drag the outputs into the same order as the plots.",
      labels = sample(outputs, 
                      size = length(outputs))
    )
  })
  
  observeEvent(
    eventExpr = input$submit, 
    handlerExpr = {
    matches <- input$rankplots == input$rankoutputs
    attempts$numattempts <- isolate(attempts$numattempts) + 1
    
    for(i in 1:3){
      output[[paste0("feedbackP", i)]] <- boastUtils::renderIcon(
        icon = ifelse(
          test = matches[i], 
          yes = "correct", 
          no = "incorrect")
      )}
   #       if (attempts$numattempts == 1) {
   #          gamescore <- isolate(as.integer(matches))
   #         } 
   #         else {
   #           gamescore <- isolate(as.integer(matches))
   #         }
   #         if (gamescore == maxScore) {gamescore <- maxScore}
   #      
   #      output$score <- renderUI({
   #        paste("Your Score:", gamescore)
   #   }
   # )
    }

  )
  
  # Clear feedback ----
  
  observeEvent(
    eventExpr = input$rankplots, 
    handlerExpr = {
      for(i in 1:3){
        output[[paste0("feedbackP", i)]] <- boastUtils::renderIcon()
    }
  })
  
  observeEvent(
    eventExpr = input$rankoutputs, 
    handlerExpr = {
      for(i in 1:3){
        output[[paste0("feedbackP", i)]] <- boastUtils::renderIcon()
    }
  })

}

boastUtils::boastApp(ui = ui, server = server)
