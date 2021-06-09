library(shiny)
library(png)
library(shinyBS)
library(shinyjs)
library(shinyDND)
library(ggplot2)
library(dplyr)  ###NEW PACKAGE 
library(shinydashboard)
library(simstudy) ### NEW PACKAGE 
library(lubridate)###NEW PACKGE
library(shinyalert)##NEW PACKAGE
library(shinyWidgets)
library(boastUtils)


####read in dataset###
seaotters <- read.csv("Otter.csv",header=T)

diet <- read.csv("Diet.csv",header=T)
diet$Diet<-as.character(diet$Diet)

bank = read.csv("questionbank.csv")
bank = data.frame(lapply(bank, as.character), stringsAsFactors = FALSE)




shinyServer(function(input, output,session) {

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
  #                          Different groups are different levels of categorical variables, 
  #                          and group means are calculated from continuous variables.',
  #                          br(),
  #                          br(),
  #                          'EX. Are the average score of three STAT 200 sections significantly different from each other?'))
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
      def<- defData(def,
                    varname = "slope", 
                    dist = "nonrandom", 
                    formula = slope1, 
                    id = "slope")
      def <- defData(def, 
                     varname = "X", 
                     dist = "uniform", 
                     formula = "0;20")
      def <- defData(def, 
                     varname = "Y", 
                     formula = "inter + X * slope", 
                     variance = 11)
      
      def2<- defData(varname = "inter", 
                     dist = "nonrandom", 
                     formula = b, 
                     id = "id")
      def2 <- defData(def2,
                      varname = "slope", 
                      dist = "nonrandom", 
                      formula = slope2, 
                      id = "slope")
      def2<- defDataAdd(def2, 
                        varname = "X", 
                        dist = "uniform", 
                        formula = "0;20")
      def2 <- defDataAdd(def2, 
                         varname = "Y", 
                         formula = "inter + X * slope", 
                         variance = 11)
      
      dt <- genData(input$sample, def)
      dt2 <- genData(input$sample,def2)
      
      names(dt2)[1]<-'id'
      
      dt$cov<-'A'
      dt2$cov<-'B'
      
      comb<-rbind(dt,dt2)
      
      aov.model<-lm(Y~X+cov+cov:X,data=comb)
      
      pred.aov <- expand.grid(X =0:20, 
                              cov = c("A","B")
                              )
      pred.aov <- mutate(pred.aov, 
                         Y = predict(aov.model, pred.aov))
      
      ggplot(pred.aov,
             aes(x = X, 
                 y = Y, 
                 colour = cov)) + 
        geom_line() + 
        geom_point(data = comb) + 
        xlab("X") + 
        ylab("Y")+
        theme(text = element_text(size=20),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              axis.line = element_line(colour = "black"))
      }
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
  
  
  
  
  # Game ----
  ######################################  Bank B #############################################################
  numbers <- reactiveValues(strong = c(), moderate = c(), insig = c(), index = c(), question = data.frame())
  
  observeEvent(input$go,{
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
    
    updateRadioButtons(session, 'radio1','',c('A','B','C'),selected='',inline=TRUE)
    updateRadioButtons(session,"radio2", '',c('A','B','C'),selected='',inline=TRUE)
    updateRadioButtons(session, "radio3", '',c('A','B','C'),selected='',inline=TRUE)
  })
  
  observeEvent(input$tabs,{
    numbers$strong = sample(1:12,1)
    numbers$moderate = sample(13:24,1)
    numbers$insig= sample(25:36,1)
    
    
    numbers$index = c("A","B","C")
    numbers$question = cbind(bank[c(numbers$strong,numbers$moderate,numbers$insig),],numbers$index)
    
  })
  
  output$plot1 <- renderUI({
    img(src = numbers$question[numbers$question[5] == "A",4], width = "100%", height = "107%", style = "text-align: center")
  })
  
  # output$table1 <- renderUI({
  #   img(src = numbers$question[numbers$question[5] == "A",3], width = "100%", height = "100%", style = "text-align: center")
  # })
  
  output$plot2 <- renderUI({
    img(src = numbers$question[numbers$question[5] == "B",4], width = "100%", height = "107%", style = "text-align: center")
  })
  
  # output$table2 <- renderUI({
  #   img(src = numbers$question[numbers$question[5] == "B",3], width = "100%", height = "100%", style = "text-align: center")
  # })
  
  output$plot3 <- renderUI({
    img(src = numbers$question[numbers$question[5] == "C",4], width = "100%", height = "107%", style = "text-align: center")
  })
  
  # output$table3 <- renderUI({
  #   img(src = numbers$question[numbers$question[5] == "C",3], width = "100%", height = "100%", style = "text-align: center")
  # })
  
  #######randomize the table######
  
  index2 <- reactiveValues(index2 = 3)
  
  observeEvent(input$new,{index2$index2 <- sample(1:4,1, replace=TRUE, prob=NULL)
  })
  
  observeEvent(input$reset,{index2$index2 <- sample(1:4,1, replace=TRUE, prob=NULL)
  })
  
  output$table1<-renderUI({
    if (index2$index2==1){img(src = numbers$question[numbers$question[5] == "A",3], width = "105%", height = "105%", style = "text-align: center")}
    else if (index2$index2==2){img(src = numbers$question[numbers$question[5] == "B",3], width = "105%", height = "105%", style = "text-align: center")}
    else if (index2$index2==3){img(src = numbers$question[numbers$question[5] == "C",3], width = "105%", height = "105%", style = "text-align: center")}
    else if (index2$index2==4){img(src = numbers$question[numbers$question[5] == "A",3], width = "105%", height = "105%", style = "text-align: center")}
  })
  
  output$table2<-renderUI({
    if (index2$index2==1){img(src = numbers$question[numbers$question[5] == "B",3], width = "105%", height = "105%", style = "text-align: center")}
    else if (index2$index2==2){img(src = numbers$question[numbers$question[5] == "C",3], width = "105%", height = "105%", style = "text-align: center")}
    else if (index2$index2==3){img(src = numbers$question[numbers$question[5] == "A",3], width = "105%", height = "105%", style = "text-align: center")}
    else if (index2$index2==4){img(src = numbers$question[numbers$question[5] == "C",3], width = "105%", height = "105%", style = "text-align: center")}
  })
  
  output$table3<-renderUI({
    if (index2$index2==1){img(src = numbers$question[numbers$question[5] == "C",3], width = "105%", height = "105%", style = "text-align: center")}
    else if (index2$index2==2){img(src = numbers$question[numbers$question[5] == "A",3], width = "105%", height = "105%", style = "text-align: center")}
    else if (index2$index2==3){img(src = numbers$question[numbers$question[5] == "B",3], width = "105%", height = "105%", style = "text-align: center")}
    else if (index2$index2==4){img(src = numbers$question[numbers$question[5] == "B",3], width = "105%", height = "105%", style = "text-align: center")}
  })
  
  
  
  ####letter for the plot
  output$a<-renderUI(h4('A'))
  output$b<-renderUI(h4('B'))
  output$c<-renderUI(h4('C'))
  #####buttons####
  
  observeEvent(input$submitA,{
    updateButton(session,"submitA",disabled = TRUE)
  })
  observeEvent(input$new,{
    updateButton(session,"submitA",disabled = FALSE)
  })
  
  observeEvent(input$submitA,{
    updateButton(session,"new",disabled = FALSE)
  })
  
  observeEvent(input$new,{
    updateButton(session,"new",disabled = TRUE)
  })
  
  
  observeEvent(input$new, {
    reset("radio1")
  })
  
  
  ###################check answers#####
  
  summationC<-reactiveValues(correct1 = c(0), started=FALSE)
  
  observeEvent(input$submitA,{
    observeEvent(input$new,{
      output$answer1 <- renderUI({
        img(src = NULL,width=30)
      })
    })
    observe({
      output$answer1 <- renderUI({
        if (!is.null(input$radio1)){
          if (index2$index2==1 &input$radio1 == 'A'){
            img(src = "check.png",width=30)
          }
          else if (index2$index2==2 &input$radio1 == 'B') {img(src = "check.png",width=30)}
          else if (index2$index2==3 &input$radio1 == 'C'){img(src = "check.png",width=30)}
          else if (index2$index2==4 &input$radio1 == 'A'){img(src = "check.png",width =30)}
          else{
            img(src = "cross.png",width=30)
          }
        }
      })
    })
  })
  
  
  observeEvent(input$submitA,{
    observeEvent(input$new,{
      output$answer2 <- renderUI({
        img(src = NULL,width=30)
      })
    })
    observe({
      output$answer2 <- renderUI({
        if (!is.null(input$radio2)){
          if (index2$index2==1 &input$radio2 == 'B'){
            img(src = "check.png",width=30)
            
          }
          else if (index2$index2==2 &input$radio2 == 'C') {img(src = "check.png",width=30)}
          else if (index2$index2==3 &input$radio2 == 'A'){img(src = "check.png",width=30)}
          else if (index2$index2==4 &input$radio2 == 'C'){img(src = "check.png",width=30)}
          else{
            img(src = "cross.png",width=30)
          }
        }
      })
    })
  })
  
  observeEvent(input$submitA,{
    observeEvent(input$new,{
      output$answer3 <- renderUI({
        img(src = NULL,width=30)
      })
    })
    observe({
      output$answer3 <- renderUI({
        if (!is.null(input$radio3)){
          if (index2$index2==1 &input$radio3 == 'C'){
            img(src = "check.png",width=30)
          }
          else if (index2$index2==2 &input$radio3 == 'A') {img(src = "check.png",width=30)}
          else if (index2$index2==3 &input$radio3 == 'B'){img(src = "check.png",width=30)}
          else if (index2$index2==4 &input$radio3 == 'B'){img(src = "check.png",width=30)}
          else{
            img(src = "cross.png",width=30);
            
          }
        }
      })
    })
  })
  
  
  #####count correct answer ########
  summationC<-reactiveValues(correct1 = c(0),total=c(), started=FALSE)
  
  observeEvent(input$submitA,{
    for (i in input$radio1){
      summationC$total = c(summationC$total,1)
      if (index2$index2==1 &input$radio1 == 'A'){
        summationC$correct1 = c(summationC$correct1,1)
      }
      else if (index2$index2==2 &input$radio1 == 'B') { summationC$correct1 = c(summationC$correct1,1)}
      else if (index2$index2==3 &input$radio1 == 'C'){summationC$correct1 = c(summationC$correct1,1)}
      else if (index2$index2==4 &input$radio1 == 'A'){summationC$correct1 = c(summationC$correct1,1)}
      else{
        summationC$correct1 = c(summationC$correct1,0)}
      
    }
    
    for (i in input$radio2){
      summationC$total = c(summationC$total,1)
      if (index2$index2==1 &input$radio2 == 'B'){
        
        summationC$correct1 = c(summationC$correct1,1)
        
      }
      else if (index2$index2==2 &input$radio2 == 'C') {summationC$correct1 = c(summationC$correct1,1)}
      else if (index2$index2==3 &input$radio2 == 'A'){summationC$correct1 = c(summationC$correct1,1)}
      else if (index2$index2==4 &input$radio2 == 'C'){summationC$correct1 = c(summationC$correct1,1)}
      else{
        
        summationC$correct1 = c(summationC$correct1,0)}
    }
    
    
    for (i in input$radio3){
      summationC$total = c(summationC$total,1)
      if (index2$index2==1 &input$radio3 == 'C'){
        i
        summationC$correct1 = c(summationC$correct1,1)
      }
      else if (index2$index2==2 &input$radio3 == 'A') {summationC$correct1 = c(summationC$correct1,1)}
      else if (index2$index2==3 &input$radio3 == 'B'){summationC$correct1 = c(summationC$correct1,1)}
      else if (index2$index2==4 &input$radio3 == 'B'){ summationC$correct1 = c(summationC$correct1,1)}
      else{
        
        summationC$correct1 = c(summationC$correct1,0)}
    }
  })
  
  
  
  output$correctC <- renderPrint({
    if (sum(c(summationC$correct1))==0) {cat("You have earned 0 points")}
    else{
      cat("You have earned",sum(c(summationC$correct1)),'points')}
  })
  
  
  ###########timer####################
  
  # Initialize the timer, 60 seconds, not active.
  timer <- reactiveVal(60)
  active <- reactiveVal(FALSE)
  
  # Output the time left.
  output$timeleft <- renderText({
    paste("Time left: ", seconds_to_period(timer()))
  })
  
  
  ########show up the scoreing panel and popup####
  
  
  # observer that invalidates every second. If timer is active, decrease by one.
  observe({
    invalidateLater(1000, session)
    isolate({
      if(active())
      {
        timer(timer()-1)
        if(timer()<1)
        {
          active(FALSE)
          shinyalert('Count Down Complete','Click to see your score',
                     type = "success")
          
          output$scoreBox <- renderValueBox({
            valueBox(
              paste0(sum(c(summationC$correct1))), "Totel Score", icon = icon("list"),
              color = "purple"
            )
          })
          
          output$percentBox <- renderValueBox({
            if (sum(c(summationC$correct1))==0){valueBox(
              paste0('0%'), "Accuracy", icon = icon("thumbs-up", lib = "glyphicon"),
              color = "light-blue"
            )}
            else{
              valueBox(
                paste0(round(sum(c(summationC$correct1))/sum(c(summationC$total))*100,digit=1),'%'), "Accuracy", icon = icon("thumbs-up", lib = "glyphicon"),
                color = "light-blue"
              )}
          })
          
          output$timeBox <- renderValueBox({
            valueBox(
              paste0(input$seconds,'s'), "Time Used", icon = icon("time",lib = "glyphicon"),
              color = "maroon"
            )
          })
          
          
        }
        
      }
    })
  })
  
  
  
  
  
  
  
  
  
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
    })
