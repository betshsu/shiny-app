library(shiny)

source("titanic_pred.R")

shinyServer(
        function(input, output) {
                output$inputValue <- renderPrint({data.frame(Age=input$Age,Sex=input$Sex, 
                                                             "Cabin Class"=input$Pclass, 
                                                             "Port of embarkation"=input$Embarked)})
                output$prediction <- renderPrint({ProbSurv(Titanic.logit.1,data.frame(Age=input$Age,
                                                                                      Sex=input$Sex,
                                                                                      Pclass=input$Pclass,
                                                                                      Embarked=input$Embarked))})
        }
)