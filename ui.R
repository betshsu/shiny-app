library(shiny)

shinyUI(
        pageWithSidebar(
                # Application title
                headerPanel("Titanic Survival"),
                
                sidebarPanel(
                        selectInput('Sex',
                                    label = "Passenger sex",
                                    choices = list("male", "female"),
                                    selected = "male"),
                        numericInput('Age',
                                     label="Passenger age",
                                     value = 18),
                        selectInput('Pclass',
                                    label = "Cabin class",
                                    choices = list("1", "2", "3"),
                                    selected = "2"),
                        selectInput('Embarked',
                                    label = "Port of embarkation",
                                    choices = list("C", "Q", "S"),
                                    selected = "S"),
                        helpText("To predict the probability of a passenger having survived the Titanic
                                 enter their sex, age, their cabin class, and their port of embarkation 
                                 where C = Cherbourg; Q = Queenstown; S = Southampton.
                                 Clicking the Update Prediction button will calculate the new predicted 
                                 survival based on the parameters you entered."),
                        submitButton("Update prediction")
                        
                        
                        
                ),
                mainPanel(
                        h3('Results of predicted survival'),
                        h4('You entered'),
                        verbatimTextOutput("inputValue"),
                        h4('Which resulted in a predicted survival of '),
                        verbatimTextOutput("prediction")
                )
        )
)