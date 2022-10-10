# ===============================================
# Fill in the following fields
# ===============================================
# Title: Stat 133 Project 2: Savings Rate Calculator
# Description: Develop a Shiny App as Savings Rate Calculator
# Author: Amber Shao 
# Date: 2022/04/08


# ===============================================
# Required packages
# ===============================================
library(shiny)
library(tidyverse)
# etc



# ===============================================
# Define User-Interface "ui" for application
# ===============================================

ui <- fluidPage(
  
  titlePanel("Savings Rate Calculator"),
  
  fluidRow(
    # Input(s) for annual-income
    column(3,
           numericInput(inputId = "income", 
                        label = h3("Annual Income"), 
                        value = 50000),
    ),
    
    # Input(s) for annual-income's saving proportion
    column(3, 
           numericInput(inputId = "saving_rate", 
                       label = h3("Saving Rate(%)"),
                       value = 20),
    ),
    
    # Input(s) for target-amount
    column(3,
           numericInput(inputId = "target", 
                        label = h3("Target Amount"), 
                        value = 1000000),
    ),
    
    # Input(s) for current-age
    column(3,
           numericInput(inputId = "age", 
                        label = h3("Current Age"), 
                        value = 25),
    ),
    
    # Input(s) for rate-of-return
    column(3,
           numericInput(inputId = "return_rate", 
                        label = h3("Return Rate(%)"), 
                        value = 5),
    )
  ),
  
  hr(),
  h4('Relationship between the Saving Rate and the Number of Years Required to Reach the Target Amount'),
  plotOutput('plot1'),

  hr(),
  h4('Relationship between the Total Contribution and the Total Growth, given different Saving Rates'),
  plotOutput('plot2'),
  
  hr(),
  h4('The Resulting Age and Contribution Table, given Income, Saving Rates, Target Amount, Current Age, and Interest Rate'),
  DT::dataTableOutput('table')
)

# ===============================================
# Define Server "server" logic
# ===============================================

server <- function(input, output) {
  
  # you may need to create reactive objects
  # (e.g. data frame to be used for graphing purposes)
  
  saving_rate <- reactive({
   if (input$saving_rate < 20) {
     saving_rate = 1:input$saving_rate/100
   } else {
     saving_rate = seq(5,input$saving_rate, 5)/100
   }
    saving_rate
  })
  
  
  
  #Columns for data used for visualization 1 and table section 
  return_rate = reactive(input$return_rate/100)
  annual_contribution = reactive(input$income * saving_rate())
  years_required = reactive(round(log(return_rate()*input$target/annual_contribution() + 1) / log(1+return_rate()), 2))
  final_age = reactive(round(input$age+years_required(),1))
  total_contribution = reactive(annual_contribution()*years_required())
  total_growth = reactive(input$target - total_contribution())
  percent_contribute = reactive(round(total_contribution()/input$target*100, 2)) 
  percent_growth = reactive(100-percent_contribute())
  
  #columns for data used for visualization 2
  two_rates = reactive(rep(saving_rate()*100, 2))
  contrib_growth = reactive(c(total_growth(),total_contribution()))
  label = reactive(c(rep("Growth",length(total_growth())),rep("Contribution",length(total_contribution()))))
  
  dat = reactive({
    data.frame(
      Saving_Rates = saving_rate() * 100, 
      Annual_Contribution = annual_contribution(), 
      Total_Contribution = total_contribution(),
      Total_Growth = total_growth(),
      Percent_Contribution = percent_contribute(),
      Percent_Growth = percent_growth(),
      Years_Required = years_required(),
      Age = final_age()
    )
  })
  
  visual2 = reactive({
    data.frame(
      rates = two_rates(),
      contribution_growth = contrib_growth(),
      label = label())
  })
  
  # code for plot-1
  # (e.g. uses reactive data frame for graphing purposes)
  output$plot1 <- renderPlot({
    # replace the code below with your code!!!
    ggplot(data = dat(), aes(x = Saving_Rates, y = Years_Required)) + geom_bar(stat="identity",fill="grey") + 
      geom_text(aes(label=Years_Required), color = "white",vjust=1.5,size=5) + 
      theme_minimal() + 
      labs(x = 'Saving Rate(%)', y = "Years Required to Reach Target Amount") + 
      geom_path(aes(x = Saving_Rates, y = Years_Required), color="darkblue", group = 1, size = 1)
  })
  

  # code for plot-2
  # (e.g. uses reactive data frame for graphing purposes)
  output$plot2 <- renderPlot({
    # replace the code below with your code!!!
    ggplot(data = visual2(), aes(x = rates, y = contribution_growth, fill = label)) + geom_bar(stat="identity") + 
      theme_minimal() +
      scale_fill_brewer(palette="Paired") +
      labs(x = 'Saving Rate(%)', y = "Amount of Total Contribution and Total Growth")
  })

    
  # code for statistics
  output$table <- DT::renderDataTable({
    # replace the code below with your code!!!
    dat()
  })
  
}



# ===============================================
# Run the application
# ===============================================

shinyApp(ui = ui, server = server)

