#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)

# Define UI for application that draws a histogram
shinyUI(
  fluidPage(
    tags$head(tags$style(HTML("table {table-layout: fixed;}"))),
    tags$head(tags$style(".datatables .display {margin-left: 0;}")),

    # Application title
    titlePanel("Cytospora Decision Support Tool"),

    # Sidebar with a slider input for number of bins
    fluidRow(
        column(2,
               numericInput("max_yield",
                            "Mature Disease-Free Yield lbs/ac/year",
                            13000),
               numericInput("inf_intro",
                            "Infectious Introductions",
                            min = 0,
                            max = 100,
                            value=10),
               numericInput("year_start",
                         "Planting Year",
                         2022),
            numericInput("year_end",
                         "Planned Replanting Year",
                         2062),
            numericInput("annual_cost",
                         "Annual Production Cost ($/ac/yr)",
                         value=5885),
            numericInput("output_price",
                         "Peach Price ($/lb)",
                         value=1.1),
            numericInput("replant_cost",
                         "Replanting Cost",
                         5500)
            
        ),
        column(2,
               sliderInput("year",
                           "Year",
                           min = 1,
                           max = 30,
                           value = 15),
               sliderInput("control1",
                           "Treatment 1 Spread Reduction (%)",
                           min = 0,
                           max = 100,
                           value=10),
               numericInput("t1_cost",
                            "Treatment 1 Cost ($/ac/yr)",
                            value=350),
               sliderInput("control2",
                           "Treatment 2 Spread Reduction (%)",
                           min = 0,
                           max = 100,
                           value=20),
               numericInput("t2_cost",
                            "Treatment 2 Cost ($/ac/yr)",
                            value=650)),

        # Show a plot of the generated distribution
        column(8,
            plotOutput("orchard_health"),
            fluidRow(
              column(6,plotOutput("tree_health")),
              column(6,DT::dataTableOutput("mytable")))
            )
            
        )
    )
)
