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

# Create UI for Landing page with CSU Branding
ui <- tabsetPanel(
  id = "panels", 
  type = "hidden", 
  selected = "landing_page", 
  
  tabPanelBody(
    value = "landing_page",
    includeCSS(path = "static/css/styles.css"), 
    tags$div(
      class="landing_page_div",
      tags$h1(class="landing_page_h1", "Cytospora Decision Support Tool"),
      fluidRow(
        column(
          width=6,
          imageOutput("logo", width="100%", height="250px"),
        ),
        column(
          width=6,
          tags$p("Welcome to CSU's Cytospora Decision Support Tool. This tool is meant
               to aid peach farmers understand the economic impact of the cytospora 
               disease and control strategies."),
        )
      ),
      tags$div(
        class = "center", 
        actionButton("go_to_app", "Go to app")
      )
    )
  ),
  
  tabPanelBody(
    value="dashboard",
    tags$div(
      id="div_dashboard",
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
                selectInput("replanting_strategy",
                             label = "Dead Tree Replanting Strategy", 
                             choices = list("Don't replant" = 'no_replant', 
                                            "Replant dead trees every year" = 'yr1_replant'),
                                            # "Replant dead trees every 5 years" = 'yr5_replant',
                                            # "Replant dead trees every 10 years" = 'yr10_replant'),
                             selected = 'no_replant'),
                numericInput("replant_cost_tree",
                             "Tree Replanting Cost",
                             10),
                numericInput("replant_cost_orchard",
                             "Orchard Replanting Cost",
                             5500),
                sliderInput("disease_spread_rate",
                            "Rate of Disease Spread Between Trees (%)",
                            min = 0,
                            max = 100,
                            value=10),
                sliderInput("disease_growth_rate",
                            "Rate of Disease Growth Within a Tree (%)",
                            min = 0,
                            max = 100,
                            value=20)
                
            ),
            column(2,
                sliderInput("year",
                            "Year",
                            min = 1,
                            max = 30,
                            value = 15,
                            animate = TRUE),
                sliderInput("start_disease_year",
                             "Year Disease Starts",
                             min = 1,
                             max = 30,
                             value = 1),
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
    )
)
