#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(plotly)
library(DT)

infoHoverLabel<- function(label, info_text=NA){
  info_text <- ifelse(is.na(info_text), label, info_text)
  return(shiny::HTML(paste("<div><text>", label, "</text>", shiny::icon("circle-info", title=info_text), "</div>")))
}
menuIconLabel <- function(label, id_prefix){
  return(shiny::HTML(
    paste("<div><text>", label, "</text>", 
            shiny::icon("chevron-down", id=paste0(id_prefix, "_menu_icon"), class="hidden_menu_arrow"),
          "</div>")
      )
    )
  }

# Create UI for Landing page with CSU Branding

ui <- tabsetPanel(
  id = "panels", 
  type = "hidden", 
  selected = "landing_page", 
  
  tabPanelBody(
    value = "landing_page",
    includeCSS(path = "www/css/styles.css"), 
    tags$div(
      class="landing_page_div",
      tags$h1(class="landing_page_h1", "Disease Decision Tool: Cytospora"),
      fluidRow(
        column(
          width=6,
          imageOutput("logo", width="100%", height="250px"),
        ),
        column(
          width=6,
          tags$p("Welcome to CSU's Disease Decision Support Tool. This tool is meant
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
                tags$div(
                  uiOutput('annual_cost_res'),
                              dropdownButton(
                                tags$h3("Production Cost Inputs"),
                                numericInput("annual_cost_1",
                                             infoHoverLabel("Labor Cost ($/ac/yr)"),
                                             value=4885),
                                numericInput("annual_cost_2",
                                             infoHoverLabel("Water Cost ($/ac/yr)"),
                                             value=1000),
                                circle = TRUE,
                                status = "primary", 
                                icon = icon("gear"), 
                                size='sm',
                                width = "300px",
                                tooltip = tooltipOptions(title = "Click for help with budgeting production cost")
                              ),
                            ),
                 numericInput("max_yield",
                              infoHoverLabel("Mature Disease-Free Yield lbs/ac/year"),
                              13000),
                 numericInput("output_price",
                              infoHoverLabel("Peach Price ($/lb)"),
                              value=1.1),
                 actionButton("disease_menu_toggle", menuIconLabel("Disease Settings", id_prefix="disease"), class="menu"),
                 tags$div(
                   id="disease_menu",
                   numericInput("inf_intro",
                                infoHoverLabel("Infectious Introductions"),
                                min = 0,
                                max = 100,
                                value=10),
                   sliderInput("disease_spread_rate",
                               infoHoverLabel("Rate of Disease Spread Between Trees (%)"),
                               min = 0,
                               max = 100,
                               value=10),
                   sliderInput("disease_growth_rate",
                               infoHoverLabel("Rate of Disease Growth Within a Tree (%)"),
                               min = 0,
                               max = 100,
                               value=20),
                   actionButton("disease_menu_hide", "Close menu")
                 ),
                 tags$br(),
                 tags$br(),
                 actionButton("replanting_menu_toggle", menuIconLabel("Replanting Settings", id_prefix="replanting"), class="menu"),
                 tags$div(
                   id="replanting_menu",
                   selectInput("replanting_strategy",
                               label=infoHoverLabel("Dead Tree Replanting Strategy"), 
                               choices = list("Don't replant" = 'no_replant', 
                                              "Replant dead trees every year" = 'tree_replant',
                                              "Replant orchard at planned replanting year" = 'orchard_replant'),
                               # "Replant dead trees every 10 years" = 'yr10_replant'),
                               selected = 'orchard_replant'),
                   sliderInput("replant_year_orchard",
                               infoHoverLabel("Planned Replanting Year"),
                               value=20,
                               min=1,
                               max=40),
                   numericInput("replant_cost_tree",
                                infoHoverLabel("Tree Replanting Cost"),
                                10),
                   numericInput("replant_cost_orchard",
                                infoHoverLabel("Orchard Replanting Cost"),
                                5500),
                   actionButton("replanting_menu_hide", "Close menu"),
                 ),
                 tags$br(),
                 tags$br(),
                 actionButton("treatments_menu_toggle", menuIconLabel("Treatment Settings", id_prefix="treatments"), class="menu"),
                 tags$div(
                   id="treatments_menu",
                   sliderInput("control1",
                               infoHoverLabel("Treatment 1 Spread Reduction (%)"),
                               min = 0,
                               max = 100,
                               value=10),
                   numericInput("t1_cost",
                                infoHoverLabel("Treatment 1 Cost ($/ac/yr)"),
                                value=350),
                   sliderInput("control2",
                               infoHoverLabel("Treatment 2 Spread Reduction (%)"),
                               min = 0,
                               max = 100,
                               value=20),
                   numericInput("t2_cost",
                                infoHoverLabel("Treatment 2 Cost ($/ac/yr)"),
                                value=650),
                   actionButton("treatments_menu_hide", "Close menu"),
                 )
            ),
            column(2,
               sliderInput("time_horizon", 
                           infoHoverLabel("Years to Run Simulation"),
                           min = as.Date('2000-01-01'), 
                           max = as.Date('2100-01-01'), 
                           value = c(as.Date('2022-01-01'), as.Date('2062-01-01')),
                           timeFormat = '%Y'),
                sliderInput("year",
                            infoHoverLabel("Year",
                                           "Year"),
                            min = 1,
                            max = 40,
                            value = 15,
                            animate = TRUE),
                sliderInput("start_disease_year",
                            infoHoverLabel("Year Disease Starts"),
                            min = 1,
                            max = 30,
                            value = 1),
                sliderInput("start_treatment_year",
                            infoHoverLabel("Year Treatment Starts"),
                            min = 1,
                            max = 30,
                            value = 1)
                ),
    
            # Show a plot of the generated distribution
            column(8,
                plotlyOutput("orchard_health", height="350px"),
                fluidRow(
                  column(6,plotlyOutput("tree_health")),
                  column(6,DT::dataTableOutput("mytable")))
                )
             )
          )
       )
    )
)
