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

infoHoverLabel<- function(label, info_text=NA, link=NA){
  info_text <- ifelse(is.na(info_text), label, info_text)
  link_part_one <- ifelse(is.na(link), '', paste0("<a href='", link, "' target='_blank'>"))
  link_part_two <- ifelse(is.na(link), '', "</a>")
  return(shiny::HTML(
    paste(
      "<div><text>", label, "</text>", link_part_one, 
      shiny::icon("circle-info", title=info_text), link_part_two, "</div>")
    ))
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
  id="panels",
  type="hidden",
  selected="landing_page",
  
  tabPanelBody(
    value="landing_page",
    includeCSS(path="www/css/styles.css"),
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
               disease and control strategies.", class="centertext"),
        )
      ),
      tags$div(
        class="center",
        actionButton("go_to_app", "Go to app")
      )
    )
  ),
  
  tabPanelBody(
    value="dashboard",
    tags$div(
      id="div_dashboard",
      
      
      fluidPage(
        tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico")),
        tags$head(tags$style(HTML("table {table-layout: fixed;}"))),
        tags$head(tags$style(".datatables .display {margin-left: 0;}")),
        tags$head(includeHTML("www/google-analytics.html")),
        tags$script(src="myscript.js"),
        
        # Application title
        titlePanel("Cytospora Decision Support Tool"),
    
        # Sidebar with a slider input for number of bins
        fluidRow(
            column(2,
                tags$div(
                  actionButton("costs_menu_toggle", menuIconLabel("Prices/Costs Settings", id_prefix="costs"), class="menu"),
                  tags$div(
                    id="costs_menu",
                    numericInput("annual_cost",
                                 # Using infoHoverLabel from ui.R to add an informational tooltip
                                 infoHoverLabel("Annual Production Cost ($/ac/yr)"),
                                 value=4885 + 1000),
                    dropdownButton(
                      tags$h3("Production Cost Inputs"),
                      numericInput("annual_cost_1",
                                   infoHoverLabel("Labor Cost ($/ac/yr)"),
                                   value=4885),
                      numericInput("annual_cost_2",
                                   infoHoverLabel("Water Cost ($/ac/yr)"),
                                   value=1000),
                      circle=TRUE,
                      status="primary",
                      icon=icon("gear"),
                      size='sm',
                      width="300px",
                      tooltip=tooltipOptions(title="Click for help with budgeting production cost")
                    ),
                numericInput("max_yield",
                              infoHoverLabel("Mature Disease-Free Yield lbs/ac/year"),
                              13000),
                numericInput("output_price",
                             infoHoverLabel("Peach Price ($/lb)"),
                             value=1.1),
                sliderInput("percent_interest",
                             infoHoverLabel("Interest Rate (%)",
                                            "Interest rate used to calculate the net present value of the orchard. Consider using a nominal interest rate that includes inflation. For more information, click this icon for an Investopedia article about interest rates.",
                                            link="https://www.investopedia.com/terms/i/interestrate.asp"),
                             value=3,
                             min=0,
                             max=20),
                sliderInput("percent_price_change",
                             infoHoverLabel("Price Change (%)",
                                            "Annual percentage change in output peach prices."),
                             value=0,
                             min=-20,
                             max=20),
                sliderInput("percent_cost_change",
                             infoHoverLabel("Cost Change (%)",
                                            "Annual percentage change in costs after the first year."),
                             value=0,
                             min=-20,
                             max=20),
                actionButton("costs_menu_hide", "Close menu")
                ),
                tags$br(),
                tags$br(),
                 actionButton("disease_menu_toggle", menuIconLabel("Disease Settings", id_prefix="disease"), class="menu"),
                 tags$div(
                   id="disease_menu",
                   numericInput("inf_intro",
                                infoHoverLabel("Initial Number of Trees Infected", "Initial number of trees infected at `Year Disease Starts`."),
                                min=0,
                                max=100,
                                value=10),
                   sliderInput("disease_spread_rate",
                               infoHoverLabel("Rate of Disease Spread Between Trees Per Year (%/yr)",
                                              "Rate of disease spread between trees per year. Reduces yield of neighboring trees by this percentage of each tree's amount of disease if no treatment is used."),
                               min=0,
                               max=100,
                               value=10),
                   sliderInput("disease_growth_rate",
                               infoHoverLabel("Rate of Disease Growth Within a Tree Per Year (%/yr)",
                                              "Rate of disease growth within a tree per year. Reduces yield by this percentage, with additional decreases from neighboring trees spreading disease. This decrease can be prevented by sufficient treatment."),
                               min=0,
                               max=100,
                               value=20),
                   actionButton("disease_menu_hide", "Close menu")
                 ),
                 tags$br(),
                 tags$br(),
                 actionButton("treatments_menu_toggle", menuIconLabel("Treatment Settings", id_prefix="treatments"), class="menu"),
                 tags$div(
                   id="treatments_menu",
                   sliderInput("control1",
                               infoHoverLabel("Treatment 1 Spread Reduction Rate Per Year (%/yr)", "Percentage reduction of disease spread of all trees per year after `Year Treatment Starts` of Treatment 1. Note that the treatments do not heal trees from disease."),
                               min=0,
                               max=100,
                               value=10),
                   numericInput("t1_cost",
                                infoHoverLabel("Treatment 1 Cost ($/ac/yr)", "Treatment 1 cost per acre per year after `Year Treatment Starts`. Note that more effective treatments are likely more expensive."),
                                value=350),
                   sliderInput("control2",
                               infoHoverLabel("Treatment 2 Spread Reduction Rate Per Year (%/yr)", "Percentage reduction of disease spread of all trees per year after `Year Treatment Starts` of Treatment 2. Note that the treatments do not heal trees from disease."),
                               min=0,
                               max=100,
                               value=20),
                   numericInput("t2_cost",
                                infoHoverLabel("Treatment 2 Cost ($/ac/yr)", "Treatment 2 cost per acre per year after `Year Treatment Starts`. Note that more effective treatments are likely more expensive."),
                                value=650),
                   actionButton("treatments_menu_hide", "Close menu"),
                 ),
                tags$br(),
                tags$br(),
                actionButton("replanting_menu_toggle", menuIconLabel("Replanting Settings", id_prefix="replanting"), class="menu"),
                tags$div(
                  id="replanting_menu",
                  selectInput("replanting_strategy",
                              label=infoHoverLabel("Dead Tree Replanting Strategy"), 
                              choices=list(
                                "Don't replant or remove"                                        = 'no_replant',
                                "Replant dead trees every year"                                  = 'tree_replant',
                                "Remove dead trees every year"                                   = 'tree_remove',
                                "Replant orchard every planned replanting cycle number of years" = 'orchard_replant'),
                              selected = 'orchard_replant'),
                  tags$div(
                    id="orchard_replant_inputs",
                    sliderInput("replant_cycle_year_orchard",
                                infoHoverLabel("Planned Replanting Cycle", 
                                               "Number of years before replanting your orchard. If the simulation is set for 40 years and the replanting cycle length is 10 years, then the orchard will be replanted 4 times."),
                                min=1, 
                                max=40,
                                value = 20),
                    textOutput("orchard_replants_count"),
                    tags$br(),
                    numericInput("replant_cost_orchard",
                                 infoHoverLabel("Orchard Replanting Cost"),
                                 5500)
                  ),
                  tags$div(
                    id="tree_replant_inputs",
                    numericInput("replant_cost_tree",
                                 infoHoverLabel("Tree Replanting Cost"),
                                 20),
                    
                    radioButtons("replant_tree_block_size",
                                 infoHoverLabel("Number of additional surrounding trees to replant",
                                                "Length of square surrounding each dead tree that will be removed. Block size of two will mean 25 total trees will be removed, two to the left and right of the tree, two up and down, and all of the trees in between."),
                                 choices=c(
                                   "0"=0,
                                   "1 (replant 8 extra trees)"=1,
                                   "2 (replant 24 extra trees)"=2,
                                   "3 (replant 48 extra trees)"=3,
                                   "4 (replant 80 extra trees)"=4)
                    )),
                    tags$div(
                      id="tree_remove_inputs",
                      numericInput("remove_cost_tree",
                                   infoHoverLabel("Tree Removal Cost"),
                                   10)
                      ),
                  actionButton("replanting_menu_hide", "Close menu"),
                )
                )
            ),
            column(2,
               sliderInput("time_horizon", 
                           infoHoverLabel("Years to Run Simulation"),
                           min=as.Date('2000-01-01'), 
                           max=as.Date('2100-01-01'), 
                           value=c(as.Date('2022-01-01'), as.Date('2062-01-01')),
                           timeFormat='%Y'),
                sliderInput("year",
                            infoHoverLabel("Current Year",
                                           "Current year in simulation. Consider changing the year the treatment starts to see the impact on the orchard."),
                            min=2022, 
                            max=2062,
                            value=2037,
                            animate=TRUE),
                sliderInput("start_disease_year",
                            infoHoverLabel("Year Disease Starts"),
                            min=2022, 
                            max=2062,
                            pre='',
                            value=2022),
                sliderInput("start_treatment_year",
                            infoHoverLabel("Year Treatment Starts",
                                           "Year Treatment 1 and Treatment 2 start being applied. Consider changing the options in `Treatment Settings` to understand their impact."),
                            min=2022, 
                            max=2062,
                            value=2022)
                ),
    
            # Show a plot of the generated distribution
            column(8,
                fluidRow(
                  column(1, offset=10,
                         dropdownButton(
                           label="Feedback",
                           HTML('<iframe src="https://docs.google.com/forms/d/e/1FAIpQLScfdX1v_VBzHB0DoFrM-SQZJ1Gh_dlTWGU1QAGSoERE2pP1_Q/viewform?embedded=true" width="640" height="808" frameborder="0" marginheight="0" marginwidth="0">Loading…</iframe>'),
                           circle=FALSE,
                           up=FALSE,
                           icon=shiny::icon("comment"),
                           right=TRUE,
                           status="feedback",
                           size='xs',
                           width="640px"
                         )
                  )
                ),
                plotlyOutput("orchard_health", height="350px"),
                fluidRow(
                  column(6,plotlyOutput("tree_health")),
                  column(6,DT::dataTableOutput("mytable"))),
                
                )
             )
          )
       )
    )
)
