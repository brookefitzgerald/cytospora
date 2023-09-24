library(DT)
library(plotly)
library(shiny)
library(shinyjs)
library(shinyWidgets)

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
landing_page_panel <-
    tags$div(
      includeCSS(path="www/css/styles.css"),
      class="landing_page_div",
      tags$h1(class="landing_page_h1", "Disease Decision Tool: Cytospora"),
      fluidRow(
        column(
          width=6,
          img(id="logo", src="images/logo.png", class= "center", width="100%", height="250px")
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

costs_menu <- tags$div(
  id="costs_menu",
  numericInput("annual_cost",
               # Using infoHoverLabel from ui.R to add an informational tooltip
               infoHoverLabel("Annual Production Cost ($/ac/yr)"),
               value=4885 + 1000),
  numericInput("max_yield",
               infoHoverLabel("Average Mature Disease-Free Yield lbs/ac/year"),
               13000),
  tags$p("To add more information about production costs or yield estimates, click the buttons below:",
         style = "font-size:16px;"),
  dropdownButton(
    tags$h3("Production Cost Inputs"),
    numericInput("annual_cost_1",
                 infoHoverLabel("Non-Harvest Labor Cost ($/ac/yr)", "Labor Cost includes cost of pruning, applying fertilizer, and general orchard maintenance."),
                 value=2964),
    numericInput("annual_cost_2",
                 infoHoverLabel("Harvest Cost ($/ac/yr)", "Cost of harvest, including labor."),
                 value=1620),
    numericInput("annual_cost_3",
                 infoHoverLabel("Water Cost ($/ac/yr)"),
                 value=210),
    numericInput("annual_cost_4",
                 infoHoverLabel("Fertilizer Cost ($/ac/yr)", "Report fertilizer cost on avrage, including higher fertilizer cost when trees are young."),
                 value=91),
    numericInput("annual_cost_5",
                 infoHoverLabel("Other Costs ($/ac/yr)", "These costs include insurance, equipment operating expenses, land costs, and other material expenses."),
                 value=1000),
    circle=TRUE,
    inline=TRUE,
    status="primary",
    icon=icon("gear"),
    size='sm',
    width="300px",
    tooltip=tooltipOptions(title="Click for help with budgeting production costs")
  ),
  dropdownButton(
    tags$h3("Annual Mature, Disease-Free Yield Estimates Over Time Per Acre"),
    tags$p("Click and draw yearly maximum yield estimates below. This can simulate bad yield years due to cold or higher yields over time.", style = "font-size:12px;"),
    tags$div(style="display:inline-block;vertical-align:top;",
             actionButton("input_yield_reset", "Reset Plot"),
             actionButton("input_yield_update", "Update Simulation"),
             plotOutput("input_yield_plot", width = "400px", height = "400px",
                        hover=hoverOpts(id = "input_yield_hover", delay = 100, delayType = "throttle", clip = TRUE, nullOutside = TRUE))
    ),
    circle=TRUE,
    inline=TRUE,
    status="primary",
    icon=icon("chart-line"),
    size='sm',
    width="450px",
    tooltip=tooltipOptions(title="Click to draw estimates for yearly yield")
  ),
  numericInput("output_price",
               infoHoverLabel("Peach Price ($/lb)"),
               value=1.1),
  sliderInput("percent_interest",
              infoHoverLabel("Interest Rate On Borrowing (%)",
                             "Interest rate used to calculate the net present value of the orchard. Consider using a nominal interest rate that includes inflation. For more information, click this icon for an Investopedia article about interest rates.",
                             link="https://www.investopedia.com/terms/i/interestrate.asp"),
              value=3,
              min=0,
              max=20),
  sliderInput("percent_price_change",
              infoHoverLabel("Price Inflation Rate (%)",
                             "Annual percentage change in output peach prices."),
              value=0,
              min=-20,
              max=20),
  sliderInput("percent_cost_change",
              infoHoverLabel("Cost Inflation Rate (%)",
                             "Annual percentage change in costs."),
              value=0,
              min=-20,
              max=20),
  actionButton("costs_menu_hide", "Close menu")
)

disease_menu <- tags$div(
  id="disease_menu",
  numericInput("inf_intro",
               infoHoverLabel("Initial Number of Trees Per Acre Infected (Out of 576 Trees/Acre)", "Initial number of trees infected at `Year Disease Starts`. This number will be out of 576 trees in an acre."),
               min=0,
               max=576,
               value=10),
  sliderInput("disease_random_share_of_spread",
              infoHoverLabel("Percent of Disease Spread Entirely Between Trees Vs. Randomly", "Percent of the disease that is spread randomly. If this is zero, infected trees only impact their neighbors. Otherwise, the disease spreads randomly by the amount specified, and the rest of the amount directly from trees' neighbors."),
              min=0,
              max=100,
              value=0),
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
)

treatments_menu <- tags$div(
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
)

replanting_menu <- tags$div(
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
                 infoHoverLabel("Tree Replanting Cost", "Cost to replant tree, including removal"),
                 45),
    
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
                 30)
  ),
  actionButton("replanting_menu_hide", "Close menu"),
)

simulation_time_settings <- tags$div(
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
                sep="",
                animate=TRUE),
    sliderInput("start_disease_year",
                infoHoverLabel("Year Disease Starts"),
                min=2022, 
                max=2062,
                sep="",
                value=2022),
    sliderInput("start_treatment_year",
                infoHoverLabel("Year Treatment Starts",
                               "Year Treatment 1 and Treatment 2 start being applied. Consider changing the options in `Treatment Settings` to understand their impact."),
                min=2022, 
                max=2062,
                sep="",
                value=2022)
)


main_dashboard_panel <- tags$div(
      id="div_dashboard",
      
      
      fluidPage(
        tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico")),
        tags$head(tags$style(HTML("table {table-layout: fixed;}"))),
        tags$head(tags$style(".datatables .display {margin-left: 0;}")),
        tags$head(includeHTML("www/google-analytics.html")),
        tags$script(src="js/update_slider_labels.js"),
        tags$script(src="js/draw_input_data.js"),
        useShinyjs(),
        # Application title
        titlePanel("Cytospora Decision Support Tool"),
        
        # Sidebar with a slider input for number of bins
        fluidRow(
          column(2,
                 tags$div(
                   actionButton("costs_menu_toggle", menuIconLabel("Prices/Costs Settings", id_prefix="costs"), class="menu"),
                   costs_menu,
                   tags$br(),
                   tags$br(),
                   actionButton("disease_menu_toggle", menuIconLabel("Disease Settings", id_prefix="disease"), class="menu"),
                   disease_menu,
                   tags$br(),
                   tags$br(),
                   actionButton("treatments_menu_toggle", menuIconLabel("Treatment Settings", id_prefix="treatments"), class="menu"),
                   treatments_menu,
                   tags$br(),
                   tags$br(),
                   actionButton("replanting_menu_toggle", menuIconLabel("Replanting Settings", id_prefix="replanting"), class="menu"),
                   replanting_menu
                 )
          ),
          column(2,
                 simulation_time_settings,
                 actionButton("run_simulation", "Run Simulation", class="btn-primary btn-md")
          ),
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
                   column(6,
                          htmlOutput("table_label"),
                          DT::dataTableOutput("mytable"))),
                 
          )
        )
      )
    )

compare_simulations_panel <- fluidPage(
    fluidRow(
      column(8, tags$h1("Compare Simulations By Outcome:")),
      column(4, selectizeInput(
        'simulation_outcome', '', 
        choices = c("Average Yearly Net Returns", "Net Present Value", "Yield"),
        options = list(
          placeholder = 'Please select a simulation outcome below',
          onInitialize = I('function() { this.setValue(""); }')
        )))
    ),
    fluidRow(
      plotOutput("simulation_outome_plot", height="350px"),
    )
) 

tutorial_panel <- tags$div("Work in Progress")

ui <- tabsetPanel(
    id = "panels",
    type = "hidden",
    selected="landing_page",
    tabPanelBody("landing_page", landing_page_panel),
    tabPanel(
      "dashboard", 
      tabsetPanel(
        type="tabs",
        tabPanel("Main",                main_dashboard_panel),
        tabPanel("Compare Simulations", compare_simulations_panel),
        tabPanel("Tutorial",            tutorial_panel)
      )
    )
)
