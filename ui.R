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
          img(id="logo", src="images/logo.png", class= "center", height="275px")
        ),
        column(
          width=6,
          tags$p("Welcome to CSU's Disease Decision Support Tool. This tool is meant
               to aid peach farmers understand the economic impact of the cytospora 
               disease and control strategies.", class=c("centertext", 'welcome-text')),
        )
      ),
      tags$div(
        class="center",
        actionButton("go_to_app", "Go to app")
      ),
      fluidRow(
        p("Funding and support provided by:", class='funding-support-text')
      ),
      fluidRow(
        img(id="funding_logos", src="images/funding_logos_lightened.png", class = "responsive-img", height="250px")
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
              infoHoverLabel("Peach Price Inflation Rate (%)",
                             "Annual percentage change in output peach prices."),
              value=0,
              min=-20,
              max=20),
  sliderInput("percent_cost_change",
              infoHoverLabel("Production Cost Inflation Rate (%)",
                             "Annual percentage change in costs."),
              value=0,
              min=-20,
              max=20),
  actionButton("costs_menu_hide", "Close menu")
)


treatments_menu <- tags$div(
  id="treatments_menu",
  numericInput("inf_intro",
              infoHoverLabel("Initial Number of Trees Per Acre Infected (Out of 576 Trees/Acre)", "Initial number of trees infected at `Year Disease Starts`. This number will be out of 576 trees in an acre."),
              min=0,
              max=576,
              value=10),
  sliderInput("detection_prob",
              infoHoverLabel("Percent of branch cankers removed (%/yr)", "Likelihood of detecting Cytospora Canker in the branches of a tree and removing the canker during pruning. A branch canker is always detected and pruned out after 3 years. Trunk cankers cannot be pruned."),
              min=0,
              max=100,
              value=100),
  sliderInput("control1",
              infoHoverLabel("Treatment 1 Spread Reduction Effectiveness Per Year (%/yr)", "Percentage reduction of disease spread of all trees per year after `Year Treatment Starts` of Treatment 1. Note that the treatments do not heal trees from disease, only prevent spread."),
              min=0,
              max=100,
              value=10),
  numericInput("t1_cost",
               infoHoverLabel("Treatment 1 Cost ($/ac/yr)", "Treatment 1 cost per acre per year after `Year Treatment Starts`. Note that more effective treatments are likely more expensive."),
               value=350),
  sliderInput("control2",
              infoHoverLabel("Treatment 2 Spread Reduction Effectiveness Per Year (%/yr)", "Percentage reduction of disease spread of all trees per year after `Year Treatment Starts` of Treatment 2. Note that the treatments do not heal trees from disease, only prevent spread."),
              min=0,
              max=100,
              value=40),
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
        tags$script(src="js/draw_input_data.js"),
        tags$script(src="js/connectSliders.js"),
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
                   actionButton("treatments_menu_toggle", menuIconLabel("Disease and Treatment Settings", id_prefix="treatments"), class="menu"),
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

all_other_settings <- tags$div(
  id="other_settings_menu", 
  column(
    6,
      sliderInput(
        "disease_random_share_of_spread_range", 
        "Percent of disease sprad randomly", 
        value=c(0.0, 0.5),
        min=0,
        max=1
      ),
      sliderInput(
        "disease_growth_rate_range", 
        "Disease growth rate", 
        value=c(0.05, 0.30), 
        min=0,
        max=1
      ),
  ),
  column(6, 
         fluidRow(
           sliderInput(
             "inf_intro_range", 
             "Number of infectius introductions", 
             value=c(1, 21), 
             min=1, 
             max=576
           )),
         fluidRow(
           sliderInput(
             "annual_cost_range",
             "Annual cost range",
             pre="$",
             value=c(3000, 10000),
             min=0,
             max=20000
           )),
  ),
  actionButton("other_settings_menu_hide", "Close menu")
)

treatment_simulation_parameters <- tags$div(
  id="treatment_simulation_parameters",
  column(6, 
         sliderInput(
           "treatment_1_effectiveness_range",
           "Treatment 1 Effectiveness",
           value=c(0.1,0.4),
           min=0,
           max=1
         ),
         sliderInput(
           "treatment_1_cost_range",
           "Treatment 1 Annual Cost",
           pre="$",
           value=c(300,600),
           min=0,
           max=2000
         ),
  ),
  column(6, 
         sliderInput(
           "treatment_2_effectiveness_range",
           "Treatment 2 Effectiveness",
           value=c(0.3,0.5),
           min=0,
           max=1
         ), 
         sliderInput(
           "treatment_2_cost_range",
           "Treatment 2 Annual Cost",
           pre="$",
           value=c(600,1200),
           min=0,
           max=2000
         )
  ),
  actionButton("other_settings_menu_toggle", menuIconLabel("All Other Settings", id_prefix="other_settings"), width="80%", class=c("menu", "center")),
  all_other_settings
)


simulations_caveat_text <- "This is the result of thousands of simulations, calibrated to your inputs. For more information on how the simulations were generated, check out the User Guide in the rightmost tab above."

decision_engine_panel <- fluidPage(
  fluidRow(column(10, titlePanel("Cytospora Risk Explorer")), 
           column(2, div(id="de_option_buttons",
                     circleButton("de_update", size="sm", style="margin-top: 20px;", icon=icon("gear"),       tooltip=tooltipOptions(title="Update simulation settings")), 
                     circleButton("de_back",   size="sm", style="margin-top: 20px;", icon=icon("arrow-left"), tooltip=tooltipOptions(title="Go back to question options"))))),
  div(
    id="de_question_btns",
    fluidRow(actionButton("de_treatment", "How much should I pay for treatment?",                class='bttn-jelly bttn-lg bttn-default', style="color: #21908CFF; border: none;", width="100%")),
    fluidRow(actionButton("de_replant",   "When should I replant?",                              class='bttn-jelly bttn-lg bttn-default', style="color: #21908CFF; border: none;", width="100%")),
    fluidRow(actionButton("de_prune",     "How much do I lose if I don't prune branch cankers?", class='bttn-jelly bttn-lg bttn-default', style="color: #21908CFF; border: none;", width="100%"))
  ),
  div(
    id="de_treatment_output",
    fluidRow(column(10,  simulations_caveat_text)),
    fluidRow(plotlyOutput("de_treatment_plot", height="500px")),
    fluidRow(column(5, offset=1, uiOutput("left")),
             column(1, actionButton("change_left_and_right", "", icon=icon("arrow-right-arrow-left"), style = "margin-top: 50px; margin-left: 15%;", width="70%")),
             column(5, uiOutput("right"))),
    div(id="de_treatment_comparison", fluidRow(column(3),column(6, uiOutput("compare_treatments"))))
  ),
  actionBttn("hidden", class="hidden"),
  
  div(
    id="de_replant_output",  
    fluidRow(column(10,  simulations_caveat_text)),
    fluidRow(div(style="padding-top: 20px, padding-bottom: 20px",
                 column(4,  actionButton(inputId="replant_option_1", label="All Treatments", width="100%")),
                 column(4,  actionButton(inputId="replant_option_2", label="$500 Treatment Cost, 50% Effective", width="100%")),
                 column(4,  actionButton(inputId="replant_option_3", label="$1200 Treatment Cost, 80% Effective", width="100%")))),
    fluidRow(plotlyOutput("de_replant_plot", height="700px"))
  ),
  div(
    id="de_prune_output", 
    fluidRow(column(10, simulations_caveat_text)),
    fluidRow(div(style="margin-top: 20px, margin-bottom: 20px",
        column(4,  actionButton(inputId="prune_option_1", label="All Treatments", width="100%")),
        column(4,  actionButton(inputId="prune_option_2", label="$500 Treatment Cost, 50% Effective", width="100%")),
        column(4,  actionButton(inputId="prune_option_3", label="$1200 Treatment Cost, 80% Effective", width="100%")))),
    fluidRow(plotlyOutput("de_prune_plot", height="700px")))
)

tutorial_panel <- tags$iframe(style="height:1500px; width:100%", src="user_manual_pdf.pdf")

ui <- tabsetPanel(
    id = "panels",
    type = "hidden",
    selected="landing_page",
    tabPanelBody("landing_page", landing_page_panel),
    tabPanel(
      "dashboard", 
      tabsetPanel(
        id="main_tabs",
        type="tabs",
        tabPanel("Risk Explorer",       decision_engine_panel, id="decision"),
        tabPanel("Simulation Explorer", main_dashboard_panel,  id="main"),
        tabPanel("User Guide",          tutorial_panel,        id="tutorial")
      )
    )
)
