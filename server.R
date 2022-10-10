#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(scales)
library(shinyjqui)
#source("global.R")

shinyServer(function(input, output, session) {
  # Hide dashboard by default
  jqui_hide(
    ui = "#div_dashboard", 
    effect = "blind"
  )
  
  observeEvent(input$go_to_app, {
    # if user has clicked away from landing page:
    # first switch to tab `dashboard`:
      updateTabsetPanel(
        session = session, 
        inputId = "panels", 
        selected = "dashboard"
      )
      
      # then show it's contents:
      jqui_show(
        ui = "#div_dashboard", 
        effect = "blind", 
        duration = 0
      )
  }
  )
  output$logo <- renderImage(
    expr=list(
      class= "center",
      src = "static/images/logo.png",
      contentType = "image/png",
      alt = "CSU Logo",
      height = "100%"),
    deleteFile=FALSE,
  )

  #Run simulations within reactive element
  start_year <- reactive(input$year_start)
  current_year <- reactive(start_year() + input$year - 1)
  
  tree_health <- reactive({
    simulateControlScenarios(
      year_start = start_year(),
      year_end = input$year_end,
      start_disease_year = input$start_disease_year,
      disease_spread_rate = input$disease_spread_rate/100, # function expects a percentage (fraction)
      disease_growth_rate = input$disease_growth_rate/100,
      max_yield = input$max_yield/576, #may want to make the number of trees an input
      output_price = input$output_price,
      annual_cost = input$annual_cost,
      replanting_strategy = input$replanting_strategy,
      replant_cost_tree = input$replant_cost_tree,
      inf_intro = input$inf_intro,
      control1 = input$control1/100,
      t1_cost = input$t1_cost,
      control2 = input$control2/100,
      t2_cost = input$t2_cost
    )})
  
    output$orchard_health <- renderPlot({
      #Raster blocks with color indicating disease spread
      tree_health() %>%
        dplyr::select(-ends_with("net_returns")) %>%
        dplyr::filter(time==current_year()) %>%
        mutate(across(-c(x,y,time),~./`Max Yield`)) %>%
        dplyr::select(-`Max Yield`) %>%
        pivot_longer(-c(x,y,time)) %>%
        mutate(value=ifelse(value<0,0,value)) %>%
        ggplot(aes(x=x,y=y,fill=value)) +
        geom_tile(size=.1,show.legend = F) +
        scale_fill_gradient(name="Tree Health",low = "red", high = "green",limit=c(0,1)) +
        scale_x_continuous(name = "Column",breaks = seq.int(1,24,2),minor_breaks = NULL) +
        scale_y_continuous(name = "Row",breaks = seq.int(1,24,2),minor_breaks = NULL) +
        theme_bw(base_size = 15) +
        coord_equal() +
        labs(title = "Block Health") +
        facet_wrap(~name,nrow = 1)
    })

    output$tree_health <- renderPlot({

      #Plot of individual tree growth
      # tree_health() %>%
      #   dplyr::filter(x==input$tree_col,y==input$tree_row) %>%
      #   pivot_longer(-c(x,y,time)) %>%
      #   ggplot(aes(x=time-1,y=value-1,color=name)) +
      #   geom_line() +
      #   geom_point() +
      #   scale_y_continuous(breaks = seq(0,10,2),limits = c(0,12)) +
      #   labs(x="Year",y="Yield",title="Tree Growth Path") +
      #   theme_bw(base_size = 15) +
      #   theme(legend.title = element_blank(),legend.position = "bottom")

      #Plot of block growth
      tree_health() %>%
        dplyr::select(-ends_with("net_returns")) %>%
        mutate(across(-c(x,y,time),~ifelse(.<0,0,.))) %>%
        group_by(time) %>%
        summarize(across(-c(x,y),~sum(.,na.rm = T))) %>%
        pivot_longer(-c(time)) %>%
        ggplot(aes(x=time-1,y=value-1,color=name)) +
        geom_line() +
        geom_point() +
        geom_vline(xintercept = current_year(),linetype="dashed") +
        scale_y_continuous(labels = label_comma()) +
        labs(x="Year",y="Yield",title="Block Yield Over Time") +
        theme_bw(base_size = 15) +
        theme(legend.title = element_blank(),legend.position = "bottom")  #Need to split legend over two rows

    })

    output$mytable <- DT::renderDataTable({
      DT::datatable(bind_rows(
                    #Row 1: yield
                    tree_health() %>%
                      mutate(across(-c(x,y,time),~ifelse(.<0,0,.))) %>%
                      group_by(time) %>%
                      summarize(across(-c(x,y),~sum(.,na.rm = T))) %>%
                      ungroup() %>%
                      summarize(across(-c(time),~mean(.,na.rm = T))) %>%
                      mutate(across(everything(),~comma(.,accuracy=1))) %>%
                      select(`Max Yield`,`No Treatment`,`Treatment 1`,`Treatment 2`) %>%
                      add_column(`Economic Result`="Yield (avg/ac/yr)",.before = 1),
                    #Row 2: net returns
                    tree_health() %>%
                      mutate(across(-c(x,y,time),~ifelse(.<0,0,.))) %>%
                      group_by(time) %>%
                      summarize(across(-c(x,y),~sum(.,na.rm = T))) %>%
                      ungroup() %>%
                      summarize(across(-c(time),~sum(.,na.rm = T))) %>%
                      mutate(across(everything(),~dollar(.,accuracy=1))) %>%
                      select(ends_with("net_returns")) %>%
                      rename(`Max Yield`=max_net_returns,`No Treatment`=nt_net_returns,`Treatment 1`=t1_net_returns,`Treatment 2`=t2_net_returns) %>%
                      add_column(`Economic Result`="Net Returns (avg/ac/yr)",.before = 1),
                    #Row 3: benefit of treatment
                    tree_health() %>%
                      mutate(across(-c(x,y,time),~ifelse(.<0,0,.))) %>%
                      group_by(time) %>%
                      summarize(across(-c(x,y),~sum(.,na.rm = T))) %>%
                      ungroup() %>%
                      summarize(across(-c(time),~sum(.,na.rm = T))) %>%
                      mutate(t1_net_returns=t1_net_returns-nt_net_returns,
                             t2_net_returns=t2_net_returns-nt_net_returns,
                             across(everything(),~dollar(.,accuracy=1))) %>%
                      select(ends_with("net_returns")) %>%
                      rename(`Max Yield`=max_net_returns,`No Treatment`=nt_net_returns,`Treatment 1`=t1_net_returns,`Treatment 2`=t2_net_returns) %>%
                      mutate(`Max Yield`="",
                             `No Treatment`="") %>%
                      add_column(`Economic Result`="Treatment Returns (avg/ac/yr)",.before = 1),
                    #Row 4: operating duration
                    tree_health() %>%
                      #mutate(across(-c(x,y,time),~ifelse(.<0,0,.))) %>%
                      group_by(time) %>%
                      summarize(across(-c(x,y),~sum(.,na.rm = T))) %>%
                      ungroup() %>%
                      filter(time>input$year_start+5) %>%
                      summarise(`Treatment 1`=as.character(first(time[t1_net_returns<0])),
                                `Treatment 2`=as.character(first(time[t2_net_returns<0])),
                                `No Treatment`=as.character(first(time[nt_net_returns<0]))) %>%
                      mutate(`Max Yield`="") %>%
                      add_column(`Economic Result`="Optimal Replanting Year",.before = 1)
                    ),
                    options = list(dom = 't',
                                   columnDefs = list(
                                     list(width = '40px', targets = 0),
                                     list(width = '30px', targets = 1:4))
                                   ),
                    rownames = FALSE)
    })

  
})
