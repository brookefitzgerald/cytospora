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
library(plotly)
library(glue)

shinyServer(function(input, output, session) {
  #  First input that is also updated by other inputs e.g. sum(annual_cost_{n})
  updateTotalAnnualCost <- function(input_name){
    observeEvent(input[[input_name]], {
      updateTextInput("annual_cost", 
                      value=input$annual_cost_1+input$annual_cost_2, 
                      session=session)
    })
  }
  updateTotalAnnualCost('annual_cost_1')
  updateTotalAnnualCost('annual_cost_2')
  
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
  })
  
  output$logo <- renderImage(
    expr=list(
      class= "center",
      src = "www/images/logo.png",
      contentType = "image/png",
      alt = "CSU Logo",
      height = "100%"),
    deleteFile=FALSE,
  )
  
  ##### Dynamically rendering the simulation options ########
  # Initially untoggled panel elements
  jqui_hide(ui="#disease_menu",        effect="blind")
  jqui_hide(ui="#replanting_menu",     effect="blind")
  jqui_hide(ui="#treatments_menu",     effect="blind")
  jqui_hide(ui="#tree_replant_inputs", effect="blind")
  
  is_hidden_menu_list <- list(
    disease=TRUE,
    replanting=TRUE,
    treatments=TRUE
  )
  
  # Updates the class of the arrow icon to make it rotate when hidden. 
  update_menu_arrow_icon_class <- function(id, is_hidden=TRUE){
    if (is_hidden) {
      jqui_add_class(glue("#{id}_menu_icon"), "hidden_menu_arrow")
    } else {
      jqui_remove_class(glue("#{id}_menu_icon"), "hidden_menu_arrow")
    }
  }
  toggle_menu_options <- function(menu_name){
    # Toggles the menu hidden and shown using the main button
      observeEvent(input[[glue("{menu_name}_menu_toggle")]], {
        jqui_toggle(glue('#{menu_name}_menu'), effect = "blind")
        is_hidden_menu_list[[menu_name]] <<- !is_hidden_menu_list[[menu_name]]
        update_menu_arrow_icon_class(menu_name, is_hidden_menu_list[[menu_name]])
      })
  }
  toggle_menu_close <- function(menu_name){
    # Closes the menu with the close button
    observeEvent(input[[glue("{menu_name}_menu_hide")]], {
      jqui_hide(glue('#{menu_name}_menu'), effect = "blind")
      is_hidden_menu_list[[menu_name]] <<- TRUE
      update_menu_arrow_icon_class(menu_name, is_hidden_menu_list[[menu_name]])
    })
  }
  
  toggle_menu_options('disease')
  toggle_menu_close('disease')
  
  toggle_menu_options('replanting')
  toggle_menu_close('replanting')
  
  toggle_menu_options('treatments')
  toggle_menu_close('treatments')
  
  # Update year slider ranges conditional on time horizon
  keep_year_proportion <- function(old_value, old_start, old_end, start, end){
    old_proportion <- (old_value-old_start)/(old_end-old_start)
    #solve (ov-os)/(oe-os)=(v-s)/(e-s) for v:
    value <- round(old_proportion*(end - start) + start)
    return(value)
  }
  
  updateYearSliderInput <- function(name, start, end, fn){
    updateSliderInput(name, value=fn(input[[name]]), min=start, max=end, session=session)
  }
  rv <- reactiveValues(
    prev_th = NULL, 
    start_year = NULL,
    end_year = NULL
  )
  get_n_cycles_possible <- (function() {
    floor((rv$end_year - rv$start_year)/input$replant_cycle_year_orchard)
  })
  get_replanting_years <- (function(with_added_start_year=FALSE){
    n_cycles_possible <- get_n_cycles_possible()
    years_replanted <- (1:n_cycles_possible)*input$replant_cycle_year_orchard
    to_add <- ifelse(with_added_start_year, rv$start_year, 0)
    return(years_replanted + to_add)
  })
  observeEvent(input$time_horizon, {
    start_yr <- rv$start_year <- get_year_from_date(input$time_horizon[1])
    end_yr   <- rv$end_year <- get_year_from_date(input$time_horizon[2])
    if(is_null(rv$prev_th)){rv$prev_th <- input$time_horizon}
    old_start <- get_year_from_date(rv$prev_th[1])
    old_end   <- get_year_from_date(rv$prev_th[2])
    
    fn <- partial(keep_year_proportion, old_start=old_start, old_end=old_end, start=start_yr, end=end_yr)
    
    # Update all the ranges and values of the year sliders. 
    # 1. Keep the same proportion of year value (if selected year is 1/3rd of the way through the simulation before, 1/3rd of the way now).
    # 2. Update ranges to be the selected years of the simulation (time_horizon).
    for (slider in c("year","start_disease_year", "start_treatment_year")) {
      updateYearSliderInput(slider, start_yr, end_yr, fn)
    }
    
    # Update the maximum value of the replant cycle orchard to be the updated time_horizon
    updateSliderInput("replant_cycle_year_orchard", max=end_yr - start_yr, session=session)
    rv$prev_th <- input$time_horizon
    
    # removes commas in the year sliders
    session$sendCustomMessage("updateSliders", 'test')
    })
  observeEvent(input$year, session$sendCustomMessage("updateSliders", 'test'))
  observeEvent(input$start_disease_year, session$sendCustomMessage("updateSliders", 'test'))
  observeEvent(input$start_treatment_year, session$sendCustomMessage("updateSliders", 'test'))
  
  # Render number of cycles conditional on cycle length and number of years in simulation
  
  output$orchard_replants_count <- renderText({
    years_orchard_is_replanted_string <- paste(get_replanting_years(with_added_start_year=TRUE), collapse=", ")
    return(paste0("Orchard is replanted ", get_n_cycles_possible(), " times in the year(s): ", years_orchard_is_replanted_string))
  })
  
  # If the replanting strategy is to only replant trees, hide the orchard replanting cost/options.
  # If the replanting strategy is to replant the whole orchard, hide the tree cost options.
  observeEvent(input$replanting_strategy, {
    if (input$replanting_strategy == 'no_replant') {
      jqui_hide(ui="#tree_replant_inputs",    effect="blind")
      jqui_hide(ui="#orchard_replant_inputs", effect="blind")
    } else if (input$replanting_strategy == 'tree_replant') {
      jqui_show(ui="#tree_replant_inputs",    effect="blind")
      jqui_hide(ui="#orchard_replant_inputs", effect="blind")
    } else if (input$replanting_strategy == 'orchard_replant') {
      jqui_hide(ui="#tree_replant_inputs",    effect="blind")
      jqui_show(ui="#orchard_replant_inputs", effect="blind")
    }
  })

  
  # Run simulations
  t_disease_year <- reactive(input$start_disease_year - rv$start_year + 1)
  t_treatment_year <- reactive(input$start_treatment_year - rv$start_year + 1)
  current_year <- reactive(input$year)
  output_price <- reactive(input$output_price)
  n_trees_in_orchard <- 576 # 24*24, number of trees planted in one acre
  max_yield <- reactive(input$max_yield/n_trees_in_orchard)
  
  tree_health_data <- reactive({
    simulateControlScenarios(
      year_start = rv$start_year,
      year_end = rv$end_year + 1,
      t_disease_year = t_disease_year(),
      t_treatment_year = t_treatment_year(),
      disease_spread_rate = input$disease_spread_rate/100, # function expects a percentage (fraction)
      disease_growth_rate = input$disease_growth_rate/100,
      max_yield = input$max_yield/n_trees_in_orchard,
      output_price = output_price(),
      annual_cost = input$annual_cost,
      replanting_strategy = input$replanting_strategy,
      replant_years = get_replanting_years(),
      replant_cost_tree = input$replant_cost_tree,
      replant_cost_orchard = input$replant_cost_orchard,
      replant_tree_block_size=as.numeric(input$replant_tree_block_size),
      inf_intro = input$inf_intro,
      control1 = input$control1/100,
      t1_cost = input$t1_cost,
      control2 = input$control2/100,
      t2_cost = input$t2_cost
    )})
  
    output$orchard_health <- renderPlotly({
      #Raster blocks with color indicating disease spread
      data2 <- tree_health_data() %>%
        dplyr::select(-ends_with(c("net_returns", "realized_costs"))) %>%
        dplyr::filter(time==current_year()) %>%
        mutate(across(-c(x,y,time), ~ifelse(`Disease Free`>0, ./`Disease Free`, 1))) %>%
        dplyr::select(-`Disease Free`) %>%
        pivot_longer(-c(x,y,time)) %>%
        mutate(yield=ifelse(value<0,0,value))
        
        # Use ggplot to plot the yield heatmap
        plot <- data2 %>%
          # The following lines ensure that if all of the yields are the same then 
          # the plot doesn't turn grey (consequence of plotly converting the colorscale)
          add_row(x=-2, y=1, yield=0, name='No Treatment') %>%
          add_row(x=-1, y=1, yield=1, name='No Treatment') %>%
          add_row(x=-2, y=1, yield=0, name='Treatment 1') %>%
          add_row(x=-1, y=1, yield=1, name='Treatment 1') %>%
          add_row(x=-2, y=1, yield=0, name='Treatment 2') %>%
          add_row(x=-1, y=1, yield=1, name='Treatment 2') %>%
          ggplot(aes(x=x,y=y,fill=yield)) +
          geom_tile(size=.1,show.legend = F) +
          scale_fill_gradient(name="Tree Health",low = "red", high = "green", limits=c(0,1), na.value="green") +
          scale_x_continuous(name = "Column") +
          scale_y_continuous(name = "Row") +
          theme_bw(base_size = 15) +
          labs(title = "Orchard Health") +
          facet_wrap(~name,nrow = 1)
        
        # Transform into a plotly plot
        fig <- ggplotly(plot, source='orchard_health') %>%
          plotly::layout(xaxis = list(label="column"),
                         yaxis = list(constrain="domain", range=c(1, 24), constraintoward='top', label="row"),
                         hovermode='closest') %>%
          # style function updates all traces with the specified options
          style(hovertemplate="Tree Yield: %{z:.0%} of maximum yield<extra></extra>") %>%
          # makes the plotly hover event accessible to plot the individual tree yields
          event_register("plotly_hover") %>% 
          event_register("plotly_unhover")
        
        # This code zooms into the actual data (ignoring the two added rows) and
        # prevents zooming the x axis to see the added rows
        for (axis in c('xaxis', 'xaxis2', 'xaxis3')){
          fig$x$layout[[axis]]$range <- c(1, 24)
          fig$x$layout[[axis]]$fixedrange <- TRUE
        }
        return(fig)
      })
    
    
    # This section prevents flickering between the tree and orchard yield plots
    # as the user changes which tree they are mousing over.
    end_hover_time <- reactiveVal()
    most_recent_x_y_hover <- reactiveVal()
    hover_x_y_event <- reactiveVal()
    unhover_x_y_event <- reactiveVal()
    
    observeEvent(event_data("plotly_hover", source = "orchard_health"), {
      hover_x_y_event(event_data("plotly_hover", source = "orchard_health"))
      most_recent_x_y_hover(list(x=hover_x_y_event()[["x"]], y=hover_x_y_event()[["y"]]))
    })
    observeEvent(event_data("plotly_unhover", source = "orchard_health"), {
      unhover_x_y_event(event_data("plotly_unhover", source = "orchard_health"))
      end_hover_time(Sys.time())
    })
    observe({
      # The following line ensures that this chunk of code is called every .5 secs,
      # even without any underlying data changing (events, reactiveVals, etc.).
      # This ensures that after unhovering on the yield graph, the plot is 
      # updated to show the orchard yield plot.  
      invalidateLater(500, session) 
      hover_and_unhover_events_are_not_null <- (!is.null(unhover_x_y_event()[["x"]]) && !is.null(hover_x_y_event()[["x"]]))
      # The two events have different x,y values until you are no longer hovering over the plot. 
      not_currently_hovering_on_plot <- ((unhover_x_y_event()[["x"]]==hover_x_y_event()[["x"]]) && (unhover_x_y_event()[["y"]]==hover_x_y_event()[["y"]]))
      if (hover_and_unhover_events_are_not_null && not_currently_hovering_on_plot) { 
        if (!is.null(end_hover_time()) && ((Sys.time() - end_hover_time()) > 0.5)){ # more than half a second after we stopped hovering
          most_recent_x_y_hover(NULL)
          end_hover_time(NULL)
        }
      }
      session$sendCustomMessage("updateSliders", 'test') # this line updates the year sliders regularly
    })
    
    output$tree_health <- renderPlotly({
      # if hovering, plot the tree's yield over time. If not hovering, plot the
      # table row selected, otherwise render the overall orchard yield.
      selected_row <- input$mytable_rows_selected
      df <- tree_health_data()
      
      if (!is.null(most_recent_x_y_hover())) {
        p <- plot_tree_yield(df, x_coord=most_recent_x_y_hover()[["x"]], y_coord=most_recent_x_y_hover()[["y"]])
        
      } else if (is.null(selected_row)) {
        p <- plot_orchard_yield(df)
        
      } else if (selected_row == 2){
        p <- plot_net_returns(df)
        
      }  else if (selected_row == 3){
        p <- plot_returns_to_treatment(df)
        
      }  else if (selected_row == 4) {
        p <- plot_npv(df, r=input$percent_interest/100., t0=rv$start_year)
          
      } else {
        p <- plot_orchard_yield(df)
      }
      
      # Plot selected graph
      (p + 
        plot_line_at_current_year(current_year())) %>%
        ggplotly()  %>%
        layout(legend = list(orientation = 'h', y=-0.5, fontsize=12)) #Need to split legend over two rows)
    })
    
    output$mytable <- DT::renderDataTable({
      tree_health_aggregated_orchard_cost_yield_and_returns <- tree_health_data() %>%
        group_by(time) %>%
        summarize(across(-c(x,y),~sum(.,na.rm = T))) %>%
        ungroup()

      first_six_years_max_net_returns <- (
        tree_health_aggregated_orchard_cost_yield_and_returns %>%
        select(time, max_net_returns) %>% 
        filter(time <= rv$start_year + TREE_FIRST_FULL_YIELD_YEAR) %>% 
        summarize(value=sum(max_net_returns))
      )$value
      
      
      
      DT::datatable(bind_rows(
                    #Row 1: yield
                    tree_health_aggregated_orchard_cost_yield_and_returns %>%
                      summarize(across(-c(time),~mean(.,na.rm = T))) %>%
                      mutate(across(everything(),~comma(.,accuracy=1))) %>%
                      select(`Disease Free`,`No Treatment`,`Treatment 1`,`Treatment 2`) %>%
                      add_column(`Economic Result`="Yield (avg/ac/yr)",.before = 1),
                    #Row 2: net returns
                    tree_health_aggregated_orchard_cost_yield_and_returns %>%
                      summarize(across(-c(time),~mean(.,na.rm = T))) %>%
                      mutate(across(everything(),~dollar(.,accuracy=1))) %>%
                      select(ends_with("net_returns")) %>%
                      rename(`Disease Free`=max_net_returns,`No Treatment`=nt_net_returns,`Treatment 1`=t1_net_returns,`Treatment 2`=t2_net_returns) %>%
                      add_column(`Economic Result`="Net Returns (avg/ac/yr)",.before = 1),
                    #Row 3: benefit of treatment
                    tree_health_aggregated_orchard_cost_yield_and_returns %>%
                      summarize(across(-c(time),~sum(.,na.rm = T)), n_years=n()) %>%
                      mutate(t1_net_returns=(t1_net_returns - nt_net_returns)/n_years,
                             t2_net_returns=(t2_net_returns - nt_net_returns)/n_years,
                             across(everything(),~dollar(.,accuracy=1))) %>%
                      select(ends_with("net_returns")) %>%
                      rename(`Disease Free`=max_net_returns,`No Treatment`=nt_net_returns,`Treatment 1`=t1_net_returns,`Treatment 2`=t2_net_returns) %>%
                      mutate(`Disease Free`="",
                             `No Treatment`="") %>%
                      add_column(`Economic Result`="Treatment Returns (avg/ac/yr)",.before = 1),
                    #Row 4: net present value of returns at selected year
                    tree_health_aggregated_orchard_cost_yield_and_returns %>%
                      select(c(time, ends_with("net_returns"))) %>%
                      filter(time >= current_year()) %>%
                      # With a 3% discount rate
                      mutate(npv_multiplier=1/((1+input$percent_interest/100.0)**(time - current_year())),
                             across(ends_with("net_returns"), ~(.*npv_multiplier), .names = "{.col}")) %>% 
                      summarise(across(ends_with("net_returns"), ~dollar(sum(.),accuracy=1))) %>%
                      rename(`Disease Free`=max_net_returns,`No Treatment`=nt_net_returns,`Treatment 1`=t1_net_returns,`Treatment 2`=t2_net_returns) %>%
                      mutate(`Disease Free`="") %>%
                      add_column(`Economic Result`="Net Present Value From Current Year ($/ac)", .before = 1),
                    #Row 5: operating duration
                    tree_health_aggregated_orchard_cost_yield_and_returns %>%
                      filter((time > rv$start_year + TREE_FIRST_FULL_YIELD_YEAR) & (time < rv$start_year + get_replanting_years()[1])) %>%
                      # orders descending order so that the function cumsum sums starting from the end of the life of the orchard
                      arrange(desc(time)) %>%
                      mutate(across(ends_with("net_returns"), ~cumsum(.), .names = "{.col}_cum")) %>%
                      # chooses the first time the expected profit from the net returns is outweighed by the replanted yield
                      summarise(`Treatment 1`=as.character(first(time[t1_net_returns_cum >= first_six_years_max_net_returns])),
                                `Treatment 2`=as.character(first(time[t2_net_returns_cum >= first_six_years_max_net_returns])),
                                `No Treatment`=as.character(first(time[nt_net_returns_cum >= first_six_years_max_net_returns])),
                                `Disease Free`=as.character(first(time[max_net_returns_cum >= first_six_years_max_net_returns]))) %>%
                      mutate(across(everything(), ~ifelse(input$replanting_strategy=='tree_replant', "", .))) %>%
                      add_column(`Economic Result`="Optimal First Replanting Year",.before = 1),
                    ),
                    options = list(dom = 't',
                                   columnDefs = list(
                                     list(width = '40px', targets = 0),
                                     list(width = '30px', targets = 1:4))
                                   ),
                    rownames = FALSE,
                    selection = 'single')
    })
    
    

  
})
