library(future)
library(plotly)
library(promises)
library(scales)
library(shiny)
library(shinyjqui)
library(tidyverse)

library(glue,       include.only = 'glue')
library(jrvFinance, include.only = 'irr')
library(tibbletime, include.only = 'rollify')

source("scripts/gs_connect.R")

plan(multisession)

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
  
  hide_ui <- function(ui, ...){jqui_hide(ui=ui, effect="blind", ...)}
  show_ui <- function(ui, ...){jqui_show(ui=ui, effect="blind", ...)}
  
  # Hide dashboard by default
  hide_ui("#div_dashboard")
  
  observeEvent(input$go_to_app, {
    # if user has clicked away from landing page:
    # first switch to tab `dashboard`:
      updateTabsetPanel(
        session = session, 
        inputId = "panels", 
        selected = "dashboard"
      )
      
      # then show it's contents:
      show_ui("#div_dashboard", duration=0)
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
  
  # Initially untoggle panel elements
  for (initially_hidden_menu in c(
    "#costs_menu", 
    "#disease_menu", 
    "#replanting_menu",
    "#treatments_menu",
    "#tree_replant_inputs",
    "#tree_remove_inputs"
  )) {hide_ui(ui=initially_hidden_menu)}
  
  # Store data on hidden menus
  is_hidden_menu_list <- list(
    costs=TRUE,
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
  setup_toggle_menu_options <- function(menu_name){
    # Toggles the menu hidden and shown using the main button
      observeEvent(input[[glue("{menu_name}_menu_toggle")]], {
        jqui_toggle(glue('#{menu_name}_menu'), effect = "blind")
        # double arrow persists the data in the `is_hidden_menu_list` object
        # needed to make sure that each menu's toggle state is persisted 
        is_hidden_menu_list[[menu_name]] <<- !is_hidden_menu_list[[menu_name]]
        update_menu_arrow_icon_class(menu_name, is_hidden_menu_list[[menu_name]])
      })
  }
  setup_toggle_menu_close <- function(menu_name){
    # Closes the menu with the close button
    observeEvent(input[[glue("{menu_name}_menu_hide")]], {
      hide_ui(glue('#{menu_name}_menu'))
      is_hidden_menu_list[[menu_name]] <<- TRUE
      update_menu_arrow_icon_class(menu_name, is_hidden_menu_list[[menu_name]])
    })
  }
  # Create hiding/showing listeners for menus
  
  setup_toggle_menu_options('costs')
  setup_toggle_menu_close('costs')
  
  setup_toggle_menu_options('disease')
  setup_toggle_menu_close('disease')
  
  setup_toggle_menu_options('replanting')
  setup_toggle_menu_close('replanting')
  
  setup_toggle_menu_options('treatments')
  setup_toggle_menu_close('treatments')
  
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
  get_n_cycles_possible <- function() {
    n_cycles <- floor((rv$end_year - rv$start_year)/input$replant_cycle_year_orchard)
    ifelse(n_cycles==0, 1, n_cycles)
  }
  get_planting_years <- (function(with_added_start_year=FALSE){
    # If not replanting on a cycle, only return the first year
    if (input$replanting_strategy %in% c('no_replant', 'tree_replant', 'tree_remove')) {
      planting_years <- c(rv$end_year - rv$start_year)
    } else {
      n_cycles_possible <- get_n_cycles_possible()
      years_replanted <- (1:n_cycles_possible)*input$replant_cycle_year_orchard
      to_add <- ifelse(with_added_start_year, rv$start_year, 0)
      planting_years <- years_replanted + to_add
    }
    
    return(planting_years)
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
    
    # Removes commas in the year sliders
    session$sendCustomMessage("updateSliders", '')
    })
  # Remove commas in the year sliders when any of these years are changed
  observeEvent(input$year, session$sendCustomMessage("updateSliders", ''))
  observeEvent(input$start_disease_year, session$sendCustomMessage("updateSliders", ''))
  observeEvent(input$start_treatment_year, session$sendCustomMessage("updateSliders", ''))
  
  # Render number of cycles conditional on cycle length and number of years in simulation
  
  output$orchard_replants_count <- renderText({
    years_orchard_is_replanted_string <- paste(get_planting_years(with_added_start_year=TRUE), collapse=", ")
    return(paste0("Orchard is replanted ", get_n_cycles_possible(), " times in the year(s): ", years_orchard_is_replanted_string))
  })
  
  # If the replanting strategy is to only replant trees, hide the orchard replanting cost/options.
  # If the replanting strategy is to replant the whole orchard, hide the tree cost options.
  observeEvent(input$replanting_strategy, {
    if (input$replanting_strategy == 'no_replant') {
      to_hide <- c("#tree_replant_inputs", "#orchard_replant_inputs", "#tree_remove_inputs")
      to_show <- c()
    } else if (input$replanting_strategy == 'tree_replant') {
      to_hide <- c("#orchard_replant_inputs", "#tree_remove_inputs")
      to_show <- c("#tree_replant_inputs")
    } else if (input$replanting_strategy == 'tree_remove') {
      to_hide <- c("#tree_replant_inputs", "#orchard_replant_inputs")
      to_show <- c("#tree_remove_inputs")
    } else if (input$replanting_strategy == 'orchard_replant') {
      to_hide <- c("#tree_replant_inputs", "#tree_remove_inputs")
      to_show <- c("#orchard_replant_inputs")
    }
    lapply(to_hide, hide_ui)
    lapply(to_show, show_ui)
  })
  
  n_trees_in_orchard <- 576 # 24*24, number of trees planted in one acre
  input_yield_values <- reactiveVal(NULL)
  vals = reactiveValues(x=NULL, y=NULL, selectedYield=NULL)
  draw = reactiveVal(FALSE)
  
  observeEvent(input$input_yield_update, {
    print(vals$selectedYield)
    print(vals$selectedYield/n_trees_in_orchard)
    input_yield_values(vals$selectedYield/rep(n_trees_in_orchard, length(vals$selectedYield)))
  })
  
  observeEvent(input$input_yield_click, {
    draw(!draw())
    vals$x <- append(vals$x, NA)
    vals$y <- append(vals$y, NA)
  })
  
  observeEvent(input$draw, {
    draw(input$draw)
    vals$x <- append(vals$x, NA)
    vals$y <- append(vals$y, NA)
  })
  
  observeEvent(input$input_yield_reset, handlerExpr = {
    vals$x <- NULL; vals$y <- NULL; input_yield_values(NULL);
  })
  
  observeEvent(input$input_yield_hover, {
    if (draw()) {
      vals$x <- c(vals$x, input$input_yield_hover$x)
      vals$y <- c(vals$y, input$input_yield_hover$y)
    }
  })
  
  output$input_yield_plot=renderPlot({
    plot(x=vals$x, y=vals$y, xlim=c(rv$start_year, rv$end_year), ylim=c(0, 15000), ylab="Yield (lbs/ac)", xlab="Year", type="l", lwd=3)
    if(length(vals$x)>1){
      years_to_simulate <- rv$start_year:rv$end_year
      n_years <- length(years_to_simulate)
      indices <- sapply(years_to_simulate, function(i){which.max((floor(vals$x)==i))}) 
      selected_yield <- vals$y[indices]
      # If drawing speed is too fast, some years will not have associated points
      # This code interpolates between values to get the average yield
      for (i in which(indices==2)) {
        following_indices <- indices[i:n_years]
        next_y_index <- following_indices[following_indices>2][1]
        previous_indices <- indices[1:(i-1)]
        prev_y_index <- tail(previous_indices[previous_indices>2], 1)
        interpolated_y_val <- coalesce(
          mean(c(vals$y[prev_y_index], vals$y[next_y_index]), na.rm=TRUE),
          input$max_yield
        )
        if (length(interpolated_y_val)>0){
          if(!is.na(interpolated_y_val)){
            selected_yield[i] <- interpolated_y_val
          }
        }
        
      }
      vals$selectedYield <- selected_yield
      points(x=years_to_simulate, y=selected_yield, pch=19)
    }
  })
  
  
  ####### Run simulations #######
  # Note that the reactive context means that whenever the relevant inputs 
  # change, the value of the variable changes and any downstream functions that  
  # use that variable are run
  t_disease_year <- reactive(input$start_disease_year - rv$start_year + 1)
  t_treatment_year <- reactive(input$start_treatment_year - rv$start_year + 1)
  current_year <- reactive(input$year)
  output_price <- reactive(input$output_price)
  
  last_t <- Sys.time()
  check_enough_time_has_passed_since_last_run <- function(current_time=Sys.time(), 
                                                          last_time=last_t){
    tdiff <- difftime(current_time, last_time, units="secs")
    tdiff <- as.numeric(tdiff, units="secs")
    return(tdiff > 5)
  }
  
  tree_health_data <- reactive({
    isolate(input_yield_values())
    if(is.null(input_yield_values())){
      per_year_per_tree_max_yield <- rep(input$max_yield/n_trees_in_orchard, rv$end_year - rv$start_year + 1)
    } else {
      per_year_per_tree_max_yield <- input_yield_values()
    }
    simulation_inputs <- list(
      year_start = rv$start_year,
      year_end = rv$end_year + 1,
      t_disease_year = t_disease_year(),
      t_treatment_year = t_treatment_year(),
      disease_random_share_of_spread=input$disease_random_share_of_spread/100, # function expects a percentage (fraction)
      disease_spread_rate = input$disease_spread_rate/100,
      disease_growth_rate = input$disease_growth_rate/100,
      max_yield = per_year_per_tree_max_yield,
      output_price = output_price(),
      annual_cost = input$annual_cost,
      replanting_strategy = input$replanting_strategy,
      replant_years = get_planting_years(),
      remove_cost_tree = input$remove_cost_tree,
      replant_cost_tree = input$replant_cost_tree,
      replant_cost_orchard = input$replant_cost_orchard,
      replant_tree_block_size=as.numeric(input$replant_tree_block_size),
      remove_tree_block_size=0, # TODO: decide if this should also be removed in blocks
      inf_intro = input$inf_intro,
      control1 = input$control1/100,
      t1_cost = input$t1_cost,
      control2 = input$control2/100,
      t2_cost = input$t2_cost,
      input_annual_price_change=input$percent_cost_change/100,
      output_annual_price_change=input$percent_price_change/100
    )
    simulation_results <- do.call(simulateControlScenarios, simulation_inputs)
    run_id <- rlang::hash(simulation_inputs)
    ## Setup logging for simulation settings_changes
    try({
      if(check_enough_time_has_passed_since_last_run()){
        # replant_years and max_yield are vectors, causing a problem
        simulation_inputs$replant_years <- paste(simulation_inputs$replant_years, collapse=',')
        simulation_inputs$max_yield <- paste(simulation_inputs$max_yield, collapse=',')
        future_promise({
          add_data_to_data_store(cbind(run_id=run_id, data.frame(simulation_inputs)))
        })
      }
    })
    return(list(data=simulation_results, id=run_id))})
  
    output$orchard_health <- renderPlotly({
      #Raster blocks with color indicating disease spread
      data2 <- tree_health_data()$data %>%
        dplyr::select(-ends_with(c("net_returns", "realized_costs"))) %>%
        dplyr::filter(time==current_year()) %>%
        mutate(
          `No Treatment`=ifelse(nt_tree_age==0, NA, `No Treatment`),
          `Treatment 1`=ifelse(t1_tree_age==0, NA, `Treatment 1`),
          `Treatment 2`=ifelse(t2_tree_age==0, NA, `Treatment 2`)) %>%
        dplyr::select(-ends_with("tree_age")) %>%
        mutate(across(-c(x,y,time), ~ifelse(`Disease Free`>0, ./`Disease Free`, 1))) %>%
        dplyr::select(-c(`Disease Free`)) %>%
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
          scale_fill_gradient(name="Tree Health",low = "red", high = "green", limits=c(0,1)) +
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
      df <- tree_health_data()$data %>% select(-ends_with("tree_age"))
      
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
      run_data_and_id <- tree_health_data()
      tree_health_aggregated_orchard_cost_yield_and_returns <- run_data_and_id$data %>%
        select(-ends_with("tree_age")) %>%
        group_by(time) %>%
        summarize(across(-c(x,y),~sum(.,na.rm = T))) %>%
        ungroup()

      first_six_years_max_net_returns <- (
        tree_health_aggregated_orchard_cost_yield_and_returns %>%
        select(time, max_net_returns) %>% 
        filter(time <= rv$start_year + TREE_FIRST_FULL_YIELD_YEAR) %>% 
        summarize(value=sum(max_net_returns))
      )$value
      sum_roll_6 <- rollify(sum, window = 6)
      
      # Creates the table transposed so that the data can be numeric when loaded to spreadsheet
      output_data <- bind_cols(
        #Col 1: yield
        data.frame(`Yield (avg/ac/yr)`=
          tree_health_aggregated_orchard_cost_yield_and_returns %>%
          summarize(across(-c(time),~mean(.,na.rm = T))) %>%
          select(`Disease Free`,`No Treatment`,`Treatment 1`,`Treatment 2`) %>%
          t(), check.names=FALSE),
        #Col 2: net returns
        data.frame(`Net Returns (avg/ac/yr)`=
          tree_health_aggregated_orchard_cost_yield_and_returns %>%
          summarize(across(-c(time),~mean(.,na.rm = T))) %>%
          select(`Disease Free`=max_net_returns,`No Treatment`=nt_net_returns,`Treatment 1`=t1_net_returns,`Treatment 2`=t2_net_returns) %>%
          t(), check.names=FALSE),
        #Row 3: benefit of treatment
        data.frame(`Treatment Returns (avg/ac/yr)`=
          tree_health_aggregated_orchard_cost_yield_and_returns %>%
          summarize(across(-c(time),~sum(.,na.rm = T)), n_years=n()) %>%
          mutate(t1_net_returns=(t1_net_returns - nt_net_returns)/n_years,
                 t2_net_returns=(t2_net_returns - nt_net_returns)/n_years) %>%
          mutate(`Disease Free`=NA,`No Treatment`=NA,`Treatment 1`=t1_net_returns,`Treatment 2`=t2_net_returns, .keep="none") %>%
          t(), check.names = FALSE),
        #Col 4: net present value of returns at selected year
        data.frame(`Net Present Value From Current Year ($/ac)`=
          tree_health_aggregated_orchard_cost_yield_and_returns %>%
          select(c(time, ends_with("net_returns"))) %>%
          filter(time >= current_year()) %>%
          # With a user-specified discount rate
          mutate(npv_multiplier=1/((1+input$percent_interest/100.0)**(time - current_year())),
                 across(ends_with("net_returns"), ~(.*npv_multiplier), .names = "{.col}")) %>% 
          summarise(across(ends_with("net_returns"), ~sum(.))) %>%
          mutate(`Disease Free`=NA,`No Treatment`=nt_net_returns,`Treatment 1`=t1_net_returns,`Treatment 2`=t2_net_returns, .keep="none") %>%
          t(), check.names = FALSE),
        #Col 5: operating duration
        data.frame(`Optimal First Replanting Year`=
          tree_health_aggregated_orchard_cost_yield_and_returns %>%
          filter((time > (rv$start_year + TREE_FIRST_FULL_YIELD_YEAR)) & (time < (rv$start_year + get_planting_years()[1]))) %>%
          # orders descending order so that the function cumsum sums starting from the end of the life of the orchard
          arrange(desc(time)) %>%
          mutate(across(ends_with("net_returns"), ~ifelse(n() >= 6, coalesce(sum_roll_6(.), cumsum(.)), NA), .names = "{.col}_cum")) %>%
          # chooses the first time the expected profit from the net returns is outweighed by the replanted yield
          # The plus one is to ensure that the year matches the year replanted (indexing problem)
          summarise(`Disease Free`= first(time[max_net_returns_cum >= first_six_years_max_net_returns]) + 1,
                    `No Treatment`= first(time[nt_net_returns_cum  >= first_six_years_max_net_returns]) + 1,
                    `Treatment 1` = first(time[t1_net_returns_cum  >= first_six_years_max_net_returns]) + 1,
                    `Treatment 2` = first(time[t2_net_returns_cum  >= first_six_years_max_net_returns]) + 1) %>%
          # TODO: decide if it makes sense or not to have the optimal replanting year when not replanting the entire orchard
          # mutate(across(everything(), ~ifelse(input$replanting_strategy %in% c('tree_replant'), "", .))) %>%
          t(), check.names = FALSE),
        #Col 6: IRR
        data.frame(`Internal Rate of Return`=c(
            coalesce(irr(tree_health_aggregated_orchard_cost_yield_and_returns$max_net_returns, r.guess=0.05), NA),
            coalesce(irr(tree_health_aggregated_orchard_cost_yield_and_returns$nt_net_returns, r.guess=0.05), NA),
            coalesce(irr(tree_health_aggregated_orchard_cost_yield_and_returns$t1_net_returns, r.guess=0.05), NA),
            coalesce(irr(tree_health_aggregated_orchard_cost_yield_and_returns$t2_net_returns, r.guess=0.05), NA)),
          row.names=c("Disease Free", "No Treatment", "Treatment 1", "Treatment 2"),
          check.names=FALSE)
      )
      
      
      if (check_enough_time_has_passed_since_last_run()) {
        future_promise({
          add_data_to_data_store(
            output_data %>%
              rownames_to_column("Treatment Condition") %>% 
              mutate(run_id=run_data_and_id$id, .before=1),
            'results')
        })
      }
      
      # takes the data per treatment simulation and formats it into strings for the data table
      formatted_output_data <- output_data %>%
        mutate(
          `Yield (avg/ac/yr)`             = comma(`Yield (avg/ac/yr)`, accuracy=1),
          `Optimal First Replanting Year` = as.character(`Optimal First Replanting Year`),
          `Internal Rate of Return`       = percent(`Internal Rate of Return`, accuracy=1),
          across(
            c(`Net Returns (avg/ac/yr)`, 
              `Treatment Returns (avg/ac/yr)`,
              `Net Present Value From Current Year ($/ac)`
            ),                           ~ dollar(., accuracy=1)),
        ) %>% 
        rownames_to_column("Economic Outcome") %>% # preparation for the transpose
        t() %>% data.frame() %>% rownames_to_column() 
      names(formatted_output_data) <- c("Economic Outcome", "Disease Free", "No Treatment", "Treatment 1", "Treatment 2")
      
      DT::datatable(formatted_output_data[-1, ], # Gets rid of the now redundant row names
                    options = list(dom = 't',
                                   columnDefs = list(
                                     list(width = '40px', targets = 0),
                                     list(width = '30px', targets = 1:4))
                                   ),
                    rownames = FALSE,
                    selection = 'single')
    })
    
    

  
})
