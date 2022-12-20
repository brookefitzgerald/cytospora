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
source("ui.R")

shinyServer(function(input, output, session) {
  # Only input that is also updated by other inputs e.g. sum(annual_cost_{n})
  output$annual_cost_res <- renderUI(
    numericInput("annual_cost",
                 # Using infoHoverLabel from ui.R to add an informational tooltip
                 infoHoverLabel("Annual Production Cost ($/ac/yr)"),
                 value = input$annual_cost_1+input$annual_cost_2)
  )
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
      src = "www/images/logo.png",
      contentType = "image/png",
      alt = "CSU Logo",
      height = "100%"),
    deleteFile=FALSE,
  )
  
  ##### Dynamically rendering the simulation options ########
  # Initially untoggled panel elements
  jqui_hide(ui="#disease_menu",    effect="blind")
  jqui_hide(ui="#replanting_menu", effect="blind")
  jqui_hide(ui="#treatments_menu", effect="blind")
  
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
      is_hidden_menu_list[[menu_name]] <- TRUE
      update_menu_arrow_icon_class(menu_name, is_hidden_menu_list[[menu_name]])
    })
  }
  
  toggle_menu_options('disease')
  toggle_menu_close('disease')
  
  toggle_menu_options('replanting')
  toggle_menu_close('replanting')
  
  toggle_menu_options('treatments')
  toggle_menu_close('treatments')
  
  
  #Run simulations within reactive element
  start_year <- reactive(get_year_from_date(input$time_horizon[1]))
  end_year <- reactive(get_year_from_date(input$time_horizon[2]))
  current_year <- reactive(start_year() + input$year - 1)
  output_price <- reactive(input$output_price)
  
  n_trees_in_orchard <- 576 # 24*24, number of trees planted in one acre
  
  tree_health_data <- reactive({
    simulateControlScenarios(
      year_start = start_year(),
      year_end = end_year(),
      t_disease_year = input$start_disease_year,
      t_treatment_year = input$start_treatment_year,
      disease_spread_rate = input$disease_spread_rate/100, # function expects a percentage (fraction)
      disease_growth_rate = input$disease_growth_rate/100,
      max_yield = input$max_yield/n_trees_in_orchard,
      output_price = output_price(),
      annual_cost = input$annual_cost,
      replanting_strategy = input$replanting_strategy,
      replant_year = input$replant_year_orchard,
      replant_cost_tree = input$replant_cost_tree,
      replant_cost_orchard = input$replant_cost_orchard,
      inf_intro = input$inf_intro,
      control1 = input$control1/100,
      t1_cost = input$t1_cost,
      control2 = input$control2/100,
      t2_cost = input$t2_cost
    )})
  
    output$orchard_health <- renderPlotly({
      #Raster blocks with color indicating disease spread
      data <- tree_health_data() %>%
        dplyr::select(-ends_with(c("net_returns", "realized_costs"))) %>%
        dplyr::filter(time==current_year()) %>%
        #When the yield is all zero, the heatmap turns grey. I add a tiny amount of noise to keep it green.
        mutate(across(-c(x,y,time),~ifelse(`Max Yield`>0, ./`Max Yield`, 1-runif(n(),max=1e-10)))) %>%
        dplyr::select(-`Max Yield`) %>%
        pivot_longer(-c(x,y,time)) %>%
        mutate(yield=ifelse(value<0,0,value))
        
        # Use ggplot to plot the yield heatmap
        plot <- ggplot(data, aes(x=x,y=y,fill=yield, customdata=name)) +
          geom_tile(size=.1,show.legend = F) +
          scale_fill_gradient(name="Tree Health",low = "red", high = "green",limit=c(0,1)) +
          scale_x_continuous(name = "Column")+#,breaks = seq.int(1,24,2),minor_breaks = NULL) +
          scale_y_continuous(name = "Row")+#,breaks = seq.int(1,24,2),minor_breaks = NULL) +
          theme_bw(base_size = 15) +
          labs(title = "Orchard Health") +
          facet_wrap(~name,nrow = 1)
        
        # Transform into a plotly plot
        fig <- ggplotly(plot, source='orchard_health') %>%
          plotly::layout(xaxis = list(label="column"),
                         yaxis = list(constrain="domain", constraintoward='top', label="row"),
                         hovermode='closest') %>%
          style(hovertemplate="Tree Yield: %{z:.0%}", name='') %>%
          event_register("plotly_hover")
        
        return(fig)
      })
    
    output$tree_health <- renderPlotly({
      # if there is no hover data, render the overall orchard yield, 
      # otherwise render the tree growth path  
      hoverData <- event_data("plotly_hover", source = "orchard_health")
      selected <- input$mytable_rows_selected
      df <- tree_health_data()
      if (!is.null(selected)) {
        selected_row <- selected
      } else {
        selected_row <- -1
      }
      selected_net_returns <- (selected_row==2)
      selected_return_to_treatment <- (selected_row==3)
      selected_npv <- (selected_row==4)
      if (!is.null(hoverData)) {
        # Get individual tree yield
        df <- df %>%
          dplyr::select(-ends_with(c("net_returns", "realized_costs"))) %>%
          filter(x==hoverData[["x"]] & y==hoverData[["y"]]) %>%
          group_by(time) %>%
          summarize(across(-c(x,y),~sum(.,na.rm = T))) %>%
          pivot_longer(c(-time)) %>%
          ggplot(aes(x=time,y=value,color=name)) +
          geom_line() +
          geom_point() +
          geom_vline(xintercept = current_year(),linetype="dashed") +
          geom_vline(xintercept = current_year(),linetype="dashed") +
          scale_y_continuous(labels = label_comma()) +
          labs(x="Year",y="Yield (lbs/tree)",title="Tree Yield Over Time") +
          theme_bw(base_size = 15) +
          theme(legend.title = element_blank(),legend.position = "bottom")
      } else if (selected_net_returns){
        df <- df %>% select(c(ends_with("net_returns"), time, x, y)) %>%
          group_by(time) %>%
          summarize(across(-c(x,y),~sum(.,na.rm = T))) %>%
          ungroup() %>% 
          rename(`Max Yield`=max_net_returns,`No Treatment`=nt_net_returns,`Treatment 1`=t1_net_returns,`Treatment 2`=t2_net_returns) %>%
          pivot_longer(c(-time)) %>%
          ggplot(aes(x=time, y=value, color=name)) +
          geom_line() +
          geom_point() +
          geom_vline(xintercept = current_year(),linetype="dashed") +
          scale_y_continuous(labels = label_comma()) +
          labs(x="Year",y="Net returns ($)",title="Net Returns Over Time") +
          theme_bw(base_size = 15) +
          theme(legend.title = element_blank(),legend.position = "bottom") 
      }  else if (selected_return_to_treatment){
        df <- df %>% 
          select(c(ends_with("net_returns"), time, x, y)) %>%
          group_by(time) %>%
          summarize(across(-c(x,y),~sum(.,na.rm = T))) %>%
          ungroup() %>%
          mutate(t1_net_returns=(t1_net_returns - nt_net_returns),
                 t2_net_returns=(t2_net_returns - nt_net_returns)) %>%
                 #across(c(t1_net_returns, t2_net_returns), dollar(., accuracy=1))) %>%
          select(c(t1_net_returns, t2_net_returns, time)) %>%
          rename(`Treatment 1`=t1_net_returns,`Treatment 2`=t2_net_returns) %>%
          pivot_longer(c(-time)) %>%
          ggplot(aes(x=time, y=value, color=name)) +
          geom_line() +
          geom_point() +
          geom_vline(xintercept = current_year(),linetype="dashed") +
          scale_y_continuous(labels = label_comma()) +
          labs(x="Year",y="Returns to Treatment ($)",title="Returns to Treatment Over Time") +
          theme_bw(base_size = 15) +
          theme(legend.title = element_blank(),legend.position = "bottom") 
      }  else if (selected_npv) {
        df <- df %>% 
          select(c(ends_with("net_returns"), time, x, y)) %>%
          group_by(time) %>%
          summarize(across(-c(x,y),~sum(.,na.rm = T))) %>%
          ungroup() %>% 
          mutate(t=time-start_year(), 
                 npv_multiplier=((1+input$percent_interest/100.)**-t)) %>% 
          arrange(desc(t)) %>% 
          mutate(across(-c(time, npv_multiplier, t),~cumsum(.*npv_multiplier), .names="{.col}_npv")) %>%
          select(c(ends_with("_npv"), time)) %>%
          rename(`Max Yield`=max_net_returns_npv,`No Treatment`=nt_net_returns_npv,`Treatment 1`=t1_net_returns_npv,`Treatment 2`=t2_net_returns_npv) %>%
          pivot_longer(c(-time)) %>%
          ggplot(aes(x=time, y=value, color=name)) +
          geom_line() +
          geom_point() +
          geom_vline(xintercept = current_year(),linetype="dashed") +
          scale_y_continuous(labels = label_comma()) +
          labs(x="Year",y="Net present value ($)",title="Net Present Value Over Time") +
          theme_bw(base_size = 15) +
          theme(legend.title = element_blank(),legend.position = "bottom") 
      } else {
        df <- df %>%
          dplyr::select(-ends_with(c("net_returns", "realized_costs"))) %>%
          group_by(time) %>%
          # Looking at thousands of dollars in profit
          summarize(across(-c(x,y),~sum(./1000,na.rm = T))) %>%
          ungroup() %>%
          pivot_longer(-c(time)) %>%
          ggplot(aes(x=time,y=value,color=name)) +
          geom_line() +
          geom_point() +
          geom_vline(xintercept = current_year(),linetype="dashed") +
          scale_y_continuous(labels = unit_format(unit = "K")) +
          labs(x="Year",y="Yield (lbs/ac)",title="Orchard Yield Over Time") +
          theme_bw(base_size = 15) +
          theme(legend.title = element_blank(),legend.position = "bottom")
      }
      #Plot of block yield over time
      ggplotly(df)  %>%
        layout(legend = list(orientation = 'h', y=-0.5, fontsize=12)) #Need to split legend over two rows)
    })
    
    output$mytable <- DT::renderDataTable({
      tree_health_aggregated_orchard_cost_yield_and_returns <- tree_health_data() %>%
        group_by(time) %>%
        summarize(across(-c(x,y),~sum(.,na.rm = T))) %>%
        ungroup()

      replanted_yield_until_max <- (tree_health_aggregated_orchard_cost_yield_and_returns %>%
        select(time, `Max Yield`) %>%
        mutate(max_net_returns= input$output_price*`Max Yield`-input$annual_cost) %>%
        # add the initial cost of planting the orchard
        add_row(time=start_year() - 1, max_net_returns=-input$replant_cost_orchard) %>%
        filter(time <= start_year() + TREE_FIRST_FULL_YIELD_YEAR) %>% 
        summarize(value=sum(max_net_returns)))$value
      
      DT::datatable(bind_rows(
                    #Row 1: yield
                    tree_health_aggregated_orchard_cost_yield_and_returns %>%
                      summarize(across(-c(time),~mean(.,na.rm = T))) %>%
                      mutate(across(everything(),~comma(.,accuracy=1))) %>%
                      select(`Max Yield`,`No Treatment`,`Treatment 1`,`Treatment 2`) %>%
                      add_column(`Economic Result`="Yield (avg/ac/yr)",.before = 1),
                    #Row 2: net returns
                    tree_health_aggregated_orchard_cost_yield_and_returns %>%
                      summarize(across(-c(time),~mean(.,na.rm = T))) %>%
                      mutate(across(everything(),~dollar(.,accuracy=1))) %>%
                      select(ends_with("net_returns")) %>%
                      rename(`Max Yield`=max_net_returns,`No Treatment`=nt_net_returns,`Treatment 1`=t1_net_returns,`Treatment 2`=t2_net_returns) %>%
                      add_column(`Economic Result`="Net Returns (avg/ac/yr)",.before = 1),
                    #Row 3: benefit of treatment
                    tree_health_aggregated_orchard_cost_yield_and_returns %>%
                      summarize(across(-c(time),~sum(.,na.rm = T)), n_years=n()) %>%
                      mutate(t1_net_returns=(t1_net_returns - nt_net_returns)/n_years,
                             t2_net_returns=(t2_net_returns - nt_net_returns)/n_years,
                             across(everything(),~dollar(.,accuracy=1))) %>%
                      select(ends_with("net_returns")) %>%
                      rename(`Max Yield`=max_net_returns,`No Treatment`=nt_net_returns,`Treatment 1`=t1_net_returns,`Treatment 2`=t2_net_returns) %>%
                      mutate(`Max Yield`="",
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
                      rename(`Max Yield`=max_net_returns,`No Treatment`=nt_net_returns,`Treatment 1`=t1_net_returns,`Treatment 2`=t2_net_returns) %>%
                      add_column(`Economic Result`="Net Present Value From Current Year ($/ac)", .before = 1),
                    #Row 5: operating duration
                    tree_health_aggregated_orchard_cost_yield_and_returns %>%
                      filter(time>start_year()+TREE_FIRST_FULL_YIELD_YEAR & time < start_year()+input$replant_year_orchard) %>%
                      # orders descending order so that the function cumsum sums starting from the end of the life of the orchard
                      arrange(desc(time)) %>%
                      mutate(across(c(`Treatment 1`, `Treatment 2`, `No Treatment`), ~cumsum(.)*input$output_price-input$annual_cost*n(), .names = "{.col}")) %>% # TODO: annualize
                      # chooses the first time the expected profit from the net returns is outweighed by the replanted yield
                      summarise(`Treatment 1`=as.character(first(time[`Treatment 1` > replanted_yield_until_max])),
                                `Treatment 2`=as.character(first(time[`Treatment 2` > replanted_yield_until_max])),
                                `No Treatment`=as.character(first(time[`No Treatment` > replanted_yield_until_max])),
                                `Max Yield`=as.character(first(time[`Max Yield` > replanted_yield_until_max]))) %>%
                      mutate(`Max Yield`="") %>%
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
