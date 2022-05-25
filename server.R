server <- function(input, output, session) {
  
  
  # Sheet Income Statement...........................................
  
  financial_15 <- reactive({
    withProgress(message = "Preparing dataset", value = 0, {
      financial_15 <- read_xlsx("./financials_15.xlsx", sheet = "Income Statement") %>% 
        t() %>% as_tibble()
    })
  })
  
  financial_18 <- reactive({
    withProgress(message = "Preparing dataset", value = 0, {
      financial_18 <- read_xlsx("./financials_18.xlsx", sheet = "Income Statement") %>% 
        t() %>% as_tibble()
    })
  })
  
  financial_21 <- reactive({
    withProgress(message = "Preparing dataset", value = 0, {
      financial_21 <- read_xlsx("./financials_21.xlsx", sheet = "Income Statement") %>% 
        t() %>% as_tibble()
    })
  })
  
  
  revenue <- reactive({
    revenue_15 <- financial_15() %>% select(V4, V6, V8) %>%
      slice(6:20) %>% filter(V4 == "Twelve Months Ended") %>% select(-V4)
    
    revenue_18 <- financial_18() %>% select(V4, V6, V8) %>%
      slice(6:20) %>% filter(V4 == "Twelve Months Ended") %>% select(-V4)
    revenue_18$V6 <- str_replace(revenue_18$V6, "\\.0", "")
    
    revenue_21 <- financial_21() %>% select(V4, V6, V8) %>%
      slice(6:20) %>% filter(V4 == "Twelve Months Ended") %>% select(-V4)
    
    revenue <- rbind(revenue_15, revenue_18, revenue_21)  %>%
      rename("year" = V6, "revenue" = V8)
    revenue$revenue <- as.numeric(revenue$revenue)
    revenue$rev_growth <- round(((revenue$revenue-lag(revenue$revenue)) / lag(revenue$revenue)) * 100, 2)
    revenue
  })
  
  
  cost_of_revenue <- reactive({
    cost_revenue_15 <- financial_15() %>% select(V4, V6, V9) %>%
      slice(6:20) %>% filter(V4 == "Twelve Months Ended") %>% select(-V4)
    cost_revenue_15$V9 <- str_replace(cost_revenue_15$V9, "\\.[0-9]+", "")
    
    cost_revenue_18 <- financial_18() %>% select(V4, V6, V9) %>%
      slice(6:20) %>% filter(V4 == "Twelve Months Ended") %>% select(-V4)
    cost_revenue_18$V6 <- str_replace(cost_revenue_18$V6, "\\.0", "")
    
    cost_revenue_21 <- financial_21() %>% select(V4, V6, V9) %>%
      slice(6:20) %>% filter(V4 == "Twelve Months Ended") %>% select(-V4)
    
    cost_of_revenue <- rbind(cost_revenue_15, cost_revenue_18, cost_revenue_21) %>%
      rename("year" = V6, "cost_of_revenue" = V9)
  })


  marketing <- reactive({
    marketing_15 <- financial_15() %>% select(V4, V6, V10) %>%
      slice(6:20) %>% filter(V4 == "Twelve Months Ended") %>% select(-V4)
    
    marketing_18 <- financial_18() %>% select(V4, V6, V10) %>%
      slice(6:20) %>% filter(V4 == "Twelve Months Ended") %>% select(-V4)
    marketing_18$V6 <- str_replace(marketing_18$V6, "\\.0", "")
    
    marketing_21 <- financial_21() %>% select(V4, V6, V10) %>%
      slice(6:20) %>% filter(V4 == "Twelve Months Ended") %>% select(-V4)
    
    marketing <- rbind(marketing_15, marketing_18, marketing_21) %>%
      rename("year" = V6, "marketing" = V10)
  })
  

  net_income <- reactive({
    income_15 <- financial_15() %>% select(V4, V6, V19) %>%
      slice(6:20) %>% filter(V4 == "Twelve Months Ended") %>% select(-V4)
    
    income_18 <- financial_18() %>% select(V4, V6, V19) %>%
      slice(6:20) %>% filter(V4 == "Twelve Months Ended") %>% select(-V4)
    income_18$V6 <- str_replace(income_18$V6, "\\.0", "")
    
    income_21 <- financial_21() %>% select(V4, V6, V19) %>%
      slice(6:20) %>% filter(V4 == "Twelve Months Ended") %>% select(-V4)
    
    net_income <- rbind(income_15, income_18, income_21) %>%
      rename("year" = V6, "net_income" = V19)
    
    net_income$net_income <- as.numeric(net_income$net_income)
    net_income$net_growth <- round(((net_income$net_income-lag(net_income$net_income)) / lag(net_income$net_income)) * 100, 2)
    net_income
  })
  
  
  income_statement <- reactive({
    income_statement <- inner_join(revenue(), net_income(), by = "year")
  })
  
  income_statement_2 <- reactive({
    income_statement_2 <- income_statement() %>%
      select(year, revenue, net_income) %>% 
      pivot_longer(
        c("revenue", "net_income"),
        names_to = "type", values_to = "value")
  })
  
  income_statement_3 <- reactive({
    income_statement_2 <- income_statement() %>%
      select(year, rev_growth, net_growth) %>% 
      pivot_longer(
        c("rev_growth", "net_growth"),
        names_to = "type", values_to = "value")
  })
  
  
  
  
  
  
  # Balance Sheet.....................................................
  
  balance_15 <- reactive({
    withProgress(message = "Preparing dataset", value = 0, {
      balance_15 <- read_xlsx("./financials_15.xlsx", sheet = "Balance Sheet") %>% 
        t() %>% as_tibble()
    })
  })
  
  balance_18 <- reactive({
    withProgress(message = "Preparing dataset", value = 0, {
      balance_18 <- read_xlsx("./financials_18.xlsx", sheet = "Balance Sheet") %>% 
        t() %>% as_tibble()
    })
  })
  
  balance_21 <- reactive({
    withProgress(message = "Preparing dataset", value = 0, {
      balance_21 <- read_xlsx("./financials_21.xlsx", sheet = "Balance Sheet") %>% 
        t() %>% as_tibble()
    })
  })
  
  asset <- reactive({
    asset_15 <- balance_15() %>% select(V5, V6, V35) %>%
      slice(8:22) %>% filter(V5 == "December 31,") %>% select(-V5) %>%
      rename("year" = V6, "asset" = V35)
    
    asset_18 <- balance_18() %>% select(V5, V6, V34) %>%
      slice(8:22) %>% filter(V5 == "December 31,") %>% select(-V5) %>%
      rename("year" = V6, "asset" = V34)
    asset_18$year <- str_replace(asset_18$year, "\\.0", "")
    
    asset_21 <- balance_21() %>% select(V5, V6, V34) %>%
      slice(8:22) %>% filter(V5 == "December 31,") %>% select(-V5) %>%
      rename("year" = V6, "asset" = V34)
    
    asset <- rbind(asset_15, asset_18, asset_21)
  })
  
  liabilities <- reactive({
    lb_15 <- balance_15() %>% select(V5, V6, V29) %>%
      slice(8:22) %>% filter(V5 == "December 31,") %>% select(-V5) %>%
      rename("year" = V6, "liabilities" = V29)
    
    lb_18 <- balance_18() %>% select(V5, V6, V28) %>%
      slice(8:22) %>% filter(V5 == "December 31,") %>% select(-V5) %>%
      rename("year" = V6, "liabilities" = V28)
    lb_18$year <- str_replace(lb_18$year, "\\.0", "")
    
    lb_21 <- balance_21() %>% select(V5, V6, V27) %>%
      slice(8:22) %>% filter(V5 == "December 31,") %>% select(-V5) %>%
      rename("year" = V6, "liabilities" = V27)
    
    liabilities <- rbind(lb_15, lb_18, lb_21)
  })
  
  balance_sheet <- reactive({
    balance_sheet <- inner_join(liabilities(), asset(), by = "year")
    balance_sheet$equity <- as.numeric(balance_sheet$asset) - as.numeric(balance_sheet$liabilities)
    balance_sheet$liabilities <- as.numeric(balance_sheet$liabilities)
    balance_sheet %>% select(-asset)
  })
  
  balance_sheet_2 <- reactive({
    balance_sheet_2 <- pivot_longer(
      balance_sheet(),
      c("liabilities", "equity"),
      names_to = "type", values_to = "value")
  })
  
  
  



  # Sheet Cashflow..........................................

  cashflow_15 <- reactive({
    withProgress(message = "Preparing dataset", value = 0, {
      cashflow_15 <- read_xlsx("./financials_15.xlsx", sheet = "Cashflow") %>% 
        t() %>% as_tibble()
    })
  })
  
  cashflow_18 <- reactive({
    withProgress(message = "Preparing dataset", value = 0, {
      cashflow_18 <- read_xlsx("./financials_18.xlsx", sheet = "Cashflow") %>% 
        t() %>% as_tibble()
    })
  })
  
  cashflow_21 <- reactive({
    withProgress(message = "Preparing dataset", value = 0, {
      cashflow_21 <- read_xlsx("./financials_21.xlsx", sheet = "Cashflow") %>% 
        t() %>% as_tibble()
    })
  })
  
  
  addition_content_assets <- reactive({
    content_assets_15 <- cashflow_15() %>% select(V4, V6, V11) %>%
      slice(8:22) %>% filter(V4 == "Twelve Months Ended") %>% select(-V4)
    
    content_assets_18 <- cashflow_18() %>% select(V4, V6, V11) %>%
      slice(8:22) %>% filter(V4 == "Twelve Months Ended") %>% select(-V4)
    content_assets_18$V6 <- str_replace(content_assets_18$V6, "\\.0", "")
    
    content_assets_21 <- cashflow_21() %>% select(V4, V6, V11) %>%
      slice(8:22) %>% filter(V4 == "Twelve Months Ended") %>% select(-V4)
    
    addition_content_assets <- rbind(content_assets_15, content_assets_18, content_assets_21) %>% rename("year" = V6, "addition_content_assets" = V11)
    addition_content_assets$addition_content_assets <- str_replace(addition_content_assets$addition_content_assets, "\\-", "")
    addition_content_assets
  })
  

  free_cashflow <- reactive({
    cf_15 <- cashflow_15() %>% select(V4, V6, V55) %>%
      slice(8:22) %>% filter(V4 == "Twelve Months Ended") %>% select(-V4) %>%
      rename("year" = V6, "free_cashflow" = V55)
    cf_15$free_cashflow <- str_replace(cf_15$free_cashflow, "\\.[0-9]+", "")

    cf_18 <- cashflow_18() %>% select(V4, V6, V54) %>%
      slice(8:22) %>% filter(V4 == "Twelve Months Ended") %>% select(-V4) %>%
      rename("year" = V6, "free_cashflow" = V54)
    cf_18$year <- str_replace(cf_18$year, "\\.0", "")

    cf_21 <- cashflow_21() %>% select(V4, V6, V49) %>%
      slice(8:22) %>% filter(V4 == "Twelve Months Ended") %>% select(-V4) %>%
      rename("year" = V6, "free_cashflow" = V49)

    free_cashflow <- rbind(cf_15, cf_18, cf_21)
  })
  
  cashflow <- reactive({
    cf_15 <- cashflow_15() %>% select(V4, V6, V46) %>%
      slice(8:22) %>% filter(V4 == "Twelve Months Ended") %>% select(-V4) %>%
      rename("year" = V6, "cashflow" = V46)
    cf_15$cashflow <- str_replace(cf_15$cashflow, "\\.[0-9]+", "")
    
    cf_18 <- cashflow_18() %>% select(V4, V6, V45) %>%
      slice(8:22) %>% filter(V4 == "Twelve Months Ended") %>% select(-V4) %>%
      rename("year" = V6, "cashflow" = V45)
    cf_18$year <- str_replace(cf_18$year, "\\.0", "")
    
    cf_21 <- cashflow_21() %>% select(V4, V6, V41) %>%
      slice(8:22) %>% filter(V4 == "Twelve Months Ended") %>% select(-V4) %>%
      rename("year" = V6, "cashflow" = V41)
    
    cashflow <- rbind(cf_15, cf_18, cf_21)
  })
  
  
  cash_from_operating_activities <- reactive({
    cash_operating_15 <- cashflow_15() %>% select(V4, V6, V27) %>%
      slice(8:22) %>% filter(V4 == "Twelve Months Ended") %>% select(-V4) %>%
      rename("year" = V6, "cash_from_operating_activities" = V27)
    cash_operating_15$cash_from_operating_activities <- str_replace(cash_operating_15$cash_from_operating_activities, "\\.[0-9]+", "")
    
    cash_operating_18 <- cashflow_18() %>% select(V4, V6, V27) %>%
      slice(8:22) %>% filter(V4 == "Twelve Months Ended") %>% select(-V4) %>%
      rename("year" = V6, "cash_from_operating_activities" = V27)
    cash_operating_18$year <- str_replace(cash_operating_18$year, "\\.0", "")
    
    cash_operating_21 <- cashflow_21() %>% select(V4, V6, V25) %>%
      slice(8:22) %>% filter(V4 == "Twelve Months Ended") %>% select(-V4) %>%
      rename("year" = V6, "cash_from_operating_activities" = V25)
    
    cash_from_operating_activities <- rbind(cash_operating_15, cash_operating_18, cash_operating_21)
  })
  
  cash_from_investing_activities <- reactive({
    cash_investing_15 <- cashflow_15() %>% select(V4, V6, V35) %>%
      slice(8:22) %>% filter(V4 == "Twelve Months Ended") %>% select(-V4) %>%
      rename("year" = V6, "cash_from_investing_activities" = V35)
    
    cash_investing_18 <- cashflow_18() %>% select(V4, V6, V35) %>%
      slice(8:22) %>% filter(V4 == "Twelve Months Ended") %>% select(-V4) %>%
      rename("year" = V6, "cash_from_investing_activities" = V35)
    cash_investing_18$year <- str_replace(cash_investing_18$year, "\\.0", "")
    
    cash_investing_21 <- cashflow_21() %>% select(V4, V6, V30) %>%
      slice(8:22) %>% filter(V4 == "Twelve Months Ended") %>% select(-V4) %>%
      rename("year" = V6, "cash_from_investing_activities" = V30)
    
    cash_from_investing_activities <- rbind(cash_investing_15, cash_investing_18, cash_investing_21)
  })
  
  cash_from_financing_activities <- reactive({
    cash_financing_15 <- cashflow_15() %>% select(V4, V6, V43) %>%
      slice(8:22) %>% filter(V4 == "Twelve Months Ended") %>% select(-V4) %>%
      rename("year" = V6, "cash_from_financing_activities" = V43)
    
    cash_financing_18 <- cashflow_18() %>% select(V4, V6, V42) %>%
      slice(8:22) %>% filter(V4 == "Twelve Months Ended") %>% select(-V4) %>%
      rename("year" = V6, "cash_from_financing_activities" = V42)
    cash_financing_18$year <- str_replace(cash_financing_18$year, "\\.0", "")
    
    cash_financing_21 <- cashflow_21() %>% select(V4, V6, V38) %>%
      slice(8:22) %>% filter(V4 == "Twelve Months Ended") %>% select(-V4) %>%
      rename("year" = V6, "cash_from_financing_activities" = V38)
    
    cash_from_financing_activities <- rbind(cash_financing_15, cash_financing_18, cash_financing_21)
  })
  
  cashflow_statement <- reactive({
    cashflow_statement <- inner_join(cash_from_operating_activities(), cash_from_investing_activities(), by = "year") %>% 
      inner_join(., cash_from_financing_activities(), by = "year")
  })
  
  cashflow_statement_2 <- reactive({
    cashflow_statement_2 <- pivot_longer(
      cashflow_statement(),
      c("cash_from_operating_activities", "cash_from_investing_activities", "cash_from_financing_activities"),
      names_to = "type", values_to = "value")
  })
  
  
  
  report_all <- reactive({
    report_all <- inner_join(income_statement(), balance_sheet(), by = "year") %>% 
      inner_join(., free_cashflow(), by = "year") %>% 
      inner_join(., cashflow_statement(), by = "year")
  })
  
  
  
  
  
  # Render........................................................
  
  output$last_revenue <- renderValueBox({
    x <- last(revenue()$revenue/1000000) %>% 
      signif(3) %>%
      paste("$", ., " bn", sep = "") %>% 
      valueBox(icon = icon("chart-bar"),
               color = "purple",
               subtitle = paste("Revenue", max(revenue()$year)))
  })
  
  output$last_net_income <- renderValueBox({
    x <- last(net_income()$net_income/1000000) %>% 
      signif(3) %>%
      paste("$", ., " bn", sep = "") %>% 
      valueBox(icon = icon("chart-bar"),
               color = "purple",
               subtitle = paste("Net Income", max(net_income()$year)))
  })
  
  output$last_cashflow <- renderValueBox({
    x <- last(free_cashflow()$free_cashflow) %>% 
      as.integer() %>% `/`(-1000) %>% signif(4) %>% 
      paste("-$", ., " mn", sep = "") %>% 
      valueBox(icon = icon("chart-bar"),
               color = "purple",
               subtitle = paste("Free Cashflow", max(free_cashflow()$year)))
  })
  
  
 
  


  # Select input...........................
  
  observe({if (input$type == "--Revenue") {
    output$plot_all <- renderPlotly({
      ay <- list(
        overlaying = "y",
        side = "right"
      )
      revenue() %>%
        plot_ly() %>% 
      add_bars(
          x = ~year,
          y = ~as.numeric(revenue)*1000,
          mode = "bar",
          texttemplate = '%{y:.3s}',
          hovertemplate = paste('Revenue:<br>%{x}', '<br>%{y}<br><extra></extra>'),
          textposition = "outside",
          name = "Revenue"
      ) %>% 
      add_lines(
        x = ~year,
        y = ~rev_growth,
        name = "Revenue Growth",
        yaxis = "y2",
        text = ~rev_growth
        ) %>% 
      add_text(
        x = ~year,
        y = ~rev_growth,
        yaxis = "y2",
        text = ~paste(rev_growth, "%", sep = ""),
        showlegend = FALSE,
        hovertemplate = paste('Growth:<br>%{x}', '<br>%{y}<br><extra></extra>')
      ) %>%
        layout(
          title = "Revenue",
          xaxis = list(title = FALSE),
          yaxis = list(title = "Value"),
          yaxis2 = ay,
          legend = list(x = 0, y = 1))
    })
    
    output$table <- renderDataTable({
      revenue()
    })
    
  } else

    if (input$type == "--Cost of Revenue") {
    output$plot_all <- renderPlotly({
      cost_of_revenue() %>%
        plot_ly(
          x = ~year,
          y = ~as.numeric(cost_of_revenue)*1000,
          mode = "bar",
          texttemplate = '%{y:.3s}',
          hovertemplate = paste('Cost of revenue:<br>%{x}', '<br>%{y}<br><extra></extra>'),
          textposition = "outside"
        ) %>%
        layout(
          title = "Cost of Revenue",
          xaxis = list(title = FALSE),
          yaxis = list(title = "Value"))
    })
    
    output$table <- renderDataTable({
      cost_of_revenue()
      })

  } else

    if (input$type == "--Marketing") {
      output$plot_all <- renderPlotly({
        marketing() %>%
          plot_ly(
            x = ~year,
            y = ~as.numeric(marketing)*1000,
            mode = "bar",
            texttemplate = '%{y:.3s}',
            hovertemplate = paste('Marketing: %{x}', '<br>%{y}<br><extra></extra>'),
            textposition = "outside"
          ) %>%
          layout(
            title = "Marketing",
            xaxis = list(title = FALSE),
            yaxis = list(title = "Value"))
      })
      
      output$table <- renderDataTable({
        marketing()
        })

  } else
    
    if (input$type == "--Net Income") {
      output$plot_all <- renderPlotly({
        net_income() %>%
          plot_ly(
            x = ~year,
            y = ~as.numeric(net_income)*1000,
            mode = "bar",
            texttemplate = '%{y:.3s}',
            hovertemplate = paste('Net income: %{x}', '<br>%{y}<br><extra></extra>'),
            textposition = "outside"
          ) %>%
          layout(
            title = "Net Income",
            xaxis = list(title = FALSE),
            yaxis = list(title = "Value"))
      })
      
      output$table <- renderDataTable({
        net_income()
      })
      
  } else
    
    if (input$type == "Income Statement") {
      output$plot_all <- renderPlotly({
        ay <- list(
          overlaying = "y",
          side = "right"
        )
        income_statement() %>%
          plot_ly() %>% 
          add_bars(
            x = ~year,
            y = ~as.numeric(revenue)*1000,
            mode = "bar",
            texttemplate = '%{y:.3s}',
            hovertemplate = paste('Revenue:<br>%{x}', '<br>%{y}<br><extra></extra>'),
            textposition = "outside",
            name = "Revenue"
          ) %>% 
          add_bars(
            x = ~year,
            y = ~as.numeric(net_income)*1000,
            mode = "bar",
            texttemplate = '%{y:.3s}',
            hovertemplate = paste('Net Income:<br>%{x}', '<br>%{y}<br><extra></extra>'),
            textposition = "outside",
            name = "Net Income"
          ) %>% 
          add_lines(
            x = ~year,
            y = ~rev_growth,
            name = "Revenue Growth",
            yaxis = "y2",
            text = ~rev_growth
          ) %>% 
          add_text(
            x = ~year,
            y = ~rev_growth,
            yaxis = "y2",
            text = ~paste(rev_growth, "%", sep = ""),
            showlegend = FALSE,
            hovertemplate = paste('Growth:<br>%{x}', '<br>%{y}<br><extra></extra>')
          ) %>%
          layout(
            title = "Income Statements",
            xaxis = list(title = FALSE),
            yaxis = list(title = "Value", showgrid = F),
            yaxis2 = ay,
            legend = list(x = 0, y = 1))
      })
      
      output$table <- renderDataTable({
        income_statement()
      })
      
  } else
    
    if (input$type == "Balance Sheet") {
      output$plot_all <- renderPlotly({
        balance_sheet_2() %>%
          plot_ly(
            x = ~year,
            y = ~as.numeric(value)*1000,
            color = ~type,
            type = "bar",
            hovertemplate = paste('%{x}', '<br>%{y}<br><extra></extra>'),
            textposition = "outside"
          ) %>%
          layout(
            title = "Balance Sheet",
            xaxis = list(title = FALSE),
            yaxis = list(title = "Value"),
            barmode = "stack",
            legend = list(x = 0, y = 1))
      })
      
      
      output$table <- renderDataTable({
        balance_sheet()
      })
      
  } else

    if (input$type == "--Addition Content Assets") {
      output$plot_all <- renderPlotly({
        addition_content_assets() %>%
          plot_ly(
            x = ~year,
            y = ~as.numeric(addition_content_assets)*1000,
            mode = "bar",
            texttemplate = '%{y:.3s}',
            hovertemplate = paste('Addition content assets:<br>%{x}', '<br>%{y}<br><extra></extra>'),
            textposition = "outside"
          ) %>%
          layout(
            title = "Addition Content Assets",
            xaxis = list(title = FALSE),
            yaxis = list(title = "Value"))
      })
      
      output$table <- renderDataTable({
        addition_content_assets()
        })

  } else
    
    if (input$type == "--Free Cashflow") {
      output$plot_all <- renderPlotly({
        free_cashflow() %>%
          plot_ly(
            x = ~year,
            y = ~as.numeric(free_cashflow)*1000,
            mode = "bar",
            texttemplate = '%{y:.3s}',
            hovertemplate = paste('Free Cashflow: %{x}', '<br>%{y}<br><extra></extra>'),
            textposition = "outside"
          ) %>%
          layout(
            title = "Free Cashflow",
            xaxis = list(title = FALSE),
            yaxis = list(title = "Value"))
      })
      
      output$table <- renderDataTable({
        free_cashflow()
      })
      
  } else

    if (input$type == "--Cashflow") {
      output$plot_all <- renderPlotly({
        cashflow() %>%
          plot_ly(
            x = ~year,
            y = ~as.numeric(cashflow)*1000,
            mode = "bar",
            texttemplate = '%{y:.3s}',
            hovertemplate = paste('Cashflow: %{x}', '<br>%{y}<br><extra></extra>'),
            textposition = "outside"
          ) %>%
          layout(
            title = "Cashflow",
            xaxis = list(title = FALSE),
            yaxis = list(title = "Value"))
      })
      
      output$table <- renderDataTable({
        cashflow()
        })

  } else

    if (input$type == "Cashflow Statement") {
      output$plot_all <- renderPlotly({
        cashflow_statement_2() %>%
          plot_ly(
            x = ~year,
            y = ~as.numeric(value)*1000,
            color = ~type,
            mode = "bar",
            texttemplate = '%{y:.3s}',
            hovertemplate = paste('%{x}', '<br>%{y}<br><extra></extra>'),
            textposition = "outside"
          ) %>%
          layout(
            title = "Cashflow Statement",
            xaxis = list(title = FALSE),
            yaxis = list(title = "Value"),
            legend = list(x = 0, y = 1))
      })
      
      output$table <- renderDataTable({
        cashflow_statement()
        })
      }
    })
  
  output$report_all <- renderDataTable({
    report_all()
  })
  
  
  
  
}