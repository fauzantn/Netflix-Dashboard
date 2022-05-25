source("global.R", local = TRUE)

ui <- dashboardPage(
  title = "Financial Statements Netflix.Inc",
  
  dashboardHeader(title = "Netflix", titleWidth = 200),
  
  dashboardSidebar(
    selectInput(
      inputId = "type",
      label = "Select report:",
      choices = c("Income Statement", "--Revenue",
                  "--Cost of Revenue", "--Marketing",
                  "--Net Income",
                  "Balance Sheet",
                  "--Addition Content Assets",
                  "Cashflow Statement",
                  "--Free Cashflow", "--Cashflow"),
      selectize = FALSE)
    ),
  
  dashboardBody(
    tabsetPanel(
      id = "tabs",
      tabPanel(
        title = "Netflix: Financial Statements",
        value = "page1",
        fluidRow(
          valueBoxOutput("last_revenue"),
          valueBoxOutput("last_net_income"),
          valueBoxOutput("last_cashflow")),
        fluidRow(
          column(width = 8, plotlyOutput("plot_all")),
          column(width = 4, DT::dataTableOutput("table"),
                 style = "height:400px; overflow-y: scroll;overflow-x: scroll;"),
          column(width = 12, DT::dataTableOutput("report_all"),
                 style = "height:500px; overflow-y: scroll;overflow-x: scroll;")
          )
        )
      )
    )
  )
