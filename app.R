library(tidyverse)
library(shiny)
library(bslib)
library(scales)
library(DT)
#load data
nba = read_csv("data/nba_teams_df.csv")

#colnames
col_names = get_col_names(nba)
to_remove = c("Year", "Team", "G")
col_names = setdiff(col_names, to_remove)


ui = navbarPage(

  theme = bs_theme(bootswatch = "minty"),
  
  title = "NBA Team Stats",
  
  tabPanel(
    title = "Trendline",
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "team", 
                    label = "Team: ", 
                    choices = sort(unique(nba$Team)),
                    selected = "Atlanta Hawks"),
        sliderInput(inputId = "start", 
                    label = "Start Year: ", 
                    min = 1980,
                    max = 2019,
                    value = 1980,
                    sep = ""),
        sliderInput(inputId = "end", 
                    label = "End Year: ", 
                    min = 1980,
                    max = 2019,
                    value = 1980,
                    sep = ""),
        selectInput(inputId = "stat",
                    label = "Statistic: ",
                    choices = col_names,
                    selected = "Rk")
      ),

      mainPanel(
        plotOutput("distPlot")
      )
    )
  ),
  tabPanel(
    title = "Averages", 
    dataTableOutput("table")
  ),
  tabPanel(
    title = "About",
    includeMarkdown("about.Rmd")
    
  )
)


server = function(input, output) {
  nba_filter = reactive({
    nba %>%
      filter(Team == input$team)
  })
  
  observeEvent(eventExpr = input$team, 
               handlerExpr = {
                 updateSliderInput(inputId = "start", 
                                   min = min(nba_filter()$Year), 
                                   max = max(nba_filter()$Year),
                                   value = min(nba_filter()$Year),
                                   step = 1)
               })
  
  observeEvent(eventExpr = input$team, 
               handlerExpr = {
                 updateSliderInput(inputId = "end", 
                                   min = min(nba_filter()$Year), 
                                   max = max(nba_filter()$Year),
                                   value = min(nba_filter()$Year),
                                   step = 1)
               })
  
  update_nba = reactive({
    nba_filter() %>%
      filter(Year >= input$start) %>%
      filter(Year <= input$end)
  })
  
  output$distPlot = renderPlot({
      if (input$stat == "Rk") {
        ggplot(data = update_nba(), aes(x = Year , y = !!rlang::sym(input$stat))) +
          geom_line() + scale_x_continuous(breaks = scales::pretty_breaks())+
          theme_classic() + scale_y_reverse()
      } else {
        ggplot(data = update_nba(), aes(x = Year , y = !!rlang::sym(input$stat))) +
          geom_line() + scale_x_continuous(breaks = scales::pretty_breaks())+
          theme_classic()
      }
  })
  
  yearly_nba = reactive({
    nba %>%
      select(-G) %>%
      filter(Year >= input$start) %>%
      filter(Year <= input$end)
  })
  
  output$table = renderDataTable({
    yearly_nba() %>%
      group_by(Team) %>%
      summarise_all(mean) %>% 
      mutate(across(where(is.numeric), round, 3)) %>%
      select(-Year) %>%
      datatable(caption = paste("Showing data from ", as.character(input$start), " to ", as.character(input$end)))
  })
}


shinyApp(ui = ui, server = server)
