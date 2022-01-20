library(shiny)
library(tidyverse)
library(ggrepel)

# Data Preparation -------------------------------------
covid19 <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")

covid_states <-
  covid19 %>%
  group_by(state) %>%
  mutate(cum_cases = cumsum(cases)) %>%
  filter(cum_cases>= 20) %>%
  mutate(days_since_20 = date - min(date))

states_options <- distinct(covid19, state)$state

max_limit <- as.integer(max(covid_states$days_since_20))


# UI----------------------------------------
ui <- fluidPage(
  selectInput("states",
              "Please choose states to compare",
              choices = states_options,
              multiple = TRUE),
  submitButton(text = "Plot"),
  plotOutput(outputId = "timeplot")
)

# SERVER -----------------------------------------------------
server <- function(input, output) {
  output$timeplot <- renderPlot({

    # Chosen Variables -----------------------
    chosen_states <- filter(covid_states, state %in% input$states)

    state_label <-
      chosen_states %>%
      group_by(state) %>%
      filter(days_since_20 == max(days_since_20))

    # Plot -----------------------------------
    ggplot(data = chosen_states ,
           aes(x = days_since_20,
               y = cum_cases,
               color = state))+
      theme(legend.position = "none")+
      geom_text_repel(data = state_label,
                      aes(label = state),
                      size = 3,
                      max.overlaps = 50,
                      direction = "both",
                      nudge_x = 4,
                      min.segment.length = 3,
                      force=3,
                      xlim = c(max_limit, max_limit + 100))+
      scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                    labels = scales::trans_format("log10", scales::math_format(10^.x)))+
      scale_x_continuous(limits = c(0, max_limit+ 100))+
      labs(x = "Number of Days after 20 Cummulative Cases", y = "Cummulative Cases")+
      geom_line()
  })
}

shinyApp(ui = ui, server = server)
