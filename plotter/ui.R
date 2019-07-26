# Shiny UI
library(shiny)
library(plotly)


#-- UI --#
shinyUI(fluidPage(
  pageWithSidebar(
    titlePanel("MicrobeMeter"),
    sidebarPanel(width = 3, 
                 selectInput('time_unit',
                             label = 'Time unit for the x-axis',
                             choices = c('Hours' = 3600,
                                         'Minutes' = 60,
                                         'Seconds' = 1),
                             selected = 60),
                 checkboxGroupInput('plot_type',
                                    label = 'What to plot',
                                    choices = c('smooth' = 'smooth',
                                                'points' = 'points'),
                                    selected = 'smooth'),
                 numericInput('round_unit', 
                              label='Rounding units', 
                              value=3, min=0, max=10)
    ),
    mainPanel(
      h4("Convert table between labware plate format and a long table format"),
      tabsetPanel(type = 'tabs', 
                  tabPanel("Input",
                           fluidRow(
                             column(6,
                                    fileInput('input_file', 
                                     'Upload your MicrobeMeter output table (tab-delimited)')
                             ),
                             column(2),
                             column(4,
                                    checkboxInput('paste_input', 'Paste input table instead of load a file?')
                             )
                           ),
                           conditionalPanel(
                             condition = "input.paste_input == true",
                             textAreaInput('input_text',
                                           'Paste your MicrobeMeter output table into this box (tab-delimited)',
                                           width = '1000px',
                                           height = '400px')
                           ),
                           hr(),
                           conditionalPanel("output.inputProvided",
                            h4('Data table'),
                            DT::dataTableOutput('turbidity_tbl')
                           )
                           ),
                  tabPanel('Turbidity plot', 
                           plotlyOutput('turbidity_curves')),
                  tabPanel('Temperature plot', 
                           plotlyOutput('temperature_curve')),
                  tabPanel('Example input', 
                           DT::dataTableOutput('ex_data'))
      )
    )
  )
))

