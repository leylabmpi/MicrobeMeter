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
                                    selected = 'points'),
                 checkboxGroupInput('which_ports',
                                    label = 'Which ports to plot?',
                                    choices = c('Port_1' = 'Port_1',
                                                'Port_2' = 'Port_2',
                                                'Port_3' = 'Port_3'),
                                    selected = c('Port_1', 'Port_2', 'Port_3')),
                 numericInput('round_unit', 
                              label='Rounding units', 
                              value=3, min=0, max=10),
                 hr(),
                 textInput('smooth_method', label = 'geom_smooth() method', value = 'auto'),
                 numericInput('smooth_span', label = 'geom_smooth() span', value = NA, min=0)
    ),
    mainPanel(
      h4("Convert table between labware plate format and a long table format"),
      tabsetPanel(type = 'tabs', 
                  tabPanel("Input",
                           fluidRow(
                             column(4,
                                    fileInput('input_file', 
                                              'Upload >=1 MicrobeMeter output tables (tab-delimited)',
                                              multiple = TRUE)
                             ),
                             column(4,
                                    fileInput('sample_names', 
                                              'Sample names file (see example input). Note: the file name must match the MicrobeMeter output file name!',
                                              multiple = TRUE)
                             ),
                             column(4,
                                    checkboxInput('paste_input', 'Paste input table instead of load >=1 file?')
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
                  tabPanel('Example data', 
                           DT::dataTableOutput('ex_data')),
                  tabPanel('Example names', 
                           DT::dataTableOutput('ex_names'))
      )
    )
  )
))

