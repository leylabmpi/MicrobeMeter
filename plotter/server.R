# Shiny server
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
source("../utils/io.R")
source("../utils/format.R")


load_ex_data = function(){
  F = './data/MicrobeMeter_validation1.tsv'
  df = read.delim(F, sep='\t', skip=1, fill=TRUE, stringsAsFactors=TRUE, row.names=NULL)
  return(df)
}

turbidityCalculator = function(turbidityCTMD, time_unit=1, round_unit=3, ports=NULL) {
  # formatting 
  time_unit = as.numeric(time_unit)
  colnames(turbidityCTMD) = c('Time', 'Temperature', 'Port_1', 
                              'Port_2', 'Port_3', 'Port_4', 'X', 'File')
  
  # Getting rid of unwanted information
  turbidityCTMD = turbidityCTMD[,c(-7)]
  
  # Converting the time stamp to Unix timestamp (seconds)
  turbidityCTMD$Time = as.numeric(as.POSIXct(strptime(turbidityCTMD$Time, "%c")))
  turbidityCTMD$Time = turbidityCTMD$Time - turbidityCTMD[2,'Time']
  
  # Normalising the measurements of Port 1-3 using Port 4 for removing temperature bias
  turbidityCTMD = turbidityCTMD %>%
      mutate(Port_1 = Port_1 * (first(Port_1) / Port_4),
             Port_2 = Port_2 * (first(Port_2) / Port_4),
             Port_3 = Port_3 * (first(Port_3) / Port_4)) 
  
  # Calculating the turbidity
  ## divide each measurement using the corresponding Blank and calculate the -log
  ## then multiply with 1/1.6 for path-length correction
  turbidityCTMD = turbidityCTMD %>%
    mutate(Port_1 = -log(Port_1 / first(Port_1), base=10) * (1/1.6),
           Port_2 = -log(Port_2 / first(Port_2), base=10) * (1/1.6),
           Port_3 = -log(Port_3 / first(Port_3), base=10) * (1/1.6)) %>%
    filter(Time >= 0) %>%
    dplyr::select(-Port_4) %>%
    mutate(Time = Time / time_unit,
           Time = round(Time, round_unit),
           Port_1 = round(Port_1, round_unit),
           Port_2 = round(Port_2, round_unit),
           Port_3 = round(Port_3, round_unit))
  
  # filter to certain ports
  if (! is.null(ports)){
    to_keep = setdiff(colnames(turbidityCTMD), c('Port_1', 'Port_2', 'Port_3'))
    to_keep = c(to_keep, ports)
    turbidityCTMD = turbidityCTMD[, to_keep]
  } else {
    return(NULL)
  }
  
  # return
  return(turbidityCTMD)
}
  

turbidity_plot = function(turbidityCTMD, plot_type=c('smooth'), 
                          time_unit=1, plot_content='turbidity',
                          smooth_method='auto', smooth_span=NA){
  if(is.null(turbidityCTMD) || nrow(turbidityCTMD) < 1){
    return(NULL)
  }
  
  # x-axis label
  x_label = switch(time_unit, '1' = 'Seconds', '60' = 'Minutes', '3600' = 'Hours')
  
  # base plot object
  if(plot_content == 'turbidity'){
    p = turbidityCTMD %>%
      gather(Port, Turbidity, -Time, -Temperature, -File) %>%
      ggplot(aes(Time, Turbidity, color=Port)) +
      labs(x=x_label) +
      theme_bw() 
  } else {
    p = turbidityCTMD %>%
      dplyr::select(Time, Temperature, File) %>%
      ggplot(aes(Time, Temperature)) +
      labs(x=x_label) +
      theme_bw() 
  }
  if(length(unique(turbidityCTMD$File)) > 1){
    p = p + facet_wrap(~ File) 
  }
  
  # how to plot the data
  if('points' %in% plot_type){
    p = p + geom_point(size=0.5, alpha=0.7) 
  }
  if('smooth' %in% plot_type){
    if(is.na(smooth_span)){
      p = p + geom_smooth(size=0.75, method=smooth_method)
    } else {
      p = p + geom_smooth(size=0.75, method=smooth_method, span=smooth_span)
    }
  } 

  return(p)
}

#-- server --#
shinyServer(function(input, output, session) {
  tbl_cols = c('Time',	'Temperature', 'Port_1',	'Port_2', 
               'Port_3', 'Port_4', 'X')
  # load & process data table
  df_turbidity = reactive({
    # input file/text
    if(input$input_text == '' & is.null(input$input_file)){
      return(NULL)
    } else 
    if(! is.null(input$input_file)){
      F = apply(input$input_file, 1, rename_tmp_file)
      df = list()
      for(i in 1:nrow(input$input_file)){
        x = read.delim(F[1], sep='\t', skip=2, fill=TRUE, header=FALSE, 
                        stringsAsFactors=TRUE, col.names=tbl_cols)
        x$File = input$input_file[i,'name']
        df[[i]] = x
      }
      df = do.call(rbind, df)
    } else
    if(input$input_text != ''){
      df = read.delim(text=input$input_text, sep='\t', skip=2, fill=TRUE, header=FALSE,
                      stringsAsFactors=TRUE, col.names=tbl_cols)
      df$File = 'Pasted_text'
    }
    # calculations
    turbidityCalculator(df, time_unit=input$time_unit, round_unit=input$round_unit, ports = input$which_ports)
  })
  
  output$inputProvided = reactive({
    return(!is.null(df_turbidity()))
  })
  outputOptions(output, 'inputProvided', suspendWhenHidden=FALSE)
  
  # plotly object of turbidity
  output$turbidity_curves = renderPlotly({
    df = df_turbidity()
    if(is.null(df)){
      return(plotly_empty())
    }
    turbidity_plot(df, 
                   plot_type=input$plot_type,
                   time_unit=input$time_unit,
                   plot_content='turbidity',
                   smooth_method=input$smooth_method, 
                   smooth_span=input$smooth_span)
  })
  # plotly object of temperature
  output$temperature_curve = renderPlotly({
    df = df_turbidity()
    if(is.null(df)){
      return(plotly_empty())
    }
    turbidity_plot(df,
                   plot_type=input$plot_type,
                   time_unit=input$time_unit,
                   plot_content='temperature')
  })
  
  # dataTable object of turbidty & temperature
  output$turbidity_tbl = DT::renderDataTable(
    df_turbidity(),
    filter = 'bottom',
    extensions = c('Buttons'),
    rownames= FALSE,
    options = list(
      pageLength = 10,
      lengthMenu = c(10, 100, 500, -1),
      dom = 'Blfrtip',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    )
  )
  
  # example data
  output$ex_data = DT::renderDataTable(
    load_ex_data(),
    filter = 'bottom',
    rownames= FALSE,
    extensions = c('Buttons'),
    options = list(
      pageLength = 10,
      lengthMenu = c(10, 100, 500),
      dom = 'Blfrtip',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    )
  )
})

