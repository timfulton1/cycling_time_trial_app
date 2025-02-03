# ================================= #
# Cycling 5k Time Trial Analysis App
# Built by Tim Fulton, Feb 2025
# ================================= #

# Source helper script
source("02_scripts/utils.R")

# UI ------------------------------------------------------------------------
ui <- page_sidebar(
  theme = bs_theme(
    bootswatch = "flatly",
    navbar_bg = "#203C34",
    "btn-hover-bg-shade-amount" = "65% !default"
  ) %>% 
    bs_theme_update(
      primary = "#25443B",
      secondary = "#5C736C",
      base_font = font_google("Roboto"), 
      heading_font = font_google("Roboto")
    ),
  title = "Cycling 5k Time Trial Analysis",
  sidebar = sidebar(
    width = 300,
    title = NULL,
    fill = FALSE,
    card(
      card_header(
        "Load Data", 
        tooltip(bs_icon("info-circle", size = "1.3em"), 
                "Load your data using the browse button. Visitors can load demo data by clicking the demo button below.", 
                placement = "right"),
        style = "background-color: white;",
        class = "d-flex justify-content-between"
      ),
      style = "background-color: white;",
      fileInput(inputId = "upload", label = NULL, placeholder = "Upload Excel", multiple = FALSE, accept = ".xlsx"),
      actionButton(inputId = "load_demo", label = "Demo data", width = 232)
    ),
    card(
      card_header(
        "Bin Distance (km)", 
        tooltip(bs_icon("info-circle", size = "1.3em"), 
                "Select the bin distance. For example, a bin distance of 1 will average the power output over 1km segments.", 
                placement = "right"),
        style = "background-color: white;",
        class = "d-flex justify-content-between"
      ),
      radioButtons("bin_distance",
                   NULL,
                   c("0.01" = 0.01,
                     "0.1" = 0.1,
                     "0.25" = 0.25,
                     "0.5" = 0.5,
                     "1" = 1)
      )
    )
  ),
  layout_columns(
    col_widths = c(8, 4),
    card(
      card_header("Results Plot"),
      plotlyOutput("power_plot"),
      style = "border-color: #D8D8D8; border-radius: 6px; box-shadow: 0px 3px 3px #B9B9B9",
    ),
    card(
      card_header("Results Table"),
      tableOutput("power_table"),
      #downloadButton("export_analyzed_data", label = "Download Data"),
      uiOutput("download_ui"),
      style = "border-color: #D8D8D8; border-radius: 6px; box-shadow: 0px 3px 3px #B9B9B9"
    )
  )
)


# Server ----------------------------------------------------------------------
server <- function(input, output, session) {
  
  # Define a reactive value to store the selected data
  selected_df <- reactiveVal(NULL)
  
  # Load the uploaded demo data if button is selected
  observeEvent(input$upload, {
    
    req(input$upload)
    
    selected_df(input$upload$datapath)
    
    output$download_ui <- renderUI({
      # When action button is clicked, render the download button
      downloadButton("export_analyzed_data", label = "Download Data")
    })
  })
  
  # Load the demo data if buton is selected
  observeEvent(input$load_demo, {
    
    selected_df("01_data/test_data.xlsx")  
    
    output$download_ui <- renderUI({
      # When action button is clicked, render the download button
      downloadButton("export_analyzed_data", label = "Download Data")
    })
    
  })
  
  # Process the selected data frame
  upload_df <- reactive({
    req(selected_df())
    load_and_process_data(selected_df()) 
  })
  
  
  result_list <- reactive({
    
    bin <- switch(input$bin_distance,
                   "0.01" = 0.01,
                   "0.1" = 0.1,
                   "0.25" = 0.25,
                   "0.5" = 0.5,
                  "1" = 1)
    
    # Create a data frame with power values that are average across the user selected distance bin.
    data_binned <- upload_df() %>%
      mutate(bin = cut(distance, breaks = seq(0, 5, by = bin), include.lowest = FALSE, right = TRUE)) %>%
      group_by(bin) %>%
      summarize(Power = round(mean(power)), .groups = 'drop') %>%
      mutate(Distance = as.numeric(sub(".*,([0-9]+\\.?[0-9]*)\\]", "\\1", as.character(bin)))) %>% 
      filter(!is.na(bin))
    
    average_power <- round(mean(upload_df()$power))
    
    # Create the plot
    plot_binned <- ggplot(data_binned, aes(x = Distance, y = Power)) +

      geom_point(
        shape = 21,
        size = 4,
        color = "black",
        fill = "white",
        stroke = 0.4
      ) +
      annotate(
        "segment",
        x = 0, xend = 5, y = average_power, yend = average_power, 
        color = "#203C34", 
        linewidth = 1.2,
        linetype = "dotted"
      ) +
      annotate(
        "text",
        x = 4.3, y = average_power+15, 
        color = "#203C34", 
        label = glue("Avg Power: {average_power} W")
      ) +
      labs(
        x = "Distance (km)",
        y = "Power (W)"
      ) +
      scale_y_continuous(
        limits = c(0, max(data_binned$Power + 10)),
        breaks = pretty_breaks(n = 5)
      ) +
      scale_x_continuous(
        limits = c(0, 5),
        breaks = c(0, 1, 2, 3, 4, 5)
      ) +
      theme_bw() +
      theme(
        axis.title.x = element_text(size = 12, face = "bold", color = "black", margin = margin(t = 15)),
        axis.title.y = element_text(size = 12, face = "bold", color = "black", margin = margin(r = 15)),
        axis.text = element_text(size = 11, color = "black")
      )
    
    plotly_binned <- ggplotly(plot_binned) %>%
      layout(
        yaxis = list(
          title = list(standoff = 15)  
        ),
        xaxis = list(
          title = list(standoff = 15)  
        )
      )
    
    return(list(data_binned, plotly_binned))
    
  })
  
  output$power_plot <- renderPlotly(result_list()[[2]])
    
  
 table_data <- reactive({
   
   result_list()[[1]] %>% 
     select(Distance, Power) %>% 
     mutate(Power = as.integer(Power)) %>% 
     rename(
       "Distance (km)" = "Distance",
       "Power (W)" = "Power"
     ) %>% 
     mutate()
   
 })
 
  output$power_table <-renderTable(
    table_data(),
    hover = TRUE,
    align = c("cc"),
    digits = 2
  )
  
  # observeEvent(input$load_demo, {
  #   output$download_ui <- renderUI({
  #     # When action button is clicked, render the download button
  #     downloadButton("export_analyzed_data", label = "Download Data")
  #   })
  # })
  
  # Output Raw and Model Fit Table for Download
  output$export_analyzed_data <- downloadHandler(
    
    filename = function() {
      if (!is.null(input$upload)) {
        # For user-uploaded file
        uploaded_filename <- input$upload$name
      } else {
        # For demo data file
        uploaded_filename <- "demo.xlsx"
      }
      
      # Remove the file extension
      filename_without_extension <- sub("\\.xlsx$", "", uploaded_filename)
      
      paste0(basename(filename_without_extension), "_analyzed_data.xlsx")
    },
    
    content = function(file) {
      write.xlsx(table_data(), file)
    }
  )
}


# Run the application 
shinyApp(ui = ui, server = server)