# ================================= #
# Nitrate and Nitrite Analysis App
# Built by Tim Fulton, Oct 2022
# Updated by Tim Fulton, Jan 2025
# ================================= #

# Source helper script
source("02_scripts/utils.R")


# UI ------------------------------------------------------------------------
ui <- page_sidebar(
  theme = bs_theme(
    bootswatch = "flatly",
    base_font = font_google("Inter"),
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
    col_widths = c(9, 3),
    card(
      card_header("Results Plot"),
      plotlyOutput("power_plot"),
      style = "border-color: #D8D8D8; border-radius: 6px; box-shadow: 0px 3px 3px #B9B9B9",
    ),
    card(
      card_header("Results Table"),
      p("three"),
      style = "border-color: #D8D8D8; border-radius: 6px; box-shadow: 0px 3px 3px #B9B9B9",
    ),
    
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
  })
  
  # Load the demo data if buton is selected
  observeEvent(input$load_demo, {
    selected_df("data/test_data.xlsx")  
  })
  
  # Process the selected data frame
  upload_df <- reactive({
    req(selected_df())
    load_and_process_data(selected_df()) 
    
    #print(load_and_process_data(selected_df()) )# Process Data
  })
  
  
  result_list <- reactive({
    
    bin <- switch(input$bin_distance,
                   "0.01" = 0.01,
                   "0.1" = 0.1,
                   "0.25" = 0.25,
                   "0.5" = 0.5,
                  "1" = 1)
    
    # Create a data frame with power values that are average across the user seleeted distance bin.
    data_binned <- upload_df() %>%
      mutate(bin = cut(distance, breaks = seq(0, 5, by = bin), include.lowest = FALSE, right = TRUE)) %>%
      group_by(bin) %>%
      summarize(Power = round(mean(power)), .groups = 'drop') %>%
      mutate(Distance = as.numeric(sub(".*,([0-9]+\\.?[0-9]*)\\]", "\\1", as.character(bin)))) %>% 
      filter(!is.na(bin))
    
    # Create the plot
    plot_binned <- ggplot(data_binned, aes(x = Distance, y = Power)) +
      geom_point(
        shape = 21,
        size = 4,
        color = "black",
        fill = "white",
        stroke = 0.4
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
    
    #plot_binned <- ggplotly(plot_binned)
    
    return(list(data_binned, plot_binned))
    
  })
  
  output$power_plot <- renderPlotly({
    
    ggplotly(result_list()[[2]]) %>%
      layout(
        yaxis = list(
          title = list(standoff = 15)  # Increase the space between the y-axis title and ticks
        ),
        xaxis = list(
          title = list(standoff = 15)  # Increase the space between the y-axis title and ticks
        )
      )
    
  })
  


  
}


# Run the application 
shinyApp(ui = ui, server = server)