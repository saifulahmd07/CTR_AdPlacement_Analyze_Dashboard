library(shiny)
library(dplyr)
library(ggplot2)
library(stats)
library(DT)
library(readxl)
library(tools)

# UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML('
      /* Mengatur warna latar belakang sidebar */
      .sidebar {
        background-color: #f8f9fa; /* Ubah ke warna yang diinginkan */
      }

      /* Mengatur warna latar belakang dashboard */
      body {
        background-color: #e9ecef; /* Ubah ke warna yang diinginkan */
        color: #333333; /* Warna teks */
      }

      /* Mengatur warna tombol */
      .btn-primary {
        background-color: #007bff; /* Ubah ke warna yang diinginkan */
        border-color: #007bff; /* Warna border tombol primer */
      }
    '))
  ),
  titlePanel("CTR Analysis Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file_input", "Choose CSV or Excel File"),
      selectInput("separator_input", "Select Separator", choices = c(",", ";"), selected = ";"),
      br(),
      actionButton("load_data", "Load Data"),
      numericInput("Ad_Placement_input", "Day (Num)", min = 1, value = 1),
      numericInput("left_sidebar_input", "Left Sidebar", value = 0),
      numericInput("center_page_input", "Center Page", value = 0),
      numericInput("right_sidebar_input", "Right Sidebar", value = 0),
      actionButton("add_data", "Add Data"),
      br(),
      actionButton("proceed_analysis", "Proceed to Analysis")
    ),
    
    mainPanel(
      DTOutput("data_table"),
      br(),
      actionButton("edit_data", "Edit Data"),
      actionButton("delete_data", "Delete Data"),
      br(),
      verbatimTextOutput("analysis_summary"),
      plotOutput("data_visualization"),
      DTOutput("input_data")
    )
  )
)

# Server logic
server <- function(input, output, session) {
  data_input <- reactiveValues(
    data = data.frame(
      Ad_Placement = character(),
      Left_Sidebar = numeric(),
      Center_Page = numeric(),
      Right_Sidebar = numeric()
    )
  )
  
  observeEvent(input$load_data, {
    if (!is.null(input$file_input)) {
      data_file <- switch(
        tolower(file_ext(input$file_input$name)),
        csv = read.csv(input$file_input$datapath, sep = input$separator_input),
        xls = read_excel(input$file_input$datapath),
        xlsx = read_excel(input$file_input$datapath)
      )
      data_input$data <- data_file
    }
  })
  
  observeEvent(input$add_data, {
    Ad_Placement_text <- paste("Day_", input$Ad_Placement_input, sep = "")
    new_row <- data.frame(
      Ad_Placement = Ad_Placement_text,
      Left_Sidebar = input$left_sidebar_input,
      Center_Page = input$center_page_input,
      Right_Sidebar = input$right_sidebar_input
    )
    data_input$data <- rbind(data_input$data, new_row)
  })
  
  output$data_table <- renderDT({
    datatable(
      data_input$data,
      selection = 'single',
      editable = TRUE,
      extensions = 'Buttons',
      options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      )
    )
  })
  
  observeEvent(input$delete_data, {
    to_delete <- as.integer(input$data_table_rows_selected)
    data_input$data <- data_input$data[-to_delete, ]
  })
  
  observeEvent(input$edit_data, {
    edited_data <- input$data_table_cell_edit
    if (!is.null(edited_data)) {
      data_input$data[edited_data$row, edited_data$col] <- edited_data$value
    }
  })
  
  observeEvent(input$proceed_analysis, {
    CTR <- c(data_input$data$Left_Sidebar, data_input$data$Center_Page, data_input$data$Right_Sidebar)
    Ad_Place <- gl(3, nrow(data_input$data), labels = c("Left Sidebar", "Center Page", "Right Sidebar"))
    new_data_B <- data.frame(CTR, Ad_Place)
    
    CTR.aov <- aov(CTR ~ Ad_Place, data = new_data_B)
    summary_result <- summary(CTR.aov)
    
    output$analysis_summary <- renderPrint({
      print(summary_result)
    })
    
    CTR_summary <- new_data_B %>%
      group_by(Ad_Place) %>%
      summarise(
        Mean = round(mean(CTR), digits = 3),
        Median = round(median(CTR), digits = 3),
        Q1 = round(quantile(CTR, 0.25), digits = 3),
        Q3 = round(quantile(CTR, 0.75), digits = 3),
        Min = round(min(CTR), digits = 3),
        Max = round(max(CTR), digits = 3)
      )
    
    output$input_data <- DT::renderDataTable({
      datatable(
        CTR_summary,
        options = list(
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
        )
      )
    })
    
    output$data_visualization <- renderPlot({
      p <- ggplot(new_data_B, aes(x = Ad_Place, y = CTR, fill = Ad_Place)) +
        geom_boxplot(alpha = 0.6) +  # Mengatur transparansi boxplot
        labs(x = "Ad Placement", y = "CTR", title = "CTR Performance by Ad Placement") +
        theme_minimal() +
        theme(legend.position="none")  # Menyembunyikan legend untuk ad placement
      
      mean_values <- CTR_summary
      
      p + geom_point(data = mean_values, aes(y = Mean), color = "red", size = 3, shape = 18) +
        scale_fill_manual(values = c("blue", "green", "orange"))  # Menentukan warna untuk Ad Placement
    })
  })
}

# Run the app
shinyApp(ui, server)
