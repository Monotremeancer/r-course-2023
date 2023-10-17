library(shiny)

source("spectrogramplotter.R", local = TRUE)

# FRONTEND ----------------------------------------------------------------

# Tab: Spectrogram viewer -------------------------------------------------

spectrogram <- tabPanel("Spectrogram viewer",
                        fluidRow(
                          column(
                            12,
                            br(),
                            fileInput(
                              inputId = "spectrogram_file_id",
                              label = "Select wav file",
                              accept = ".wav",
                              width = "100%"
                            )
                          ),
                          # Show full spectrogram.
                          column(12,
                                 plotOutput("spectrogram_plot_id")),
                          
                          # Show detail of spectrogram.
                          column(12,
                                 plotOutput("detailed_plot_id"))
                        ),
                        
                        # Slider for detailed spectrogram.
                        
                        fluidRow(column(
                          12,
                          sliderInput(
                            inputId = "slider_id",
                            label = "",
                            min = 0,
                            max = 5,
                            step = 0.02,
                            value = 2.5,
                            width = "100%"
                          )
                        )))

# Tab: Convert TE/FS ------------------------------------------------------

# convert_te_fs <- tabPanel("Convert TE/FS ",
#                           fluidRow(column(
#                             12,
#                             br(),
#                             fileInput(
#                               inputId = "convert_file_id",
#                               label = "Select wav file",
#                               accept = ".wav",
#                               width = "100%"
#                             )
#                           )),
#                           h4("...TODO...", align = "center"))

# Tab: About --------------------------------------------------------------

about <- tabPanel(
  "About",
  br(),
  h4("Shiny app for sound files recorded from bats", align = "center"),
  p(
    "This is a demo app developed in the R programming language.",
    br(),
    "It was used in an R course held for members of BatLife Sweden 2023.",
    br(),
    "The source code can be found in the GitHub repository used during the course:",
    br(),
    a(href = "https://github.com/batlife-sweden/r-course-2023",
      "https://github.com/batlife-sweden/r-course-2023"),
    align = "center"
  )
)

# Create UI. Title and tabset ---------------------------------------------

ui <-
  fluidPage(titlePanel(title = div(
    img(
      src = "Mbra-small.jpg",
      height = 40,
      width = 40
    ),
    "Tools for ultrasound"
  )),
  
  fluidPage(tabsetPanel(spectrogram,
                        # convert_te_fs,
                        about)))

# BACKEND -----------------------------------------------------------------

server <- function(input, output) {
  # Full size spectrogram.
  output$spectrogram_plot_id <- renderPlot({
    uploaded_file <- input$spectrogram_file_id
    if (is.null(uploaded_file)) {
      # Dummy plot.
      plot(
        1,
        type = "n",
        xlab = "",
        ylab = "",
        xlim = c(0, 5),
        ylim = c(0, 100)
      )
    } else {
      file_name <- uploaded_file$name
      file_path <- uploaded_file$datapath
      stft_info_full <-
        wave_to_stft(
          wave_file_path = file_path,
          window_size = 1024,
          overlap = 0.50
        )
      plot_stft(stft_info_full,
                max_freq_khz = 170)
    }
  })
  
  # Part of spectrogram, 200 ms.
  output$detailed_plot_id <- renderPlot({
    uploaded_file <- input$spectrogram_file_id
    if (is.null(uploaded_file)) {
      # Dummy plot.
      plot(
        1,
        type = "n",
        xlab = "",
        ylab = "",
        xlim = c(0, 5),
        ylim = c(0, 100)
      )
    } else {
      file_name <- uploaded_file$name
      file_path <- uploaded_file$datapath
      slider_pos_s <- input$slider_id
      # slider_pos_s = 6.0 * (slider_pos / 100.0)
      min_pos_s = slider_pos_s - 0.1
      max_pos_s = slider_pos_s + 0.1
      if (min_pos_s <= 0.0) {
        min_pos_s = 0.0
        max_pos_s = 0.2
      }
      if (max_pos_s > 6.0) {
        min_pos_s = 5.8
        max_pos_s = 6.0
      }
      stft_info_full <-
        wave_to_stft(
          wave_file_path = file_path,
          from_sec = min_pos_s,
          to_sec = max_pos_s,
          window_size = 512,
          overlap = 0.90
        )
      plot_stft(stft_info_full,
                max_freq_khz = 170)
    }
  })
}

# Run the shiny app -------------------------------------------------------

shinyApp(ui = ui, server = server)
