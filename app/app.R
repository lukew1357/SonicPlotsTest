options(repos = c(CRAN = "https://cloud.r-project.org"))
library(shiny)
library(lubridate)
library(ggplot2)
library(dplyr)
library(tuneR)
library(forecast)
library(fluidsynth)
if (!requireNamespace("future", quietly = TRUE)) {
  install.packages("future")
}
library(future)

#remotes::install_github("moodymudskipper/midi")
library(midi)

plan(multisession)

# Function to remove initial note events from the second track
remove_initial_events <- function(mid) {
  mid$tracks[[2]] <- mid$tracks[[2]] %>%
    filter(!(event %in% c("Note On", "Note Off") & deltatime %in% c(0, 100)))
  return(mid)
}

# Function to convert CSV data to MIDI format and add to template
sonify_data <- function(csv_file, template_midi_file, output_midi_file, key_scale, instrument, tempo) {
  data <- read.csv(csv_file, stringsAsFactors = FALSE)
  colnames(data)[1:2] <- c("date", "value")

  if (!"date" %in% colnames(data)) {
    stop("The 'date' column is missing in the CSV file.")
  }

  if (any(is.na(data$date))) {
    stop("The 'date' column contains NA values.")
  }

  if (any(data$date == "")) {
    stop("The 'date' column contains empty values.")
  }

  #data <- data %>%
   # mutate(date = mdy(date)) %>%
    #arrange(date)

  # Set a constant time interval between notes using input tempo
  constant_ticks <- 57600/tempo

  # Define note ranges for different keys/scales
  note_ranges <- list(
    Cmaj = c(60, 62, 64, 65, 67, 69, 71, 72),  # C Major scale
    Cmin = c(60, 62, 63, 65, 67, 68, 70, 72),  # C Minor scale
    CSmaj = c(61, 63, 65, 66, 68, 70, 72, 73), # C# Major scale
    CSmin = c(61, 63, 64, 66, 68, 69, 71, 73), # C# Minor scale
    Dmaj = c(62, 64, 66, 67, 69, 71, 73, 74),  # D Major scale
    Dmin = c(62, 64, 65, 67, 69, 70, 72, 74),  # D Minor scale
    DSmaj = c(63, 65, 67, 68, 70, 72, 74, 75), # D# Major scale
    DSmin = c(63, 65, 66, 68, 70, 71, 73, 75), # D# Minor scale
    Emaj = c(64, 66, 68, 69, 71, 73, 75, 76),  # E Major scale
    Emin = c(64, 66, 67, 69, 71, 72, 74, 76),  # E Minor scale
    Fmaj = c(65, 67, 69, 70, 72, 74, 76, 77),  # F Major scale
    Fmin = c(65, 67, 68, 70, 72, 73, 75, 77),  # F Minor scale
    FSmaj = c(66, 68, 70, 71, 73, 75, 77, 78), # F# Major scale
    FSmin = c(66, 68, 69, 71, 73, 74, 76, 78), # F# Minor scale
    Gmaj = c(67, 69, 71, 72, 74, 76, 78, 79),  # G Major scale
    Gmin = c(67, 69, 70, 72, 74, 75, 77, 79),  # G Minor scale
    GSmaj = c(68, 70, 72, 73, 75, 77, 79, 80), # G# Major scale
    GSmin = c(68, 70, 71, 73, 75, 76, 78, 80), # G# Minor scale
    Amaj = c(69, 71, 73, 74, 76, 78, 80, 81),  # A Major scale
    Amin = c(69, 71, 72, 74, 76, 77, 79, 81),  # A Minor scale
    ASmaj = c(70, 72, 74, 75, 77, 79, 81, 82), # A# Major scale
    ASmin = c(70, 72, 73, 75, 77, 78, 80, 82), # A# Minor scale
    Bmaj = c(71, 73, 75, 76, 78, 80, 82, 83),  # B Major scale
    Bmin = c(71, 73, 74, 76, 78, 79, 81, 83)   # B Minor scale
  )

  # Select the note range based on the chosen key/scale
  selected_notes <- note_ranges[[key_scale]]

  data <- data %>%
    mutate(note = sample(selected_notes, n(), replace = TRUE),
           deltatime = constant_ticks)

  mid <- midi$new(template_midi_file)
  mid <- remove_initial_events(mid)

  # Add a Program Change event to set the instrument to the selected instrument
  mid$tracks[[2]] <- dplyr::add_row(
    mid$tracks[[2]],
    deltatime = 0,
    event_type = "channel_voice",
    event = "Program Change",
    params = list(list(channel = 0, program = instrument)), # Subtract 1 because MIDI instruments are 0-indexed
    EventChannel = as.raw(0xC0),
    .before = 1
  )

  note_events <- data.frame(
    deltatime = rep(data$deltatime, each = 2),
    event_type = "channel_voice",
    event = rep(c("Note On", "Note Off"), nrow(data)),
    params = I(unlist(lapply(data$note, function(note) {
      list(list(channel = 0, key_number = note, velocity = 64), list(channel = 0, key_number = note, velocity = 64))
    }), recursive = FALSE)),
    EventChannel = as.raw(c(rep(0x90, nrow(data)), rep(0x80, nrow(data)))),
    type = NA
  )

  if (any(is.na(note_events$note))) {
    stop("Error: Missing values detected in note parameters.")
  }

  end_of_track <- mid$tracks[[2]] %>% filter(event == "End of Track")
  mid$tracks[[2]] <- mid$tracks[[2]] %>% filter(event != "End of Track")

  mid$tracks[[2]] <- bind_rows(mid$tracks[[2]], note_events, end_of_track)

  mid$encode(output_midi_file)
  cat("New MIDI file saved at:", output_midi_file)
}

ui <- fluidPage(
  titlePanel("SonicPlots - A Data Sonification Tool By Luke Williams"),
  sidebarLayout(
    sidebarPanel(
      h4("Sonification"),
      # Data source selection checkboxes next to each other
      fluidRow(
        column(6, checkboxInput("builtinData", "Use Built-in Datasets", value = TRUE)),
        column(6, checkboxInput("uploadData", "Upload Your Own Data", value = FALSE))
      ),
      conditionalPanel(
        condition = "input.builtinData",
        selectInput("builtinDataset", "Select Dataset",
                    choices = list("Carbon-based Net Primary Production",
                                   "CMEMS Mixed Layer Depth",
                                   "East-West Surface Current",
                                   "Hadley Ice and Sea Surface Temperature",
                                   "Hadley Subserface Salinity",
                                   "ICOADS Scalar Windspeed",
                                   "NASA Surface Chlorophyll",
                                   "North-South Surface Current",
                                   "OCCCI Surface Chlorophyll",
                                   "Reynolds OI Sea Surface Temperature",
                                   "SeaWinds Scalar Windspeed"))
      ),
      conditionalPanel(
        condition = "input.uploadData",
        fileInput("file", "Choose CSV File", accept = ".csv")
      ),
      selectInput("keyNote", "Select Note",
                  choices = c("C", "C#" = "CS", "D", "D#" = "DS", "E", "F", "F#" = "FS", "G", "G#" = "GS", "A", "A#" = "AS", "B")),
      fluidRow(
        column(6, checkboxInput("isMajor", "Major", value = TRUE)),
        column(6, checkboxInput("isMinor", "Minor", value = FALSE))
      ),
      sliderInput("tempo", "Tempo (BPM):", min = 40, max = 200, value = 120),
      selectInput("instrument", "Select Instrument",
                  choices = as.list(1:128), selected = sample(1:128, 1)),
      tags$p("Visit the",
             tags$a(href = "https://en.wikipedia.org/wiki/General_MIDI#Parameter_interpretations", "General MIDI Standard"),
             "as reference for instruments"),
      fluidRow(
        column(6, actionButton("sonify", "Sonify Data"), style = "color:green"),
        column(6, actionButton("stop", "Stop Playback"))
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("View Data",
                 numericInput("numRows", "Number of Rows to View", value = 10, min = 1),
                 fluidRow(
                   column(6, tableOutput("dataPreview")),
                   column(6, verbatimTextOutput("dataSummary"))
                 )
        ),
        tabPanel("Plot Data",
                 selectInput("graphType", "Select Graph Type",
                             choices = c("Time Series", "Bar Graph", "Scatter Plot")),
                 selectInput("lineColor", "Select Line Color",
                             choices = c("Red" = "red", "Blue" = "blue", "Green" = "darkgreen", "Black" = "black"), selected = "blue"),
                 conditionalPanel(
                   condition = "input.graphType == 'Time Series'",
                   checkboxInput("shading", "Shade Area Under Line", value = FALSE)
                 ),
                 conditionalPanel(
                   condition = "input.graphType == 'Bar Graph'",
                   sliderInput("numBars", "Number of Bars", min = 1, max = 50, value = 10)
                 ),
                 plotOutput("dataPlot")
        ),
        tabPanel("Model",
                 checkboxInput("fitARIMA", "Fit ARMA/ARIMA Model", value = FALSE),
                 conditionalPanel(
                   condition = "input.fitARIMA",
                   checkboxInput("seasonal", "Include Seasonal Component", value = FALSE),
                   conditionalPanel(
                     condition = "input.seasonal",
                     numericInput("seasonalPeriod", "Seasonal Period", value = 12, min = 1)
                   ),
                   plotOutput("modelPlot"),
                   verbatimTextOutput("modelSummary")
                 )
        )
      ),
      verbatimTextOutput("sonificationOutput")
    )
  )
)

# Define server logic required to draw a plot and fit the model
server <- function(input, output, session) {
  # Ensure mutual exclusivity for major/minor checkboxes
  observeEvent(input$isMajor, {
    if (input$isMajor) {
      updateCheckboxInput(session, "isMinor", value = FALSE)
    }
  })

  observeEvent(input$isMinor, {
    if (input$isMinor) {
      updateCheckboxInput(session, "isMajor", value = FALSE)
    }
  })

  # Ensure mutual exclusivity for data source checkboxes
  observeEvent(input$builtinData, {
    if (input$builtinData) {
      updateCheckboxInput(session, "uploadData", value = FALSE)
    }
  })

  observeEvent(input$uploadData, {
    if (input$uploadData) {
      updateCheckboxInput(session, "builtinData", value = FALSE)
    }
  })

  data <- reactive({
    if (input$builtinData) {
      # Load the selected built-in dataset
      dataset_path <- system.file("extdata", paste0(input$builtinDataset, ".csv"), package = "SonicPlots")
      df <- read.csv(dataset_path)
    } else if (input$uploadData) {
      req(input$file)
      df <- read.csv(input$file$datapath)
    }
    # Ensure there are no NA values in the date column
    df <- df %>% filter(!is.na(df[[1]]))
    df <- df[, 1:2]
    return(df)
  })

  output$dataPlot <- renderPlot({
    req(data())
    df <- data()
    # Parse the first column to Date type
    df[[1]] <- parse_date_time(df[[1]], orders = c("dmy", "mdy", "ymd", "ydm"))
    p <- ggplot(df, aes(x = df[[1]], y = df[[2]])) +
      xlab("Date") + ylab(names(df)[2])
    if (input$graphType == "Time Series") {
      p <- p + geom_line(color = input$lineColor) + ggtitle("Time Series Plot")
      if (input$shading) {
        p <- p + geom_ribbon(aes(ymin = 0, ymax = df[[2]]), fill = input$lineColor, alpha = 0.3)
      }
    } else if (input$graphType == "Bar Graph") {
      df <- df %>% head(input$numBars) # Limit the number of bars
      p <- ggplot(df, aes(x = factor(df[[1]]), y = df[[2]])) +
        geom_bar(stat = "identity", fill = input$lineColor) +
        xlab("Date") + ylab(names(df)[2]) + ggtitle("Bar Graph")
    } else if (input$graphType == "Scatter Plot") {
      p <- p + geom_point(color = input$lineColor) + ggtitle("Scatter Plot")
    }

    print(p)
  })

  output$dataPreview <- renderTable({
    req(data())
    head(data(), input$numRows)
  })

  output$dataSummary <- renderPrint({
    req(data())
    summary(data())
  })

  # Variable to store the future object for playing the MIDI
  midi_future <- reactiveVal(NULL)

  observeEvent(input$sonify, {
    req(data())
    # Save the reactive data to a temporary CSV file
    temp_csv <- tempfile(fileext = ".csv")
    write.csv(data(), temp_csv, row.names = FALSE)
    # Define file paths for the template and output MIDI files
    template_midi_path <- system.file("extdata/template.mid", package = "SonicPlots")
    output_midi_path <- "output_midi.mid"

    # Determine the key scale
    key_scale <- paste0(input$keyNote, if (input$isMajor) "maj" else "min")

    # Run the sonification function
    sonify_data(temp_csv, template_midi_path, output_midi_path, key_scale, input$instrument, input$tempo)

    # Stop any previous playback
    if (!is.null(midi_future())) {
      future::resolved(midi_future())
      midi_future(NULL)
    }

    # Play the generated MIDI file using fluidsynth
    new_future <- future({
      if (!file.exists(output_midi_path)) {
        stop("MIDI file does not exist: ", output_midi_path)
      }

      # Read the MIDI file
      midi_df <- midi_read(output_midi_path)

      # Download the SoundFont file
      soundfont_download()

      # Get the path to the downloaded SoundFont file
      sf_path <- soundfont_path()

      # Ensure the SoundFont file exists
      if (!file.exists(sf_path)) {
        stop("SoundFont file does not exist: ", sf_path)
      }

      # Play the MIDI file using the SoundFont
      midi_play(
        output_midi_path,
        soundfont = sf_path,
        verbose = TRUE
      )

      midi_df$encode(output_midi_path)
    })

    # Store the future object
    midi_future(new_future)

    # Provide feedback to the user
    output$sonificationOutput <- renderText({
      "Sonification complete. Playing the sound."
    })
  })

  observeEvent(input$stop, {
    # Stop the playback by resolving the future
    if (!is.null(midi_future())) {
      future::resolved(midi_future())
      midi_future(NULL)
    }
    session$reload()
  })

  observe({
    req(input$fitARIMA)
    req(data())
    df <- data()
    # Convert to time series
    seasonal_period <- if (input$seasonal) input$seasonalPeriod else 1
    ts_data <- ts(df[[2]], frequency = seasonal_period)
    # Fit the ARIMA model
    fit <- auto.arima(ts_data, seasonal = input$seasonal)

    # Output model summary
    output$modelSummary <- renderPrint({
      summary(fit)
    })

    # Plot the fitted model
    output$modelPlot <- renderPlot({
      autoplot(forecast(fit)) + ggtitle("ARIMA Model Forecast")
    })
  })
}

#Run the application

shinyApp(ui = ui, server = server)
