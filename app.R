# Experience Tracker App
# Track daily experiences across different times of day

library(shiny)
library(bslib)
library(tidyverse)
library(lubridate)
library(plotly)

# =============================================================================
# Data Storage Functions (CSV-based for GitHub compatibility)
# =============================================================================

DATA_FILE <- "data/experiences.csv"

# Initialize data directory and file
init_data_storage <- function() {
  if (!dir.exists("data")) {
    dir.create("data")
  }

  if (!file.exists(DATA_FILE)) {
    # Create empty CSV with proper structure
    empty_df <- tibble(
      id = character(),
      date = character(),
      time_of_day = character(),
      experience_type = character(),
      description = character(),
      timestamp = character()
    )
    write_csv(empty_df, DATA_FILE)
  }
}

# Load all experiences
load_experiences <- function() {
  if (file.exists(DATA_FILE)) {
    df <- read_csv(DATA_FILE, col_types = cols(
      id = col_character(),
      date = col_date(),
      time_of_day = col_character(),
      experience_type = col_character(),
      description = col_character(),
      timestamp = col_datetime()
    ))
    return(df)
  }
  return(tibble())
}

# Save a new experience
save_experience <- function(date, time_of_day, experience_type, description) {
  new_entry <- tibble(
    id = paste0(format(Sys.time(), "%Y%m%d%H%M%S"), "_", sample(1000:9999, 1)),
    date = as.Date(date),
    time_of_day = time_of_day,
    experience_type = experience_type,
    description = description,
    timestamp = Sys.time()
  )

  # Append to existing file
  if (file.exists(DATA_FILE) && file.size(DATA_FILE) > 0) {
    existing <- load_experiences()
    combined <- bind_rows(existing, new_entry)
    write_csv(combined, DATA_FILE)
  } else {
    write_csv(new_entry, DATA_FILE)
  }

  return(new_entry)
}

# Delete an experience by ID
delete_experience <- function(id) {
  if (file.exists(DATA_FILE)) {
    df <- load_experiences()
    df <- df %>% filter(id != !!id)
    write_csv(df, DATA_FILE)
  }
}

# Initialize storage on app start
init_data_storage()

# =============================================================================
# Define Experience Types
# =============================================================================

EXPERIENCE_TYPES <- c(

  "Work/Productivity",
  "Exercise/Physical",
  "Social/Connection",
  "Creative/Hobby",
  "Learning/Growth",
  "Rest/Relaxation",
  "Nature/Outdoors",
  "Mindfulness/Meditation",
  "Entertainment",
  "Family Time",
  "Self-Care",
  "Other"
)

TIME_OF_DAY <- c(
  "Morning" = "morning",
  "Day" = "day",
  "Evening" = "evening",
  "Night" = "night"
)

# =============================================================================
# UI
# =============================================================================

ui <- page_fillable(
  theme = bs_theme(
    preset = "minty",
    primary = "#78c2ad",
    font_scale = 0.95
  ),

  tags$head(
    tags$style(HTML("
      .experience-card { margin-bottom: 10px; }
      .time-badge { font-size: 0.8em; padding: 3px 8px; border-radius: 12px; }
      .morning { background-color: #ffeeba; color: #856404; }
      .day { background-color: #b8daff; color: #004085; }
      .evening { background-color: #f5c6cb; color: #721c24; }
      .night { background-color: #c3e6cb; color: #155724; }
      .delete-btn { padding: 2px 6px; font-size: 0.7em; }
      .trend-card { min-height: 400px; }
    "))
  ),

  titlePanel(
    div(
      "Experience Tracker",
      tags$small(class = "text-muted", " - Track your daily experiences")
    )
  ),

  layout_columns(
    col_widths = c(4, 8),

    # Left column - Input form
    card(
      card_header(
        class = "bg-primary text-white",
        "Log Experience"
      ),
      card_body(
        dateInput(
          "exp_date",
          "Date",
          value = Sys.Date(),
          max = Sys.Date()
        ),

        radioButtons(
          "time_of_day",
          "Time of Day",
          choices = TIME_OF_DAY,
          selected = {
            hour <- hour(Sys.time())
            if (hour < 12) "morning"
            else if (hour < 17) "day"
            else if (hour < 21) "evening"
            else "night"
          },
          inline = TRUE
        ),

        selectInput(
          "experience_type",
          "Experience Type",
          choices = EXPERIENCE_TYPES,
          multiple = TRUE,
          selectize = TRUE
        ),

        textAreaInput(
          "description",
          "Brief Description",
          placeholder = "What did you experience?",
          rows = 3
        ),

        actionButton(
          "save_btn",
          "Save Experience",
          class = "btn-primary w-100 mt-2",
          icon = icon("save")
        )
      )
    ),

    # Right column - Display and trends
    navset_card_tab(
      title = "View Experiences",

      # Daily Snapshot Tab
      nav_panel(
        title = "Daily Snapshot",
        icon = icon("calendar-day"),

        layout_columns(
          col_widths = c(6, 6),

          dateInput(
            "snapshot_date",
            "Select Date",
            value = Sys.Date(),
            max = Sys.Date()
          ),

          div(
            class = "text-end pt-4",
            textOutput("daily_count")
          )
        ),

        hr(),

        uiOutput("daily_experiences")
      ),

      # Trends Tab
      nav_panel(
        title = "Trends",
        icon = icon("chart-line"),

        layout_columns(
          col_widths = c(6, 6),

          selectInput(
            "trend_period",
            "Time Period",
            choices = c(
              "Last 7 Days" = "week",
              "Last 30 Days" = "month",
              "Year to Date" = "ytd",
              "Last 12 Months" = "year"
            )
          ),

          selectInput(
            "trend_type",
            "View",
            choices = c(
              "Experience Types" = "types",
              "Time of Day" = "time",
              "Daily Activity" = "activity"
            )
          )
        ),

        plotlyOutput("trend_chart", height = "350px")
      ),

      # All Experiences Tab
      nav_panel(
        title = "History",
        icon = icon("list"),

        layout_columns(
          col_widths = c(4, 4, 4),

          selectInput(
            "filter_type",
            "Filter by Type",
            choices = c("All" = "", EXPERIENCE_TYPES)
          ),

          selectInput(
            "filter_time",
            "Filter by Time",
            choices = c("All" = "", TIME_OF_DAY)
          ),

          dateRangeInput(
            "filter_dates",
            "Date Range",
            start = Sys.Date() - 30,
            end = Sys.Date()
          )
        ),

        tableOutput("history_table")
      )
    )
  )
)

# =============================================================================
# Server
# =============================================================================

server <- function(input, output, session) {

  # Reactive values for data
  experiences_data <- reactiveVal(load_experiences())

  # Refresh data from file
  refresh_data <- function() {
    experiences_data(load_experiences())
  }

  # Save new experience
  observeEvent(input$save_btn, {
    # Validation
    if (is.null(input$experience_type) || length(input$experience_type) == 0) {
      showNotification("Please select at least one experience type", type = "error")
      return()
    }

    # Save each experience type as separate entry
    for (exp_type in input$experience_type) {
      save_experience(
        date = input$exp_date,
        time_of_day = input$time_of_day,
        experience_type = exp_type,
        description = input$description
      )
    }

    # Refresh data and reset form
    refresh_data()

    updateSelectInput(session, "experience_type", selected = character(0))
    updateTextAreaInput(session, "description", value = "")

    showNotification(
      paste(length(input$experience_type), "experience(s) logged successfully!"),
      type = "message"
    )
  })

  # Daily count
  output$daily_count <- renderText({
    df <- experiences_data()
    if (nrow(df) == 0) return("No experiences yet")

    count <- df %>%
      filter(date == input$snapshot_date) %>%
      nrow()

    paste(count, "experience(s) logged")
  })

  # Daily snapshot display
  output$daily_experiences <- renderUI({
    df <- experiences_data()
    if (nrow(df) == 0) {
      return(div(class = "text-muted text-center py-4", "No experiences logged yet"))
    }

    daily <- df %>%
      filter(date == input$snapshot_date) %>%
      arrange(factor(time_of_day, levels = c("morning", "day", "evening", "night")))

    if (nrow(daily) == 0) {
      return(div(class = "text-muted text-center py-4", "No experiences for this date"))
    }

    # Group by time of day
    experience_cards <- daily %>%
      group_by(time_of_day) %>%
      group_map(~ {
        time_label <- names(TIME_OF_DAY)[TIME_OF_DAY == .y$time_of_day]

        items <- .x %>%
          pmap(function(id, experience_type, description, ...) {
            div(
              class = "d-flex justify-content-between align-items-start mb-2",
              div(
                tags$strong(experience_type),
                if (nchar(description) > 0) {
                  tags$p(class = "mb-0 small text-muted", description)
                }
              ),
              actionButton(
                paste0("delete_", id),
                "",
                icon = icon("times"),
                class = "btn-outline-danger delete-btn",
                onclick = sprintf("Shiny.setInputValue('delete_id', '%s', {priority: 'event'})", id)
              )
            )
          })

        div(
          class = "mb-3",
          div(
            class = paste("time-badge", .y$time_of_day, "mb-2"),
            time_label
          ),
          items
        )
      })

    do.call(tagList, experience_cards)
  })

  # Delete handler
  observeEvent(input$delete_id, {
    delete_experience(input$delete_id)
    refresh_data()
    showNotification("Experience deleted", type = "message")
  })

  # Trend chart
  output$trend_chart <- renderPlotly({
    df <- experiences_data()

    if (nrow(df) == 0) {
      return(
        plot_ly() %>%
          add_annotations(
            text = "No data to display",
            x = 0.5, y = 0.5,
            xref = "paper", yref = "paper",
            showarrow = FALSE
          ) %>%
          layout(
            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
          )
      )
    }

    # Filter by period
    end_date <- Sys.Date()
    start_date <- switch(
      input$trend_period,
      "week" = end_date - days(7),
      "month" = end_date - days(30),
      "ytd" = floor_date(end_date, "year"),
      "year" = end_date - 365
    )

    filtered <- df %>%
      filter(date >= start_date & date <= end_date)

    if (nrow(filtered) == 0) {
      return(
        plot_ly() %>%
          add_annotations(
            text = "No data for selected period",
            x = 0.5, y = 0.5,
            xref = "paper", yref = "paper",
            showarrow = FALSE
          ) %>%
          layout(
            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
          )
      )
    }

    # Create chart based on type
    if (input$trend_type == "types") {
      # Experience types distribution
      type_counts <- filtered %>%
        count(experience_type) %>%
        arrange(desc(n))

      plot_ly(
        data = type_counts,
        x = ~reorder(experience_type, n),
        y = ~n,
        type = "bar",
        marker = list(color = "#78c2ad")
      ) %>%
        layout(
          title = "Experience Types",
          xaxis = list(title = ""),
          yaxis = list(title = "Count"),
          margin = list(b = 100)
        ) %>%
        config(displayModeBar = FALSE)

    } else if (input$trend_type == "time") {
      # Time of day distribution
      time_counts <- filtered %>%
        count(time_of_day) %>%
        mutate(time_of_day = factor(
          time_of_day,
          levels = c("morning", "day", "evening", "night"),
          labels = c("Morning", "Day", "Evening", "Night")
        ))

      colors <- c("#ffc107", "#17a2b8", "#dc3545", "#28a745")

      plot_ly(
        data = time_counts,
        labels = ~time_of_day,
        values = ~n,
        type = "pie",
        marker = list(colors = colors)
      ) %>%
        layout(title = "Time of Day Distribution") %>%
        config(displayModeBar = FALSE)

    } else {
      # Daily activity over time
      daily_counts <- filtered %>%
        count(date) %>%
        complete(
          date = seq(start_date, end_date, by = "day"),
          fill = list(n = 0)
        )

      plot_ly(
        data = daily_counts,
        x = ~date,
        y = ~n,
        type = "scatter",
        mode = "lines+markers",
        line = list(color = "#78c2ad"),
        marker = list(color = "#78c2ad")
      ) %>%
        layout(
          title = "Daily Activity",
          xaxis = list(title = "Date"),
          yaxis = list(title = "Experiences Logged")
        ) %>%
        config(displayModeBar = FALSE)
    }
  })

  # History table
  output$history_table <- renderTable({
    df <- experiences_data()

    if (nrow(df) == 0) {
      return(data.frame(Message = "No experiences logged yet"))
    }

    # Apply filters
    filtered <- df %>%
      filter(date >= input$filter_dates[1] & date <= input$filter_dates[2])

    if (input$filter_type != "") {
      filtered <- filtered %>% filter(experience_type == input$filter_type)
    }

    if (input$filter_time != "") {
      filtered <- filtered %>% filter(time_of_day == input$filter_time)
    }

    if (nrow(filtered) == 0) {
      return(data.frame(Message = "No experiences match the filters"))
    }

    filtered %>%
      arrange(desc(date), desc(timestamp)) %>%
      mutate(
        Date = format(date, "%Y-%m-%d"),
        Time = case_when(
          time_of_day == "morning" ~ "Morning",
          time_of_day == "day" ~ "Day",
          time_of_day == "evening" ~ "Evening",
          time_of_day == "night" ~ "Night"
        ),
        Type = experience_type,
        Description = description
      ) %>%
      select(Date, Time, Type, Description) %>%
      head(100)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
}

# =============================================================================
# Run App
# =============================================================================

shinyApp(ui, server)
