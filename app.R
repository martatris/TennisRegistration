# app.R
# Tennis Court Registration - Admin via ?admin=1
# Admin sets price, which courts exist, and fixed duration. Users register & upload payment screenshot.
library(shiny)
library(shinythemes)
library(DT)
library(lubridate)
library(shinyjs)
library(uuid)

# -----------------------------
# File paths and defaults
# -----------------------------
BOOKINGS_RDS <- "bookings_data.rds"
CONFIG_RDS   <- "config.rds"
UPLOAD_DIR   <- "payment_uploads"

if (!dir.exists(UPLOAD_DIR)) dir.create(UPLOAD_DIR, recursive = TRUE)

default_config <- list(
  price = 0,                            # price per hour
  duration = 1,                         # fixed duration (hours)
  courts = paste0("Court ", 1:4)        # default courts Court 1..4
)

# initial non-reactive bookings (safe in global)
initial_bookings <- data.frame(
  id = character(0),
  name = character(0),
  phone = character(0),
  email = character(0),
  date = as.Date(character(0)),
  start_time = character(0),
  end_time = character(0),
  court = character(0),
  created_at = as.POSIXct(character(0)),
  notes = character(0),
  price = numeric(0),
  payment_file = character(0),
  stringsAsFactors = FALSE
)

# -----------------------------
# Helpers
# -----------------------------
time_seq <- function(start = "06:00", end = "22:00", by = "30 mins") {
  s <- as.POSIXct(start, format = "%H:%M", tz = "UTC")
  e <- as.POSIXct(end, format = "%H:%M", tz = "UTC")
  ts <- seq(s, e, by = by)
  format(ts, "%H:%M")
}
TIME_OPTIONS <- time_seq()

make_dt <- function(date, time_str) {
  as.POSIXct(paste(as.character(date), time_str), format = "%Y-%m-%d %H:%M", tz = "UTC")
}

read_config_safe <- function() {
  if (file.exists(CONFIG_RDS)) {
    cfg <- tryCatch(readRDS(CONFIG_RDS), error = function(e) NULL)
    if (is.list(cfg) && !is.null(cfg$price) && !is.null(cfg$duration) && !is.null(cfg$courts)) return(cfg)
  }
  default_config
}

has_conflict <- function(df, court, date_only, start_dt, end_dt) {
  if (nrow(df) == 0) return(FALSE)
  existing <- df[df$court == court & as.Date(df$date) == as.Date(date_only), , drop = FALSE]
  if (nrow(existing) == 0) return(FALSE)
  ex_start <- as.POSIXct(paste(existing$date, existing$start_time), format = "%Y-%m-%d %H:%M", tz = "UTC")
  ex_end   <- as.POSIXct(paste(existing$date, existing$end_time),   format = "%Y-%m-%d %H:%M", tz = "UTC")
  any((start_dt < ex_end) & (end_dt > ex_start))
}

# -----------------------------
# UI
# -----------------------------
ui <- fluidPage(
  useShinyjs(),
  theme = shinytheme("flatly"),
  titlePanel("Tennis Court Registration"),
  sidebarLayout(
    sidebarPanel(
      uiOutput("side_ui"),
      width = 4
    ),
    mainPanel(
      uiOutput("main_ui"),
      width = 8
    )
  )
)

# -----------------------------
# Server
# -----------------------------
server <- function(input, output, session) {
  # determine admin mode inside reactive context (Option B)
  is_admin <- reactiveVal(FALSE)
  observe({
    url <- session$clientData$url_search  # reactive, safe inside observe
    if (!is.null(url) && nzchar(url)) {
      q <- parseQueryString(url)
      if (!is.null(q$admin) && q$admin == "1") {
        is_admin(TRUE)
      } else {
        is_admin(FALSE)
      }
    } else {
      is_admin(FALSE)
    }
  })
  
  # reactive config and bookings
  cfg <- reactiveVal(read_config_safe())
  bookings <- reactiveVal({
    if (file.exists(BOOKINGS_RDS)) {
      loaded <- tryCatch(readRDS(BOOKINGS_RDS), error = function(e) NULL)
      if (is.data.frame(loaded)) return(loaded)
    }
    initial_bookings
  })
  
  courts <- reactive({ as.character(cfg()$courts) })
  
  # -----------------------------
  # Side UI (admin vs user)
  # -----------------------------
  output$side_ui <- renderUI({
    if (is_admin()) {
      tagList(
        h4("Admin Settings"),
        numericInput("admin_price", "Price (IDR per hour)", value = cfg()$price, min = 0),
        numericInput("admin_duration", "Fixed booking duration (hours)", value = cfg()$duration, min = 0.5, step = 0.5),
        hr(),
        h4("Manage courts"),
        textInput("new_court", "New court name", placeholder = "e.g. Court 5 or Indoor Court"),
        actionButton("add_court", "Add court", class = "btn-primary"),
        br(), br(),
        selectInput("delete_court_select", "Delete court", choices = courts()),
        actionButton("delete_court_btn", "Delete selected court", class = "btn-danger"),
        hr(),
        actionButton("save_cfg", "Save config to disk", class = "btn-success"),
        hr(),
        h5("Admin actions"),
        actionButton("reload_bookings", "Reload bookings from disk"),
        br(), br(),
        downloadButton("download_bookings", "Download bookings (CSV)")
      )
    } else {
      tagList(
        h4("Book a Court"),
        div(style = "font-weight:600; margin-bottom:8px;",
            "Current price: ", span(textOutput("display_price", inline = TRUE), style = "color: #2c7; font-weight:700")
        ),
        textInput("name", "Full name", placeholder = "John Doe"),
        textInput("phone", "Phone number", placeholder = "+62 812..."),
        textInput("email", "Email (optional)", placeholder = "you@example.com"),
        dateInput("date", "Date", value = Sys.Date(), min = Sys.Date()),
        selectInput("start_time", "Start time", choices = TIME_OPTIONS, selected = TIME_OPTIONS[1]),
        tags$div(style = "margin-bottom:8px;",
                 tags$label("Duration (hours) [fixed by admin]"),
                 tags$div(style = "font-weight:600;", textOutput("display_duration", inline = TRUE))
        ),
        selectInput("court", "Choose court", choices = courts()),
        textAreaInput("notes", "Notes (optional)", rows = 2),
        fileInput("payment_ss", "Upload payment screenshot (PNG/JPEG)", accept = c("image/png","image/jpeg")),
        actionButton("check_avail", "Check availability", class = "btn-primary"),
        actionButton("confirm_book", "Confirm booking", class = "btn-success")
      )
    }
  })
  
  # -----------------------------
  # Main UI (admin vs user)
  # -----------------------------
  output$main_ui <- renderUI({
    if (is_admin()) {
      tabsetPanel(
        tabPanel("Bookings",
                 h4("Bookings (Admin)"),
                 checkboxInput("show_past", "Include past bookings", value = FALSE),
                 DTOutput("admin_table"),
                 br(),
                 uiOutput("preview_payment"),
                 actionButton("delete_selected", "Delete selected", class = "btn-danger")
        ),
        tabPanel("Config",
                 h4("Current config"),
                 verbatimTextOutput("cfg_print")
        )
      )
    } else {
      tagList(
        tabsetPanel(
          tabPanel("Availability & Bookings",
                   h4("Selected date bookings"),
                   fluidRow(
                     column(4, dateInput("view_date", "View date", value = Sys.Date(), min = Sys.Date())),
                     column(4, selectInput("view_court", "Court", choices = c("All", courts())))
                   ),
                   uiOutput("availability_box"),
                   hr(),
                   DTOutput("bookings_table")
          ),
          tabPanel("Instructions",
                   h4("How to register"),
                   p("Fill the form, upload your payment screenshot, then press 'Confirm booking'."),
                   p("Bookings use the fixed duration set by the admin.")
          )
        )
      )
    }
  })
  
  # small displays
  output$cfg_print <- renderPrint({ cfg() })
  output$display_price <- renderText({ paste0("IDR ", formatC(cfg()$price, format = "f", big.mark = ",", digits = 0)) })
  output$display_duration <- renderText({ as.character(cfg()$duration) })
  
  # -----------------------------
  # Admin: add / delete courts
  # -----------------------------
  observeEvent(input$add_court, {
    new_name <- trimws(input$new_court)
    if (is.null(new_name) || new_name == "") {
      showModal(modalDialog(title = "Invalid name", "Please enter a court name before adding.", easyClose = TRUE))
      return()
    }
    current <- as.character(cfg()$courts)
    if (new_name %in% current) {
      showModal(modalDialog(title = "Exists", "That court already exists.", easyClose = TRUE))
      return()
    }
    current <- c(current, new_name)
    new_cfg <- cfg()
    new_cfg$courts <- current
    cfg(new_cfg)
    # update delete selectInput choices via re-render (reactive)
    showModal(modalDialog(title = "Added", paste0("Court '", new_name, "' added."), easyClose = TRUE))
    # persist immediately
    tryCatch(saveRDS(new_cfg, CONFIG_RDS), error = function(e) NULL)
  })
  
  observeEvent(input$delete_court_btn, {
    sel <- input$delete_court_select
    if (is.null(sel) || sel == "") {
      showModal(modalDialog(title = "Select court", "Please select a court to delete.", easyClose = TRUE)); return()
    }
    current <- as.character(cfg()$courts)
    if (!(sel %in% current)) {
      showModal(modalDialog(title = "Not found", "Selected court not found.", easyClose = TRUE)); return()
    }
    new_list <- setdiff(current, sel)
    new_cfg <- cfg()
    new_cfg$courts <- new_list
    cfg(new_cfg)
    showModal(modalDialog(title = "Deleted", paste0("Court '", sel, "' deleted."), easyClose = TRUE))
    tryCatch(saveRDS(new_cfg, CONFIG_RDS), error = function(e) NULL)
  })
  
  # Also update the choices for delete_court_select dynamically
  observe({
    # updateSelectInput must be called inside observe
    updateSelectInput(session, "delete_court_select", choices = courts())
  })
  
  # Save config button (writes price/duration/courts to disk)
  observeEvent(input$save_cfg, {
    new_cfg <- list(
      price = as.numeric(input$admin_price %||% cfg()$price),
      duration = as.numeric(input$admin_duration %||% cfg()$duration),
      courts = as.character(cfg()$courts)
    )
    cfg(new_cfg)
    tryCatch({
      saveRDS(new_cfg, CONFIG_RDS)
      showModal(modalDialog(title = "Saved", "Configuration saved.", easyClose = TRUE))
    }, error = function(e) {
      showModal(modalDialog(title = "Error", "Failed to save config.", easyClose = TRUE))
    })
  })
  
  # reload bookings from disk
  observeEvent(input$reload_bookings, {
    if (file.exists(BOOKINGS_RDS)) {
      loaded <- tryCatch(readRDS(BOOKINGS_RDS), error = function(e) NULL)
      if (is.data.frame(loaded)) {
        bookings(loaded)
        showModal(modalDialog(title = "Reloaded", "Bookings reloaded from disk.", easyClose = TRUE))
      } else {
        showModal(modalDialog(title = "Error", "Failed to load bookings.", easyClose = TRUE))
      }
    } else {
      showModal(modalDialog(title = "No file", "No bookings_data.rds found.", easyClose = TRUE))
    }
  })
  
  # admin bookings table
  output$admin_table <- renderDT({
    df <- bookings()
    if (!input$show_past) df <- df[as.Date(df$date) >= Sys.Date(), , drop = FALSE]
    if (nrow(df) == 0) return(datatable(df[0, ], options = list(dom = 't')))
    datatable(df, selection = "single", options = list(pageLength = 10))
  })
  
  # preview payment for selected booking
  output$preview_payment <- renderUI({
    sel <- input$admin_table_rows_selected
    df <- bookings()
    if (is.null(sel) || length(sel) == 0) return(tags$p("Select booking to preview payment screenshot."))
    if (!input$show_past) df <- df[as.Date(df$date) >= Sys.Date(), , drop = FALSE]
    if (nrow(df) < sel) return(NULL)
    row <- df[sel, , drop = FALSE]
    pf <- row$payment_file
    if (is.na(pf) || pf == "") return(tags$p("No payment screenshot."))
    img_rel <- file.path(UPLOAD_DIR, basename(pf))
    if (!file.exists(img_rel)) return(tags$p("Payment file not found on disk."))
    tags$div(
      h5("Payment screenshot"),
      tags$img(src = img_rel, style = "max-width:100%; height:auto; border:1px solid #ddd; padding:6px;")
    )
  })
  
  # delete selected booking
  observeEvent(input$delete_selected, {
    sel <- input$admin_table_rows_selected
    df <- bookings()
    if (is.null(sel) || length(sel) == 0) {
      showModal(modalDialog(title = "No selection", "Please select a booking.", easyClose = TRUE))
      return()
    }
    if (!input$show_past) df <- df[as.Date(df$date) >= Sys.Date(), , drop = FALSE]
    if (nrow(df) < sel) return()
    id <- df[sel, "id"]
    full <- bookings()
    newdf <- full[full$id != id, , drop = FALSE]
    bookings(newdf)
    try({ saveRDS(newdf, BOOKINGS_RDS) }, silent = TRUE)
    showModal(modalDialog(title = "Deleted", "Booking deleted.", easyClose = TRUE))
  })
  
  # download bookings
  output$download_bookings <- downloadHandler(
    filename = function() paste0("bookings_", Sys.Date(), ".csv"),
    content = function(file) write.csv(bookings(), file, row.names = FALSE)
  )
  
  # -----------------------------
  # Availability UI + table (Version B)
  # -----------------------------
  output$availability_box <- renderUI({
    req(input$view_date)
    df <- bookings()
    vdf <- df[as.Date(df$date) == as.Date(input$view_date), , drop = FALSE]
    if (!is.null(input$view_court) && input$view_court != "All") vdf <- vdf[vdf$court == input$view_court, , drop = FALSE]
    if (nrow(vdf) == 0) {
      tagList(
        h5(sprintf("Bookings on %s", as.character(input$view_date))),
        tags$p("No bookings for selected date/court."),
        DTOutput("availability_table") # placeholder (will be empty)
      )
    } else {
      tagList(
        h5(sprintf("Bookings on %s", as.character(input$view_date))),
        DTOutput("availability_table")
      )
    }
  })
  
  # actual DT rendering for availability (separate from renderUI)
  output$availability_table <- renderDT({
    req(input$view_date)
    df <- bookings()
    vdf <- df[as.Date(df$date) == as.Date(input$view_date), , drop = FALSE]
    if (!is.null(input$view_court) && input$view_court != "All") vdf <- vdf[vdf$court == input$view_court, , drop = FALSE]
    if (nrow(vdf) == 0) return(datatable(data.frame()))
    vdf2 <- vdf
    vdf2$start_time <- as.character(vdf2$start_time); vdf2$end_time <- as.character(vdf2$end_time)
    datatable(vdf2[, c("name","phone","email","court","date","start_time","end_time","notes","price")], options = list(pageLength = 5))
  })
  
  # user bookings table
  output$bookings_table <- renderDT({
    df <- bookings()
    df_future <- df[as.Date(df$date) >= Sys.Date(), , drop = FALSE]
    if (nrow(df_future) == 0) return(datatable(df_future[0, ], options = list(dom = 't')))
    datatable(df_future[, c("name","phone","email","court","date","start_time","end_time","notes","price")], options = list(pageLength = 8))
  })
  
  # -----------------------------
  # Check availability & confirm booking (user)
  # -----------------------------
  observeEvent(input$check_avail, {
    if (is.null(input$name) || nzchar(trimws(input$name)) == FALSE) {
      showModal(modalDialog(title = "Missing name", "Enter your name.", easyClose = TRUE)); return()
    }
    start_dt <- make_dt(input$date, input$start_time)
    duration <- as.numeric(cfg()$duration)
    end_dt <- start_dt + dhours(duration)
    df <- bookings()
    if (has_conflict(df, input$court, input$date, start_dt, end_dt)) {
      showModal(modalDialog(title = "Unavailable", paste0(input$court, " is NOT available from ", format(start_dt, "%H:%M"), " to ", format(end_dt, "%H:%M")), easyClose = TRUE))
    } else {
      showModal(modalDialog(title = "Available", paste0(input$court, " is available from ", format(start_dt, "%H:%M"), " to ", format(end_dt, "%H:%M")), easyClose = TRUE))
    }
  })
  
  observeEvent(input$confirm_book, {
    if (is.null(input$name) || nzchar(trimws(input$name)) == FALSE) {
      showModal(modalDialog(title = "Missing name", "Enter your name.", easyClose = TRUE)); return()
    }
    start_dt <- make_dt(input$date, input$start_time)
    duration <- as.numeric(cfg()$duration)
    end_dt <- start_dt + dhours(duration)
    if (end_dt <= start_dt) {
      showModal(modalDialog(title = "Invalid duration", "Invalid duration.", easyClose = TRUE)); return()
    }
    df <- bookings()
    if (has_conflict(df, input$court, input$date, start_dt, end_dt)) {
      showModal(modalDialog(title = "Conflict", "Selected slot conflicts with existing booking.", easyClose = TRUE)); return()
    }
    # save uploaded file to UPLOAD_DIR
    payment_file <- ""
    if (!is.null(input$payment_ss) && nzchar(input$payment_ss$name) && file.exists(input$payment_ss$datapath)) {
      ext <- tools::file_ext(input$payment_ss$name)
      newname <- paste0("payment_", UUIDgenerate(), ".", ext)
      dest <- file.path(UPLOAD_DIR, newname)
      ok <- file.copy(input$payment_ss$datapath, dest, overwrite = TRUE)
      if (ok) payment_file <- dest
    }
    # compute price (price per hour * duration)
    booking_price <- as.numeric(cfg()$price) * duration
    new <- data.frame(
      id = UUIDgenerate(),
      name = trimws(input$name),
      phone = trimws(input$phone),
      email = trimws(input$email),
      date = as.Date(input$date),
      start_time = format(start_dt, "%H:%M"),
      end_time = format(end_dt, "%H:%M"),
      court = input$court,
      created_at = Sys.time(),
      notes = trimws(input$notes),
      price = booking_price,
      payment_file = ifelse(payment_file == "", NA_character_, payment_file),
      stringsAsFactors = FALSE
    )
    updated <- rbind(bookings(), new)
    bookings(updated)
    try({ saveRDS(updated, BOOKINGS_RDS) }, silent = TRUE)
    showModal(modalDialog(title = "Booked", paste0("Thanks ", new$name, ". Booking recorded. Price: IDR ", formatC(new$price, format = "f", big.mark = ",", digits = 0)), easyClose = TRUE))
  })
}

# Run app
shinyApp(ui = ui, server = server)

