# study-2/app/different-contracts-app/app.R
library(shiny)

# -----------------------------
# Data loaders (shinylive-safe)
# -----------------------------
load_data <- function() {
  p <- "data/df_bsc_longforapp.csv"
  if (!file.exists(p)) stop("Missing data/df_bsc_longforapp.csv in study-2/app/different-contracts-app/data/")
  read.csv(p, check.names = FALSE, stringsAsFactors = FALSE)
}

load_var_info <- function() {
  p <- "data/var_info.csv"
  if (!file.exists(p)) {
    return(data.frame(var=character(), label=character(), description=character(), stringsAsFactors = FALSE))
  }
  vi <- read.csv(p, check.names = FALSE, stringsAsFactors = FALSE)
  names(vi) <- tolower(names(vi))
  if (!"var" %in% names(vi)) vi$var <- character(0)
  if (!"label" %in% names(vi)) vi$label <- vi$var
  if (!"description" %in% names(vi)) vi$description <- ""
  vi$var <- trimws(vi$var); vi$label <- trimws(vi$label); vi$description <- trimws(vi$description)
  unique(vi[c("var","label","description")])
}

# -----------------------------
# Helpers
# -----------------------------
count_people <- function(df_sub) {
  if ("PID" %in% names(df_sub)) {
    return(length(unique(df_sub$PID)))
  }
  # fallback heuristic (shouldn't be needed now)
  if ("value" %in% names(df_sub)) {
    denom <- length(unique(df_sub$value))
    if (is.finite(denom) && denom > 0) return(as.integer(round(nrow(df_sub) / denom)))
  }
  as.integer(nrow(df_sub))
}

pretty_label <- function(var, var_info) {
  lb <- var_info$label[match(var, var_info$var)]
  ifelse(is.na(lb) | !nzchar(lb), var, lb)
}

pretty_desc <- function(var, var_info) {
  ds <- var_info$description[match(var, var_info$var)]
  ifelse(is.na(ds) | !nzchar(ds), "", ds)
}

# Ensure we have expected columns in the long data
build_value_table <- function(df_sub) {
  nm <- tolower(names(df_sub))
  vcol <- names(df_sub)[match("value", nm)]
  wcol <- names(df_sub)[match("weight", nm)]
  dcol <- names(df_sub)[match("weighted_score", nm)]
  if (any(is.na(c(vcol, wcol, dcol)))) {
    stop("Expected columns value, weight, weighted_score not all found in df_bsc_longforapp.csv")
  }
  out <- df_sub[, c(vcol, wcol, dcol)]
  names(out) <- c("Value", "Perceived Promise", "Perceived Delivery")
  out <- out[order(tolower(out$Value)), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# -----------------------------
# UI
# -----------------------------
ui <- fluidPage(
  titlePanel("Different Contracts for Different people"),
  tags$p(
    "In this app, you can explore how different sub-populations of the American public see the social contract. ",
    "Specifically, you can see what different groups believe the U.S.'s promise is on paper, and right next to it, ",
    "how they believe the U.S. government is doing in the delivery on that promise. We arrived at these numbers from ",
    "the way people assigned weights to what they believe the U.S. stands for on paper and from the weighted score ",
    "they gave to the U.S. government (across administrations) on each of the dimensions."
  ),
  sidebarLayout(
    sidebarPanel(
      helpText("Choose a characteristic to break out the tables:"),
      selectInput("groupvar", "Break down by", choices = NULL),
      uiOutput("state_picker"),
      tags$hr(),
      helpText("Notes:"),
      tags$p("• Each table shows eight overarching values (alphabetically)."),
      tags$p("• Columns: Value, Perceived Promise (weights), Perceived Delivery (weighted scores).")
    ),
    mainPanel(
      uiOutput("tables_ui"),
      tags$hr(),
      h4("Description"),
      textOutput("chosen_desc")
    )
  )
)

# -----------------------------
# Server
# -----------------------------
server <- function(input, output, session) {
  df0 <- load_data()
  var_info <- load_var_info()
  names(df0) <- trimws(names(df0))
  
  allowed <- c("TIPI_extra","TIPI_agree","TIPI_consc","TIPI_neuro","TIPI_open",
               "ideo_con","ideo_lib","ideo_demsoc","ideo_lbrtn","ideo_prog",
               "party_id","edu","income","age","race_eth","gender","region","state")
  
  present_allowed <- intersect(allowed, names(df0))
  validate(need(length(present_allowed) > 0, "None of the requested grouping variables are present in the data."))
  
  labels <- var_info$label[match(present_allowed, var_info$var)]
  labels[is.na(labels) | !nzchar(labels)] <- present_allowed
  choices <- as.list(present_allowed); names(choices) <- labels
  updateSelectInput(session, "groupvar", choices = choices, selected = present_allowed[1])
  
  # State multi-select widget
  output$state_picker <- renderUI({
    req(input$groupvar)
    if (identical(input$groupvar, "state")) {
      states <- sort(unique(na.omit(as.character(df0$state))))
      selectizeInput("states", "States to show", choices = states, multiple = TRUE,
                     options = list(plugins = list("remove_button"),
                                    placeholder = "Select one or more states"))
    } else {
      NULL
    }
  })
  
  # Description for chosen variable
  output$chosen_desc <- renderText({
    req(input$groupvar)
    pretty_desc(input$groupvar, var_info)
  })
  
  # Build UI blocks per subgroup (table + plot)
  output$tables_ui <- renderUI({
    req(input$groupvar)
    grp <- input$groupvar
    
    if (identical(grp, "state")) {
      levs <- input$states
      validate(need(length(levs) >= 1, "Select at least one state to display tables."))
    } else {
      levs <- sort(unique(na.omit(as.character(df0[[grp]]))))
      validate(need(length(levs) >= 1, "No categories found for the selected variable."))
    }
    
    # One wellPanel per level
    out_list <- lapply(seq_along(levs), function(i) {
      lv <- levs[i]
      df_sub <- df0[which(as.character(df0[[grp]]) == lv), , drop = FALSE]
      vt <- build_value_table(df_sub)
      n_people <- count_people(df_sub)
      
      # dynamic ids
      tbl_id  <- paste0("tbl_",  i)
      plot_id <- paste0("plot_", i)
      
      wellPanel(
        tags$h4(paste0(pretty_label(grp, var_info), ": ", lv, "  —  N = ", n_people)),
        tableOutput(outputId = tbl_id),
        tags$div(style = "height:12px;"),  # small spacer
        plotOutput(outputId = plot_id, height = "300px")
      )
    })
    
    # Register renderers for each level (table + plot)
    for (i in seq_along(levs)) {
      local({
        idx <- i
        lv  <- levs[idx]
        df_sub <- df0[which(as.character(df0[[grp]]) == lv), , drop = FALSE]
        vt <- build_value_table(df_sub)
        
        # -- Table render (formatted decimals for numeric cols) --
        fmt <- vt
        num_cols <- names(fmt)[sapply(fmt, is.numeric)]
        for (cn in num_cols) fmt[[cn]] <- sprintf("%.3f", fmt[[cn]])
        output[[paste0("tbl_", idx)]] <- renderTable(
          fmt,
          striped = TRUE, bordered = TRUE, hover = TRUE, spacing = "s"
        )
        
        # -- Plot render (horizontal grouped bars, Promise vs Delivery) --
        output[[paste0("plot_", idx)]] <- renderPlot({
          # Use unformatted numeric data for plotting
          vals   <- vt$Value
          prom   <- suppressWarnings(as.numeric(vt[["Perceived Promise"]]))
          deliv  <- suppressWarnings(as.numeric(vt[["Perceived Delivery"]]))
          
          # Handle all-NA edge cases gracefully
          if (all(is.na(prom)) && all(is.na(deliv))) {
            plot.new()
            title(main = "No numeric data to plot")
            return(invisible(NULL))
          }
          prom[is.na(prom)]  <- 0
          deliv[is.na(deliv)] <- 0
          
          M <- rbind(Promise = prom, Delivery = deliv)
          
          # Make sure labels have room on the left
          oldpar <- par(no.readonly = TRUE)
          on.exit(par(oldpar), add = TRUE)
          par(mar = c(4, 10, 2, 2))  # bottom, left, top, right
          
          # Choose a sensible xlim to fit both series
          xmax <- max(M, na.rm = TRUE)
          xmin <- min(M, na.rm = TRUE)
          if (!is.finite(xmin)) xmin <- 0
          if (!is.finite(xmax)) xmax <- 1
          xlim <- range(0, xmin, xmax)  # include 0 for reference
          
          # Colors & legend
          cols <- c("#4C78A8", "#F58518")  # blue for Promise, orange for Delivery
          
          bp <- barplot(
            M,
            beside = TRUE,
            horiz  = TRUE,
            names.arg = vals,
            las = 1,            # horizontal y-axis labels
            xlim = xlim,
            col  = cols,
            border = NA,
            cex.names = 0.9
          )
          
          grid(col = "#eaeaea")
          legend("bottomright",
                 legend = c("Perceived Promise", "Perceived Delivery"),
                 fill = cols, bty = "n", cex = 0.9, inset = 0.02)
        })
      })
    }
    
    tagList(out_list)
  })
}

shinyApp(ui, server)
