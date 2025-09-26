# study-1/lm-table-app/app.R
library(shiny)

# ---- Data loaders (local files: shinylive-safe) ----
load_data <- function() {
  p <- "data/df_bsc_elg.csv"
  if (!file.exists(p)) stop("Missing data/df_bsc_elg.csv in study-1/lm-table-app/data/")
  read.csv(p, check.names = FALSE, stringsAsFactors = FALSE)
}
load_var_info <- function() {
  p <- "data/var_info.csv"
  if (!file.exists(p)) return(data.frame(var=character(),label=character(),description=character()))
  vi <- read.csv(p, check.names = FALSE, stringsAsFactors = FALSE)
  names(vi) <- tolower(names(vi))
  if (!"var" %in% names(vi)) vi$var <- character(0)
  if (!"label" %in% names(vi)) vi$label <- vi$var
  if (!"description" %in% names(vi)) vi$description <- ""
  vi$var <- trimws(vi$var); vi$label <- trimws(vi$label); vi$description <- trimws(vi$description)
  unique(vi[c("var","label","description")])
}

# ---- Helpers ----
is_numlike <- function(x) {
  if (is.numeric(x)) return(TRUE)
  if (!is.character(x)) return(FALSE)
  ok <- suppressWarnings(!is.na(as.numeric(x)))
  mean(ok, na.rm = TRUE) > 0.9
}
coerce_for_lm <- function(df) {
  # Numeric-like -> numeric; everything else -> factor (safe for lm)
  for (nm in names(df)) {
    if (is_numlike(df[[nm]])) {
      df[[nm]] <- suppressWarnings(as.numeric(df[[nm]]))
    } else if (!is.factor(df[[nm]])) {
      df[[nm]] <- factor(trimws(as.character(df[[nm]])))
    }
  }
  df
}

ui <- fluidPage(
  titlePanel("Broken Social Contract — Linear Model Table"),
  sidebarLayout(
    sidebarPanel(
      helpText("Build a linear model and view a tidy coefficient table."),
      # ORDER: Predictor → Outcome → Moderator → Controls
      selectInput("xvar", "Predictor (X)", choices = NULL),
      selectInput("yvar", "Outcome (Y)", choices = NULL),
      selectInput("zvar", "Moderator (Z) — optional", choices = NULL),
      selectizeInput(
        "controls", "Controls (0+)", choices = NULL, multiple = TRUE,
        options = list(
          plugins = list("remove_button"),
          placeholder = "Add any variables"
        )
      ),
      tags$hr(),
      helpText("Model:"),
      verbatimTextOutput("formtxt")
    ),
    mainPanel(
      tableOutput("lm_table"),
      tags$hr(),
      tags$h4("Selected variable info"),
      tableOutput("var_info_table"),
      tags$hr(),
      tags$h4("Notes"),
      tags$p("• Labels/descriptions are used only for variables present in var_info.csv; others show raw names."),
      tags$p("• If a moderator is chosen, the model includes main effects and the interaction (X × Z).")
    )
  )
)

server <- function(input, output, session) {
  df0 <- load_data(); names(df0) <- trimws(names(df0))
  var_info <- load_var_info()
  
  # Use ALL variables in the dataset for choices
  available_all <- names(df0)
  validate(need(length(available_all) >= 2, "Need at least two variables in the dataset."))
  
  # Label list using var_info where available; fall back to var name
  labs <- var_info$label[match(available_all, var_info$var)]
  labs[is.na(labs) | !nzchar(labs)] <- available_all
  choices_all <- as.list(available_all); names(choices_all) <- labs
  
  # Initialize selects (Predictor first, Outcome second)
  updateSelectInput(session, "xvar", choices = choices_all, selected = available_all[1])
  updateSelectInput(session, "yvar", choices = choices_all,
                    selected = available_all[min(2, length(available_all))])
  updateSelectInput(session, "zvar", choices = c("None" = "", choices_all), selected = "")
  
  # Controls from the entire dataset (excluding current Y/X/Z dynamically)
  observe({
    cur_exclude <- unique(c(input$yvar, input$xvar, input$zvar))
    all_controls <- setdiff(names(df0), cur_exclude[cur_exclude != ""])
    lab_ctrl <- var_info$label[match(all_controls, var_info$var)]
    lab_ctrl[is.na(lab_ctrl) | !nzchar(lab_ctrl)] <- all_controls
    ctrl_choices <- as.list(all_controls); names(ctrl_choices) <- lab_ctrl
    updateSelectizeInput(session, "controls", choices = ctrl_choices, server = TRUE)
  })
  
  # Pretty label lookup (supports interactions like "a:b" => "Label(a) × Label(b)")
  term_label <- function(term) {
    if (term == "(Intercept)") return("Intercept")
    parts <- strsplit(term, ":", fixed = TRUE)[[1]]
    labs <- var_info$label[match(parts, var_info$var)]
    labs[is.na(labs) | !nzchar(labs)] <- parts
    if (length(parts) > 1) paste(labs, collapse = " × ") else labs
  }
  
  # Label & description helpers for selected variables
  get_label <- function(v) {
    lb <- var_info$label[match(v, var_info$var)]
    ifelse(is.na(lb) | !nzchar(lb), v, lb)
  }
  get_desc <- function(v) {
    ds <- var_info$description[match(v, var_info$var)]
    ifelse(is.na(ds) | !nzchar(ds), "", ds)
  }
  
  # Build formula string and model
  model_spec <- reactive({
    req(input$yvar, input$xvar)
    y <- input$yvar
    x <- input$xvar
    z <- input$zvar
    ctrls <- input$controls
    rhs <- if (nzchar(z)) paste(c(x, z, paste0(x, ":", z), ctrls), collapse = " + ")
    else            paste(c(x, ctrls), collapse = " + ")
    as.formula(paste(y, "~", rhs))
  })
  
  output$formtxt <- renderText({
    f <- model_spec()
    paste(capture.output(f), collapse = "\n")
  })
  
  lm_fit <- reactive({
    f <- model_spec()
    d <- df0
    vars_needed <- all.vars(f)
    d <- d[, intersect(vars_needed, names(d)), drop = FALSE]
    d <- coerce_for_lm(d)
    lm(f, data = d)
  })
  
  output$lm_table <- renderTable({
    fit <- lm_fit()
    sm  <- summary(fit)
    co  <- sm$coefficients
    rn  <- rownames(co)
    df  <- sm$df[2]  # residual df
    
    out <- data.frame(
      term     = vapply(rn, term_label, character(1)),
      beta     = unname(co[, "Estimate"]),
      t        = unname(co[, "t value"]),
      df       = rep(df, length(rn)),
      p_value  = unname(co[, "Pr(>|t|)"]),
      stringsAsFactors = FALSE
    )
    
    # Formatting
    out$beta    <- sprintf("%.4f", out$beta)
    out$t       <- sprintf("%.3f", out$t)
    out$df      <- as.integer(out$df)
    out$p_value <- ifelse(out$p_value < .001, "< .001", sprintf("%.3f", out$p_value))
    out
  }, striped = TRUE, bordered = TRUE, hover = TRUE, spacing = "m")
  
  # Small table showing label/description for selected X/Y/Z
  output$var_info_table <- renderTable({
    x <- input$xvar; y <- input$yvar; z <- input$zvar
    rows <- list(
      c(role = "Predictor (X)", var = x, label = get_label(x), description = get_desc(x)),
      c(role = "Outcome (Y)",   var = y, label = get_label(y), description = get_desc(y))
    )
    if (nzchar(z)) {
      rows <- c(rows, list(c(role = "Moderator (Z)", var = z, label = get_label(z), description = get_desc(z))))
    }
    as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  }, striped = TRUE, bordered = TRUE, hover = TRUE, spacing = "s")
}

shinyApp(ui, server)
