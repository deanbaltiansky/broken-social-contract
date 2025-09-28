# study-1/app/lm-table-app/app.R
library(shiny)
library(ggplot2)

`%||%` <- function(x, y) if (is.null(x)) y else x

# ---- Data loaders (shinylive-safe local files) ----
load_data <- function() {
  p <- "data/df_bsc_elg.csv"
  if (!file.exists(p)) stop("Missing data/df_bsc_elg.csv in study-1/app/lm-table-app/data/")
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
          persist = TRUE,
          openOnFocus = TRUE,
          closeAfterSelect = FALSE,
          placeholder = "Add any variables"
        )
      ),
      tags$hr(),
      helpText("Model:"),
      verbatimTextOutput("formtxt")
    ),
    mainPanel(
      tableOutput("lm_table"),
      plotOutput("viz", height = 420),
      tags$hr(),
      tags$h4("Selected variable info"),
      tableOutput("var_info_table"),
      tags$hr(),
      tags$h4("Notes"),
      tags$p("• Only variables listed in var_info.csv (and present in the data) are available."),
      tags$p("• If a moderator is chosen, the model includes main effects and the interaction (X × Z)."),
      tags$p("• Data were collected in March 2024. Therefore, voting variables indicate voting intentions, rather than voting behavior."),
      uiOutput("mod_note")
    )
  )
)

server <- function(input, output, session) {
  df0 <- load_data(); names(df0) <- trimws(names(df0))
  var_info <- load_var_info()
  
  # Restrict to vars that are BOTH listed in var_info and present in data
  listed <- unique(trimws(var_info$var))
  available_all <- intersect(names(df0), listed)
  validate(need(length(available_all) >= 2,
                "Need at least two variables that are listed in var_info.csv and present in the dataset."))
  
  # Binary moderator whitelist
  bin_vars <- c("republican","democrat","independent",
                "vote_2024_trump","vote_2024_biden","vote_2024_rfkj","vote_2024_other",
                "white","man")
  
  # Label helpers
  lab_of <- function(v) {
    lb <- var_info$label[match(v, var_info$var)]
    ifelse(is.na(lb) | !nzchar(lb), v, lb)
  }
  get_desc <- function(v) {
    ds <- var_info$description[match(v, var_info$var)]
    ifelse(is.na(ds) | !nzchar(ds), "", ds)
  }
  
  # Choices
  labs <- lab_of(available_all)
  choices_all <- as.list(available_all); names(choices_all) <- labs
  
  # Initialize selects
  updateSelectInput(session, "xvar", choices = choices_all, selected = available_all[1])
  updateSelectInput(session, "yvar", choices = choices_all, selected = available_all[min(2, length(available_all))])
  updateSelectInput(session, "zvar", choices = c("None" = "", choices_all), selected = "")
  
  # Controls: prevent flicker by updating only when pool changes
  prev_ctrl_pool <- reactiveVal(character(0))
  observe({
    cur_exclude <- unique(c(input$yvar, input$xvar, input$zvar))
    cur_exclude <- cur_exclude[nzchar(cur_exclude)]
    ctrl_pool <- setdiff(available_all, cur_exclude)
    
    if (identical(ctrl_pool, prev_ctrl_pool())) return(NULL)
    
    lab_ctrl <- lab_of(ctrl_pool)
    ctrl_choices <- as.list(ctrl_pool); names(ctrl_choices) <- lab_ctrl
    keep <- intersect(isolate(input$controls) %||% character(0), ctrl_pool)
    
    freezeReactiveValue(input, "controls")
    updateSelectizeInput(session, "controls",
                         choices = ctrl_choices,
                         selected = keep,
                         server = TRUE
    )
    prev_ctrl_pool(ctrl_pool)
  })
  
  # Pretty term label (handles interactions)
  term_label <- function(term) {
    if (term == "(Intercept)") return("Intercept")
    parts <- strsplit(term, ":", fixed = TRUE)[[1]]
    lbls  <- lab_of(parts)
    if (length(parts) > 1) paste(lbls, collapse = " × ") else lbls
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
    lm(f, data = d, na.action = na.omit)
  })
  
  # ----- Coefficient table -----
  output$lm_table <- renderTable({
    sm <- summary(lm_fit())
    co <- sm$coefficients
    rn <- rownames(co)
    rdf <- sm$df[2]
    
    out <- data.frame(
      term     = vapply(rn, term_label, character(1)),
      beta     = sprintf("%.4f", unname(co[, "Estimate"])),
      t        = sprintf("%.3f", unname(co[, "t value"])),
      df       = as.integer(rdf),
      p_value  = ifelse(unname(co[, "Pr(>|t|)"]) < .001, "< .001",
                        sprintf("%.3f", unname(co[, "Pr(>|t|)"]))),
      stringsAsFactors = FALSE
    )
    out
  }, striped = TRUE, bordered = TRUE, hover = TRUE, spacing = "m")
  
  # ----- Visualization (right after table) -----
  output$viz <- renderPlot({
    fit <- lm_fit()
    d_fit <- model.frame(fit)  # data actually used after na.omit
    x <- input$xvar; y <- input$yvar; z <- input$zvar
    
    # Helper: typical values for controls (mean for numeric, mode for factor/character)
    typical_val <- function(v) {
      vv <- d_fit[[v]]
      if (is.numeric(vv)) {
        mean(vv, na.rm = TRUE)
      } else if (is.factor(vv)) {
        lev <- names(sort(table(vv), decreasing = TRUE))[1]
        factor(lev, levels = levels(vv))
      } else {
        tab <- sort(table(vv), decreasing = TRUE)
        names(tab)[1]
      }
    }
    
    # Build X sequence
    xv <- d_fit[[x]]
    if (!is.numeric(xv)) {
      # Non-numeric X: fallback to jitter + category means
      p <- ggplot(d_fit, aes_string(x = x, y = y)) +
        geom_jitter(alpha = 0.3, width = 0.2, height = 0, size = 1) +
        stat_summary(fun = mean, geom = "point", size = 3) +
        labs(x = lab_of(x), y = lab_of(y), title = "Mean outcome by X (X is non-numeric)") +
        theme_minimal()
      print(p)
      return(invisible(NULL))
    }
    x_seq <- seq(min(xv, na.rm = TRUE), max(xv, na.rm = TRUE), length.out = 100)
    
    # Base row with typical values for all vars in the fit
    nd_base <- setNames(vector("list", length = length(names(d_fit))), names(d_fit))
    for (nm in names(d_fit)) nd_base[[nm]] <- typical_val(nm)
    nd_base <- as.data.frame(nd_base, stringsAsFactors = FALSE)
    nd_base[[x]] <- x_seq
    
    # Decide moderator handling
    use_z <- nzchar(z)
    z_is_numeric <- use_z && is.numeric(d_fit[[z]])
    z_is_binary <- use_z && (z %in% bin_vars)
    
    # Base plot
    p <- ggplot(d_fit, aes_string(x = x, y = y)) +
      geom_point(alpha = 0.25, size = 1) +
      labs(x = lab_of(x), y = lab_of(y)) +
      theme_minimal()
    
    if (use_z && z_is_binary) {
      # Two lines for z = 0 and z = 1
      z_vals <- c(0, 1)
      z_labs <- c("0", "1")
      
      # If the fit used a factor for z, coerce to factor with original levels containing "0"/"1"
      z_in_fit <- d_fit[[z]]
      is_factor_z <- is.factor(z_in_fit)
      pred_lines <- lapply(seq_along(z_vals), function(i) {
        nd <- nd_base
        if (is_factor_z) {
          levs <- levels(z_in_fit)
          # If levels don't include "0"/"1", try to coerce; else fallback to first/second level
          if (all(c("0","1") %in% levs)) {
            nd[[z]] <- factor(z_labs[i], levels = levs)
          } else {
            nd[[z]] <- factor(levs[i], levels = levs)
            z_labs[i] <<- levs[i]  # keep legend honest
          }
        } else {
          nd[[z]] <- z_vals[i]
        }
        nd$.__label__ <- z_labs[i]
        nd$.__yhat__  <- as.numeric(predict(fit, newdata = nd))
        nd
      })
      pred_df <- do.call(rbind, pred_lines)
      
      p <- p +
        geom_line(data = pred_df,
                  aes(x = !!sym(x), y = .__yhat__, color = .__label__),
                  linewidth = 1.1) +
        labs(title = paste0("Effect of ", lab_of(x), " on ", lab_of(y),
                            " by ", lab_of(z), " (0 vs 1)")) +
        scale_color_discrete(name = lab_of(z))
      output$mod_note <- renderUI(HTML("&nbsp;"))
      
    } else if (use_z && z_is_numeric) {
      # Three simple slopes: -1SD, mean, +1SD
      z_mean <- mean(d_fit[[z]], na.rm = TRUE)
      z_sd   <- sd(d_fit[[z]], na.rm = TRUE)
      z_vals <- c(z_mean - z_sd, z_mean, z_mean + z_sd)
      z_labs <- c("-1 SD", "Mean", "+1 SD")
      
      pred_lines <- lapply(seq_along(z_vals), function(i) {
        nd <- nd_base
        nd[[z]] <- z_vals[i]
        nd$.__label__ <- z_labs[i]
        nd$.__yhat__  <- as.numeric(predict(fit, newdata = nd))
        nd
      })
      pred_df <- do.call(rbind, pred_lines)
      
      p <- p +
        geom_line(data = pred_df,
                  aes(x = !!sym(x), y = .__yhat__, color = .__label__),
                  linewidth = 1.1) +
        labs(title = paste0("Simple slopes at ", lab_of(z), " = {−1 SD, mean, +1 SD}")) +
        scale_color_discrete(name = lab_of(z))
      output$mod_note <- renderUI(HTML("&nbsp;"))
      
    } else {
      # No moderator or non-numeric moderator: single model-implied line
      nd <- nd_base
      nd$.__yhat__ <- as.numeric(predict(fit, newdata = nd))
      p <- p +
        geom_line(data = nd, aes(x = !!sym(x), y = .__yhat__), linewidth = 1.1) +
        labs(title = "Model-implied trend")
      if (use_z && !z_is_numeric) {
        output$mod_note <- renderUI({
          tags$p(style="color:#666;",
                 paste("Moderator", shQuote(lab_of(z)),
                       "is non-numeric; showing a single trend line at its typical value.")
          )
        })
      } else {
        output$mod_note <- renderUI(HTML("&nbsp;"))
      }
    }
    
    print(p)
  })
  
  # ----- Variable info table -----
  output$var_info_table <- renderTable({
    x <- input$xvar; y <- input$yvar; z <- input$zvar
    rows <- list(
      c(role = "Predictor (X)", var = x, label = lab_of(x), description = get_desc(x)),
      c(role = "Outcome (Y)",   var = y, label = lab_of(y), description = get_desc(y))
    )
    if (nzchar(z)) {
      rows <- c(rows, list(c(role = "Moderator (Z)", var = z, label = lab_of(z), description = get_desc(z))))
    }
    as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  }, striped = TRUE, bordered = TRUE, hover = TRUE, spacing = "s")
}

shinyApp(ui, server)