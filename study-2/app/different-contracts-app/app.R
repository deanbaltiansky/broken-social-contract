# study-2/app/different-contracts-app/app.R
library(shiny)
library(dplyr)

# -----------------------------
# Data loaders
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
  if ("PID" %in% names(df_sub)) return(length(unique(df_sub$PID)))
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

# Build per-group summary table (8 rows; means by value)
summarize_values <- function(df_sub) {
  df_sub %>%
    group_by(value) %>%
    summarise(
      `Perceived Promise` = mean(weight, na.rm = TRUE),
      `Perceived Delivery` = mean(weighted_score, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(Value = value) %>%
    select(Value, `Perceived Promise`, `Perceived Delivery`) %>%
    arrange(tolower(Value))
}

# Build consolidated wide table: one row per Value; for each category, two columns
build_consolidated_table <- function(df, grp, categories) {
  # For each category -> summarise, then join
  vals_all <- sort(unique(df$value))
  out <- data.frame(Value = sort(vals_all), stringsAsFactors = FALSE)
  for (cat in categories) {
    sub <- df[df[[grp]] == cat, , drop = FALSE]
    if (nrow(sub) == 0) {
      # empty: add NA columns
      out[[paste0(cat, " — Promise")]]  <- NA_real_
      out[[paste0(cat, " — Delivery")]] <- NA_real_
      next
    }
    s <- summarize_values(sub)
    names(s) <- c("Value", "Promise", "Delivery")
    out <- out %>%
      left_join(s, by = "Value") %>%
      rename(!!paste0(cat, " — Promise") := Promise,
             !!paste0(cat, " — Delivery") := Delivery)
  }
  out
}

# Pattern assignment for categories in consolidated plot
# returns vectors density, angle (recycled as needed)
pattern_for_categories <- function(n) {
  # First 6 distinct patterns; recycle afterwards
  dens <- c(0, 20, 40, 20, 40, 60)   # 0 = solid; >0 = hatched
  ang  <- c(0, 45, 90, 135, 30, 60)
  list(
    density = rep(dens, length.out = n),
    angle   = rep(ang,  length.out = n)
  )
}

# -----------------------------
# UI
# -----------------------------
ui <- fluidPage(
  titlePanel("Different Contracts for Different People"),
  tags$p(
    "In this app, you can explore how different sub-populations of the American public see the social contract. ",
    "Specifically, you can see what different groups believe the U.S.'s promise is on paper, and right next to it, ",
    "how they believe the U.S. government is doing in the delivery on that promise. We arrived at these numbers from ",
    "the way people assigned weights to what they believe the U.S. stands for on paper and from the weighted score ",
    "they gave to the U.S. government (across administrations) on each of the dimensions."
  ),
  sidebarLayout(
    sidebarPanel(
      helpText("Choose a characteristic to explore:"),
      selectInput("groupvar", "Break down by", choices = NULL),
      uiOutput("state_picker"),
      tags$hr(),
      helpText("Display modes:"),
      tags$p("• For age, race/ethnicity, region, and state: separate tables and plots for each category."),
      tags$p("• For all other variables: a consolidated table (superordinate headers) and one combined plot with patterned fills per category.")
    ),
    mainPanel(
      uiOutput("main_ui"),
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
  
  # Variables that use split mode
  split_vars <- c("age","race_eth","region","state")
  
  # State picker (only for state)
  output$state_picker <- renderUI({
    req(input$groupvar)
    if (identical(input$groupvar, "state")) {
      states <- sort(unique(na.omit(as.character(df0$state))))
      selectizeInput("states", "States to show", choices = states, multiple = TRUE,
                     options = list(plugins = list("remove_button"),
                                    placeholder = "Select one or more states"))
    } else NULL
  })
  
  # Description
  output$chosen_desc <- renderText({
    req(input$groupvar)
    pretty_desc(input$groupvar, var_info)
  })
  
  # Top-level UI switches between split vs consolidated
  output$main_ui <- renderUI({
    req(input$groupvar)
    grp <- input$groupvar
    
    if (grp %in% split_vars) {
      # ---- Split mode (one panel per category) ----
      if (identical(grp, "state")) {
        levs <- input$states
        validate(need(length(levs) >= 1, "Select at least one state to display."))
      } else {
        levs <- sort(unique(na.omit(as.character(df0[[grp]]))))
      }
      
      # build a wellPanel per level
      panels <- lapply(seq_along(levs), function(i) {
        lv <- levs[i]
        df_sub <- df0[df0[[grp]] == lv, , drop = FALSE]
        n_people <- count_people(df_sub)
        sum_df <- summarize_values(df_sub)
        
        tbl_id  <- paste0("tbl_", i)
        plot_id <- paste0("plot_", i)
        
        # register table
        local({
          idx <- i; df_sum <- sum_df
          output[[tbl_id]] <- renderTable({
            df_sum %>%
              mutate(
                `Perceived Promise` = sprintf("%.3f", `Perceived Promise`),
                `Perceived Delivery` = sprintf("%.3f", `Perceived Delivery`)
              )
          }, striped = TRUE, bordered = TRUE, hover = TRUE, spacing = "s")
        })
        # register plot (two colors; one subgroup)
        local({
          idx <- i; df_sum <- sum_df
          output[[plot_id]] <- renderPlot({
            vals <- df_sum$Value
            prom <- df_sum$`Perceived Promise`
            del  <- df_sum$`Perceived Delivery`
            
            if (all(is.na(prom)) && all(is.na(del))) {
              plot.new(); title("No numeric data to plot"); return()
            }
            M <- rbind(Promise = prom, Delivery = del)
            cols <- c("#4C78A8", "#F58518")
            
            oldpar <- par(no.readonly = TRUE)
            on.exit(par(oldpar), add = TRUE)
            par(mar = c(4, 10, 2, 2))
            xlim <- range(0, M, na.rm = TRUE)
            
            barplot(
              M, beside = TRUE, horiz = TRUE,
              names.arg = vals, las = 1, xlim = xlim,
              col = cols, border = NA, cex.names = 0.9
            )
            grid(col = "#eaeaea")
            legend("bottomright",
                   legend = c("Perceived Promise", "Perceived Delivery"),
                   fill = cols, bty = "n", cex = 0.9, inset = 0.02)
          })
        })
        
        wellPanel(
          tags$h4(paste0(pretty_label(grp, var_info), ": ", lv, " — N = ", n_people)),
          tableOutput(tbl_id),
          tags$div(style="height:12px;"),
          plotOutput(plot_id, height = "300px")
        )
      })
      tagList(panels)
      
    } else {
      # ---- Consolidated mode ----
      levs <- sort(unique(na.omit(as.character(df0[[grp]]))))
      validate(need(length(levs) >= 2, "Not enough categories to consolidate."))
      
      # consolidated wide table
      cons_tbl <- build_consolidated_table(df0, grp, levs)
      
      # ---------- Table UI with superordinate headers ----------
      tbl_id <- "cons_table"
      output[[tbl_id]] <- renderUI({
        # Build a two-row header: first row has the categories (colspan=2 each),
        # second row has Promise / Delivery subheaders.
        header_top <- tags$tr(
          tags$th("Value", style="border:1px solid #ddd; padding:6px; background:#f2f2f2; text-align:left;"),
          lapply(levs, function(lv) {
            tags$th(colspan = 2, style="border:1px solid #ddd; padding:6px; background:#f2f2f2; text-align:center;",
                    lv)
          })
        )
        header_sub <- tags$tr(
          tags$th("", style="border:1px solid #ddd; padding:6px;"),
          lapply(rep(1, length(levs)), function(i) {
            list(
              tags$th("Promise",  style="border:1px solid #ddd; padding:6px; text-align:right;"),
              tags$th("Delivery", style="border:1px solid #ddd; padding:6px; text-align:right;")
            )
          })
        )
        # body rows
        body_rows <- apply(cons_tbl, 1, function(row) {
          # row is a named vector: Value, then pairs of cols
          cells <- list(tags$td(row[[1]], style="border:1px solid #ddd; padding:6px; text-align:left;"))
          # format numbers to 3 d.p.
          for (j in 2:length(row)) {
            val <- suppressWarnings(as.numeric(row[[j]]))
            if (is.na(val)) {
              cells <- c(cells, list(tags$td("", style="border:1px solid #ddd; padding:6px; text-align:right;")))
            } else {
              cells <- c(cells, list(tags$td(sprintf('%.3f', val), style="border:1px solid #ddd; padding:6px; text-align:right;")))
            }
          }
          do.call(tags$tr, cells)
        })
        
        tags$table(
          style="border-collapse:collapse; width:100%; table-layout:fixed; font-size:0.9em;",
          header_top, header_sub, body_rows
        )
      })
      
      # ---------- Consolidated plot ----------
      plot_id <- "cons_plot"
      output[[plot_id]] <- renderPlot({
        # Build a matrix M with rows = (for each category: Promise, then Delivery), columns = Values
        vals <- sort(unique(df0$value))
        # For consistent ordering with cons_tbl:
        vals <- cons_tbl$Value
        
        # For each category, pull Promise/Delivery vectors aligned to 'vals'
        prom_list <- list()
        delv_list <- list()
        for (lv in levs) {
          pcol <- paste0(lv, " — Promise")
          dcol <- paste0(lv, " — Delivery")
          prom_list[[lv]] <- as.numeric(cons_tbl[[pcol]])
          delv_list[[lv]] <- as.numeric(cons_tbl[[dcol]])
        }
        # Assemble rows: [cat1-Promise, cat1-Delivery, cat2-Promise, cat2-Delivery, ...]
        rows_mat <- do.call(rbind, unlist(mapply(function(p, d) list(p, d), prom_list, delv_list, SIMPLIFY = FALSE), recursive = FALSE))
        if (is.null(rows_mat)) {
          plot.new(); title("No data to plot"); return()
        }
        rownames(rows_mat) <- as.vector(unlist(lapply(levs, function(lv) c(paste0(lv," — Promise"), paste0(lv," — Delivery")))))
        
        # Colors by measure: Promise (blue), Delivery (orange)
        # Patterns by category: solid/striped/dotted/etc.
        meas_cols <- c("#4C78A8", "#F58518")
        # row color vector: alternate blue/orange
        row_colors <- rep(meas_cols, times = length(levs))
        
        # density/angle per category, repeated for Promise and Delivery rows
        pat <- pattern_for_categories(length(levs))
        row_density <- rep(pat$density, each = 2)
        row_angle   <- rep(pat$angle,   each = 2)
        
        oldpar <- par(no.readonly = TRUE)
        on.exit(par(oldpar), add = TRUE)
        par(mar = c(4, 12, 2, 2))
        
        # x-range across all rows
        xr <- range(0, rows_mat, na.rm = TRUE)
        
        bp <- barplot(
          rows_mat,
          beside = TRUE,
          horiz  = TRUE,
          names.arg = vals,
          las = 1,
          xlim = xr,
          col = row_colors,
          density = row_density,
          angle   = row_angle,
          border  = NA,
          cex.names = 0.9
        )
        grid(col = "#eaeaea")
        
        # Legends: one for color (measure), one for pattern (category)
        legend("bottomright",
               legend = c("Perceived Promise", "Perceived Delivery"),
               fill = meas_cols, bty = "n", cex = 0.9, inset = 0.02)
        
        # Pattern legend: draw small proxy bars with matching density/angle but neutral color
        # We'll use grey to focus on pattern
        par(xpd = NA)
        legend_text <- levs
        legend_fill <- rep("grey50", length(levs))
        legend_density <- pat$density
        legend_angle   <- pat$angle
        legend("bottomleft",
               legend = legend_text,
               fill = legend_fill,
               density = legend_density,
               angle = legend_angle,
               border = NA,
               bty = "n", cex = 0.9, inset = 0.02,
               title = "Category pattern")
      })
      
      # Compose consolidated UI
      tagList(
        h4(paste0(pretty_label(input$groupvar, var_info), " — Consolidated")),
        uiOutput(tbl_id),
        tags$div(style="height:12px;"),
        plotOutput(plot_id, height = "420px")
      )
    }
  })
}

shinyApp(ui, server)
