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

# Ideology variables
IDEO_VARS <- c("ideo_con","ideo_lib","ideo_lbrtn","ideo_demsoc","ideo_prog")

map_ideology_labels <- function(grp, levs_char) {
  if (!(grp %in% IDEO_VARS)) return(levs_char)
  lab0 <- switch(grp,
                 "ideo_con"   = "Not Conservatives",
                 "ideo_lib"   = "Not Liberals",
                 "ideo_lbrtn" = "Not Libertarians",
                 "ideo_demsoc"= "Not Democratic Socialists",
                 "ideo_prog"  = "Not Progressives"
  )
  lab1 <- switch(grp,
                 "ideo_con"   = "Conservatives",
                 "ideo_lib"   = "Liberals",
                 "ideo_lbrtn" = "Libertarians",
                 "ideo_demsoc"= "Democratic Socialists",
                 "ideo_prog"  = "Progressives"
  )
  out <- levs_char
  out[levs_char %in% c("0","0.0", "0L")] <- lab0
  out[levs_char %in% c("1","1.0", "1L")] <- lab1
  out
}

ideology_desc_override <- function(grp) {
  if (grp %in% IDEO_VARS) {
    "Anyone above the mid-point (4) on the ideology scale was counted as part of this ideological group."
  } else ""
}

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

build_consolidated_table <- function(df, grp, categories) {
  vals_all <- sort(unique(df$value))
  out <- data.frame(Value = sort(vals_all), stringsAsFactors = FALSE)
  for (cat in categories) {
    sub <- df[df[[grp]] == cat, , drop = FALSE]
    if (nrow(sub) == 0) {
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
      tags$p("• For all other variables: a consolidated table (superordinate headers) and paneled plots (one panel per category).")
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
  
  split_vars <- c("age","race_eth","region","state")
  
  output$state_picker <- renderUI({
    req(input$groupvar)
    if (identical(input$groupvar, "state")) {
      states <- sort(unique(na.omit(as.character(df0$state))))
      selectizeInput("states", "States to show", choices = states, multiple = TRUE,
                     options = list(plugins = list("remove_button"),
                                    placeholder = "Select one or more states"))
    } else NULL
  })
  
  output$chosen_desc <- renderText({
    req(input$groupvar)
    base <- pretty_desc(input$groupvar, var_info)
    over <- ideology_desc_override(input$groupvar)
    txt <- paste(c(base, over), collapse = if (nzchar(base) && nzchar(over)) " " else "")
    txt
  })
  
  output$main_ui <- renderUI({
    req(input$groupvar)
    grp <- input$groupvar
    
    if (grp %in% split_vars) {
      # ---- Split mode ----
      levs_raw <- if (grp == "state") input$states else sort(unique(na.omit(as.character(df0[[grp]]))))
      validate(need(length(levs_raw) >= 1, "Select at least one category to display."))
      
      panels <- lapply(seq_along(levs_raw), function(i) {
        lv <- levs_raw[i]
        df_sub <- df0[df0[[grp]] == lv, , drop = FALSE]
        n_people <- count_people(df_sub)
        sum_df <- summarize_values(df_sub)
        
        tbl_id  <- paste0("tbl_", i)
        plot_id <- paste0("plot_", i)
        
        local({
          df_sum <- sum_df
          output[[tbl_id]] <- renderTable({
            df_sum %>%
              mutate(
                `Perceived Promise` = sprintf("%.3f", `Perceived Promise`),
                `Perceived Delivery` = sprintf("%.3f", `Perceived Delivery`)
              )
          }, striped = TRUE, bordered = TRUE, hover = TRUE, spacing = "s")
        })
        
        local({
          df_sum <- sum_df
          output[[plot_id]] <- renderPlot({
            vals <- df_sum$Value
            prom <- df_sum$`Perceived Promise`
            del  <- df_sum$`Perceived Delivery`
            if (all(is.na(prom)) && all(is.na(del))) {
              plot.new(); title("No numeric data to plot"); return()
            }
            xlim <- range(0, prom, del, na.rm = TRUE)
            oldpar <- par(no.readonly = TRUE); on.exit(par(oldpar), add = TRUE)
            par(mfrow = c(2,1), mar = c(1,10,2,2), oma = c(4,0,0,0))
            barplot(prom, horiz = TRUE, names.arg = vals, las = 1, xlim = xlim,
                    col = "lightblue", border = "black", cex.names = 0.9) -> mp
            rect(xleft = 0, ybottom = mp - 0.4, xright = del, ytop = mp + 0.4,
                 col = adjustcolor("grey50", alpha.f = 0.5), border = "black")
            abline(v = axTicks(1), col = "grey70", lwd = 1)
            par(mar = c(0,0,0,0))
            plot.new()
            legend("center",
                   legend = c("Perceived Promise", "Perceived Delivery"),
                   fill   = c("lightblue", adjustcolor("grey50", 0.5)),
                   border = c("black", "black"),
                   bty = "n", horiz = TRUE, cex = 0.9, xpd = NA)
          })
        })
        
        wellPanel(
          tags$h4(paste0(pretty_label(grp, var_info), ": ", lv, " — N = ", n_people)),
          tableOutput(tbl_id),
          tags$div(style="height:12px;"),
          plotOutput(plot_id, height = "350px")
        )
      })
      tagList(panels)
      
    } else {
      # ---- Consolidated mode ----
      levs_raw <- sort(unique(na.omit(as.character(df0[[grp]]))))
      levs_disp <- if (grp %in% IDEO_VARS) map_ideology_labels(grp, levs_raw) else levs_raw
      cons_tbl <- build_consolidated_table(df0, grp, levs_raw)
      
      tbl_id <- "cons_table"
      output[[tbl_id]] <- renderUI({
        header_top <- tags$tr(
          tags$th("Value", style="border:1px solid #ddd; padding:6px; background:#f2f2f2; text-align:left;"),
          lapply(seq_along(levs_disp), function(j) {
            tags$th(colspan = 2,
                    style="border:1px solid #ddd; padding:6px; background:#f2f2f2; text-align:center;",
                    levs_disp[j])
          })
        )
        header_sub <- tags$tr(
          tags$th("", style="border:1px solid #ddd; padding:6px;"),
          lapply(rep(1, length(levs_disp)), function(i) {
            list(
              tags$th("Promise",  style="border:1px solid #ddd; padding:6px; text-align:center;"),
              tags$th("Delivery", style="border:1px solid #ddd; padding:6px; text-align:center;")
            )
          })
        )
        body_rows <- apply(cons_tbl, 1, function(row) {
          cells <- list(tags$td(row[[1]], style="border:1px solid #ddd; padding:6px; text-align:left;"))
          for (j in 2:length(row)) {
            val <- suppressWarnings(as.numeric(row[[j]]))
            if (is.na(val)) {
              cells <- c(cells, list(tags$td("", style="border:1px solid #ddd; padding:6px; text-align:center;")))
            } else {
              cells <- c(cells, list(tags$td(sprintf('%.3f', val), style="border:1px solid #ddd; padding:6px; text-align:center;")))
            }
          }
          do.call(tags$tr, cells)
        })
        tags$table(
          style="border-collapse:collapse; width:100%; table-layout:fixed; font-size:0.9em;",
          header_top, header_sub, body_rows
        )
      })
      
      plot_id <- "cons_plot"
      output[[plot_id]] <- renderPlot({
        per_cat <- lapply(seq_along(levs_raw), function(j) {
          lv_raw <- levs_raw[j]
          lv_lab <- levs_disp[j]
          sub <- df0[df0[[grp]] == lv_raw, , drop = FALSE]
          s   <- summarize_values(sub)
          list(vals = s$Value, prom = s$`Perceived Promise`, del = s$`Perceived Delivery`, lab = lv_lab)
        })
        xr <- range(0, unlist(lapply(per_cat, function(pc) c(pc$prom, pc$del))), na.rm = TRUE)
        n <- length(per_cat)
        cols <- if (n <= 2) 2 else if (n <= 4) 2 else if (n <= 6) 3 else 3
        rows <- ceiling(n / cols)
        oldpar <- par(no.readonly = TRUE); on.exit(par(oldpar), add = TRUE)
        lay_mat <- matrix(seq_len(rows * cols), nrow = rows, byrow = TRUE)
        lay_mat <- rbind(lay_mat, rep(max(lay_mat) + 1, ncol(lay_mat)))
        layout(lay_mat, heights = c(rep(1, rows), 0.30))
        par(mar = c(4.5, 9.5, 2, 1))
        for (i in seq_len(n)) {
          pc <- per_cat[[i]]
          mp <- barplot(pc$prom, horiz = TRUE, names.arg = pc$vals, las = 1, xlim = xr,
                        col = "lightblue", border = "black", cex.names = 0.85, main = pc$lab)
          rect(xleft = 0, ybottom = mp - 0.4, xright = pc$del, ytop = mp + 0.4,
               col = adjustcolor("grey50", alpha.f = 0.5), border = "black")
          abline(v = axTicks(1), col = "grey70", lwd = 1)
        }
        par(mar = c(0,0,0,0))
        plot.new()
        legend("center",
               legend = c("Perceived Promise", "Perceived Delivery"),
               fill   = c("lightblue", adjustcolor("grey50", 0.5)),
               border = c("black", "black"),
               bty = "n", horiz = TRUE, cex = 0.95, xpd = NA)
      })
      
      tagList(
        h4(pretty_label(input$groupvar, var_info)),
        uiOutput(tbl_id),
        tags$div(style="height:12px;"),
        plotOutput(plot_id, height = "480px")
      )
    }
  })
}

shinyApp(ui, server)
