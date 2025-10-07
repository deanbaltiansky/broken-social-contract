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

# Summary table builder
summarize_values <- function(df_sub) {
  df_sub %>%
    group_by(value) %>%
    summarise(
      `Perceived Promise` = mean(weight, na.rm = TRUE),
      `Perceived Delivery` = mean(weighted_score, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(Value = value) %>%
    select(Value, `Perceived Promise`, `Perceived Delivery`) %>%
    arrange(tolower(Value))
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
      helpText("Choose a characteristic to break out the summary tables:"),
      selectInput("groupvar", "Break down by", choices = NULL),
      uiOutput("state_picker"),
      tags$hr(),
      helpText("Notes:"),
      tags$p("• Each table shows average perceived promise and delivery for each of the eight values."),
      tags$p("• Numbers represent group means across respondents.")
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
  
  # state multi-select
  output$state_picker <- renderUI({
    req(input$groupvar)
    if (identical(input$groupvar, "state")) {
      states <- sort(unique(na.omit(as.character(df0$state))))
      selectizeInput("states", "States to show", choices = states, multiple = TRUE,
                     options = list(plugins = list("remove_button"),
                                    placeholder = "Select one or more states"))
    } else NULL
  })
  
  # variable description
  output$chosen_desc <- renderText({
    req(input$groupvar)
    pretty_desc(input$groupvar, var_info)
  })
  
  # main output
  output$tables_ui <- renderUI({
    req(input$groupvar)
    grp <- input$groupvar
    
    if (identical(grp, "state")) {
      levs <- input$states
      validate(need(length(levs) >= 1, "Select at least one state to display tables."))
    } else {
      levs <- sort(unique(na.omit(as.character(df0[[grp]]))))
    }
    
    out_list <- lapply(seq_along(levs), function(i) {
      lv <- levs[i]
      df_sub <- df0[df0[[grp]] == lv, , drop = FALSE]
      n_people <- count_people(df_sub)
      df_sum <- summarize_values(df_sub)
      
      tbl_id  <- paste0("tbl_", i)
      plot_id <- paste0("plot_", i)
      
      wellPanel(
        tags$h4(paste0(pretty_label(grp, var_info), ": ", lv, " — N = ", n_people)),
        tableOutput(tbl_id),
        tags$div(style="height:12px;"),
        plotOutput(plot_id, height="300px")
      )
    })
    
    # register outputs
    for (i in seq_along(levs)) {
      local({
        idx <- i
        lv  <- levs[idx]
        df_sub <- df0[df0[[grp]] == lv, , drop = FALSE]
        df_sum <- summarize_values(df_sub)
        
        # Table
        output[[paste0("tbl_", idx)]] <- renderTable({
          df_sum %>%
            mutate(
              `Perceived Promise` = sprintf("%.3f", `Perceived Promise`),
              `Perceived Delivery` = sprintf("%.3f", `Perceived Delivery`)
            )
        }, striped = TRUE, bordered = TRUE, hover = TRUE, spacing = "s")
        
        # Plot
        output[[paste0("plot_", idx)]] <- renderPlot({
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
            M,
            beside = TRUE,
            horiz = TRUE,
            names.arg = vals,
            las = 1,
            xlim = xlim,
            col = cols,
            border = NA,
            cex.names = 0.9
          )
          grid(col = "#eaeaea")
          legend("bottomright", legend = c("Perceived Promise", "Perceived Delivery"),
                 fill = cols, bty = "n", cex = 0.9, inset = 0.02)
        })
      })
    }
    
    tagList(out_list)
  })
}

shinyApp(ui, server)
