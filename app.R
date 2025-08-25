# app.R
library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(shinyWidgets)
library(shinyjs)
library(broom)   # for tidy()
library(stats)   # aov, TukeyHSD, shapiro.test, bartlett.test, kruskal.test

bcl <- read.csv("drinks_dataset.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
  useShinyjs(),
  titlePanel("Liquor Store Prices"),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("resetBtn", "Reset All Filters", icon = icon("redo")),
      br(), br(),
      
      sliderInput("priceInput", "Price Range ($)", 
                  min = 0, max = max(bcl$Price, na.rm = TRUE), value = c(0, 5000)),
      
      radioButtons("typeInput", "Product Type",
                   choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
                   selected = "WINE"),
      
      pickerInput("subtypeInput", "Subtype",
                  choices = unique(bcl$Subtype[bcl$Type == "WINE"]),
                  selected = unique(bcl$Subtype[bcl$Type == "WINE"]),
                  multiple = TRUE,
                  options = list(`actions-box`=TRUE, `live-search`=TRUE)),
      
      checkboxInput("selectAllOrigins", "Select All Countries of Origin", value = TRUE),
      
      pickerInput(
        "originInput", "Country of Origin",
        choices = sort(unique(bcl$Country)),
        selected = sort(unique(bcl$Country)),
        multiple = TRUE,
        options = list(
          `actions-box` = TRUE,
          `live-search` = TRUE,
          `selected-text-format` = "count > 3"
        )
      ),
      
      textInput("searchInput", "Search Product Name", value = ""),
      
      checkboxInput("sortInput", "Sort results table by Price", value = FALSE),
      
      br(),
      downloadButton("downloadData", "Download Filtered Data (CSV)"),
      br(), br(),
      
      textOutput("resultCount"),
      br(),
      htmlOutput("appliedFilters")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Summary",
                 fluidRow(
                   column(4,
                          wellPanel(
                            h4("Average Price (Filtered)"),
                            textOutput("avgPrice")
                          )),
                   column(4,
                          wellPanel(
                            h4("Median Alcohol Content (Filtered)"),
                            textOutput("medAlcohol")
                          )),
                   column(4,
                          wellPanel(
                            h4("Unique Countries of Origin"),
                            textOutput("uniqueOrigins")
                          ))
                 ),
                 br(),
                 h4("Price Statistics by Country of Origin"),
                 DT::dataTableOutput("statsPriceByOrigin"),
                 br(),
                 h4("Alcohol Content Statistics by Country of Origin"),
                 DT::dataTableOutput("statsAlcoholByOrigin"),
                 br(),
                 h4("Summary by Product Subtype"),
                 DT::dataTableOutput("statsBySubtype")
        ),
        tabPanel("Plots",
                 plotOutput("alcoholPlot", height = "350px"),
                 downloadButton("downloadAlcoholPlot", "Download Alcohol Plot as PNG"),
                 br(), br(),
                 
                 plotOutput("pricePlot", height = "350px"),
                 downloadButton("downloadPricePlot", "Download Price Plot as PNG")
        ),
        tabPanel("Table", DT::dataTableOutput("results")),
        
        # Hypothesis Testing tab
        tabPanel("Hypothesis Testing",
                 sidebarLayout(
                   sidebarPanel(
                     h4("T-test: Domestic (Canada) vs Imported"),
                     helpText("Compares mean Price between products produced in Canada and not in Canada (filtered data)."),
                     actionButton("run_ttest", "Run T-test"),
                     br(), br(),
                     
                     h4("ANOVA: Price by Subtype"),
                     helpText("One-way ANOVA to test whether mean Price differs between Subtypes (filtered)."),
                     actionButton("run_anova_subtype", "Run ANOVA by Subtype"),
                     br(), br(),
                     
                     h4("ANOVA: Price by Country of Origin"),
                     helpText("One-way ANOVA to test whether mean Price differs between Countries of Origin (filtered)."),
                     actionButton("run_anova_country", "Run ANOVA by Country"),
                     br(), br(),
                     
                     helpText("Notes: If ANOVA assumptions are violated, a Kruskal-Wallis test will be provided instead."),
                     width = 4
                   ),
                   mainPanel(
                     h3("T-test (Domestic vs Imported)"),
                     DT::dataTableOutput("ttest_table"),
                     verbatimTextOutput("ttest_interpret"),
                     plotOutput("ttest_plot"),
                     br(),
                     
                     h3("ANOVA: Price by Subtype"),
                     DT::dataTableOutput("anova_subtype_table"),
                     verbatimTextOutput("anova_subtype_interpret"),
                     DT::dataTableOutput("anova_subtype_tukey"),
                     plotOutput("anova_subtype_plot"),
                     br(),
                     
                     h3("ANOVA: Price by Country of Origin"),
                     DT::dataTableOutput("anova_country_table"),
                     verbatimTextOutput("anova_country_interpret"),
                     DT::dataTableOutput("anova_country_tukey"),
                     plotOutput("anova_country_plot")
                   )
                 )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # ---- Filters UI behavior ----
  observe({
    subtypes <- unique(bcl$Subtype[bcl$Type == input$typeInput])
    updatePickerInput(session, "subtypeInput", choices = subtypes, selected = subtypes)
  })
  
  observeEvent(input$selectAllOrigins, {
    if (input$selectAllOrigins) {
      updatePickerInput(session, "originInput", selected = sort(unique(bcl$Country)))
    } else {
      updatePickerInput(session, "originInput", selected = character(0))
    }
  })
  
  observeEvent(input$resetBtn, {
    updateSliderInput(session, "priceInput", value = c(0, 5000))
    updateRadioButtons(session, "typeInput", selected = "WINE")
    updatePickerInput(session, "subtypeInput", choices = unique(bcl$Subtype[bcl$Type == "WINE"]),
                      selected = unique(bcl$Subtype[bcl$Type == "WINE"]))
    updateCheckboxInput(session, "selectAllOrigins", value = TRUE)
    updatePickerInput(session, "originInput", selected = sort(unique(bcl$Country)))
    updateTextInput(session, "searchInput", value = "")
    updateCheckboxInput(session, "sortInput", value = FALSE)
  })
  
  # ---- Filtered data reactive ----
  filtered <- reactive({
    data <- bcl %>%
      filter(Price >= input$priceInput[1],
             Price <= input$priceInput[2],
             Type == input$typeInput)
    
    if (!is.null(input$subtypeInput) && length(input$subtypeInput) > 0) {
      data <- data %>% filter(Subtype %in% input$subtypeInput)
    } else {
      data <- data[0, ]
    }
    
    if (!is.null(input$originInput) && length(input$originInput) > 0) {
      data <- data %>% filter(Country %in% input$originInput)
    } else {
      data <- data[0, ]
    }
    
    if (nzchar(input$searchInput)) {
      pattern <- tolower(input$searchInput)
      data <- data %>% filter(grepl(pattern, tolower(Name), fixed = TRUE))
    }
    
    if (input$sortInput) data <- data %>% arrange(Price)
    data
  })
  
  # ---- Summary & tables ----
  output$resultCount <- renderText({
    n <- nrow(filtered())
    paste("We found", n, ifelse(n == 1, "option", "options"), "for you")
  })
  
  output$appliedFilters <- renderUI({
    filters <- list()
    filters <- c(filters, paste0("<b>Price Range:</b> $", input$priceInput[1], " - $", input$priceInput[2]))
    filters <- c(filters, paste0("<b>Type:</b> ", input$typeInput))
    subtypes <- if (length(input$subtypeInput) == 0) "None" else paste(input$subtypeInput, collapse = ", ")
    filters <- c(filters, paste0("<b>Subtype:</b> ", subtypes))
    origins <- if (length(input$originInput) == 0) "None" else paste(input$originInput, collapse = ", ")
    filters <- c(filters, paste0("<b>Countries of Origin:</b> ", origins))
    searchTxt <- ifelse(nzchar(input$searchInput), input$searchInput, "None")
    filters <- c(filters, paste0("<b>Product name search:</b> ", searchTxt))
    HTML(paste(filters, collapse = "<br>"))
  })
  
  output$avgPrice <- renderText({
    if(nrow(filtered()) == 0) return("N/A")
    sprintf("$%.2f", mean(filtered()$Price, na.rm = TRUE))
  })
  
  output$medAlcohol <- renderText({
    if(nrow(filtered()) == 0) return("N/A")
    median(filtered()$Alcohol_Content, na.rm = TRUE)
  })
  
  output$uniqueOrigins <- renderText({
    if(nrow(filtered()) == 0) return("N/A")
    length(unique(filtered()$Country))
  })
  
  output$statsPriceByOrigin <- DT::renderDataTable({
    req(filtered())
    df <- filtered() %>%
      group_by(Country) %>%
      summarise(
        Count = n(),
        Min = round(min(Price, na.rm = TRUE), 2),
        Max = round(max(Price, na.rm = TRUE), 2),
        Mean = round(mean(Price, na.rm = TRUE), 2),
        Median = round(median(Price, na.rm = TRUE), 2),
        SD = round(sd(Price, na.rm = TRUE), 2)
      ) %>% arrange(Country)
    DT::datatable(df, options = list(pageLength = 5, scrollX = TRUE))
  })
  
  output$statsAlcoholByOrigin <- DT::renderDataTable({
    req(filtered())
    df <- filtered() %>%
      group_by(Country) %>%
      summarise(
        Count = n(),
        Min = round(min(Alcohol_Content, na.rm = TRUE), 2),
        Max = round(max(Alcohol_Content, na.rm = TRUE), 2),
        Mean = round(mean(Alcohol_Content, na.rm = TRUE), 2),
        Median = round(median(Alcohol_Content, na.rm = TRUE), 2),
        SD = round(sd(Alcohol_Content, na.rm = TRUE), 2)
      ) %>% arrange(Country)
    DT::datatable(df, options = list(pageLength = 5, scrollX = TRUE))
  })
  
  output$statsBySubtype <- DT::renderDataTable({
    req(filtered())
    df <- filtered() %>%
      group_by(Subtype) %>%
      summarise(
        Count = n(),
        Min_Price = round(min(Price, na.rm = TRUE), 2),
        Max_Price = round(max(Price, na.rm = TRUE), 2),
        Mean_Price = round(mean(Price, na.rm = TRUE), 2),
        Median_Price = round(median(Price, na.rm = TRUE), 2),
        SD_Price = round(sd(Price, na.rm = TRUE), 2),
        Min_Alcohol = round(min(Alcohol_Content, na.rm = TRUE), 2),
        Max_Alcohol = round(max(Alcohol_Content, na.rm = TRUE), 2),
        Mean_Alcohol = round(mean(Alcohol_Content, na.rm = TRUE), 2),
        Median_Alcohol = round(median(Alcohol_Content, na.rm = TRUE), 2),
        SD_Alcohol = round(sd(Alcohol_Content, na.rm = TRUE), 2)
      )
    DT::datatable(df, options = list(pageLength = 5, scrollX = TRUE))
  })
  
  # ---- Plots & downloads (unchanged) ----
  output$alcoholPlot <- renderPlot({
    req(nrow(filtered()) > 0)
    ggplot(filtered(), aes(x = factor(Alcohol_Content), fill = Country)) +
      geom_bar(position = position_dodge()) +
      geom_text(stat = 'count', aes(label = ..count..),
                position = position_dodge(width = 0.9), vjust = -0.3, size = 3) +
      labs(title = paste("Count by Alcohol Content and Country of Origin for", input$typeInput),
           x = "Alcohol Content", y = "Count", fill = "Country of Origin") +
      theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$downloadAlcoholPlot <- downloadHandler(
    filename = function() paste0("alcohol_plot_", Sys.Date(), ".png"),
    content = function(file) {
      png(file, width = 900, height = 600)
      print(
        ggplot(filtered(), aes(x = factor(Alcohol_Content), fill = Country)) +
          geom_bar(position = position_dodge()) +
          geom_text(stat = 'count', aes(label = ..count..),
                    position = position_dodge(width = 0.9), vjust = -0.3, size = 4) +
          labs(title = paste("Count by Alcohol Content and Country of Origin for", input$typeInput),
               x = "Alcohol Content", y = "Count", fill = "Country of Origin") +
          theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
      )
      dev.off()
    }
  )
  
  output$pricePlot <- renderPlot({
    req(nrow(filtered()) > 0)
    ggplot(filtered(), aes(x = Country, y = Price, fill = Country)) +
      geom_boxplot() +
      labs(title = "Price Distribution by Country of Origin", x = "Country of Origin", y = "Price ($)") +
      theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$downloadPricePlot <- downloadHandler(
    filename = function() paste0("price_plot_", Sys.Date(), ".png"),
    content = function(file) {
      png(file, width = 900, height = 600)
      print(
        ggplot(filtered(), aes(x = Country, y = Price, fill = Country)) +
          geom_boxplot() +
          labs(title = "Price Distribution by Country of Origin", x = "Country of Origin", y = "Price ($)") +
          theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
      )
      dev.off()
    }
  )
  
  # ---- Table of filtered data ----
  output$results <- DT::renderDataTable({
    req(filtered())
    DT::datatable(filtered(), options = list(pageLength = 10, searchHighlight = TRUE), rownames = FALSE)
  })
  
  # ---- Download filtered data ----
  output$downloadData <- downloadHandler(
    filename = function() paste0("filtered_liquor_data_", Sys.Date(), ".csv"),
    content = function(file) write.csv(filtered(), file, row.names = FALSE)
  )
  
  # ---- T-test: Domestic vs Imported ----
  observeEvent(input$run_ttest, {
    df <- filtered()
    # prepare outputs
    output$ttest_table <- DT::renderDataTable({
      if (nrow(df) == 0) return(NULL)
      df$Domestic <- ifelse(toupper(df$Country) == "CANADA", "Domestic", "Imported")
      counts <- table(df$Domestic)
      if (any(counts < 2)) {
        return(DT::datatable(data.frame(Message = "Not enough observations in one of the groups for t-test.")))
      }
      # run Shapiro only for small groups, but we'll still compute p-values if possible
      sh_dom <- tryCatch(shapiro.test(df$Price[df$Domestic == "Domestic"])$p.value, error = function(e) NA)
      sh_imp <- tryCatch(shapiro.test(df$Price[df$Domestic == "Imported"])$p.value, error = function(e) NA)
      use_wilcox <- FALSE
      if (!is.na(sh_dom) && !is.na(sh_imp)) {
        if (sh_dom < 0.05 || sh_imp < 0.05) use_wilcox <- TRUE
      }
      if (!use_wilcox) {
        tt <- t.test(Price ~ Domestic, data = df)
        tidy_tt <- broom::tidy(tt)
        # also show group means
        means <- df %>% group_by(Domestic) %>% summarise(Mean = mean(Price, na.rm = TRUE), N = n())
        out <- list(tidy = tidy_tt, means = means)
        # present as a single combined table: tidy stats first, then means
        tt_table <- as.data.frame(tidy_tt)
        tt_table <- tt_table %>% rename(statistic = statistic, p.value = p.value)
        tt_table$conf.low <- round(tt_table$conf.low, 3); tt_table$conf.high <- round(tt_table$conf.high, 3)
        tt_table$estimate <- round(tt_table$estimate, 3)
        DT::datatable(tt_table, options = list(dom = 't'))
      } else {
        wt <- wilcox.test(Price ~ Domestic, data = df)
        tidy_wt <- broom::tidy(wt)
        wt_table <- as.data.frame(tidy_wt)
        DT::datatable(wt_table, options = list(dom = 't'))
      }
    })
    
    output$ttest_interpret <- renderText({
      if (nrow(df) == 0) return("No data after filtering.")
      df$Domestic <- ifelse(toupper(df$Country) == "CANADA", "Domestic", "Imported")
      if (length(unique(df$Domestic)) < 2) return("Not enough groups.")
      sh_dom <- tryCatch(shapiro.test(df$Price[df$Domestic == "Domestic"])$p.value, error = function(e) NA)
      sh_imp <- tryCatch(shapiro.test(df$Price[df$Domestic == "Imported"])$p.value, error = function(e) NA)
      use_wilcox <- FALSE
      if (!is.na(sh_dom) && !is.na(sh_imp)) {
        if (sh_dom < 0.05 || sh_imp < 0.05) use_wilcox <- TRUE
      }
      if (!use_wilcox) {
        tt <- t.test(Price ~ Domestic, data = df)
        paste0("T-test: t = ", round(tt$statistic, 3),
               ", df = ", round(tt$parameter, 2),
               ", p = ", signif(tt$p.value, 4),
               ". Domestic mean = ", round(mean(df$Price[df$Domestic=="Domestic"], na.rm=TRUE),2),
               ", Imported mean = ", round(mean(df$Price[df$Domestic=="Imported"], na.rm=TRUE),2),
               ifelse(tt$p.value < 0.05, " — evidence of a difference at alpha=0.05.", " — no evidence of difference at alpha=0.05."))
      } else {
        wt <- wilcox.test(Price ~ Domestic, data = df)
        paste0("Wilcoxon rank-sum test used (normality violated). W = ", round(wt$statistic,3),
               ", p = ", signif(wt$p.value,4),
               ifelse(wt$p.value < 0.05, " — evidence of distributional difference at alpha=0.05.", " — no evidence of difference at alpha=0.05."))
      }
    })
    
    output$ttest_plot <- renderPlot({
      if (nrow(df) == 0) return(NULL)
      df$Domestic <- ifelse(toupper(df$Country) == "CANADA", "Domestic", "Imported")
      ggplot(df, aes(x = Domestic, y = Price, fill = Domestic)) +
        geom_boxplot() + geom_jitter(width = 0.2, alpha = 0.6) +
        labs(title = "Price: Domestic (Canada) vs Imported", x = "", y = "Price ($)") +
        theme_minimal()
    })
  })
  
  # ---- ANOVA by Subtype ----
  observeEvent(input$run_anova_subtype, {
    df <- filtered()
    
    # ANOVA table
    output$anova_subtype_table <- DT::renderDataTable({
      if (nrow(df) == 0) return(NULL)
      counts <- df %>% group_by(Subtype) %>% summarise(n = n())
      valid <- counts %>% filter(n >= 2)
      if (nrow(valid) < 2) {
        return(DT::datatable(data.frame(Message = "Not enough subtypes with >=2 observations for ANOVA.")))
      }
      df$Subtype <- as.factor(df$Subtype)
      aov_fit <- aov(Price ~ Subtype, data = df)
      tidy_aov <- broom::tidy(aov_fit)
      DT::datatable(tidy_aov, options = list(pageLength = 10, scrollX = TRUE))
    })
    
    # interpretation + diagnostics
    output$anova_subtype_interpret <- renderText({
      if (nrow(df) == 0) return("No data after filtering.")
      counts <- df %>% group_by(Subtype) %>% summarise(n = n())
      valid <- counts %>% filter(n >= 2)
      if (nrow(valid) < 2) return("Not enough subtypes with >=2 observations for ANOVA.")
      df$Subtype <- as.factor(df$Subtype)
      aov_fit <- aov(Price ~ Subtype, data = df)
      bart <- tryCatch(bartlett.test(Price ~ Subtype, data = df)$p.value, error = function(e) NA)
      sh <- tryCatch(shapiro.test(residuals(aov_fit))$p.value, error = function(e) NA)
      # decide test
      use_kruskal <- FALSE
      if (!is.na(bart) && bart < 0.05) use_kruskal <- TRUE
      if (!is.na(sh) && sh < 0.05) use_kruskal <- TRUE
      if (!use_kruskal) {
        p_val <- summary(aov_fit)[[1]]$`Pr(>F)`[1]
        text <- paste0("ANOVA: p = ", signif(p_val,4), 
                       ifelse(p_val < 0.05, " → reject H0: means differ between subtypes.", " → fail to reject H0: no evidence of mean differences."))
        paste0("Diagnostics — Bartlett p = ", signif(bart,4), ", Shapiro (resid) p = ", signif(sh,4), "\n", text)
      } else {
        kw <- kruskal.test(Price ~ Subtype, data = df)
        paste0("Assumptions violated (Bartlett or Shapiro). Kruskal-Wallis p = ", signif(kw$p.value,4),
               ifelse(kw$p.value < 0.05, " → reject H0: distributions differ between subtypes.", " → fail to reject H0."))
      }
    })
    
    # Tukey HSD table (if appropriate)
    output$anova_subtype_tukey <- DT::renderDataTable({
      df_local <- filtered()
      counts <- df_local %>% group_by(Subtype) %>% summarise(n = n()) %>% filter(n >= 2)
      if (nrow(counts) < 2) return(NULL)
      df_local$Subtype <- as.factor(df_local$Subtype)
      aov_fit <- aov(Price ~ Subtype, data = df_local)
      bart <- tryCatch(bartlett.test(Price ~ Subtype, data = df_local)$p.value, error = function(e) NA)
      sh <- tryCatch(shapiro.test(residuals(aov_fit))$p.value, error = function(e) NA)
      if (!is.na(bart) && bart < 0.05) return(NULL)
      if (!is.na(sh) && sh < 0.05) return(NULL)
      tk <- TukeyHSD(aov_fit)
      tk_df <- as.data.frame(tk$Subtype)
      tk_df$Comparison <- rownames(tk_df)
      tk_df <- tk_df %>% select(Comparison, diff, lwr, upr, `p adj`)
      names(tk_df) <- c("Comparison", "Diff", "Lower", "Upper", "p.adj")
      DT::datatable(tk_df, options = list(pageLength = 10, scrollX = TRUE))
    })
    
    # boxplot
    output$anova_subtype_plot <- renderPlot({
      req(nrow(df) > 0)
      ggplot(df, aes(x = Subtype, y = Price, fill = Subtype)) +
        geom_boxplot() + geom_jitter(width = 0.2, alpha = 0.6) +
        labs(title = "Price by Subtype", x = "Subtype", y = "Price ($)") +
        theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
  })
  
  # ---- ANOVA by Country ----
  observeEvent(input$run_anova_country, {
    df <- filtered()
    
    output$anova_country_table <- DT::renderDataTable({
      if (nrow(df) == 0) return(NULL)
      counts <- df %>% group_by(Country) %>% summarise(n = n())
      valid <- counts %>% filter(n >= 2)
      if (nrow(valid) < 2) {
        return(DT::datatable(data.frame(Message = "Not enough countries with >=2 observations for ANOVA.")))
      }
      df$Country <- as.factor(df$Country)
      aov_fit <- aov(Price ~ Country, data = df)
      tidy_aov <- broom::tidy(aov_fit)
      DT::datatable(tidy_aov, options = list(pageLength = 10, scrollX = TRUE))
    })
    
    output$anova_country_interpret <- renderText({
      if (nrow(df) == 0) return("No data after filtering.")
      counts <- df %>% group_by(Country) %>% summarise(n = n())
      valid <- counts %>% filter(n >= 2)
      if (nrow(valid) < 2) return("Not enough countries with >=2 observations for ANOVA.")
      df$Country <- as.factor(df$Country)
      aov_fit <- aov(Price ~ Country, data = df)
      bart <- tryCatch(bartlett.test(Price ~ Country, data = df)$p.value, error = function(e) NA)
      sh <- tryCatch(shapiro.test(residuals(aov_fit))$p.value, error = function(e) NA)
      use_kruskal <- FALSE
      if (!is.na(bart) && bart < 0.05) use_kruskal <- TRUE
      if (!is.na(sh) && sh < 0.05) use_kruskal <- TRUE
      if (!use_kruskal) {
        p_val <- summary(aov_fit)[[1]]$`Pr(>F)`[1]
        paste0("Diagnostics — Bartlett p = ", signif(bart,4), ", Shapiro (resid) p = ", signif(sh,4),
               "\nANOVA: p = ", signif(p_val,4),
               ifelse(p_val < 0.05, " → reject H0: means differ between countries.", " → fail to reject H0: no evidence of mean differences."))
      } else {
        kw <- kruskal.test(Price ~ Country, data = df)
        paste0("Assumptions violated. Kruskal-Wallis p = ", signif(kw$p.value,4),
               ifelse(kw$p.value < 0.05, " → reject H0: distributions differ between countries.", " → fail to reject H0."))
      }
    })
    
    output$anova_country_tukey <- DT::renderDataTable({
      df_local <- filtered()
      counts <- df_local %>% group_by(Country) %>% summarise(n = n()) %>% filter(n >= 2)
      if (nrow(counts) < 2) return(NULL)
      df_local$Country <- as.factor(df_local$Country)
      aov_fit <- aov(Price ~ Country, data = df_local)
      bart <- tryCatch(bartlett.test(Price ~ Country, data = df_local)$p.value, error = function(e) NA)
      sh <- tryCatch(shapiro.test(residuals(aov_fit))$p.value, error = function(e) NA)
      if (!is.na(bart) && bart < 0.05) return(NULL)
      if (!is.na(sh) && sh < 0.05) return(NULL)
      tk <- TukeyHSD(aov_fit)
      tk_df <- as.data.frame(tk$Country)
      tk_df$Comparison <- rownames(tk_df)
      tk_df <- tk_df %>% select(Comparison, diff, lwr, upr, `p adj`)
      names(tk_df) <- c("Comparison", "Diff", "Lower", "Upper", "p.adj")
      DT::datatable(tk_df, options = list(pageLength = 10, scrollX = TRUE))
    })
    
    output$anova_country_plot <- renderPlot({
      req(nrow(df) > 0)
      ggplot(df, aes(x = Country, y = Price, fill = Country)) +
        geom_boxplot() + geom_jitter(width = 0.2, alpha = 0.6) +
        labs(title = "Price by Country of Origin", x = "Country of Origin", y = "Price ($)") +
        theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
  })
  
}

shinyApp(ui, server)
