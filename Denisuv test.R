#####Návod na použití Denisovi analýzy dat#####
## 1. spustit celý kód od nadpisu start až po nadpis konec
## 2. načíst svoje data do objektu Ddata (prostor je pod nadpisem KONEC)!!
## 3. spustit analýzu spuštěním analysis_results <- analyze_object(Ddata)
## 4. pro ukázku grafů a testů spustit run_graphs(analysis_results$graph_code, Ddata) a run_tests(analysis_results$test_code, Ddata)

#####START#####
# Funkce pro kontrolu a instalaci potřebných balíčků
install_if_missing <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    }
  }
}

# Seznam potřebných balíčků
required_packages <- c("ggplot2", "dplyr", "moments", "nortest", "car", "scales", "readr", "readxl", "lmtest")

# Kontrola a instalace balíčků
install_if_missing(required_packages)

# Načtení balíčků
library(ggplot2)
library(dplyr)
library(moments)
library(nortest)
library(car)
library(scales)
library(readr)
library(readxl)
library(lmtest)

# Funkce pro detekci oddělovače ve vstupních datech
detect_delimiter <- function(file_path) {
  lines <- readLines(file_path, n = 1)
  delimiters <- c(",", ";", "\t", "|")
  counts <- sapply(delimiters, function(d) sum(grepl(d, lines)))
  names(delimiters)[which.max(counts)]
}

# Funkce pro zaokrouhlování čísel na daný počet desetinných míst
round_data <- function(data, digits) {
  round(data, digits)
}

# Funkce pro zjištění typu vztahu mezi dvěma číselnými sloupci
detect_relationship <- function(x, y) {
  linear_model <- lm(y ~ x)
  exponential_model <- lm(log(y + 1) ~ x)  # Přičítáme 1, aby nedošlo k log(0)
  
  linear_r2 <- summary(linear_model)$r.squared
  exponential_r2 <- summary(exponential_model)$r.squared
  
  if (linear_r2 > exponential_r2) {
    return("linear")
  } else if (exponential_r2 > linear_r2) {
    return("exponential")
  } else {
    return("nonlinear")
  }
}

# Funkce pro analýzu dat a doporučení úprav a grafů
analyze_data <- function(data) {
  recommendations <- list()
  graph_code <- list()
  test_code <- list()
  issues <- list()  # Seznam pro sledování problémů a nesrovnalostí
  
  # Základní statistiky
  summary_stats <- summary(data)
  recommendations$summary <- summary_stats
  
  # Kontrola typu dat
  data_type <- sapply(data, class)
  recommendations$data_type <- data_type
  
  # Ujistěte se, že všechny sloupce mají stejnou délku
  for (col in names(data)) {
    if (length(data[[col]]) != nrow(data)) {
      issues <- c(issues, paste("Column", col, "has a different length than the number of rows in the data."))
    }
  }
  
  # Pro každý sloupec dat
  for (col in names(data)) {
    cat(paste("\nAnalyzing column:", col, "\n"))
    
    column_recommendations <- list()
    column_graph_code <- NULL  # Pouze jeden graf pro každý sloupec
    column_test_code <- list()
    
    # Kontrola na chybějící hodnoty
    na_count <- sum(is.na(data[[col]]))
    if (na_count > 0) {
      column_recommendations$missing_values <- paste(
        "Column contains", na_count, "missing values. Consider using imputation methods like mean/median imputation or removal of missing values. Example code: Ddata[['", col, "']][is.na(Ddata[['", col, "']])] <- mean(Ddata[['", col, "']], na.rm = TRUE)", sep = ""
      )
    } else {
      column_recommendations$missing_values <- "No missing values detected."
    }
    
    # Pokud je sloupec číselný
    if (is.numeric(data[[col]])) {
      # Kontrola normality
      if (length(data[[col]][!is.na(data[[col]])]) > 7) {
        shapiro_test <- tryCatch({
          shapiro.test(data[[col]][!is.na(data[[col]])])
        }, error = function(e) {
          issues <- c(issues, paste("Shapiro-Wilk test failed for column", col, "with error:", e$message))
          NULL
        })
        
        if (!is.null(shapiro_test) && shapiro_test$p.value < 0.05) {
          column_recommendations$normality <- "Data are not normally distributed (Shapiro-Wilk test p-value < 0.05). Consider using non-parametric tests."
          # Doporučení neparametrických testů
          column_recommendations$stat_tests <- paste(
            "For hypothesis testing, consider using non-parametric tests like Mann-Whitney U test or Kruskal-Wallis test. Example code: wilcox.test(Ddata[['", col, "']] ~ Ddata$group)", sep = ""
          )
          column_test_code$wilcox_test <- paste("wilcox.test(Ddata[['", col, "']] ~ Ddata$group)", sep = "")
          # Doporučení konkrétního testu
          column_recommendations$recommended_test <- "Mann-Whitney U test is recommended if you are comparing two independent groups."
        } else if (!is.null(shapiro_test)) {
          column_recommendations$normality <- "Data are normally distributed (Shapiro-Wilk test p-value >= 0.05). Consider using parametric tests."
          # Doporučení parametrických testů
          column_recommendations$stat_tests <- paste(
            "For hypothesis testing, consider using parametric tests like t-test or ANOVA. Example code: t.test(Ddata[['", col, "']] ~ Ddata$group)", sep = ""
          )
          column_test_code$t_test <- paste("t.test(Ddata[['", col, "']] ~ Ddata$group)", sep = "")
          # Doporučení konkrétního testu
          column_recommendations$recommended_test <- "t-test is recommended if you are comparing means of two independent groups."
        }
      } else {
        column_recommendations$normality <- "Sample size too small for Shapiro-Wilk test. Check skewness and kurtosis for normality assessment."
      }
      
      # Identifikace outlierů
      outliers <- boxplot.stats(data[[col]])$out
      if (length(outliers) > 0) {
        column_recommendations$outliers <- paste(
          "Detected", length(outliers), "outliers. Consider handling outliers using methods like winsorizing or removal. Example code: Ddata[['", col, "']] <- ifelse(Ddata[['", col, "']] %in% c(", paste(outliers, collapse = ", "), "), NA, Ddata[['", col, "']])", sep = ""
        )
      } else {
        column_recommendations$outliers <- "No outliers detected."
      }
      
      # Doporučení grafu
      column_recommendations$graph <- paste(
        "Consider using Histogram for visualization. Example code: ggplot(Ddata, aes(x = `", col, "`)) + geom_histogram(bins = 30) + theme_minimal()", sep = ""
      )
      column_graph_code <- paste("ggplot(Ddata, aes(x = `", col, "`)) + geom_histogram(bins = 30) + theme_minimal()", sep = "")
      column_recommendations$graph_usage <- "Histogram is useful for visualizing the distribution of numeric data."
      
    } else if (is.factor(data[[col]]) || is.character(data[[col]])) {
      # Pokud je sloupec kategoriální
      
      # Doporučení grafu
      column_recommendations$graph <- paste(
        "Consider using Bar plot for visualization. Example code: ggplot(Ddata, aes(x = factor(`", col, "`))) + geom_bar() + theme_minimal()", sep = ""
      )
      column_graph_code <- paste("ggplot(Ddata, aes(x = factor(`", col, "`))) + geom_bar() + theme_minimal()", sep = "")
      column_recommendations$graph_usage <- "Bar plot is useful for visualizing the frequency of categories in categorical data."
      
      # Doporučení statistických testů
      column_recommendations$stat_tests <- paste(
        "For hypothesis testing, consider using Chi-squared test for independence or Fisher's exact test for small sample sizes. Example code: chisq.test(table(Ddata[['", col, "']], Ddata$group))", sep = ""
      )
      column_test_code$chisq_test <- paste("chisq.test(table(Ddata[['", col, "']], Ddata$group))", sep = "")
      
    } else {
      column_recommendations$graph <- "Unknown data type."
      column_recommendations$stat_tests <- "No statistical test recommendation available for unknown data type."
      issues <- c(issues, paste("Unknown data type for column", col))
    }
    
    recommendations[[col]] <- column_recommendations
    graph_code[[col]] <- column_graph_code
    test_code[[col]] <- column_test_code
  }
  
  # Zjištění typu vztahu mezi číselnými sloupci
  numeric_cols <- names(data)[sapply(data, is.numeric)]
  if (length(numeric_cols) >= 2) {
    relationship <- detect_relationship(data[[numeric_cols[1]]], data[[numeric_cols[2]]])
    recommendations$relationship <- paste("The relationship between", numeric_cols[1], "and", numeric_cols[2], "is", relationship, ".")
    
    if (relationship == "linear") {
      recommendations$graph_relationship <- paste(
        "Consider using Scatter plot and Linear Regression plot for visualization. Example code: ggplot(Ddata, aes(x = `", numeric_cols[1], "`, y = `", numeric_cols[2], "`)) + geom_point() + geom_smooth(method = 'lm') + theme_minimal()", sep = ""
      )
      graph_code$relationship <- paste("ggplot(Ddata, aes(x = `", numeric_cols[1], "`, y = `", numeric_cols[2], "`)) + geom_point() + geom_smooth(method = 'lm') + theme_minimal()", sep = "")
      recommendations$graph_relationship_usage <- "Scatter plot with Linear Regression is useful for visualizing linear relationships between two numeric variables."
    } else if (relationship == "exponential") {
      recommendations$graph_relationship <- paste(
        "Consider using Scatter plot and Exponential Regression plot for visualization. Example code: ggplot(Ddata, aes(x = `", numeric_cols[1], "`, y = `", numeric_cols[2], "`)) + geom_point() + geom_smooth(method = 'lm', formula = y ~ exp(x)) + theme_minimal()", sep = ""
      )
      graph_code$relationship <- paste("ggplot(Ddata, aes(x = `", numeric_cols[1], "`, y = `", numeric_cols[2], "`)) + geom_point() + geom_smooth(method = 'lm', formula = y ~ exp(x)) + theme_minimal()", sep = "")
      recommendations$graph_relationship_usage <- "Scatter plot with Exponential Regression is useful for visualizing exponential relationships between two numeric variables."
    } else {
      recommendations$graph_relationship <- paste(
        "Consider using Scatter plot for visualization. Example code: ggplot(Ddata, aes(x = `", numeric_cols[1], "`, y = `", numeric_cols[2], "`)) + geom_point() + theme_minimal()", sep = ""
      )
      graph_code$relationship <- paste("ggplot(Ddata, aes(x = `", numeric_cols[1], "`, y = `", numeric_cols[2], "`)) + geom_point() + theme_minimal()", sep = "")
      recommendations$graph_relationship_usage <- "Scatter plot is useful for visualizing relationships between two numeric variables."
    }
  }
  
  recommendations$thanks <- "Thank you for using Denis' data analysis program."
  
  # Pokud se vyskytly nějaké problémy, přidáme je do doporučení
  if (length(issues) > 0) {
    recommendations$issues <- issues
  }
  
  return(list(recommendations = recommendations, graph_code = graph_code, test_code = test_code))
}

# Funkce pro detekci desetinných míst
detect_decimal_places <- function(data) {
  decimal_counts <- sapply(data, function(col) {
    if (is.numeric(col)) {
      max(sapply(col, function(x) {
        parts <- strsplit(as.character(x), "\\.")[[1]]
        if (length(parts) > 1) nchar(parts[2]) else 0
      }))
    } else {
      NA
    }
  })
  decimal_counts
}

# Funkce pro spuštění doporučených grafů
run_graphs <- function(graph_code, Ddata) {
  # Sledování typů grafů, aby se zjistilo, zda jsou všechny stejné
  graph_types <- c()
  
  for (col in names(graph_code)) {
    graph <- graph_code[[col]]
    if (!is.null(graph)) {
      cat(paste("\nRunning graph for column:", col, "\n"))
      print(eval(parse(text = graph), envir = list(Ddata = Ddata)))
      graph_type <- sub(".*geom_", "", graph) # Extrahujeme typ grafu z kódu
      graph_types <- c(graph_types, graph_type)
    }
  }
  
  # Kontrola, zda jsou všechny typy grafů stejné
  if (length(unique(graph_types)) == 1) {
    cat("All columns are visualized using the same type of graph, which is appropriate for the data.\n")
  }
}

# Funkce pro spuštění doporučených testů
run_tests <- function(test_code, Ddata) {
  # Vytvoření kopie dat pro testování
  test_data <- Ddata
  
  # Přidání náhodné skupinové proměnné, pokud neexistuje
  if (!("group" %in% names(test_data))) {
    test_data$group <- sample(c("A", "B"), nrow(test_data), replace = TRUE)
  }
  
  for (col in names(test_code)) {
    for (test in test_code[[col]]) {
      cat(paste("\nRunning test for column:", col, "\n"))
      result <- tryCatch({
        eval(parse(text = test), envir = list(Ddata = test_data))
      }, warning = function(w) {
        message("Warning: ", conditionMessage(w))
        return(eval(parse(text = test), envir = list(Ddata = test_data)))
      }, error = function(e) {
        message("Error: ", conditionMessage(e))
        return(NULL)
      })
      if (!is.null(result)) {
        print(result)
      } else {
        cat(paste("Test not possible for column:", col, "\n"))
      }
    }
  }
}

# Celková analýza datového objektu
analyze_object <- function(Ddata) {
  decimal_places <- detect_decimal_places(Ddata)
  cat("Detected decimal places per column:\n")
  print(decimal_places)
  
  analysis_results <- analyze_data(Ddata)
  recommendations <- analysis_results$recommendations
  graph_code <- analysis_results$graph_code
  test_code <- analysis_results$test_code
  
  print(recommendations)
  
  return(list(graph_code = graph_code, test_code = test_code))
}
#####KONEC#####

#####Zde vložte svá data do objektu Ddata#####
Ddata<-

#####Analýza dat#####
# Proveďte analýzu
analysis_results <- analyze_object(Ddata)

#####Zobrazení doporučených grafů#####
#Někdy kód odstraní řádky s NA nebo jinak chybné řádky pro správné zobrazení grafu, tudíž grafy nebrat prosím jako 100% správné, spíše jako orientační !!
run_graphs(analysis_results$graph_code, Ddata)

#####Zobrazení doporučených testů#####
run_tests(analysis_results$test_code, Ddata)