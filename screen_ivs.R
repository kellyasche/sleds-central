screen_ivs <- function(data,
                       dv,
                       candidate_ivs,
                       categorical_vars,
                       continuous_vars,
                       cor_flag = 0.5,
                       cor_problem = 0.7,
                       vif_flag = 5,
                       vif_problem = 10) {
  
  cat("=====", dv, "=====\n\n")
  
  ## --- Step 1: Missingness ---------------------------------------------
  cat("-- Step 1: Missingness --\n")
  missing_summary <- data %>%
    select(all_of(candidate_ivs)) %>%
    summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "pct_missing") %>%
    arrange(desc(pct_missing))
  print(missing_summary)
  
  n_full <- nrow(data)
  n_complete <- data %>% select(all_of(candidate_ivs)) %>% drop_na() %>% nrow()
  cat("\nFull population:", n_full, "\n")
  cat("Complete-case population:", n_complete,
      "(", round(n_complete / n_full * 100, 1), "% )\n\n")
  
  ## --- Step 2a: Categorical x Categorical (Cramer's V) ------------------
  cat("-- Step 2a: Cramer's V (categorical x categorical) --\n")
  cramer_matrix <- matrix(NA, nrow = length(categorical_vars), ncol = length(categorical_vars),
                          dimnames = list(categorical_vars, categorical_vars))
  for (i in seq_along(categorical_vars)) {
    for (j in seq_along(categorical_vars)) {
      if (i < j) {
        tbl <- table(data[[categorical_vars[i]]], data[[categorical_vars[j]]])
        v <- suppressWarnings(cramerV(tbl))
        cramer_matrix[i, j] <- v
        cramer_matrix[j, i] <- v
      }
    }
  }
  flagged_cat <- which(cramer_matrix > cor_flag, arr.ind = TRUE)
  flagged_cat <- flagged_cat[flagged_cat[, 1] < flagged_cat[, 2], , drop = FALSE]
  if (nrow(flagged_cat) > 0) {
    print(data.frame(
      var1 = categorical_vars[flagged_cat[, 1]],
      var2 = categorical_vars[flagged_cat[, 2]],
      cramers_v = round(cramer_matrix[flagged_cat], 3)
    ))
  } else {
    cat("No categorical pairs above", cor_flag, "\n")
  }
  
  ## --- Step 2b: Continuous x Continuous (correlation) --------------------
  cat("\n-- Step 2b: Correlation (continuous x continuous) --\n")
  cor_matrix <- data %>% select(all_of(continuous_vars)) %>% cor(use = "complete.obs")
  print(round(cor_matrix, 3))
  
  ## --- Step 2c: Categorical x Continuous (eta-squared) --------------------
  cat("\n-- Step 2c: Eta-squared (categorical x continuous), flagged above",
      cor_flag^2, "(~ r >", cor_flag, ") --\n")
  eta_results <- data.frame(categorical = character(), continuous = character(),
                            eta_squared = numeric())
  for (cat_var in categorical_vars) {
    for (cont_var in continuous_vars) {
      f <- as.formula(paste(cont_var, "~", cat_var))
      m <- aov(f, data = data)
      eta <- eta_squared(m)$Eta2[1]
      eta_results <- rbind(eta_results,
                           data.frame(categorical = cat_var, continuous = cont_var,
                                      eta_squared = eta))
    }
  }
  eta_results <- eta_results %>% arrange(desc(eta_squared))
  print(eta_results %>% filter(eta_squared > cor_flag^2))
  
  ## --- Step 3: Full-model VIF --------------------------------------------
  cat("\n-- Step 3: Full-model VIF --\n")
  f <- as.formula(paste(dv, "~", paste(candidate_ivs, collapse = " + ")))
  model_data <- data %>% select(all_of(candidate_ivs), all_of(dv)) %>% drop_na()
  full_model <- glm(f, family = binomial, data = model_data)
  vif_result <- vif(full_model)
  print(vif_result)
  
  cat("\n(Flag thresholds: correlation/Cramer's V >", cor_flag, "= inspect, >", cor_problem,
      "= near-certain problem. VIF/GVIF^(1/(2*Df)) >", vif_flag, "= inspect, >", vif_problem,
      "= clear problem.)\n")
  
  invisible(list(
    missingness = missing_summary,
    cramer = cramer_matrix,
    cor = cor_matrix,
    eta = eta_results,
    vif = vif_result
  ))
}