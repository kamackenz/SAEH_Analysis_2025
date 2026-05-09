##----------------------------------------------------------------------------##
## File name: SAEH Preliminary Cross Tabulations                              ##  
## Programmer: Kelsey MacKenzie                                               ##
## Date: 22-AUG-2025                                                          ##
## Last modified: 18-AUG-2025                                                 ##
## Purpose: Visualize time trends amongst procedure codes.                    ##
## PI: Emily Boniface                                                         ##
##----------------------------------------------------------------------------##


#### SETUP ---------------------------------------------------------------------

##### Load packages  ----
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(gt)
library(webshot2)
library(rmarkdown)
library(janitor)
library(magick)
library(stringr)
library(RColorBrewer)
library(gtsummary)
library(scales)
library(forcats)
library(gtsummary)



#### Import Data ---------------------------------------------------------------
final_saeh <- readRDS("SAEHdata_2018_2024.rds")


#### Cross Tabs ----------------------------------------------------------------

# Define combos with logical groups
proc_combos <- list(
  "D&C (6901) + Aspiration (6951)"              = list(c("proc_dc_ab"), c("proc_asp_ab")),
  "Miso (75A1) + D&C (6901)"                    = list(c("proc_miso_ab"), c("proc_dc_ab")),
  "Other meds (75A3) + D&C (6901)"              = list(c("proc_oth_ab"), c("proc_dc_ab")),
  "Aspiration (6951) + Miso (75A1)"             = list(c("proc_asp_ab"), c("proc_miso_ab")),
  "Aspiration (6951) + Other meds (75A3)"       = list(c("proc_asp_ab"), c("proc_oth_ab")),
  "Miso (75A1) + Other meds (75A3)"             = list(c("proc_miso_ab"), c("proc_oth_ab")),
  
  # Expanded (pre + post grouped)
  "D&C (6901+6902) + Aspiration (6951+6952)"    = list(c("proc_dc_ab","proc_dc_post"), c("proc_asp_ab","proc_asp_post")),
  "D&C (6901+6902) + Miso (75A1+75A2)"          = list(c("proc_dc_ab","proc_dc_post"), c("proc_miso_ab","proc_miso_post")),
  "D&C (6901+6902) + Other Meds (75A3+75A4)"    = list(c("proc_dc_ab","proc_dc_post"), c("proc_oth_ab","proc_oth_post")),
  "Miso (75A1+75A2) + Aspiration (6951+6952)"   = list(c("proc_miso_ab","proc_miso_post"), c("proc_asp_ab","proc_asp_post")),
  "Other Meds (75A3+75A4) + Aspiration (6951+6952)" = list(c("proc_oth_ab","proc_oth_post"), c("proc_asp_ab","proc_asp_post")),
  "Miso (75A1+75A2) + Other Meds (75A3+75A4)"   = list(c("proc_miso_ab","proc_miso_post"), c("proc_oth_ab","proc_oth_post")),
  
  # Same-group
  "D&C (6901 + 6902)"                           = list(c("proc_dc_ab","proc_dc_post")),
  "Aspiration (6951 + 6952)"                    = list(c("proc_asp_ab","proc_asp_post")),
  "Miso (75A1 + 75A2)"                          = list(c("proc_miso_ab","proc_miso_post")),
  "Other meds (75A3 + 75A4)"                    = list(c("proc_oth_ab","proc_oth_post")))


# Function to calculate % per year
calc_combo_fast <- function(df, combo, name) {
  
  # If combo has 2 groups → require at least one from each
  if (length(combo) == 2) {
    group1 <- rowSums(df[combo[[1]]], na.rm = TRUE) > 0
    group2 <- rowSums(df[combo[[2]]], na.rm = TRUE) > 0
    both   <- group1 & group2
    
    # If combo has only 1 group → require at least 2 present (for things like 6901 + 6902)
  } else {
    both <- rowSums(df[combo[[1]]], na.rm = TRUE) == length(combo[[1]])
  }
  
  df %>%
    group_by(year) %>%
    summarise(
      n = n(),
      count = sum(both, na.rm = TRUE),
      percent = 100 * count / n,
      .groups = "drop"
    ) %>%
    mutate(combo = name) %>%
    select(combo, year, percent)
}

# Run results
results <- purrr::map2_dfr(proc_combos, names(proc_combos),
                           ~ calc_combo_fast(final_saeh, .x, .y))

##### Graph the combinations all together ------
ggplot(results, aes(x = year, y = percent)) +
  geom_line(size = 0.5, color = "steelblue") +
  geom_point(size = 1, color = "steelblue") +
  facet_wrap(~ combo, scales = "fixed") +
  labs(
    x = "Year",
    y = "Percent of Cases") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 5.5),        # shrink combo labels
    axis.text.x = element_text(size = 6, hjust = 1))


##### Graph of termination codes ------
combos1 <- c(
  "D&C (6901) + Aspiration (6951)",
  "Miso (75A1) + D&C (6901)",
  "Other meds (75A3) + D&C (6901)",
  "Aspiration (6951) + Miso (75A1)",
  "Aspiration (6951) + Other meds (75A3)",
  "Miso (75A1) + Other meds (75A3)")

results1 <- results %>% filter(combo %in% combos1)

ggplot(results1, aes(x = year, y = percent)) +
  geom_line(size = 0.5, color = "steelblue") +
  geom_point(size = 1, color = "steelblue") +
  facet_wrap(~ combo, scales = "fixed") +
  labs(
    x = "Year",
    y = "Percent of Cases") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 5.5),
    axis.text.x = element_text(size = 6, hjust = 1))


##### Graph comparing all procedures codes ------

combos2 <- c(
  "D&C (6901+6902) + Aspiration (6951+6952)",
  "D&C (6901+6902) + Miso (75A1+75A2)",
  "D&C (6901+6902) + Other Meds (75A3+75A4)",
  "Miso (75A1+75A2) + Aspiration (6951+6952)",
  "Other Meds (75A3+75A4) + Aspiration (6951+6952)",
  "Miso (75A1+75A2) + Other Meds (75A3+75A4)")

results2 <- results %>% filter(combo %in% combos2)

ggplot(results2, aes(x = year, y = percent)) +
  geom_line(size = 0.5, color = "steelblue") +
  geom_point(size = 1, color = "steelblue") +
  facet_wrap(~ combo, scales = "fixed") +
  labs(
    x = "Year",
    y = "Percent of Cases") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 5.5),
    axis.text.x = element_text(size = 6, hjust = 1))

##### Graph comparing procedure codes against themselves ------

combos3 <- c(
  "D&C (6901 + 6902)",
  "Aspiration (6951 + 6952)",
  "Miso (75A1 + 75A2)",
  "Other meds (75A3 + 75A4)")

results3 <- results %>% filter(combo %in% combos3)

ggplot(results3, aes(x = year, y = percent)) +
  geom_line(size = 0.5, color = "steelblue") +
  geom_point(size = 1, color = "steelblue") +
  facet_wrap(~ combo, scales = "fixed") +
  labs(
    x = "Year",
    y = "Percent of Cases") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 5.5),
    axis.text.x = element_text(size = 6, hjust = 1))


##### Graph comparing single codes of interest ------
single_codes <- list(
  "Aspiration (6951)" = "proc_asp_ab",
  "D&C (6901)"        = "proc_dc_ab",
  "Miso (75A1)"       = "proc_miso_ab",
  "Other meds (75A3)" = "proc_oth_ab",
  "Other D&C (6909)" = "proc_dc_ab_other",
  "A&C (6959)" = "proc_asp_ab_other"
)

calc_single <- function(df, code_var, name) {
  df %>%
    group_by(year) %>%
    summarise(
      n = n(),
      count = sum(.data[[code_var]] > 0, na.rm = TRUE),
      percent = 100 * count / n,
      .groups = "drop"
    ) %>%
    mutate(combo = name) %>%
    select(combo, year, percent)
}

single_results <- purrr::map2_dfr(single_codes, names(single_codes),
                                  ~ calc_single(final_saeh, .x, .y))

ggplot(single_results, aes(x = year, y = percent)) +
  geom_line(size = 0.5, color = "steelblue") +
  geom_point(size = 1, color = "steelblue") +
  facet_wrap(~ combo, scales = "fixed") +
  labs(x = "Year", y = "Percent of Cases") +
  theme_minimal()


single_codes_2 <- list(
  "Aspiration (6952)" = "proc_asp_post",
  "D&C (6902)"        = "proc_dc_post",
  "Miso (75A2)"       = "proc_miso_post",
  "Other meds (75A4)" = "proc_oth_post"
)

calc_single_2 <- function(df, code_var, name) {
  df %>%
    group_by(year) %>%
    summarise(
      n = n(),
      count = sum(.data[[code_var]] > 0, na.rm = TRUE),
      percent = 100 * count / n,
      .groups = "drop"
    ) %>%
    mutate(combo = name) %>%
    select(combo, year, percent)
}

single_results_2 <- purrr::map2_dfr(single_codes_2, names(single_codes_2),
                                  ~ calc_single_2(final_saeh, .x, .y))

ggplot(single_results_2, aes(x = year, y = percent)) +
  geom_line(size = 0.5, color = "steelblue") +
  geom_point(size = 1, color = "steelblue") +
  facet_wrap(~ combo, scales = "fixed") +
  labs(x = "Year", y = "Percent of Cases") +
  theme_minimal()




#### Table of percentages ------------------------------------------------------
crosstab_big <- results %>%
  select(year, combo, percent) %>%
  pivot_wider(names_from = year, values_from = percent) %>%
  rename(`Procedure Code Combination` = combo) %>% #this is showing proportions
  mutate(across(-`Procedure Code Combination`, ~ round(.x, 3)))

crosstab_table <- results %>%
  select(year, combo, percent) %>%
  mutate(percent = percent * 100) %>%
  pivot_wider(names_from = year, values_from = percent) %>%
  rename(`Procedure Code Combination` = combo) %>%
  mutate(across(-`Procedure Code Combination`, ~ round(.x, 2)))


# Save as PNG
tab <- ggtexttable(crosstab_table, rows = NULL)
ggsave("allcrosstab_table.png", tab, width = 10, height = 6, dpi = 300)


#### Cross-tab tables ----------------------------------------------------------

# --- Lookup for nicer labels ---
proc_labels <- c(
  proc_asp_ab = "Aspiration",
  proc_dc_ab  = "D&C",
  proc_miso_ab = "Miso",
  proc_oth_ab = "Other meds")

# Function to create formatted cross-tab
make_crosstab <- function(df, code1, code2, name) {
  tab <- df %>%
    transmute(
      c1 = rowSums(across(all_of(code1)), na.rm = TRUE) > 0,
      c2 = rowSums(across(all_of(code2)), na.rm = TRUE) > 0
    ) %>%
    table()
  
  tab_df <- as.data.frame.matrix(tab)
  total <- sum(tab_df)
  
  # Column percentages
  col_pct <- prop.table(tab, margin = 2) * 100
  col_pct_df <- as.data.frame.matrix(round(col_pct, 1))
  
  # Combine counts + percentages
  formatted <- tab_df
  for (i in 1:nrow(tab_df)) {
    for (j in 1:ncol(tab_df)) {
      formatted[i, j] <- paste0(tab_df[i, j], " (", col_pct_df[i, j], ")")
    }
  }
  
  # Add totals (convert to character for consistency)
  formatted$Total <- as.character(rowSums(tab_df))
  total_row <- as.character(colSums(tab_df))
  formatted <- rbind(formatted, Total = c(total_row, Total = total))
  
  list(name = name, table = formatted)
}


# Define the 12 pairs your mentor asked for
crosstab_pairs <- list(
  "6901 & 6951" = list("proc_dc_ab", "proc_asp_ab"),
  "6901 & 75A1" = list("proc_dc_ab", "proc_miso_ab"),
  "6901 & 75A3" = list("proc_dc_ab", "proc_oth_ab"),
  "6951 & 75A1" = list("proc_asp_ab", "proc_miso_ab"),
  "6951 & 75A3" = list("proc_asp_ab", "proc_oth_ab"),
  "75A1 & 75A3" = list("proc_miso_ab", "proc_oth_ab"),
  
  # Expanded set
  "6902 & 6952" = list("proc_dc_post", "proc_asp_post"),
  "6902 & 75A2" = list("proc_dc_post", "proc_miso_post"),
  "6902 & 75A4" = list("proc_dc_post", "proc_oth_post"),
  "6952 & 75A2" = list("proc_asp_post", "proc_miso_post"),
  "6952 & 75A4" = list("proc_asp_post", "proc_oth_post"),
  "75A2 & 75A4" = list("proc_miso_post", "proc_oth_post")
)

# Run all cross-tabs
make_combo_results <- function(df, combos, id_col = "proc", year_col = "year", case_col = "case_id") {
  results <- list()
  
  for (procs in combos) {
    if (length(procs) == 1) {
      # Single procedure table
      proc <- procs[1]
      title <- switch(proc,
                      "proc_asp_ab" = "Aspiration",
                      "proc_dc_ab"  = "D&C",
                      proc)  # fallback to raw name
      tbl <- df %>%
        count(!!sym(proc)) %>%
        mutate(Percent = round(100 * n / sum(n), 1)) %>%
        rename(Count = n) %>%
        mutate(!!proc := ifelse(!!sym(proc) == 1, "Yes", "No")) %>%
        bind_rows(tibble(!!proc := "Total", Count = sum(.$Count), Percent = NA))
      
      results[[title]] <- list(title = title, table = tbl)
      
    } else {
      # Combo table for multiple procedures
      combo_name <- paste(procs, collapse = " + ")
      title <- paste(switch(procs[1],
                            "proc_asp_ab" = "Aspiration",
                            "proc_dc_ab"  = "D&C",
                            procs[1]),
                     "vs",
                     paste(sapply(procs, function(p) switch(p,
                                                            "proc_asp_ab" = "Aspiration",
                                                            "proc_dc_ab"  = "D&C",
                                                            p)),
                           collapse = " + "))
      
      # Make indicator for having *all* procedures in combo
      df <- df %>%
        mutate(combo = ifelse(rowSums(select(., all_of(procs))) == length(procs), 1, 0))
      
      tbl <- table(df[[procs[1]]], df$combo)
      tbl <- addmargins(tbl) # adds totals
      colnames(tbl) <- c("No", "Yes", "Total")
      rownames(tbl) <- c("No", "Yes", "Total")
      
      results[[combo_name]] <- list(title = title, table = tbl)
    }
  }
  results
}

# --- Run on your data ---
out <- make_combo_results(final_saeh,
                          list(c("proc_asp_ab"),
                               c("proc_dc_ab"),
                               c("proc_asp_ab", "proc_dc_ab")))

# --- Print nicely ---
for (nm in names(out)) {
  cat("\n", out[[nm]]$title, "\n")
  print(out[[nm]]$table)
}




#### Cross-tab tables PDF ------------------------------------------------------

# --- Pretty labels for your variables ---
proc_labels <- c(
  proc_dc_ab   = "D&C (6901)",
  proc_asp_ab  = "Aspiration (6951)",
  proc_miso_ab = "Miso (75A1)",
  proc_oth_ab  = "Other meds (75A3)",
  proc_dc_post   = "D&C (6902)",
  proc_asp_post  = "Aspiration (6952)",
  proc_miso_post = "Miso (75A2)",
  proc_oth_post  = "Other meds (75A4)"
)

# --- The 12 pairs your mentor listed (row var first, column var second) ---
crosstab_pairs <- list(
  list("proc_dc_ab",   "proc_asp_ab"),
  list("proc_dc_ab",   "proc_miso_ab"),
  list("proc_dc_ab",   "proc_oth_ab"),
  list("proc_asp_ab",  "proc_miso_ab"),
  list("proc_asp_ab",  "proc_oth_ab"),
  list("proc_miso_ab", "proc_oth_ab"),
  list("proc_dc_post",   "proc_asp_post"),
  list("proc_dc_post",   "proc_miso_post"),
  list("proc_dc_post",   "proc_oth_post"),
  list("proc_asp_post",  "proc_miso_post"),
  list("proc_asp_post",  "proc_oth_post"),
  list("proc_miso_post", "proc_oth_post")
)

# --- Helper to make ONE formatted crosstab (row var = code1, col var = code2) ---
make_pretty_crosstab <- function(df, code1, code2, labels, digits = 1) {
  # presence of each code (treat NA as 0)
  c1 <- rowSums(df[, code1, drop = FALSE], na.rm = TRUE) > 0
  c2 <- rowSums(df[, code2, drop = FALSE], na.rm = TRUE) > 0
  
  # 2x2 table + margins, with "Total" label
  tb <- table(c1, c2)
  tb <- addmargins(tb, 1:2, FUN = list(Total = sum))
  
  # to data frame and relabel axes
  df_tab <- as.data.frame.matrix(tb)
  rownames(df_tab) <- sub("^FALSE$", "No",  sub("^TRUE$", "Yes", rownames(df_tab)))
  colnames(df_tab) <- sub("^FALSE$", "No",  sub("^TRUE$", "Yes", colnames(df_tab)))
  
  # order rows/cols
  row_order <- intersect(c("No","Yes","Total"), rownames(df_tab))
  col_order <- intersect(c("No","Yes","Total"), colnames(df_tab))
  df_tab <- df_tab[row_order, col_order, drop = FALSE]
  
  # format counts + column %
  fmt <- as.data.frame(df_tab, stringsAsFactors = FALSE)
  col_totals <- sapply(c("No","Yes"), function(cn) {
    if (cn %in% colnames(df_tab)) sum(df_tab[c("No","Yes"), cn]) else NA_integer_
  })
  
  for (r in c("No","Yes")) {
    if (!r %in% rownames(df_tab)) next
    for (c in c("No","Yes")) {
      if (!c %in% colnames(df_tab)) next
      n <- df_tab[r, c]
      denom <- col_totals[c]
      pct <- if (is.na(denom) || denom == 0) NA_real_ else 100 * n / denom
      fmt[r, c] <- if (is.na(pct)) {
        formatC(n, format = "d", big.mark = ",")
      } else {
        paste0(formatC(n, format = "d", big.mark = ","), " (", sprintf(paste0("%.", digits, "f"), pct), ")")
      }
    }
  }
  
  # totals as plain counts with commas
  if ("Total" %in% colnames(fmt)) {
    for (r in rownames(fmt)) fmt[r, "Total"] <- formatC(df_tab[r, "Total"], format = "d", big.mark = ",")
  }
  if ("Total" %in% rownames(fmt)) {
    for (c in colnames(fmt)) fmt["Total", c] <- formatC(df_tab["Total", c], format = "d", big.mark = ",")
  }
  
  list(
    title_top  = labels[code2][1],  # column label (printed first line)
    title_side = labels[code1][1],  # row label    (printed second line)
    table      = fmt
  )
}

# --- Run all 12 crosstabs ---
ct_results <- lapply(crosstab_pairs, function(p) {
  make_pretty_crosstab(final_saeh, p[[1]], p[[2]], labels = proc_labels, digits = 1)
})

# --- Print in mentor's style (top = column var, side = row var) ---
for (res in ct_results) {
  cat("\n", res$title_top, "\n", res$title_side, "\n", sep = "")
  print(res$table, right = TRUE, quote = FALSE)
}


# Loop over your ct_results list
for (res in ct_results) {
  cat("\n==============================\n")
  cat("Column variable:", res$title_top, "\n")
  cat("Row variable:   ", res$title_side, "\n\n")
  
  # Print the table
  print(res$table, right = TRUE, quote = FALSE)
  
  cat("\n")  # extra spacing between tables
}



# create a list of gt tables
gt_tables <- lapply(seq_along(ct_results), function(i) {
  res <- ct_results[[i]]
  
  res$table %>%
    as.data.frame() %>%
    tibble::rownames_to_column("Row") %>%
    gt(rowname_col = "Row") %>%         # make 'Row' the row header
    tab_stubhead(label = res$title_side) %>%  # label the row stub
    tab_header(title = res$title_top)          # label the columns
})



# Save to one PDF
gtsave_extra <- function(gt_list, file) {
  tmp <- tempfile(fileext = ".html")
  htmltools::save_html(htmltools::tagList(gt_list), tmp)
  pagedown::chrome_print(tmp, output = file)
}

gtsave_extra(gt_tables, "all_tables.pdf")




###--- Cross-tab 16 tables PDF -------------------------------------------------


#  Create combo variables 
final_saeh <- final_saeh %>%
  mutate(
    combo_dc   = ifelse(rowSums(select(., proc_dc_ab, proc_dc_post), na.rm = TRUE) > 0, 1, 0),
    combo_asp  = ifelse(rowSums(select(., proc_asp_ab, proc_asp_post), na.rm = TRUE) > 0, 1, 0),
    combo_miso = ifelse(rowSums(select(., proc_miso_ab, proc_miso_post), na.rm = TRUE) > 0, 1, 0),
    combo_oth  = ifelse(rowSums(select(., proc_oth_ab, proc_oth_post), na.rm = TRUE) > 0, 1, 0))

#  Extend the labels
proc_labels <- c(
  proc_dc_ab   = "D&C (6901)",
  proc_asp_ab  = "Aspiration (6951)",
  proc_miso_ab = "Miso (75A1)",
  proc_oth_ab  = "Other meds (75A3)",
  proc_dc_post   = "D&C (6902)",
  proc_asp_post  = "Aspiration (6952)",
  proc_miso_post = "Miso (75A2)",
  proc_oth_post  = "Other meds (75A4)",
  combo_dc   = "D&C (6901 + 6902)",
  combo_asp  = "Aspiration (6951 + 6952)",
  combo_miso = "Miso (75A1 + 75A2)",
  combo_oth  = "Other meds (75A3 + 75A4)")

#  Define pairs
combo_pairs <- list(
  list("proc_dc_ab", "proc_dc_post"),    # D&C (6901) vs D&C (6902)
  list("proc_asp_ab", "proc_asp_post"),  # Aspiration (6951) vs Aspiration (6952)
  list("proc_miso_ab", "proc_miso_post"),# Miso (75A1) vs Miso (75A2)
  list("proc_oth_ab", "proc_oth_post")   # Other meds (75A3) vs Other meds (75A4)
)

#  Generate combo tables
combo_ct_results <- lapply(combo_pairs, function(p) {
  make_pretty_crosstab(final_saeh, p[[1]], p[[2]], labels = proc_labels, digits = 1)
})




#  Merge with existing 12 tables
all_ct_results <- c(ct_results, combo_ct_results)

#  Make final PDF
gt_tables <- lapply(seq_along(all_ct_results), function(i) {
  res <- all_ct_results[[i]]
  
  res$table %>%
    as.data.frame() %>%
    tibble::rownames_to_column("Row") %>%
    gt(rowname_col = "Row") %>%
    tab_stubhead(label = res$title_side) %>%
    tab_header(title = res$title_top)
})

gtsave_extra(gt_tables, "all_tables.pdf")


###--- Variable to sum abortion codes ------------------------------------------


# Add procedure count in one step
final_saeh <- final_saeh %>%
  mutate(proc_ab_count = rowSums(select(., proc_dc_ab:proc_oth_post, proc_dc_ab_other, proc_asp_ab_other), na.rm = TRUE))


# Overall summary table (0–8 codes)
proc_count_tab <- final_saeh %>%
  count(proc_ab_count) %>%
  complete(proc_ab_count = 0:8, fill = list(n = 0)) %>%   # ensure all 0–8 present
  mutate(
    pct = 100 * n / sum(n),
    n_pct = sprintf("%s (%.1f%%)", formatC(n, big.mark=","), pct)
  ) %>%
  select(`Number of codes found in discharge record` = proc_ab_count,
         `n (%)` = n_pct)

print(proc_count_tab)


# Create gt table
proc_count_gt <- proc_count_tab %>%
  gt() %>%
  tab_header(
    title = md("**Distribution of 8 abortion procedure flag variables**")
  ) %>%
  cols_align(align = "center", columns = everything()) %>%
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_column_labels(everything())
  ) %>%
  tab_options(
    table.font.size = px(14),
    table.border.top.style = "solid",
    table.border.bottom.style = "solid",
    column_labels.border.top.style = "solid",
    column_labels.border.bottom.style = "solid"
  )

# Save as PNG
gtsave(proc_count_gt, "proc_count_summary.png")

###--- Tabs of the first couple of promed variables among those with 0 promed codes ---------

abortion_codes <- c("6901","6902","6951","6952","75A1","75A2","75A3","75A4")

# restrict to cases with none of the 8 abortion procedure flags
df_zero <- final_saeh %>%
  filter(
    proc_dc_ab     == 0 &
      proc_dc_post   == 0 &
      proc_asp_ab    == 0 &
      proc_asp_post  == 0 &
      proc_miso_ab   == 0 &
      proc_miso_post == 0 &
      proc_oth_ab    == 0 &
      proc_oth_post  == 0
  )

make_promed_tab <- function(df, var, keep_codes = abortion_codes) {
  tb <- table(df[[var]], useNA = "ifany")
  tb <- tb[names(tb) %in% keep_codes]   # keep only abortion codes
  tb <- addmargins(tb)                  # add totals back in
  
  df_tab <- as.data.frame(tb, stringsAsFactors = FALSE)
  colnames(df_tab) <- c("Procedure Code", "Count")
  
  # Percent of non-total
  df_tab$Percent <- ifelse(
    df_tab$`Procedure Code` == "Sum",
    "",
    sprintf("%.1f%%", 100 * df_tab$Count / sum(df_tab$Count[df_tab$`Procedure Code` != "Sum"]))
  )
  
  df_tab
}



promed_labels <- c(
  promed1 = "promed1",
  promed2 = "promed2",
  promed3 = "promed2"
)

promed_results <- lapply(names(promed_labels), function(v) {
  make_promed_tab(df_zero, v, promed_labels[v], digits = 1)
})


for (res in promed_results) {
  cat("\n", res$title, "\n", sep = "")
  print(res$table, right = TRUE, quote = FALSE)
}


gt_tables <- lapply(promed_results, function(res) {
  res$table %>%
    gt() %>%
    tab_header(title = res$title)
})

gtsave_extra(gt_tables, "promed_tabulations.pdf")


###--- Tabs of promed codes  ---------------------------------------------------


# Step 2: Flag if any abortion procedure code is present in any promed field
final_saeh <- final_saeh %>%
  mutate(any_abortion = str_detect(promed_concat, paste0("\\b(", paste(abortion_codes, collapse="|"), ")\\b")))

# Step 3: Keep only rows with no evidence of abortion procedure codes
final_saeh_no_abortion <- final_saeh %>%
  filter(!any_abortion)

# Step 4: Tabulate promed1–3
tab_promed1 <- final_saeh_no_abortion %>%
  filter(!is.na(promed1)) %>%
  count(promed1) %>%
  arrange(desc(n)) %>%
  slice_head(n = 5)

tab_promed2 <- final_saeh_no_abortion %>%
  filter(!is.na(promed2)) %>%
  count(promed2) %>%
  arrange(desc(n)) %>%
  slice_head(n = 5)

tab_promed3 <- final_saeh_no_abortion %>%
  filter(!is.na(promed3)) %>%
  count(promed3) %>%
  arrange(desc(n)) %>%
  slice_head(n = 5)

# (Optional: View or print)
print(tab_promed1)
print(tab_promed2)
print(tab_promed3)

# Create gt tables for each tabulation
gt_promed1 <- tab_promed1 %>% gt() %>% tab_header(title = "Top 5 promed1 Codes")
gt_promed2 <- tab_promed2 %>% gt() %>% tab_header(title = "Top 5 promed2 Codes")
gt_promed3 <- tab_promed3 %>% gt() %>% tab_header(title = "Top 5 promed3 Codes")

# Save each gt table as a PNG (requires webshot2 and phantomjs installed)
gtsave(gt_promed1, "promed1.png")
gtsave(gt_promed2, "promed2.png")
gtsave(gt_promed3, "promed3.png")

# Read images with magick
img1 <- image_read("promed1.png")
img2 <- image_read("promed2.png")
img3 <- image_read("promed3.png")

# Combine vertically
combined_img <- image_append(c(img1, img2, img3), stack = TRUE)

# Save combined image
image_write(combined_img, "combined_promed_tables.png")






###--- Stacked Bar Graphs  -----------------------------------------------------


# Add missing flag (1 if promed1 is NA, else 0)
final_saeh <- final_saeh %>%
  mutate(proc_missing = ifelse(is.na(promed1), 1, 0))

# Add total procedure count including old codes + new codes 6909 and 6959
final_saeh <- final_saeh %>%
  mutate(proc_total = proc_dc_ab + proc_dc_post +
           proc_asp_ab + proc_asp_post +
           proc_miso_ab + proc_miso_post +
           proc_oth_ab + proc_oth_post +
           proc_dc_ab_other + proc_asp_ab_other)

# Update proc_labels to include the new codes for plotting labels
proc_labels <- c(
  proc_dc_ab         = "D&C (6901)",
  proc_asp_ab        = "Aspiration (6951)",
  proc_miso_ab       = "Misoprostol (75A1)",
  proc_oth_ab        = "Other (75A3)",
  proc_dc_post       = "D&C (6902)",
  proc_asp_post      = "Aspiration (6952)",
  proc_miso_post     = "Misoprostol (75A2)",
  proc_oth_post      = "Other (75A4)",
  proc_dc_ab_other   = "D&C (6909)",
  proc_asp_ab_other  = "A&C (6959)"
)

# Pivot long over all flags in proc_labels plus proc_missing for plotting
long_data <- final_saeh %>%
  pivot_longer(
    cols = c(names(proc_labels), "proc_missing"),
    names_to = "proc_code", values_to = "flag"
  ) %>%
  filter(flag == 1) %>%
  mutate(
    group = recode(proc_code, !!!proc_labels, proc_missing = "Missing"),
    phase = case_when(
      proc_code %in% c("proc_dc_ab", "proc_asp_ab", "proc_miso_ab", "proc_oth_ab", "proc_dc_ab_other", "proc_asp_ab_other") ~ "Pre",
      proc_code %in% c("proc_dc_post", "proc_asp_post", "proc_miso_post", "proc_oth_post", "proc_dc_ab_other", "proc_asp_ab_other") ~ "Post",
      proc_code == "proc_missing" ~ "Missing"
    )
  ) %>%
  select(year, phase, group) %>%
  bind_rows(
    final_saeh %>%
      filter(proc_total == 0 & !is.na(promed1)) %>%
      select(year) %>%
      mutate(phase = "Pre", group = "None"),
    final_saeh %>%
      filter(proc_total == 0 & !is.na(promed1)) %>%
      select(year) %>%
      mutate(phase = "Post", group = "None")
  )

# Summarize counts per year, phase, group (use counts, not percentages)
plot_data <- long_data %>%
  group_by(year, phase, group) %>%
  summarise(n = n(), .groups = "drop")

# Updated color palette to add new codes and Missing group
custom_colors <- c(
  "D&C (6901)"        = "#E66101",  # strong orange
  "Aspiration (6951)" = "#1F78B4",  # medium blue
  "Misoprostol (75A1)"= "#FDB863",  # light orange
  "Other (75A3)"      = "#A6CEE3",  # light blue
  "D&C (6902)"        = "#66a61e",  # dark burnt orange
  "Aspiration (6952)" = "#08519C",  # dark blue
  "Misoprostol (75A2)"= "#FF7F00",  # vibrant orange
  "Other (75A4)"      = "#3182BD",  # steel blue
  "D&C (6909)"        = "#FD8D3C",  # medium orange
  "A&C (6959)"        = "#6BAED6",  # sky blue
  "None"              = "#B0B0B0",  # neutral gray
  "Missing"           = "#FF4C4C"   # bright red for missing
)



# Plot Pre phase with counts on y-axis
ggplot(filter(plot_data, phase == "Pre"),
       aes(x = factor(year), y = n, fill = group)) +
  geom_bar(stat = "identity") +
  labs(x = "Year", y = "Count", title = "Pre Abortion Procedure Codes") +
  scale_fill_manual(values = custom_colors) +
  theme_minimal()

# Plot Post phase with counts on y-axis
ggplot(filter(plot_data, phase == "Post"),
       aes(x = factor(year), y = n, fill = group)) +
  geom_bar(stat = "identity") +
  labs(x = "Year", y = "Count", title = "Post Abortion Procedure Codes") +
  scale_fill_manual(values = custom_colors) +
  theme_minimal()




### By percent ---

# Make long but keep case IDs
long_data <- final_saeh %>%
  mutate(case_id = row_number()) %>%
  pivot_longer(
    cols = c(names(proc_labels), "proc_missing"),
    names_to = "proc_code", values_to = "flag") %>%
  filter(flag == 1) %>%
  mutate(
    group = recode(proc_code, !!!proc_labels, proc_missing = "Missing"),
    phase = case_when(
      proc_code %in% c("proc_dc_ab", "proc_asp_ab", "proc_miso_ab", "proc_oth_ab", 
                       "proc_dc_ab_other", "proc_asp_ab_other") ~ "Pre",
      proc_code %in% c("proc_dc_post", "proc_asp_post", "proc_miso_post", "proc_oth_post",
                       "proc_dc_ab_other", "proc_asp_ab_other") ~ "Post",
      proc_code == "proc_missing" ~ "Missing"))


# Calculate percentages per year, phase, group
plot_data_percent <- long_data %>%
  group_by(year, phase, group) %>%
  summarise(n = n_distinct(case_id), .groups = "drop") %>%
  group_by(year) %>%
  mutate(denom = n_distinct(long_data$case_id[long_data$year == first(year)]),
         pct = 100 * n / denom) %>%
  ungroup() %>%
  select(-denom)




# Plot Pre phase with percentages
ggplot(filter(plot_data_percent, phase == "Pre"),
       aes(x = factor(year), y = pct, fill = group)) +
  geom_bar(stat = "identity") +
  labs(x = "Year", y = "Percent", title = "Pre Abortion Procedure Codes") +
  scale_fill_manual(values = custom_colors) +
  theme_minimal()

# Plot Post phase with percentages
ggplot(filter(plot_data_percent, phase == "Post"),
       aes(x = factor(year), y = pct, fill = group)) +
  geom_bar(stat = "identity") +
  labs(x = "Year", y = "Percent", title = "Post Abortion Procedure Codes") +
  scale_fill_manual(values = custom_colors) +
  theme_minimal()










###--- Histogram of gestational age  -------------------------------------------

# Calculate overall missing proportion
total_n <- nrow(final_saeh)
missing_n <- sum(is.na(final_saeh$gestac))
missing_prop <- missing_n / total_n

# Create age groups for stratification (adjust breaks as desired)
final_saeh <- final_saeh %>%
  mutate(age_group = case_when(
    edad < 20 ~ "<20",
    edad >= 20 & edad < 30 ~ "20-29",
    edad >= 30 & edad < 40 ~ "30-39",
    edad >= 40 ~ "40+",
    TRUE ~ "Unknown"
  ))

# Calculate missing proportion per age group
missing_summary <- final_saeh %>%
  group_by(age_group) %>%
  summarise(
    missing_count = sum(is.na(gestac)),
    total_count = n(),
    missing_prop = missing_count / total_count
  )

# Overall histogram with missing proportion in title
ggplot(final_saeh, aes(x = gestac)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black", na.rm = TRUE) +
  labs(title = paste0("Gestational Age Histogram\nMissing: ", percent(missing_prop, accuracy = 0.1)),
       x = "Gestational Age",
       y = "Count") +
  theme_minimal()

# Histogram stratified by age group with facets
ggplot(final_saeh, aes(x = gestac)) +
  geom_histogram(binwidth = 1, fill = "darkgreen", color = "black", na.rm = TRUE) +
  facet_wrap(~age_group) +
  labs(title = "Gestational Age Histogram by Age Group",
       subtitle = "Each facet includes missing proportion for gestational age",
       x = "Gestational Age",
       y = "Count") +
  theme_minimal() +
  geom_text(data = missing_summary,
            aes(x = Inf, y = Inf, label = paste0("Missing: ", percent(missing_prop, accuracy = 0.1))),
            hjust = 1.1, vjust = 1.5, inherit.aes = FALSE)







###--- TABLE ONE  --------------------------------------------------------------

final_saeh <- final_saeh %>%
  mutate(
    habla_lengua_label = case_when(
      habla_lengua == 2 ~ "No",
      habla_lengua == 1 ~ "Yes",
      habla_lengua == 3 ~ "No response",
      habla_lengua == 4 ~ "No response",
      TRUE ~ "Missing"),
    indigena_label = case_when(
      indigena == 1 ~ "Yes",
      indigena == 2 ~ "No",
      indigena == 9 ~ "No response",
      TRUE ~ "Missing"))


final_saeh <- final_saeh %>%
  mutate(
    # State region categorization
    region = case_when(
      state %in% c("Baja California", "Baja California Sur", "Sonora", "Sinaloa", "Chihuahua", "Coahuila de Zaragoza", "Nuevo León", "Tamaulipas") ~ "North",
      state %in% c("Jalisco", "Colima", "Nayarit", "Michoacán de Ocampo", "Aguascalientes", "Zacatecas", "Guanajuato", "Durango", "San Luis Potosí", "Querétaro de Arteaga", "Hidalgo", "Veracruz de Ignacio de la Llave") ~ "Central",
      state == "Ciudad de México" ~ "Mexico City",
      state %in% c("Oaxaca", "Chiapas", "Tabasco", "Campeche", "Yucatán", "Quintana Roo", "Guerrero", "Puebla", "Tlaxcala", "Morelos") ~ "South",
      is.na(state) | state %in% c("Not Specified", "No response", "Not Applicable") ~ "Missing",
      TRUE ~ NA),
    
    
    # Age category
    age_cat = case_when(
      edad < 15 ~ "under 15",
      edad >= 15 & edad <= 16 ~ "15-16",
      edad >= 17 & edad <= 19 ~ "17-19",
      edad >= 20 & edad <= 24 ~ "20-24",
      edad >= 25 & edad <= 29 ~ "25-29",
      edad >= 30 & edad <= 34 ~ "30-34",
      edad >= 35 & edad <= 39 ~ "35-39",
      edad >= 40 ~ "40+",
      TRUE ~ "Missing"),
    
    
    # Relationship status
    relationship_status = case_when(
      relationship_status %in% c("Married", "Domestic partnership") ~ "Married/Partnership",
      relationship_status %in% c("Widowed", "Divorced", "Separated") ~ "Widowed/Divorced/Separated",
      is.na(relationship_status) | relationship_status %in% c("No response", "Not specified", "Not applicable", "Missing") ~ "Missing",
      TRUE ~ relationship_status),
    
    # Parity category
    parity_cat = case_when(
      is.na(parity) ~ "Missing",
      parity == 0 ~ "0",
      parity == 1 ~ "1",
      parity == 2 ~ "2",
      parity >= 3 ~ "3+",
      TRUE ~ "Missing"),
    
    # Insurance combination
    insurance_grouped = case_when(
      insurance %in% c("Public insurance", "INSABI", "IMSS BIENESTAR", "OPD IMSS BIENESTAR", "Prospera", "State government") ~ "Public/State programs",
      insurance %in% c("ISSFAM", "PEMEX", "SEDENA", "SEMAR", "ISSSTE") ~ "ISSFAM/PEMEX/SEDENA/SEMAR/ISSSTE",
      insurance %in% c("No response", "Not specified", "Not Specified", "Missing") | is.na(insurance) ~ "No response/Not specified/Missing",
      TRUE ~ insurance)
 )



# Handle the missing
final_saeh <- final_saeh %>%
  mutate(
    region = ifelse(is.na(region), "Missing", region),
    age_cat = ifelse(is.na(age_cat), "Missing", age_cat),
    habla_lengua_label = ifelse(is.na(habla_lengua_label), "Missing", habla_lengua_label),
    indigena_label = ifelse(is.na(indigena_label), "Missing", indigena_label),
    parity_cat = ifelse(is.na(parity_cat), "Missing", parity_cat),
    insurance_grouped  = ifelse(is.na(insurance_grouped ), "Missing", insurance_grouped )
  )



# Factor variables 
final_saeh <- final_saeh %>%
  mutate(
    region = fct_relevel(
      as.factor(region),
      "North",
      "Central",
      "Mexico City",
      "South",
      "Missing"),
    indigena_label = fct_relevel(
      as.factor(indigena_label),
      "Yes",
      "No",
      "No response",
      "Missing"),
    habla_lengua_label = fct_relevel(
      as.factor(habla_lengua_label),
      "Yes",
      "No",
      "No response",
      "Missing"),
    age_cat = fct_relevel(
      as.factor(age_cat),
      "under 15",
      "15-16",
      "17-19",
      "20-24",
      "25-29",
      "30-34",
      "35-39",
      "40+",
      "Missing"),
    insurance_grouped = fct_relevel(
      as.factor(insurance_grouped),
      "Uninsured",
      "IMSS",
      "Gratuidad",
      "Public/State programs",
      "Private insurance",
      "ISSFAM/PEMEX/SEDENA/SEMAR/ISSSTE",
      "Other",
      "Unknown",
      "Missing"),
    relationship_status = fct_relevel(
      as.factor(relationship_status),
      "Married/Partnership",
      "Widowed/Divorced/Separated",
      "Unknown",
      "Missing"))




# Create Table 1 summary with new categories
table_one <- final_saeh %>%
  filter(relationship_status != "Single") %>%
  mutate(
    relationship_status = fct_drop(relationship_status)  # Drop unused "Single" level here
  ) %>%
  select(
    region,
    age_cat,
    indigena_label,
    habla_lengua_label,
    relationship_status,
    parity_cat,
    insurance_grouped 
  ) %>%
  tbl_summary(
    type = list(
      age_cat ~ "categorical",
      parity_cat ~ "categorical"
    ),
    statistic = list(
      all_categorical() ~ "{n} ({p}%)"
    ),
    missing = "ifany",
    label = list(
      region ~ "State of Residence (Region)",
      age_cat ~ "Age",
      indigena_label ~ "Indigenous Identity",
      habla_lengua_label ~ "Speaks Indigenous Language",
      relationship_status ~ "Relationship Status",
      parity_cat ~ "Parity",
      insurance_grouped ~ "Insurance"
    )
  ) %>%
  modify_caption("Table 1. Descriptive Characteristics") %>%
  bold_labels()

# Convert to gt and save as PNG as before
table_one_gt <- as_gt(table_one)
Sys.setenv(CHROMOTE_HEADLESS = "new")
gtsave(table_one_gt, "saeh_tableone_10.9.png")








###--- Stacked Bar Graph by Insurance  -----------------------------------------


# Define your pre and post procedure code variable names:
pre_procs <- c("proc_dc_ab", "proc_asp_ab", "proc_miso_ab", "proc_oth_ab", "proc_dc_ab_other", "proc_asp_ab_other")
post_procs <- c("proc_dc_post", "proc_asp_post", "proc_miso_post", "proc_oth_post", "proc_dc_ab_other", "proc_asp_ab_other")

# Assign phase to each patient based on procedures present:
final_saeh <- final_saeh %>%
  mutate(
    phase = case_when(
      rowSums(select(., all_of(pre_procs)), na.rm = TRUE) > 0 ~ "Pre",
      rowSums(select(., all_of(post_procs)), na.rm = TRUE) > 0 ~ "Post",
      TRUE ~ NA))

# Define the colors
insurance_colors <- c(
  "Uninsured" = "#08519C",
  "Gratuidad" = "#1b9e77",
  "IMSS" = "#1F78B4",
  "ISSFAM/PEMEX/SEDENA/SEMAR/ISSSTE" = "#d95f02",
  "Private insurance" = "#A6CEE3",
  "Public/State programs" = "#66a61e",
  "No response/Not specified/Missing" = "#f768a1",
  "Unknown" = "#7570b3",
  "Other" = "#B0B0B0")



# Summarize counts by year, phase, insurance
insurance_counts <- final_saeh %>%
  filter(!is.na(insurance_grouped), !is.na(phase)) %>%
  mutate(case_id = row_number()) %>%
  group_by(year, phase, insurance_grouped) %>%
  summarise(count = n_distinct(case_id), .groups = "drop") %>%
  ungroup() %>%
  group_by(year, phase) %>%
  mutate(total = sum(count),
         percent = count / total * 100) %>%
  ungroup()

plot_insurance_percent <- function(phase_filter) {
  ggplot(filter(insurance_counts, phase == phase_filter),
         aes(x = factor(year), y = percent, fill = insurance_grouped)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_manual(values = insurance_colors) +
    labs(title = paste("Insurance Distribution -", phase_filter),
         x = "Year",
         y = "Percent",
         fill = "Insurance Type") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Generate plots
plot_pre_perc <- plot_insurance_percent("Pre")
plot_post_perc <- plot_insurance_percent("Post")

# Print plots
print(plot_pre_perc)
print(plot_post_perc)

# Recalculate counts and percentages by year and insurance
insurance_counts_year <- final_saeh %>%
  filter(!is.na(insurance_grouped)) %>%
  mutate(case_id = row_number()) %>%
  group_by(year, insurance_grouped) %>%
  summarise(count = n_distinct(case_id), .groups = "drop") %>%
  group_by(year) %>%
  mutate(percent = count / sum(count) * 100) %>%
  ungroup()

# Plot: one stacked bar per year, sum to 100%
ggplot(insurance_counts_year,
       aes(x = factor(year), y = percent, fill = insurance_grouped)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = insurance_colors) +
  labs(title = "Insurance Distribution by Year",
       x = "Year",
       y = "Percent",
       fill = "Insurance Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))






###--- Stacked Bar Graph by Region  --------------------------------------------

# Define color palette for regions (adjust colors and regions as appropriate)
region_colors <- c(
  "Central" = "#00441b",
  "North" = "#41ab5d",
  "Mexico City" = "#f768a1",
  "South" = "#41b6c4",
  "Missing" = "#08589e")



# Summarize counts by year, phase, region
region_counts <- final_saeh %>%
  filter(!is.na(region), !is.na(phase)) %>%
  mutate(case_id = row_number()) %>%
  group_by(year, phase, region) %>%
  summarise(count = n_distinct(case_id), .groups = "drop") %>%
  ungroup()

plot_region_counts <- function(phase_filter) {
  ggplot(filter(region_counts, phase == phase_filter),
         aes(x = factor(year), y = count, fill = region)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_manual(values = region_colors) +
    labs(title = paste("Region Distribution -", phase_filter),
         x = "Year",
         y = "Count",
         fill = "Region") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Generate plots
plot_pre_region <- plot_region_counts("Pre")
plot_post_region <- plot_region_counts("Post")

# Print plots
print(plot_pre_region)
print(plot_post_region)
