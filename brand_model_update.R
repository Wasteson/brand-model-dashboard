#!/usr/local/bin/Rscript

# --- Pakker ---
suppressPackageStartupMessages({
  library(rmarkdown)
  library(dplyr)
  library(bigrquery)
  library(ggplot2)
  library(tidyr)
  library(scales)
  library(forcats)
  library(stringr)
})

# --- Logging ---
timestamp <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")
log_msg <- function(msg) cat(sprintf("[%s] %s\n", timestamp(), msg))

log_msg("🚀 Starter brand_model_update job...")

# --- Base path setup ---
base_dir <- "/Users/oystein/Desktop/wasteson/TrackSights/datating/looker"
project_dir <- file.path(base_dir, "bigquery_3 auto merke og model analyse")

# Filbaner
csv_path       <- file.path(project_dir, "brand_model_data.csv")
png_brand_path <- file.path(project_dir, "brand_share_by_country_dark.png")
png_models_path <- file.path(project_dir, "top_models_by_brand_dark.png")
rmd_path       <- file.path(project_dir, "brand_dashboard.Rmd")
output_dir     <- project_dir

# --- Pandoc ---
Sys.setenv(RSTUDIO_PANDOC = "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/aarch64")

# --- BigQuery autentisering ---
tryCatch({
  bq_auth(path = "~/.gcp/oystein-service-account_2.json")
  log_msg("✅ Autentisering OK.")
}, error = function(e) {
  log_msg(paste("❌ Feil under autentisering:", e$message))
  quit(status = 1)
})

# --- Hent data fra BigQuery ---
project <- "imposing-yen-426717-u4"
sql <- "SELECT * FROM `imposing-yen-426717-u4.wasteson_insight.daily_brand_model_share`"

tryCatch({
  log_msg("📡 Henter data fra BigQuery...")
  query_job <- bq_project_query(project, sql)
  df <- bq_table_download(query_job)
  log_msg(paste("✅ Hentet", nrow(df), "rader."))
}, error = function(e) {
  log_msg(paste("❌ Feil under henting av data:", e$message))
  quit(status = 1)
})

# --- Lagre CSV ---
tryCatch({
  write.csv(df, csv_path, row.names = FALSE)
  log_msg(paste("✅ Lagret data til", csv_path))
}, error = function(e) {
  log_msg(paste("❌ Feil under lagring av CSV:", e$message))
  quit(status = 1)
})

# --- Dataforberedelse for grafer ---
df <- df %>%
  filter(!is.na(model), model != "", toupper(model) != "OTHERS") %>%
  mutate(country_full = str_to_title(country_full))

# Aggregering
df_brand_country <- df %>%
  group_by(country_full, brand) %>%
  summarise(num_ads = sum(num_ads), .groups = "drop") %>%
  group_by(country_full) %>%
  mutate(total_ads = sum(num_ads), pct = num_ads / total_ads) %>%
  ungroup()

# Topp 10 brands
top_brands <- df_brand_country %>%
  group_by(brand) %>%
  summarise(total = sum(num_ads), .groups = "drop") %>%
  arrange(desc(total)) %>%
  slice_head(n = 10) %>%
  pull(brand)

df_brand_country <- df_brand_country %>%
  filter(brand %in% top_brands)

# Toppmodeller
df_model_brand <- df %>%
  filter(brand %in% top_brands) %>%
  group_by(brand, model) %>%
  summarise(num_ads = sum(num_ads), .groups = "drop") %>%
  arrange(brand, desc(num_ads)) %>%
  group_by(brand) %>%
  slice_head(n = 5) %>%
  ungroup()

# --- Tilpasset tema ---
theme_dark_custom <- theme_minimal(base_size = 11) +
  theme(
    plot.background = element_rect(fill = "black", color = NA),
    panel.background = element_rect(fill = "black", color = NA),
    strip.background = element_rect(fill = "black"),
    strip.text = element_text(color = "white", face = "bold"),
    axis.text = element_text(color = "white"),
    axis.title = element_text(color = "white"),
    plot.title = element_text(color = "white", face = "bold"),
    plot.subtitle = element_text(color = "white"),
    legend.text = element_text(color = "white"),
    legend.title = element_text(color = "white")
  )

# --- Generer grafer ---
tryCatch({
  log_msg("📊 Lager grafer...")
  
  # A) Merkeandel per land
  p1 <- ggplot(df_brand_country, aes(x = pct, y = reorder_within(brand, pct, country_full), fill = brand)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ country_full, scales = "free_y") +
    scale_x_continuous(labels = percent_format()) +
    scale_y_reordered() +
    labs(title = "Top 10 Brands – Market Share by Country", x = "Markedsandel", y = NULL) +
    theme_dark_custom
  ggsave(filename = png_brand_path, plot = p1, width = 12, height = 8, dpi = 300, bg = "black")
  
  # B) Toppmodeller per merke
  p2 <- ggplot(df_model_brand, aes(x = num_ads, y = reorder_within(model, num_ads, brand), fill = brand)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ brand, scales = "free_y") +
    scale_x_continuous(labels = comma) +
    scale_y_reordered() +
    labs(title = "Top 5 Models for Each of the 10 Largest Brands", x = "Antall Annonser", y = NULL) +
    theme_dark_custom
  ggsave(filename = png_models_path, plot = p2, width = 14, height = 10, dpi = 300, bg = "black")
  
  log_msg("✅ Grafer lagret.")
}, error = function(e) {
  log_msg(paste("❌ Feil under grafgenerering:", e$message))
})

# --- Render dashboard ---
tryCatch({
  log_msg("🖼 Kjører rmarkdown::render for brand dashboard...")
  rmarkdown::render(input = rmd_path, output_file = "index.html", output_dir = output_dir)
  log_msg("✅ Dashboard generert OK.")
}, error = function(e) {
  log_msg(paste("⚠️ Dashboardfil ikke funnet:", e$message))
})

log_msg("🎉 Jobb fullført uten feil!")
