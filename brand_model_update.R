# brand_model_update.R

# --- 1. Laster biblioteker ---
library(DBI)
library(bigrquery)
library(dplyr)
library(ggplot2)
library(scales)
library(tidytext)
library(forcats)
library(stringr)
library(ggrepel)

# --- 2. Koble til BigQuery og hent data ---
project_id <- "imposing-yen-426717-u4"
dataset <- "wasteson_insight_brand_model_data"
table <- "daily_brand_model_share"

con <- dbConnect(
  bigrquery::bigquery(),
  project = project_id,
  dataset = dataset,
  billing = project_id
)

df <- dbReadTable(con, table)

# --- 3. Rens og valider data ---
df <- df %>%
  filter(
    !is.na(model),
    model != "",
    toupper(model) != "OTHERS"
  ) %>%
  mutate(
    country_full = if_else(country_code == 0, "Denmark", country_full),
    country_full = str_to_title(country_full)
  )

# Valideringsfunksjon (kan evt. kommenteres ut etter behov)
validate_data <- function(df) {
  cat("🧪 Starter datavalidering...\n\n")
  cat("🔍 Manglende verdier:\n")
  print(sapply(df, function(x) sum(is.na(x))))
  cat("\n🚗 Topp 10 merker:\n")
  df %>% count(brand, sort = TRUE) %>% slice_head(n = 10) %>% print()
  cat("\n🚘 Topp 10 modeller:\n")
  df %>% count(model, sort = TRUE) %>% slice_head(n = 10) %>% print()
  cat("\n🔢 Unike kombinasjoner av brand + model + country:\n")
  cat(df %>% distinct(brand, model, country_full) %>% nrow(), "\n")
  cat("\n⚠️ Korte modellnavn (<3 tegn):\n")
  df %>% filter(nchar(model) < 3) %>% count(model, sort = TRUE) %>% print()
  cat("\n✅ Validering ferdig.\n")
}

validate_data(df)

# --- 4. Aggregering og utvalg ---
df <- df %>%
  group_by(country_full, brand, model) %>%
  summarise(num_ads = sum(num_ads), total_ads = sum(total_ads), .groups = "drop")

top_countries <- df %>%
  group_by(country_full) %>%
  summarise(total = sum(num_ads), .groups = "drop") %>%
  arrange(desc(total)) %>%
  slice_head(n = 8) %>%
  pull(country_full)

df <- df %>% filter(country_full %in% top_countries)

total_ads_country <- df %>%
  group_by(country_full) %>%
  summarise(total_ads = sum(num_ads), .groups = "drop")

top_brands <- df %>%
  group_by(brand) %>%
  summarise(total = sum(num_ads), .groups = "drop") %>%
  arrange(desc(total)) %>%
  slice_head(n = 10) %>%
  pull(brand)

df_brand_country <- df %>%
  filter(brand %in% top_brands) %>%
  group_by(country_full, brand) %>%
  summarise(num_ads = sum(num_ads), .groups = "drop") %>%
  left_join(total_ads_country, by = "country_full") %>%
  mutate(pct = num_ads / total_ads)

df_model_brand <- df %>%
  filter(brand %in% top_brands) %>%
  group_by(brand, model) %>%
  summarise(num_ads = sum(num_ads), .groups = "drop") %>%
  arrange(brand, desc(num_ads)) %>%
  group_by(brand) %>%
  slice_head(n = 5) %>%
  ungroup()

# --- 5. Visualiseringer ---
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

# A) Merkeandel per land
ggplot(df_brand_country, aes(x = pct, y = reorder_within(brand, pct, country_full), fill = brand)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ country_full, scales = "free_y") +
  scale_x_continuous(labels = percent_format()) +
  scale_y_reordered() +
  labs(
    title = "Top 10 Brands – Market Share by Country",
    x = "Markedsandel",
    y = NULL
  ) +
  theme_dark_custom

ggsave("brand_share_by_country_dark.png", width = 12, height = 8, dpi = 300, bg = "black")

# B) Toppmodeller per merke
ggplot(df_model_brand, aes(x = num_ads, y = reorder_within(model, num_ads, brand), fill = brand)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ brand, scales = "free_y") +
  scale_x_continuous(labels = comma) +
  scale_y_reordered() +
  labs(
    title = "Top 5 Models for Each of the 10 Largest Brands",
    x = "Antall Annonser",
    y = NULL
  ) +
  theme_dark_custom

ggsave("top_models_by_brand_dark.png", width = 14, height = 10, dpi = 300, bg = "black")
