# SQL ->

-- Lookup for landnavn
WITH country_lookup AS (
  SELECT country_code, comment AS country_full
  FROM `imposing-yen-426717-u4.enum_lookup_tables.countries`
),

-- Mobile + Autoscout med bruktfilter
base_data AS (
  SELECT
  'mobile' AS source,
  IFNULL(sellerCountry, 51) AS country_code,
  make AS brand,
  model
  FROM `imposing-yen-426717-u4.mobile.staging_mobile_prod`
  WHERE make IS NOT NULL
  AND model IS NOT NULL
  AND (firstRegistration IS NOT NULL OR mileage > 100)
  
  UNION ALL
  
  SELECT
  'autoscout',
  sellerCountry AS country_code,
  make AS brand,
  model
  FROM `imposing-yen-426717-u4.autoscout.staging_autoscout_prod`
  WHERE make IS NOT NULL
  AND model IS NOT NULL
  AND (firstRegistration IS NOT NULL OR mileage > 100)
),

-- Bilinfo med bruktfilter basert på kjørelengde
bilinfo_data AS (
  SELECT
  'bilinfo' AS source,
  0 AS country_code,
  Make AS brand,
  Model AS model
  FROM `imposing-yen-426717-u4.bilinfo.prod_bilinfo_prod`
  WHERE Make IS NOT NULL
  AND Model IS NOT NULL
  AND Mileage > 100
),

-- Kombiner alle datakilder
all_data AS (
  SELECT * FROM base_data
  UNION ALL
  SELECT * FROM bilinfo_data
),

-- Aggreger per brand/model/land
brand_model_counts AS (
  SELECT
  country_code,
  UPPER(TRIM(brand)) AS brand,
  UPPER(TRIM(model)) AS model,
  COUNT(*) AS num_ads
  FROM all_data
  GROUP BY country_code, brand, model
),

-- Totalannonser per land
total_per_country AS (
  SELECT
  country_code,
  SUM(num_ads) AS total_ads
  FROM brand_model_counts
  GROUP BY country_code
),

-- Sluttresultat
final AS (
  SELECT
  bmc.country_code,
  cl.country_full,
  bmc.brand,
  bmc.model,
  bmc.num_ads,
  tpc.total_ads
  FROM brand_model_counts bmc
  JOIN total_per_country tpc USING (country_code)
  JOIN country_lookup cl USING (country_code)
  WHERE tpc.total_ads >= 1000
)

SELECT *
  FROM final
ORDER BY total_ads DESC, country_full, num_ads DESC;


# -------

library(readxl)
library(dplyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(segmented)
library(purrr)
library(tibble)
library(gridExtra)
library(grid)
library(scales)  # for number_format()
library(knitr)
library(tidyr)
library(ggrepel)
library(zoo)
library(bigrquery)
library(DBI)
library(tidyverse)
library(countrycode)
library(readr)
library(forcats)
library(tidytext)  # For reorder_within() og scale_y_reordered()


# 1. Les inn datasettet
df <- read_csv("brand_model_data.csv")

# 2. Rens modellnavn og land
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

# 3. Validering
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

# 4. Slå sammen evt. duplikater i country_full
df <- df %>%
  group_by(country_full, brand, model) %>%
  summarise(
    num_ads = sum(num_ads),
    total_ads = sum(total_ads),
    .groups = "drop"
  )

# 5. Finn de 8 største landene
top_countries <- df %>%
  group_by(country_full) %>%
  summarise(total = sum(num_ads), .groups = "drop") %>%
  arrange(desc(total)) %>%
  slice_head(n = 8) %>%
  pull(country_full)

df <- df %>% filter(country_full %in% top_countries)

# 6. Merkede andeler per land
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

# 7. Toppmodeller per merke (hele Europa)
df_model_brand <- df %>%
  filter(brand %in% top_brands) %>%
  group_by(brand, model) %>%
  summarise(num_ads = sum(num_ads), .groups = "drop") %>%
  arrange(brand, desc(num_ads)) %>%
  group_by(brand) %>%
  slice_head(n = 5) %>%
  ungroup()

# 8. Visualiseringer

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
