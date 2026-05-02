# =============================================================================
#  — DATA COLLECTION, CLASSIFICATION & QUALITY CHECK
# =============================================================================
# Thesis  : Carbon Risk Pricing in Indian Bond Markets 
# Author  : Padmanava majee | central university of south bihar 
# Date    : 2026
# Purpose : Download NSE equity prices for 15 Brown + 15 Green stocks,
#           quality check, clean, and save for downstream scripts.
#
# Stock classification basis:
#   BROWN → SEBI BRSR Core FY2023 Scope 1+2 top emitters; BEE PAT Scheme
#            Cycle 2-3; RBI Climate Risk Paper Jan 2023; MoPNG/MoC lists
#   GREEN → CBI-certified green bond issuers; RE100 India members; MNRE
#            registered renewables; SBTi-aligned firms; CDP A-list India


pkgs <- c("tidyquant","tidyverse","ggplot2","openxlsx","lubridate","scales","purrr")
new  <- pkgs[!pkgs %in% installed.packages()[,"Package"]]
if (length(new)) install.packages(new, dependencies = TRUE)

suppressPackageStartupMessages({
  library(tidyquant); library(tidyverse); library(ggplot2)
  library(openxlsx);  library(lubridate); library(scales)
  library(purrr)
})

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
dir.create("data/clean", recursive = TRUE, showWarnings = FALSE)
dir.create("output/plots", recursive = TRUE, showWarnings = FALSE)
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)






# ── Time Period ─────────────────────────────────────────
START <- "2018-07-01"
END   <- "2025-12-31"

# ── BROWN STOCKS ────────────────────────────────────────
brown_stocks <- c(
  "COALINDIA.NS","NMDC.NS","ONGC.NS","IOC.NS","BPCL.NS",
  "HINDPETRO.NS","OIL.NS","TATASTEEL.NS","JSWSTEEL.NS",
  "SAIL.NS","ULTRACEMCO.NS","SHREECEM.NS","AMBUJACEM.NS",
  "HINDALCO.NS","VEDL.NS"
)

cat("Downloading BROWN stocks...\n")

brown_data <- purrr::map_dfr(brown_stocks, function(x){
  tryCatch({
    tidyquant::tq_get(x, from = START, to = END)
  }, error = function(e){
    message(paste("Failed:", x))
    return(NULL)
  })
})

# ── GREEN STOCKS ────────────────────────────────────────
green_stocks <- c(
  # Renewable Power Generation (7)
  "NHPC.NS", "SJVN.NS", "NTPC.NS", "TATAPOWER.NS",
  "ADANIGREEN.NS",     # Listed June 2018 — verify first row
  "TORNTPOWER.NS", "JSWENERGY.NS",
  
  # Wind & Solar Equipment (2) — BOROSIL removed
  "SUZLON.NS", "INOXWIND.NS",
  
  # Green Infrastructure & Clean Tech (3)
  "POWERGRID.NS", "SIEMENS.NS", "ABB.NS",
  
  # Green Transition Utility (1) — replaces BOROSIL
  "CESC.NS",
  
  # Clean Technology Enabler (1) — replaces TATAMOTORS
  "HAVELLS.NS",
  
  # EV / Mobility (1) — kept one EV stock
  "M&M.NS"
)

cat("Downloading GREEN stocks...\n")

green_data <- purrr::map_dfr(green_stocks, function(x){
  tryCatch({
    tidyquant::tq_get(x, from = START, to = END)
  }, error = function(e){
    message(paste("Failed:", x))
    return(NULL)
  })
})

# ── 2. SAFE DOWNLOAD FUNCTION ─────────────────────────────────────────────────
safe_get <- function(ticker){
  tryCatch({
    tq_get(ticker, from = "2018-07-01", to = "2025-12-31")
  }, error = function(e){
    message(paste("FAILED:", ticker))
    return(NULL)
  })
}

cat("Downloading data safely...\n")

brown_raw <- map_dfr(brown_stocks, safe_get)
green_raw <- map_dfr(green_stocks, safe_get)

# Remove failed tickers
brown_raw <- brown_raw %>% drop_na(symbol)
green_raw <- green_raw %>% drop_na(symbol)


# ── 3. QUALITY CHECK (IMPROVED) ───────────────────────────────────────────────

qc <- function(df, label){
  df %>%
    group_by(symbol) %>%
    arrange(date) %>%
    mutate(
      return = log(adjusted / lag(adjusted)),
      zero_price = adjusted <= 0
    ) %>%
    summarise(
      group = label,
      obs = n(),
      missing = sum(is.na(adjusted)),
      pct_missing = round(100 * missing / obs, 2),
      zero_prices = sum(zero_price, na.rm = TRUE),
      extreme_returns = sum(abs(return) > 0.5, na.rm = TRUE), # >50% daily move
      start = min(date),
      end = max(date),
      .groups = "drop"
    ) %>%
    mutate(
      flag = case_when(
        pct_missing > 3 ~ "REVIEW",
        zero_prices > 0 ~ "REVIEW",
        extreme_returns > 5 ~ "REVIEW",
        TRUE ~ "OK"
      )
    )
}

brown_qc <- qc(brown_raw, "BROWN")
green_qc <- qc(green_raw, "GREEN")

print(brown_qc)
print(green_qc)


# ── 4. REMOVE BAD STOCKS ──────────────────────────────────────────────────────

b_remove <- brown_qc %>% filter(flag=="REVIEW") %>% pull(symbol)
g_remove <- green_qc %>% filter(flag=="REVIEW") %>% pull(symbol)

brown_clean <- brown_raw %>%
  filter(!symbol %in% b_remove) %>%
  drop_na(adjusted) %>%
  mutate(group="BROWN")

green_clean <- green_raw %>%
  filter(!symbol %in% g_remove) %>%
  drop_na(adjusted) %>%
  mutate(group="GREEN")

all_data <- bind_rows(brown_clean, green_clean)
# ── 4. REMOVE BAD STOCKS ──────────────────────────────────────────────────────

b_remove <- brown_qc %>% filter(flag=="REVIEW") %>% pull(symbol)
g_remove <- green_qc %>% filter(flag=="REVIEW") %>% pull(symbol)

brown_clean <- brown_raw %>%
  filter(!symbol %in% b_remove) %>%
  drop_na(adjusted) %>%
  mutate(group="BROWN")

green_clean <- green_raw %>%
  filter(!symbol %in% g_remove) %>%
  drop_na(adjusted) %>%
  mutate(group="GREEN")

all_data <- bind_rows(brown_clean, green_clean)

# ── 5. DYNAMIC TRADING DAYS (FIXED) ───────────────────────────────────────────

total_days <- all_data %>%
  distinct(date) %>%
  nrow()

cov_df <- all_data %>%
  group_by(symbol, group) %>%
  summarise(days = n(), .groups="drop") %>%
  mutate(pct = round(100 * days / total_days,1))

# ── 6. PLOT ───────────────────────────────────────────────────────────────────

p <- ggplot(cov_df, aes(x=days, y=reorder(symbol, days), fill=group)) +
  geom_col() +
  labs(title="Coverage Check (Corrected)", x="Trading Days", y="Ticker") +
  theme_minimal()

ggsave("output/plots/coverage_fixed.png", p, width=10, height=8)


# ── 7. SAVE CLEAN DATA ────────────────────────────────────────────────────────

saveRDS(brown_clean, "data/clean/brown_clean.rds")
saveRDS(green_clean, "data/clean/green_clean.rds")
saveRDS(all_data, "data/clean/all_data.rds")

write.xlsx(list(
  Brown_QC = brown_qc,
  Green_QC = green_qc
), "output/tables/QC_fixed.xlsx")

# =============================================================================
# SCRIPT 02 — MONTHLY RETURNS & WAR (REFACTORED)
# =============================================================================
install.packages("moments")
suppressPackageStartupMessages({
  library(tidyquant); library(tidyverse); library(ggplot2)
  library(openxlsx);  library(lubridate); library(scales)
  library(zoo);     library("moments")
})

cat("\n===  RETURNS & WAR (CLEAN VERSION) ===\n\n")

# ── 2. MONTHLY LOG RETURNS ───────────────────────────────
calc_ret <- function(df, label){
  df %>%
    group_by(symbol) %>%
    tq_transmute(
      select     = adjusted,
      mutate_fun = periodReturn,
      period     = "monthly",
      type       = "log",
      col_rename = "ret"
    ) %>%
    mutate(group = label,
           mth   = floor_date(date, "month")) %>%
    ungroup()
}

brown_ret <- calc_ret(brown_clean, "BROWN")
green_ret <- calc_ret(green_clean, "GREEN")
all_ret   <- bind_rows(brown_ret, green_ret)

# ── 3. RETURN SUMMARY ────────────────────────────────────
ret_stats <- all_ret %>%
  group_by(group) %>%
  summarise(
    mean = mean(ret, na.rm=TRUE),
    sd   = sd(ret,   na.rm=TRUE),
    skew = moments::skewness(ret, na.rm=TRUE),
    kurt = moments::kurtosis(ret, na.rm=TRUE) - 3,
    .groups="drop"
  )

print(ret_stats)

# ── 4. PROXY VALUE WEIGHTS (NO LOOK-AHEAD BIAS) ──────────
proxy_weights <- function(df){
  df %>%
    mutate(mth = floor_date(date, "month")) %>%
    group_by(symbol, mth) %>%
    summarise(
      px  = last(adjusted),
      vol = mean(volume, na.rm=TRUE),
      cap = px * vol,
      .groups="drop"
    ) %>%
    group_by(mth) %>%
    mutate(w = cap / sum(cap, na.rm=TRUE)) %>%
    ungroup()
}

w_brown <- proxy_weights(brown_clean)
w_green <- proxy_weights(green_clean)

# ── 5. VALUE-WEIGHTED WAR ────────────────────────────────
calc_war_vw <- function(ret_df, w_df, label){
  ret_df %>%
    filter(group == label) %>%
    left_join(w_df, by = c("symbol","mth")) %>%
    group_by(mth) %>%
    summarise(
      WAR = sum(ret * w, na.rm=TRUE),
      n   = n_distinct(symbol),
      .groups="drop"
    )
}

b_vw <- calc_war_vw(brown_ret, w_brown, "BROWN")
g_vw <- calc_war_vw(green_ret, w_green, "GREEN")

WAR_vw <- b_vw %>%
  rename(WAR_brown_vw = WAR) %>%
  left_join(g_vw %>% rename(WAR_green_vw = WAR), by="mth") %>%
  rename(date = mth)
# ── 6. EQUAL-WEIGHTED WAR ────────────────────────────────
WAR_ew <- all_ret %>%
  group_by(group, date = mth) %>%
  summarise(WAR = mean(ret, na.rm=TRUE), .groups="drop") %>%
  pivot_wider(names_from=group, values_from=WAR) %>%
  rename(
    WAR_brown_ew = BROWN,
    WAR_green_ew = GREEN
  )


# ── 7. FINAL MERGE ───────────────────────────────────────
WAR <- WAR_vw %>%
  left_join(WAR_ew, by="date") %>%
  arrange(date)


# ── 8. CARBON PREMIUM (IMPORTANT ADDITION) ───────────────
WAR <- WAR %>%
  mutate(
    carbon_premium_vw = WAR_brown_vw - WAR_green_vw,
    carbon_premium_ew = WAR_brown_ew - WAR_green_ew
  )

cat("\nCarbon premium (VW mean):",
    round(mean(WAR$carbon_premium_vw, na.rm=TRUE)*100,4), "%\n")



# ── 9. CUMULATIVE RETURNS ────────────────────────────────
WAR_cum <- WAR %>%
  mutate(
    cum_brown = cumsum(replace_na(WAR_brown_vw,0)),
    cum_green = cumsum(replace_na(WAR_green_vw,0))
  )

# ── 7. Master WAR ─────────────────────────────────────────────────────────────

WAR <- left_join(WAR_vw, WAR_ew, by = "date") %>% arrange(date)

cat("\n── WAR summary ───────────────────────────────────────────────\n")
cat(sprintf("  WAR_brown_vw  mean: %.4f%% | sd: %.4f%%\n",
            mean(WAR$WAR_brown_vw,na.rm=TRUE)*100, sd(WAR$WAR_brown_vw,na.rm=TRUE)*100))
cat(sprintf("  WAR_green_vw  mean: %.4f%% | sd: %.4f%%\n",
            mean(WAR$WAR_green_vw,na.rm=TRUE)*100, sd(WAR$WAR_green_vw,na.rm=TRUE)*100))
cat(sprintf("  WAR_brown_ew  mean: %.4f%% | sd: %.4f%% (robustness)\n",
            mean(WAR$WAR_brown_ew,na.rm=TRUE)*100, sd(WAR$WAR_brown_ew,na.rm=TRUE)*100))
cat(sprintf("  WAR_green_ew  mean: %.4f%% | sd: %.4f%% (robustness)\n",
            mean(WAR$WAR_green_ew,na.rm=TRUE)*100, sd(WAR$WAR_green_ew,na.rm=TRUE)*100))


# ── 8. Cumulative returns ─────────────────────────────────────────────────────

WAR_cum <- WAR %>%
  arrange(date) %>%
  mutate(
    cum_brown_vw = cumsum(replace_na(WAR_brown_vw, 0)) * 100,
    cum_green_vw = cumsum(replace_na(WAR_green_vw, 0)) * 100,
    cum_brown_ew = cumsum(replace_na(WAR_brown_ew, 0)) * 100,
    cum_green_ew = cumsum(replace_na(WAR_green_ew, 0)) * 100
  )

