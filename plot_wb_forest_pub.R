# ============================================================
# Publication-quality within-between forest plots
# Target: Translational Psychiatry (Nature family, 180 mm wide)
#
# Palette note: yellow (#F1C40F) excluded — ΔE 0.4 deutan vs
# orange, fails CVD separation. Replaced with steel blue.
# All three colours below pass the 5-check validator.
# ============================================================

library(ggplot2)
library(patchwork)
library(dplyr)

setwd("/Users/msy/Documents/PRISM-V/suicide")

# ─────────────────────────────────────────────────────────────
# 0. Validated significance palette
# ─────────────────────────────────────────────────────────────
SIG_COLORS <- c("[FDR]" = "#9B1F1F",   # crimson
                "*"     = "#D4721A",   # amber
                "."     = "#3876A8")   # steel blue (secondary; print-safe)
SIG_SIZES  <- c("[FDR]" = 2.8,
                "*"     = 2.2,
                "."     = 1.8)
SIG_BREAKS <- c("[FDR]", "*", ".")

# ─────────────────────────────────────────────────────────────
# 1. Feature label maps
# ─────────────────────────────────────────────────────────────

# F0: pattern-based labels; Δ prefix for within-session delta features
label_f0_base <- function(base) {
  dplyr::case_when(
    base == "iqr1-2"         ~ "IQR Q₁–Q₂",
    base == "iqr2-3"         ~ "IQR Q₂–Q₃",
    base == "iqr1-3"         ~ "IQR Q₁–Q₃",
    base == "quartile1"      ~ "Q₁ (25th %ile)",
    base == "quartile2"      ~ "Q₂ (median)",
    base == "quartile3"      ~ "Q₃ (75th %ile)",
    base == "percentile1.0"  ~ "P1",
    base == "percentile99.0" ~ "P99",
    base == "kurtosis"       ~ "Kurtosis",
    base == "skewness"       ~ "Skewness",
    base == "qregerrQ"       ~ "Quadratic fit RMSE",
    base == "linregerrQ"     ~ "Linear fit RMSE",
    base == "pctlrange0-1"   ~ "Pctlrange 0–1",
    base == "qregc3"         ~ "Quadratic coeff. c₃",
    base == "qregc2"         ~ "Quadratic coeff. c₂",
    base == "qregc1"         ~ "Quadratic coeff. c₁",
    base == "linregc2"       ~ "Linear slope",
    base == "linregc1"       ~ "Linear intercept",
    base == "amean"          ~ "Mean F0",
    base == "stddev"         ~ "SD",
    base == "range"          ~ "Range",
    TRUE                     ~ tools::toTitleCase(gsub("[-_]", " ", base))
  )
}

label_f0_full <- function(raw) {
  # raw = full col name e.g. "F0final_sma_de_iqr1-2"
  stripped <- sub("^F0final_sma_", "", raw)
  delta    <- grepl("^de_", stripped)
  base     <- sub("^de_", "", stripped)
  readable <- label_f0_base(base)
  ifelse(delta, paste0("Δ ", readable), readable)
}

# LIWC: lookup table with fallback
LIWC_LABELS <- c(
  "liwc_death"                  = "Death-related words",
  "liwc_inaction"               = "Inaction",
  "liwc_focus_present"          = "Present focus",
  "liwc_emotional_tone"         = "Emotional tone",
  "liwc_positive_emotion_words" = "Positive emotion",
  "liwc_avoidance"              = "Avoidance",
  "liwc_high_empathy"           = "High empathy",
  "liwc_family"                 = "Family",
  "liwc_six_plus_words"         = "Long words (≥6 letters)",
  "liwc_i"                      = "First-person singular (I)",
  "liwc_we"                     = "First-person plural (we)",
  "liwc_you"                    = "Second-person (you)",
  "liwc_shehe"                  = "Third-person singular",
  "liwc_they"                   = "Third-person plural",
  "liwc_ppron"                  = "Personal pronouns",
  "liwc_ipron"                  = "Impersonal pronouns",
  "liwc_negative_emotion"       = "Negative emotion",
  "liwc_anxiety"                = "Anxiety",
  "liwc_anger"                  = "Anger",
  "liwc_sad"                    = "Sadness",
  "liwc_social"                 = "Social processes",
  "liwc_cogproc"                = "Cognitive processes",
  "liwc_insight"                = "Insight",
  "liwc_cause"                  = "Causation",
  "liwc_discrep"                = "Discrepancy",
  "liwc_tentat"                 = "Tentativeness",
  "liwc_certain"                = "Certainty",
  "liwc_differ"                 = "Differentiation",
  "liwc_percept"                = "Perceptual processes",
  "liwc_see"                    = "Seeing",
  "liwc_hear"                   = "Hearing",
  "liwc_feel"                   = "Feeling",
  "liwc_bio"                    = "Biological processes",
  "liwc_body"                   = "Body",
  "liwc_health"                 = "Health",
  "liwc_sexual"                 = "Sexuality",
  "liwc_ingest"                 = "Ingestion",
  "liwc_drives"                 = "Drives",
  "liwc_affiliation"            = "Affiliation",
  "liwc_achieve"                = "Achievement",
  "liwc_power"                  = "Power",
  "liwc_reward"                 = "Reward",
  "liwc_risk"                   = "Risk",
  "liwc_focuspast"              = "Past focus",
  "liwc_focusfuture"            = "Future focus",
  "liwc_focus_past"             = "Past focus",
  "liwc_focus_future"           = "Future focus",
  "liwc_relativ"                = "Relativity",
  "liwc_motion"                 = "Motion",
  "liwc_space"                  = "Space",
  "liwc_time"                   = "Time",
  "liwc_work"                   = "Work",
  "liwc_leisure"                = "Leisure",
  "liwc_home"                   = "Home",
  "liwc_money"                  = "Money",
  "liwc_relig"                  = "Religion",
  "liwc_swear"                  = "Swearing",
  "liwc_netspeak"               = "Netspeak",
  "liwc_assent"                 = "Assent",
  "liwc_nonflu"                 = "Non-fluencies",
  "liwc_filler"                 = "Filler words"
)

label_liwc <- function(raw) {
  lbl <- LIWC_LABELS[raw]
  ifelse(is.na(lbl),
         tools::toTitleCase(gsub("liwc_|_", " ", raw)),
         lbl)
}

# ─────────────────────────────────────────────────────────────
# 2. Core forest-plot function
# ─────────────────────────────────────────────────────────────
forest_pub <- function(res,
                       coef_col, lo_col, hi_col,
                       p_col, q_col,
                       label_fn,
                       x_label         = "Standardized β (95% CI)",
                       subtitle        = NULL,
                       legend_position = "none",
                       p_thresh        = 0.10) {

  show <- res[!is.na(res[[p_col]]) & res[[p_col]] < p_thresh, , drop = FALSE]
  if (nrow(show) == 0) {
    return(ggplot() +
             annotate("text", x = 0.5, y = 0.5,
                      label = "No features at p < 0.10",
                      size = 3, color = "grey50") +
             theme_void() +
             labs(subtitle = subtitle) +
             theme(plot.subtitle = element_text(size = 8, face = "bold",
                                               color = "grey30")))
  }

  show <- show[order(show[[coef_col]]), , drop = FALSE]
  show$label <- label_fn(show$feature)

  show$sig <- dplyr::case_when(
    show[[q_col]] < 0.05 ~ "[FDR]",
    show[[p_col]] < 0.05 ~ "*",
    TRUE                  ~ "."
  )
  show$sig   <- factor(show$sig,   levels = SIG_BREAKS)
  show$label <- factor(show$label, levels = show$label)

  ggplot(show,
         aes(x     = .data[[coef_col]],
             y     = label,
             color = sig)) +
    geom_vline(xintercept = 0, linetype = "dashed",
               color = "grey55", linewidth = 0.35) +
    geom_errorbar(
      aes(xmin = .data[[lo_col]], xmax = .data[[hi_col]]),
      width = 0.3, linewidth = 0.45, alpha = 0.80,
      orientation = "y"
    ) +
    geom_point(aes(size = sig)) +
    scale_color_manual(
      values = SIG_COLORS, breaks = SIG_BREAKS,
      name   = "Significance",
      labels = c("[FDR]" = "FDR q < 0.05",
                 "*"     = "p < 0.05",
                 "."     = "p < 0.10"),
      drop   = FALSE
    ) +
    scale_size_manual(
      values = SIG_SIZES, breaks = SIG_BREAKS,
      guide  = "none"
    ) +
    guides(
      color = guide_legend(
        override.aes = list(size = c(2.8, 2.2, 1.8),
                            linewidth = 0)
      )
    ) +
    labs(x = x_label, y = NULL, subtitle = subtitle) +
    theme_bw(base_size = 8.5) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_line(color = "grey92", linewidth = 0.3),
      axis.text.y        = element_text(size = 7.5, color = "black"),
      axis.text.x        = element_text(size = 7.5),
      axis.title.x       = element_text(size = 8),
      legend.position    = legend_position,
      legend.justification = c(1, 0),
      legend.key.size    = unit(0.35, "cm"),
      legend.text        = element_text(size = 7),
      legend.title       = element_text(size = 7.5, face = "bold"),
      legend.background  = element_rect(fill  = scales::alpha("white", 0.90),
                                        color = "grey75",
                                        linewidth = 0.3),
      plot.subtitle      = element_text(size = 8, face = "bold",
                                        color = "grey25", margin = margin(b = 3)),
      plot.margin        = margin(4, 6, 4, 4, "mm")
    )
}

# ─────────────────────────────────────────────────────────────
# 3. Load results
# ─────────────────────────────────────────────────────────────
liwc_res <- read.csv("results_R/wb_lme_LIWC_SSI.csv", stringsAsFactors = FALSE)
f0_res   <- read.csv("results_R/wb_lme_F0_SSI.csv",   stringsAsFactors = FALSE)

dir.create("results_R", showWarnings = FALSE)

# ─────────────────────────────────────────────────────────────
# 4. LIWC: within + between panels
# ─────────────────────────────────────────────────────────────
p_liwc_w <- forest_pub(
  liwc_res,
  coef_col = "b_within",  lo_col = "CI_lo_within",  hi_col = "CI_hi_within",
  p_col    = "p_within",  q_col  = "q_within",
  label_fn = label_liwc,
  subtitle = "A  Within-person (state)",
  legend_position = "none"
)

p_liwc_b <- forest_pub(
  liwc_res[order(liwc_res$p_between), ],
  coef_col = "b_between", lo_col = "CI_lo_between", hi_col = "CI_hi_between",
  p_col    = "p_between", q_col  = "q_between",
  label_fn = label_liwc,
  subtitle = "B  Between-person (trait)",
  legend_position = c(0.99, 0.01)
)

fig_liwc <- p_liwc_w + p_liwc_b +
  plot_layout(widths = c(1, 1))

ggsave("results_R/fig_liwc_ssi_wb.pdf",
       plot = fig_liwc,
       width = 180, height = 180,
       units = "mm", device = cairo_pdf)
cat("Saved: results_R/fig_liwc_ssi_wb.pdf\n")

# ─────────────────────────────────────────────────────────────
# 5. F0: within + between panels
# ─────────────────────────────────────────────────────────────
p_f0_w <- forest_pub(
  f0_res,
  coef_col = "b_within",  lo_col = "CI_lo_within",  hi_col = "CI_hi_within",
  p_col    = "p_within",  q_col  = "q_within",
  label_fn = label_f0_full,
  subtitle = "A  Within-person (state)",
  legend_position = "none"
)

p_f0_b <- forest_pub(
  f0_res[order(f0_res$p_between), ],
  coef_col = "b_between", lo_col = "CI_lo_between", hi_col = "CI_hi_between",
  p_col    = "p_between", q_col  = "q_between",
  label_fn = label_f0_full,
  subtitle = "B  Between-person (trait)",
  legend_position = c(0.99, 0.01)
)

fig_f0 <- p_f0_w + p_f0_b +
  plot_layout(widths = c(1, 1))

ggsave("results_R/fig_f0_ssi_wb.pdf",
       plot = fig_f0,
       width = 180, height = 140,
       units = "mm", device = cairo_pdf)
cat("Saved: results_R/fig_f0_ssi_wb.pdf\n")
