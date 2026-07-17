# ============================================================
# SSI ~ F0 / LIWC: Within-Between Person Decomposition
# + Publication-quality forest plots (Translational Psychiatry)
# ============================================================

library(Matrix)
library(lme4)
library(lmerTest)
library(dplyr)
library(ggplot2)
library(patchwork)

setwd("/Users/sooyeonmin/Documents/PRISM-V/suicide")

# ─────────────────────────────────────────────────────────────
# 0. Inverse Normal Transformation (Blom)
# ─────────────────────────────────────────────────────────────
invnorm <- function(x) {
  x  <- as.numeric(x)
  ok <- !is.na(x)
  y  <- rep(NA_real_, length(x))
  if (sum(ok) <= 1) return(y)
  y[ok] <- qnorm((rank(x[ok], ties.method = "average") - 0.5) / sum(ok))
  y
}

# ─────────────────────────────────────────────────────────────
# 1. Data
# ─────────────────────────────────────────────────────────────
df <- read.csv("prism_softImpute4_smile_with_items.csv",
               stringsAsFactors = FALSE)

df$time_numeric <- dplyr::recode(df$case_episode,
  "baseline" = 0, "2m" = 2, "4m" = 4, "8m" = 8, "12m" = 12)
df$Dx <- relevel(factor(df$Dx), ref = "BPII")

for (col in c("SSI", "age", "sex", "edu_yrs", "AP_dose")) {
  df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
}

f0_feats   <- grep("^F0final_sma", names(df), value = TRUE)
liwc_feats <- grep("^liwc_",       names(df), value = TRUE)
for (col in c(f0_feats, liwc_feats)) {
  df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
}

cat("Rows:", nrow(df), "| Patients:", n_distinct(df$id), "\n")
cat("F0 features:", length(f0_feats),
    "| LIWC features:", length(liwc_feats), "\n")

# ─────────────────────────────────────────────────────────────
# 2. Within-Between LME runner
# ─────────────────────────────────────────────────────────────
run_wb_lme <- function(data, features, outcome, covariates) {

  data$Y <- as.numeric(scale(data[[outcome]]))

  results <- lapply(features, function(feat) {

    tmp <- data[, c("id", "Y", feat, "time_numeric",
                    "age", "sex", "Dx", "AP_dose", "edu_yrs")] |>
      na.omit()
    if (nrow(tmp) < 10) return(NULL)

    tmp$feat_x <- invnorm(tmp[[feat]])
    s <- sd(tmp$feat_x, na.rm = TRUE)
    if (is.na(s) || s == 0) return(NULL)
    tmp <- tmp[!is.na(tmp$feat_x), ]
    if (nrow(tmp) < 10) return(NULL)

    tmp <- tmp %>%
      group_by(id) %>%
      mutate(
        feat_between = mean(feat_x, na.rm = TRUE),
        feat_within  = feat_x - feat_between
      ) %>%
      ungroup()

    if (sd(tmp$feat_within,  na.rm = TRUE) == 0) return(NULL)
    if (sd(tmp$feat_between, na.rm = TRUE) == 0) return(NULL)

    formula_str <- paste0(
      "Y ~ feat_within + feat_between + ", covariates, " + (1 | id)"
    )

    fit <- tryCatch(
      suppressWarnings(
        lmer(as.formula(formula_str), data = tmp, REML = TRUE,
             control = lmerControl(optimizer = "bobyqa"))
      ),
      error = function(e) NULL
    )
    if (is.null(fit)) return(NULL)

    coef_tbl <- suppressWarnings(as.data.frame(coef(summary(fit))))
    if (!all(c("feat_within", "feat_between") %in% rownames(coef_tbl)))
      return(NULL)

    rw <- coef_tbl["feat_within",  ]
    rb <- coef_tbl["feat_between", ]

    data.frame(
      feature       = feat,
      b_within      = rw$Estimate,
      SE_within     = rw$`Std. Error`,
      CI_lo_within  = rw$Estimate - 1.96 * rw$`Std. Error`,
      CI_hi_within  = rw$Estimate + 1.96 * rw$`Std. Error`,
      p_within      = rw$`Pr(>|t|)`,
      b_between     = rb$Estimate,
      SE_between    = rb$`Std. Error`,
      CI_lo_between = rb$Estimate - 1.96 * rb$`Std. Error`,
      CI_hi_between = rb$Estimate + 1.96 * rb$`Std. Error`,
      p_between     = rb$`Pr(>|t|)`,
      n             = nrow(tmp),
      n_subj        = n_distinct(tmp$id)
    )
  })

  res <- bind_rows(results)
  if (nrow(res) == 0) return(res)

  res$q_within  <- p.adjust(res$p_within,  method = "BH")
  res$q_between <- p.adjust(res$p_between, method = "BH")

  res[order(res$p_within), ]
}

# ─────────────────────────────────────────────────────────────
# 3. Run models
# ─────────────────────────────────────────────────────────────
cov_f0   <- "time_numeric + age + sex + Dx + AP_dose"
cov_liwc <- "time_numeric + age + sex + Dx + edu_yrs"

cat("\n=== F0 -> SSI (within-between, invnorm) ===\n")
res_f0_wb <- run_wb_lme(df, f0_feats, "SSI", cov_f0)
cat("Within  FDR-sig:", sum(res_f0_wb$q_within  < 0.05, na.rm = TRUE),
    " nominal:", sum(res_f0_wb$p_within  < 0.05, na.rm = TRUE), "\n")
cat("Between FDR-sig:", sum(res_f0_wb$q_between < 0.05, na.rm = TRUE),
    " nominal:", sum(res_f0_wb$p_between < 0.05, na.rm = TRUE), "\n")

cat("\n=== LIWC -> SSI (within-between, invnorm) ===\n")
res_liwc_wb <- run_wb_lme(df, liwc_feats, "SSI", cov_liwc)
cat("Within  FDR-sig:", sum(res_liwc_wb$q_within  < 0.05, na.rm = TRUE),
    " nominal:", sum(res_liwc_wb$p_within  < 0.05, na.rm = TRUE), "\n")
cat("Between FDR-sig:", sum(res_liwc_wb$q_between < 0.05, na.rm = TRUE),
    " nominal:", sum(res_liwc_wb$p_between < 0.05, na.rm = TRUE), "\n")

# ─────────────────────────────────────────────────────────────
# 4. Print top results
# ─────────────────────────────────────────────────────────────
print_wb <- function(res, label, n = 15) {
  cat(sprintf("\n--- %s: top %d by p_within ---\n", label, n))
  show <- head(res, n)
  show$feat_short <- gsub("F0final_sma_|liwc_", "", show$feature)
  show$sig_w <- ifelse(show$q_within  < 0.05, "[FDR]",
                ifelse(show$p_within  < 0.05, "*", ""))
  show$sig_b <- ifelse(show$q_between < 0.05, "[FDR]",
                ifelse(show$p_between < 0.05, "*", ""))
  cat(sprintf("  %-28s %7s %6s %9s %9s   %7s %9s %9s\n",
              "Feature", "b_W", "SE_W", "p_W", "q_W", "b_B", "p_B", "q_B"))
  cat("  ", strrep("-", 82), "\n", sep = "")
  for (i in seq_len(nrow(show))) {
    r <- show[i, ]
    cat(sprintf(
      "  %-28s %7.3f %6.3f %9.4f %9.4f   %7.3f %9.4f %9.4f  %s / %s\n",
      r$feat_short,
      r$b_within, r$SE_within, r$p_within, r$q_within,
      r$b_between, r$p_between, r$q_between,
      r$sig_w, r$sig_b
    ))
  }
}

print_wb(res_f0_wb,   "F0")
print_wb(res_liwc_wb, "LIWC")

# ─────────────────────────────────────────────────────────────
# 5. Save CSVs
# ─────────────────────────────────────────────────────────────
dir.create("results_R", showWarnings = FALSE)
write.csv(res_f0_wb,   "results_R/wb_lme_F0_SSI.csv",   row.names = FALSE)
write.csv(res_liwc_wb, "results_R/wb_lme_LIWC_SSI.csv", row.names = FALSE)
cat("\nSaved: results_R/wb_lme_F0_SSI.csv\n")
cat("Saved: results_R/wb_lme_LIWC_SSI.csv\n")

# ─────────────────────────────────────────────────────────────
# 6. Publication forest plots
# ─────────────────────────────────────────────────────────────

# Validated palette — all 5 CVD/contrast checks pass.
# Yellow excluded: deltaE 0.4 deutan vs orange (fails CVD separation).
SIG_COLORS <- c("[FDR]" = "#9B1F1F",  # crimson
                "*"     = "#D4721A",  # amber
                "."     = "#3876A8") # steel blue
SIG_SIZES  <- c("[FDR]" = 2.8, "*" = 2.2, "." = 1.8)
SIG_BREAKS <- c("[FDR]", "*", ".")

# ── F0 label builder (ASCII only — avoids mbcsToSbcs on macOS pdf device) ──
label_f0_base <- function(base) {
  dplyr::case_when(
    base == "iqr1-2"         ~ "IQR Q1-Q2",
    base == "iqr2-3"         ~ "IQR Q2-Q3",
    base == "iqr1-3"         ~ "IQR Q1-Q3",
    base == "quartile1"      ~ "Q1 (25th %ile)",
    base == "quartile2"      ~ "Q2 (median)",
    base == "quartile3"      ~ "Q3 (75th %ile)",
    base == "percentile1.0"  ~ "P1",
    base == "percentile99.0" ~ "P99",
    base == "kurtosis"       ~ "Kurtosis",
    base == "skewness"       ~ "Skewness",
    base == "qregerrQ"       ~ "Quadratic fit RMSE",
    base == "linregerrQ"     ~ "Linear fit RMSE",
    base == "pctlrange0-1"   ~ "Pctlrange 0-1",
    base == "qregc3"         ~ "Quadratic coeff. c3",
    base == "qregc2"         ~ "Quadratic coeff. c2",
    base == "qregc1"         ~ "Quadratic coeff. c1",
    base == "linregc2"       ~ "Linear slope",
    base == "linregc1"       ~ "Linear intercept",
    base == "amean"          ~ "Mean F0",
    base == "stddev"         ~ "SD",
    base == "range"          ~ "Range",
    TRUE                     ~ tools::toTitleCase(gsub("[-_]", " ", base))
  )
}

label_f0_full <- function(raw) {
  stripped <- sub("^F0final_sma_", "", raw)
  delta    <- grepl("^de_", stripped)
  base     <- sub("^de_", "", stripped)
  readable <- label_f0_base(base)
  ifelse(delta, paste0("d.", readable), readable)
}

# ── LIWC label lookup (ASCII only) ──
LIWC_LABELS <- c(
  "liwc_death"                  = "Death-related words",
  "liwc_inaction"               = "Inaction",
  "liwc_focus_present"          = "Present focus",
  "liwc_emotional_tone"         = "Emotional tone",
  "liwc_positive_emotion_words" = "Positive emotion",
  "liwc_avoidance"              = "Avoidance",
  "liwc_high_empathy"           = "High empathy",
  "liwc_family"                 = "Family",
  "liwc_six_plus_words"         = "Long words (6+ letters)",
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

# ── Core plot function ──
forest_pub <- function(res,
                       coef_col, lo_col, hi_col,
                       p_col, q_col,
                       label_fn,
                       x_label         = "Standardized beta (95% CI)",
                       subtitle        = NULL,
                       legend_position = "none",
                       p_thresh        = 0.10) {

  show <- res[!is.na(res[[p_col]]) & res[[p_col]] < p_thresh, , drop = FALSE]
  if (nrow(show) == 0) {
    return(
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "No features at p < 0.10",
                 size = 3, color = "grey50") +
        theme_void() +
        labs(subtitle = subtitle) +
        theme(plot.subtitle = element_text(size = 8, face = "bold",
                                           color = "grey30"))
    )
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

  ggplot(show, aes(x = .data[[coef_col]], y = label, color = sig)) +
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
    scale_size_manual(values = SIG_SIZES, breaks = SIG_BREAKS, guide = "none") +
    guides(color = guide_legend(
      override.aes = list(size = c(2.8, 2.2, 1.8), linewidth = 0)
    )) +
    labs(x = x_label, y = NULL, subtitle = subtitle) +
    theme_bw(base_size = 8.5) +
    theme(
      panel.grid.major.y   = element_blank(),
      panel.grid.minor     = element_blank(),
      panel.grid.major.x   = element_line(color = "grey92", linewidth = 0.3),
      axis.text.y          = element_text(size = 7.5, color = "black"),
      axis.text.x          = element_text(size = 7.5),
      axis.title.x         = element_text(size = 8),
      legend.position      = legend_position,
      legend.justification = c(1, 0),
      legend.key.size      = unit(0.35, "cm"),
      legend.text          = element_text(size = 7),
      legend.title         = element_text(size = 7.5, face = "bold"),
      legend.background    = element_rect(fill      = scales::alpha("white", 0.90),
                                          color     = "grey75",
                                          linewidth = 0.3),
      plot.subtitle        = element_text(size = 8, face = "bold",
                                          color = "grey25",
                                          margin = margin(b = 3)),
      plot.margin          = margin(4, 6, 4, 4, "mm")
    )
}

# ── LIWC plots ──
p_liwc_w <- forest_pub(
  res_liwc_wb,
  coef_col = "b_within",  lo_col = "CI_lo_within",  hi_col = "CI_hi_within",
  p_col    = "p_within",  q_col  = "q_within",
  label_fn = label_liwc,
  subtitle = "A  Within-person (state)",
  legend_position = "none"
)

p_liwc_b <- forest_pub(
  res_liwc_wb[order(res_liwc_wb$p_between), ],
  coef_col = "b_between", lo_col = "CI_lo_between", hi_col = "CI_hi_between",
  p_col    = "p_between", q_col  = "q_between",
  label_fn = label_liwc,
  subtitle = "B  Between-person (trait)",
  legend_position = c(0.99, 0.01)
)

fig_liwc <- p_liwc_w + p_liwc_b + plot_layout(widths = c(1, 1))

ggsave("results_R/fig_liwc_ssi_wb.pdf",
       plot = fig_liwc, width = 180, height = 180,
       units = "mm", device = "pdf")
cat("Saved: results_R/fig_liwc_ssi_wb.pdf\n")

# ── F0 plots ──
p_f0_w <- forest_pub(
  res_f0_wb,
  coef_col = "b_within",  lo_col = "CI_lo_within",  hi_col = "CI_hi_within",
  p_col    = "p_within",  q_col  = "q_within",
  label_fn = label_f0_full,
  subtitle = "A  Within-person (state)",
  legend_position = "none"
)

p_f0_b <- forest_pub(
  res_f0_wb[order(res_f0_wb$p_between), ],
  coef_col = "b_between", lo_col = "CI_lo_between", hi_col = "CI_hi_between",
  p_col    = "p_between", q_col  = "q_between",
  label_fn = label_f0_full,
  subtitle = "B  Between-person (trait)",
  legend_position = c(0.99, 0.01)
)

fig_f0 <- p_f0_w + p_f0_b + plot_layout(widths = c(1, 1))

ggsave("results_R/fig_f0_ssi_wb.pdf",
       plot = fig_f0, width = 180, height = 140,
       units = "mm", device = "pdf")
cat("Saved: results_R/fig_f0_ssi_wb.pdf\n")
