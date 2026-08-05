# ============================================================
# Cross-Lagged Panel Model: LIWC(T) -> SSI(T+1)
# + Reverse direction: SSI(T) -> liwc_death(T+1)
#
# Model:
#   SSI(T+1) ~ feat(T) + SSI(T) + time_gap + age + sex + Dx + edu_yrs
#              + (1 | id)
#
# SSI(T) is the autoregressive control: the cross-lagged beta for
# feat(T) answers whether language at T predicts suicidal ideation
# at T+1 *above and beyond* current ideation severity.
# ============================================================

library(Matrix)
library(lme4)
library(lmerTest)
library(dplyr)
library(ggplot2)
library(patchwork)

setwd("/Users/sooyeonmin/Documents/PRISM-V/suicide")

# ─────────────────────────────────────────────────────────────
# 0. Helpers
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

liwc_feats <- grep("^liwc_", names(df), value = TRUE)
for (col in liwc_feats) {
  df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
}

cat("Rows:", nrow(df), "| Patients:", n_distinct(df$id), "\n")

# ─────────────────────────────────────────────────────────────
# 2. Build lagged dataset (T -> T+1 consecutive pairs)
# ─────────────────────────────────────────────────────────────
df_lag <- df %>%
  arrange(id, time_numeric) %>%
  group_by(id) %>%
  mutate(
    SSI_t1   = lead(SSI),           # outcome: SSI at next visit
    time_t1  = lead(time_numeric),
    time_gap = time_t1 - time_numeric
  ) %>%
  filter(!is.na(SSI_t1)) %>%        # drop last time point (no T+1)
  ungroup()

cat("Lagged pairs:", nrow(df_lag),
    "| Patients:", n_distinct(df_lag$id), "\n")
cat("Transitions:\n")
print(table(df_lag$time_numeric, df_lag$time_t1))

# ─────────────────────────────────────────────────────────────
# 3. CLPM runner: feat(T) -> SSI(T+1), controlling for SSI(T)
# ─────────────────────────────────────────────────────────────
run_clpm <- function(data, features) {

  # z-score outcome and autoregressive term
  data$Y      <- as.numeric(scale(data$SSI_t1))
  data$SSI_ar <- as.numeric(scale(data$SSI))

  results <- lapply(features, function(feat) {

    tmp <- data[, c("id", "Y", "SSI_ar", feat,
                    "time_gap", "age", "sex", "Dx", "edu_yrs")] %>%
      na.omit()
    if (nrow(tmp) < 10) return(NULL)

    tmp$feat_t <- invnorm(tmp[[feat]])
    s <- sd(tmp$feat_t, na.rm = TRUE)
    if (is.na(s) || s == 0) return(NULL)
    tmp <- tmp[!is.na(tmp$feat_t), ]
    if (nrow(tmp) < 10) return(NULL)

    fit <- tryCatch(
      suppressWarnings(
        lmer(Y ~ feat_t + SSI_ar + time_gap + age + sex + Dx + edu_yrs
               + (1 | id),
             data = tmp, REML = TRUE,
             control = lmerControl(optimizer = "bobyqa"))
      ),
      error = function(e) NULL
    )
    if (is.null(fit)) return(NULL)

    coef_tbl <- suppressWarnings(as.data.frame(coef(summary(fit))))
    if (!all(c("feat_t", "SSI_ar") %in% rownames(coef_tbl))) return(NULL)

    r  <- coef_tbl["feat_t",  ]
    ar <- coef_tbl["SSI_ar",  ]

    data.frame(
      feature  = feat,
      b_cross  = r$Estimate,
      SE_cross = r$`Std. Error`,
      CI_lo    = r$Estimate - 1.96 * r$`Std. Error`,
      CI_hi    = r$Estimate + 1.96 * r$`Std. Error`,
      p_cross  = r$`Pr(>|t|)`,
      b_AR     = ar$Estimate,
      p_AR     = ar$`Pr(>|t|)`,
      n        = nrow(tmp),
      n_subj   = n_distinct(tmp$id)
    )
  })

  res <- bind_rows(results)
  if (nrow(res) == 0) return(res)

  res$q_cross <- p.adjust(res$p_cross, method = "BH")
  res[order(res$p_cross), ]
}

# ─────────────────────────────────────────────────────────────
# 4. Forward: LIWC(T) -> SSI(T+1)
# ─────────────────────────────────────────────────────────────
cat("\n=== LIWC(T) -> SSI(T+1) ===\n")
res_fwd <- run_clpm(df_lag, liwc_feats)
cat("FDR-sig:", sum(res_fwd$q_cross < 0.05, na.rm = TRUE),
    " nominal:", sum(res_fwd$p_cross < 0.05, na.rm = TRUE), "\n")

# Print top 15
cat(sprintf("\n  %-32s %7s %6s %9s %9s %7s\n",
            "Feature", "b_CL", "SE", "p", "q_FDR", "b_AR"))
cat("  ", strrep("-", 78), "\n", sep = "")
for (i in seq_len(min(15, nrow(res_fwd)))) {
  r <- res_fwd[i, ]
  sig <- ifelse(r$q_cross < 0.05, "[FDR]",
         ifelse(r$p_cross < 0.05, "*", ""))
  cat(sprintf("  %-32s %7.3f %6.3f %9.4f %9.4f %7.3f  %s\n",
              gsub("liwc_", "", r$feature),
              r$b_cross, r$SE_cross, r$p_cross, r$q_cross,
              r$b_AR, sig))
}

# ─────────────────────────────────────────────────────────────
# 5. Reverse: SSI(T) -> liwc_death(T+1)
#    (bidirectionality check for key features)
# ─────────────────────────────────────────────────────────────
run_clpm_reverse <- function(data, outcome_feat) {

  data$feat_ar <- invnorm(data[[outcome_feat]])
  data$SSI_t   <- as.numeric(scale(data$SSI))

  tmp <- data[, c("id", "feat_ar", outcome_feat, "SSI_t",
                  "time_gap", "age", "sex", "Dx", "edu_yrs")] %>%
    na.omit()

  # outcome: invnorm(feat at T+1)
  tmp$Y <- invnorm(lead_feat <- {
    df_lag2 <- df %>%
      arrange(id, time_numeric) %>%
      group_by(id) %>%
      mutate(feat_next = lead(.data[[outcome_feat]])) %>%
      filter(!is.na(feat_next)) %>%
      ungroup()
    df_lag2$feat_next
  })

  # Rebuild properly
  df_rev <- df %>%
    arrange(id, time_numeric) %>%
    group_by(id) %>%
    mutate(
      feat_next = lead(.data[[outcome_feat]]),
      time_gap  = lead(time_numeric) - time_numeric
    ) %>%
    filter(!is.na(feat_next)) %>%
    ungroup()

  df_rev$Y      <- invnorm(df_rev$feat_next)
  df_rev$feat_t <- invnorm(df_rev[[outcome_feat]])
  df_rev$SSI_t  <- as.numeric(scale(df_rev$SSI))

  tmp2 <- df_rev[, c("id", "Y", "feat_t", "SSI_t",
                     "time_gap", "age", "sex", "Dx", "edu_yrs")] %>%
    na.omit()
  if (nrow(tmp2) < 10) return(NULL)

  fit <- tryCatch(
    suppressWarnings(
      lmer(Y ~ SSI_t + feat_t + time_gap + age + sex + Dx + edu_yrs
             + (1 | id),
           data = tmp2, REML = TRUE,
           control = lmerControl(optimizer = "bobyqa"))
    ),
    error = function(e) NULL
  )
  if (is.null(fit)) return(NULL)

  coef_tbl <- suppressWarnings(as.data.frame(coef(summary(fit))))
  list(
    forward_check = coef_tbl,
    b_SSI_to_feat = coef_tbl["SSI_t", "Estimate"],
    p_SSI_to_feat = coef_tbl["SSI_t", "Pr(>|t|)"],
    b_AR_feat     = coef_tbl["feat_t", "Estimate"],
    p_AR_feat     = coef_tbl["feat_t", "Pr(>|t|)"],
    n             = nrow(tmp2)
  )
}

cat("\n=== Reverse: SSI(T) -> liwc_death(T+1) ===\n")
rev_death <- run_clpm_reverse(df_lag, "liwc_death")
if (!is.null(rev_death)) {
  cat(sprintf("  SSI(T) -> death(T+1):  b = %.3f, p = %.4f\n",
              rev_death$b_SSI_to_feat, rev_death$p_SSI_to_feat))
  cat(sprintf("  death(T) AR:           b = %.3f, p = %.4f\n",
              rev_death$b_AR_feat, rev_death$p_AR_feat))
}

cat("\n=== Reverse: SSI(T) -> liwc_quantifiers(T+1) ===\n")
rev_quant <- run_clpm_reverse(df_lag, "liwc_quantifiers")
if (!is.null(rev_quant)) {
  cat(sprintf("  SSI(T) -> quantifiers(T+1):  b = %.3f, p = %.4f\n",
              rev_quant$b_SSI_to_feat, rev_quant$p_SSI_to_feat))
  cat(sprintf("  quantifiers(T) AR:           b = %.3f, p = %.4f\n",
              rev_quant$b_AR_feat, rev_quant$p_AR_feat))
}

# ─────────────────────────────────────────────────────────────
# 6. Save
# ─────────────────────────────────────────────────────────────
dir.create("results_R", showWarnings = FALSE)
write.csv(res_fwd, "results_R/clpm_liwc_ssi.csv", row.names = FALSE)
cat("\nSaved: results_R/clpm_liwc_ssi.csv\n")

# ─────────────────────────────────────────────────────────────
# 7. Forest plot: forward cross-lagged effects (p < 0.10)
# ─────────────────────────────────────────────────────────────
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
  "liwc_quantifiers"            = "Quantifiers",
  "liwc_numbers"                = "Numbers",
  "liwc_articles"               = "Articles",
  "liwc_adjectives"             = "Adjectives",
  "liwc_adverbs"                = "Adverbs",
  "liwc_verbs"                  = "Verbs",
  "liwc_negative_emotion"       = "Negative emotion",
  "liwc_anxiety"                = "Anxiety",
  "liwc_anger"                  = "Anger",
  "liwc_sad"                    = "Sadness",
  "liwc_social"                 = "Social processes",
  "liwc_social_processes"       = "Social processes",
  "liwc_cogproc"                = "Cognitive processes",
  "liwc_insight"                = "Insight",
  "liwc_cause"                  = "Causation",
  "liwc_discrep"                = "Discrepancy",
  "liwc_discrepancies"          = "Discrepancies",
  "liwc_tentat"                 = "Tentativeness",
  "liwc_certain"                = "Certainty",
  "liwc_differ"                 = "Differentiation",
  "liwc_percept"                = "Perceptual processes",
  "liwc_bio"                    = "Biological processes",
  "liwc_body"                   = "Body",
  "liwc_health"                 = "Health",
  "liwc_sexual"                 = "Sexuality",
  "liwc_ingest"                 = "Ingestion",
  "liwc_drives"                 = "Drives",
  "liwc_affiliation"            = "Affiliation",
  "liwc_achieve"                = "Achievement",
  "liwc_achievement"            = "Achievement",
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
  "liwc_nonflu"                 = "Non-fluencies",
  "liwc_nonfluencies"           = "Non-fluencies",
  "liwc_filler"                 = "Filler words",
  "liwc_action"                 = "Action words",
  "liwc_absolutist"             = "Absolutist words",
  "liwc_authentic"              = "Authenticity",
  "liwc_avoidance"              = "Avoidance",
  "liwc_dashes"                 = "Dashes",
  "liwc_commas"                 = "Commas",
  "liwc_apostrophes"            = "Apostrophes",
  "liwc_female"                 = "Female references",
  "liwc_time_orientation"       = "Time orientation",
  "liwc_personal_pronouns"      = "Personal pronouns",
  "liwc_pronouns"               = "Pronouns",
  "liwc_other_grammar"          = "Other grammar"
)

label_liwc <- function(raw) {
  lbl <- LIWC_LABELS[raw]
  ifelse(is.na(lbl),
         tools::toTitleCase(gsub("liwc_|_", " ", raw)),
         lbl)
}

SIG_COLORS <- c("[FDR]" = "#9B1F1F", "*" = "#D4721A", "." = "#3876A8")
SIG_SIZES  <- c("[FDR]" = 2.8, "*" = 2.2, "." = 1.8)
SIG_BREAKS <- c("[FDR]", "*", ".")

show <- res_fwd[!is.na(res_fwd$p_cross) & res_fwd$p_cross < 0.10, , drop = FALSE]

if (nrow(show) > 0) {
  show <- show[order(show$b_cross), ]
  show$label <- label_liwc(show$feature)
  show$sig <- dplyr::case_when(
    show$q_cross < 0.05 ~ "[FDR]",
    show$p_cross < 0.05 ~ "*",
    TRUE                 ~ "."
  )
  show$sig   <- factor(show$sig,   levels = SIG_BREAKS)
  show$label <- factor(show$label, levels = show$label)

  p_clpm <- ggplot(show, aes(x = b_cross, y = label, color = sig)) +
    geom_vline(xintercept = 0, linetype = "dashed",
               color = "grey55", linewidth = 0.35) +
    geom_errorbar(aes(xmin = CI_lo, xmax = CI_hi),
                  width = 0.3, linewidth = 0.45, alpha = 0.80,
                  orientation = "y") +
    geom_point(aes(size = sig)) +
    scale_color_manual(values = SIG_COLORS, breaks = SIG_BREAKS,
                       name = "Significance",
                       labels = c("[FDR]" = "FDR q < 0.05",
                                  "*"     = "p < 0.05",
                                  "."     = "p < 0.10"),
                       drop = FALSE) +
    scale_size_manual(values = SIG_SIZES, breaks = SIG_BREAKS, guide = "none") +
    guides(color = guide_legend(
      override.aes = list(size = c(2.8, 2.2, 1.8), linewidth = 0)
    )) +
    labs(x = "Cross-lagged beta (95% CI)",
         y = NULL,
         subtitle = "LIWC(T) -> SSI(T+1), controlling for SSI(T)") +
    theme_bw(base_size = 8.5) +
    theme(
      panel.grid.major.y   = element_blank(),
      panel.grid.minor     = element_blank(),
      panel.grid.major.x   = element_line(color = "grey92", linewidth = 0.3),
      axis.text.y          = element_text(size = 7.5, color = "black"),
      axis.text.x          = element_text(size = 7.5),
      axis.title.x         = element_text(size = 8),
      legend.position      = c(0.99, 0.01),
      legend.justification = c(1, 0),
      legend.key.size      = unit(0.35, "cm"),
      legend.text          = element_text(size = 7),
      legend.title         = element_text(size = 7.5, face = "bold"),
      legend.background    = element_rect(fill      = scales::alpha("white", 0.90),
                                          color     = "grey75",
                                          linewidth = 0.3),
      plot.subtitle        = element_text(size = 8, face = "bold",
                                          color = "grey25"),
      plot.margin          = margin(4, 6, 4, 4, "mm")
    )

  ggsave("results_R/fig_clpm_liwc_ssi.pdf",
         plot = p_clpm, width = 120, height = 140,
         units = "mm", device = "pdf")
  cat("Saved: results_R/fig_clpm_liwc_ssi.pdf\n")
} else {
  cat("No features at p < 0.10 for forest plot.\n")
}
