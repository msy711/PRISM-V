# ============================================================
# PRISM-V: Speech/Language Features & Suicidal Ideation
# Mixed Effects Model Analysis — Full Pipeline
# ============================================================
# Required packages:
#   lme4, lmerTest, RNOmni (or custom invnorm), dplyr, tidyr,
#   ggplot2, patchwork, pheatmap, viridis
#
# install.packages(c("lme4","lmerTest","RNOmni","dplyr","tidyr",
#                    "ggplot2","patchwork","pheatmap","viridis"))
# ============================================================

library(lme4)
library(lmerTest)   # adds p-values to lmer via Satterthwaite df
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

# ─────────────────────────────────────────────────────────────
# 0. Inverse Normal Transformation (Blom / rank-based)
# ─────────────────────────────────────────────────────────────
invnorm <- function(x) {
  x  <- as.numeric(x)
  ok <- !is.na(x)
  y  <- rep(NA_real_, length(x))
  if (sum(ok) <= 1) return(y)
  y[ok] <- qnorm((rank(x[ok], ties.method = "average") - 0.5) / sum(ok))
  return(y)
}

# ─────────────────────────────────────────────────────────────
# 1. Data
# ─────────────────────────────────────────────────────────────
df <- read.csv("data/prism_softImpute4_smile_with_items.csv",
               stringsAsFactors = FALSE)

# time encoding
df$time_numeric <- dplyr::recode(df$case_episode,
  "baseline" = 0, "2m" = 2, "4m" = 4, "8m" = 8, "12m" = 12)

# Dx as factor (BPII = reference, most frequent)
df$Dx <- relevel(factor(df$Dx), ref = "BPII")

# numeric coercion
numeric_cols <- c("SSI", "HAMD", "PHQ", "BAI", "BHOL",
                  "liwc_death", "F0final_sma_qregc3",
                  "age", "sex", "edu_yrs", "AP_dose",
                  "self_inj", "suicide_behavior",
                  paste0("HAMD", 1:17),
                  paste0("PHQ",  1:9),
                  paste0("BAI",  1:21),
                  paste0("BHOL", 1:20),
                  paste0("SSI",  1:19))
for (col in intersect(numeric_cols, names(df))) {
  df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
}

# F0 and LIWC feature lists
f0_feats   <- grep("^F0final_sma", names(df), value = TRUE)
liwc_feats <- grep("^liwc_",       names(df), value = TRUE)
for (col in c(f0_feats, liwc_feats)) {
  df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
}

cat("Rows:", nrow(df), "| Patients:", n_distinct(df$id), "\n")
cat("F0 features:", length(f0_feats),
    "| LIWC features:", length(liwc_feats), "\n")

# ─────────────────────────────────────────────────────────────
# 2. Helper: run univariate LME for a list of features
# ─────────────────────────────────────────────────────────────
# outcome and features are z-scored; invnorm optional for features
run_lme_batch <- function(data, features, outcome,
                          covariates, use_invnorm = FALSE) {
  # z-score outcome
  data$Y <- scale(data[[outcome]])[, 1]

  results <- lapply(features, function(feat) {
    tmp <- data[, c("id", "Y", feat, "time_numeric",
                    "age", "sex", "Dx", "AP_dose", "edu_yrs")] |>
      na.omit()

    if (nrow(tmp) < 10) return(NULL)

    # transform feature
    if (use_invnorm) {
      tmp$feat_x <- invnorm(tmp[[feat]])
    } else {
      tmp$feat_x <- as.numeric(scale(tmp[[feat]]))
    }

    # skip if constant or all NA after transform
    s <- sd(tmp$feat_x, na.rm = TRUE)
    if (is.na(s) || s == 0) return(NULL)

    # drop any NA introduced by transform
    tmp <- tmp[!is.na(tmp$feat_x), ]
    if (nrow(tmp) < 10) return(NULL)

    formula_str <- paste0("Y ~ feat_x + ", covariates, " + (1 | id)")

    fit <- tryCatch(
      lmer(as.formula(formula_str), data = tmp, REML = TRUE,
           control = lmerControl(optimizer = "bobyqa")),
      error = function(e) NULL
    )
    if (is.null(fit)) return(NULL)

    coef_tbl <- as.data.frame(coef(summary(fit)))
    if (!"feat_x" %in% rownames(coef_tbl)) return(NULL)
    row <- coef_tbl["feat_x", ]

    data.frame(
      feature   = feat,
      coef      = row$Estimate,
      SE        = row$`Std. Error`,
      CI_lo     = row$Estimate - 1.96 * row$`Std. Error`,
      CI_hi     = row$Estimate + 1.96 * row$`Std. Error`,
      p         = row$`Pr(>|t|)`,
      n         = nrow(tmp),
      n_subj    = n_distinct(tmp$id)
    )
  })

  res <- bind_rows(results)
  if (nrow(res) == 0) return(res)
  res$q_fdr <- p.adjust(res$p, method = "BH")
  res[order(res$p), ]
}

# ─────────────────────────────────────────────────────────────
# 3. Analysis 1: F0 / LIWC -> SSI
#    (a) raw z-scored  (b) invnorm-transformed
# ─────────────────────────────────────────────────────────────
cov_f0   <- "time_numeric + age + sex + Dx + AP_dose"
cov_liwc <- "time_numeric + age + sex + Dx + edu_yrs"

sub_full <- df[complete.cases(df[, c("SSI", "time_numeric", "age",
                                      "sex", "Dx", "AP_dose", "edu_yrs")]), ]

# --- (a) z-scored ---
cat("\n=== F0 -> SSI (z-scored) ===\n")
res_f0_ssi   <- run_lme_batch(sub_full, f0_feats,   "SSI", cov_f0)
cat("FDR-sig:", sum(res_f0_ssi$q_fdr < 0.05, na.rm=TRUE), "\n")

cat("\n=== LIWC -> SSI (z-scored) ===\n")
res_liwc_ssi <- run_lme_batch(sub_full, liwc_feats, "SSI", cov_liwc)
cat("FDR-sig:", sum(res_liwc_ssi$q_fdr < 0.05, na.rm=TRUE), "\n")

# --- (b) invnorm ---
cat("\n=== F0 -> SSI (invnorm) ===\n")
res_f0_ssi_in   <- run_lme_batch(sub_full, f0_feats,   "SSI", cov_f0,   TRUE)
cat("FDR-sig:", sum(res_f0_ssi_in$q_fdr < 0.05, na.rm=TRUE), "\n")

cat("\n=== LIWC -> SSI (invnorm) ===\n")
res_liwc_ssi_in <- run_lme_batch(sub_full, liwc_feats, "SSI", cov_liwc, TRUE)
cat("FDR-sig:", sum(res_liwc_ssi_in$q_fdr < 0.05, na.rm=TRUE), "\n")

# ─────────────────────────────────────────────────────────────
# 4. Analysis 2: All features × HAMD / PHQ / BAI / BHOL
# ─────────────────────────────────────────────────────────────
outcomes_scale <- c("HAMD", "PHQ", "BAI", "BHOL")

run_feature_x_scales <- function(data, feats, cov_str,
                                  outcomes, use_invnorm = FALSE,
                                  group_label = "") {
  all_res <- lapply(outcomes, function(out) {
    res <- run_lme_batch(data, feats, out, cov_str, use_invnorm)
    res$outcome <- out
    res
  })
  res <- bind_rows(all_res)
  res$group <- group_label
  res
}

cat("\n=== F0 × Scales (invnorm) ===\n")
res_f0_scale <- run_feature_x_scales(
  sub_full, f0_feats,   cov_f0,   outcomes_scale, TRUE, "F0")

cat("\n=== LIWC × Scales (invnorm) ===\n")
res_liwc_scale <- run_feature_x_scales(
  sub_full, liwc_feats, cov_liwc, outcomes_scale, TRUE, "LIWC")

res_scale_all <- bind_rows(res_f0_scale, res_liwc_scale)

# FDR summary
cat("\n── FDR-sig per group × outcome ──\n")
res_scale_all |>
  group_by(group, outcome) |>
  summarise(fdr_sig = sum(q_fdr < 0.05, na.rm = TRUE),
            total   = n(), .groups = "drop") |>
  print()

# ─────────────────────────────────────────────────────────────
# 5. Analysis 3: Item-level LME
#    HAMD1–17, PHQ1–9, BAI1–21, BHOL1–20 -> SSI / liwc_death / F0_qregc3
# ─────────────────────────────────────────────────────────────
hamd_items <- paste0("HAMD", 1:17)
phq_items  <- paste0("PHQ",  1:9)
bai_items  <- paste0("BAI",  1:21)
bhs_items  <- paste0("BHOL", 1:20)
all_items  <- c(hamd_items, phq_items, bai_items, bhs_items)

item_outcomes <- list(
  SSI        = list(cov = cov_liwc),          # same base covariates
  liwc_death = list(cov = cov_liwc),
  F0final_sma_qregc3 = list(cov = cov_f0)
)

item_results <- list()
for (out in names(item_outcomes)) {
  cat("\n=== Items ->", out, "===\n")
  item_results[[out]] <- run_lme_batch(
    sub_full, all_items, out, item_outcomes[[out]]$cov
  )
  cat("FDR-sig:", sum(item_results[[out]]$q_fdr < 0.05, na.rm=TRUE), "\n")
}

# overlap across outcomes
sig_ssi  <- item_results$SSI$feature[item_results$SSI$q_fdr < 0.05]
sig_liwc <- item_results$liwc_death$feature[item_results$liwc_death$q_fdr < 0.05]
sig_f0   <- item_results$F0final_sma_qregc3$feature[
              item_results$F0final_sma_qregc3$q_fdr < 0.05]

cat("\nSSI ∩ liwc_death (FDR):", intersect(sig_ssi, sig_liwc), "\n")
cat("SSI ∩ F0_qregc3  (FDR):", intersect(sig_ssi, sig_f0),   "\n")

# ─────────────────────────────────────────────────────────────
# 6. Analysis 4: Subscale-level LME (literature-based)
# ─────────────────────────────────────────────────────────────

# --- BHS reverse scoring ---
bhs_reversed <- c(1,3,5,6,8,10,13,15,19)

for (n in bhs_reversed) {
  col <- paste0("BHOL", n)
  if (col %in% names(df)) {
    df[[paste0(col, "_rev")]] <- df[[col]] |> as.numeric() |>
      (\(x) { mx <- max(x, na.rm=TRUE); mx - x })()
  }
}

# --- Define subscales (Shafer 2006; Boothroyd 2019; Hewitt & Norton 1993; Aish & Wasserman 2001) ---
subscales <- list(
  # HAMD-17 (Shafer 2006, PMID 16287149)
  HAMD_Depression = paste0("HAMD", c(1,2,3,7,8)),
  HAMD_Anxiety    = paste0("HAMD", c(9,10,11,15,17)),
  HAMD_Somatic    = paste0("HAMD", c(12,13,14,16)),
  HAMD_Sleep      = paste0("HAMD", c(4,5,6)),
  # PHQ-9 (Boothroyd et al. 2019, doi:10.1016/j.psychres.2018.12.048)
  PHQ_CogAffect   = paste0("PHQ",  c(1,2,6,9)),
  PHQ_Somatic     = paste0("PHQ",  c(3,4,5,7,8)),
  # BAI-21 (Hewitt & Norton 1993)
  BAI_Somatic     = paste0("BAI",  c(1,2,3,6,7,8,12,13,17,18,19,20,21)),
  BAI_Cognitive   = paste0("BAI",  c(4,5,9,10,11,14,15,16)),
  # BHS-20 (Aish & Wasserman 2001, doi:10.1017/S0033291701003300)
  BHS_Hopeless    = paste0("BHOL", c(2,4,7,9,11,12,14,16,17,18,20)),
  BHS_Hopeful     = paste0("BHOL", paste0(c(1,3,5,6,8,10,13,15,19), "_rev"))
)

# compute subscale scores on df and sub_full
for (sc in names(subscales)) {
  cols <- subscales[[sc]]
  valid_cols <- intersect(cols, names(df))
  df[[sc]]       <- rowSums(df[,       valid_cols, drop = FALSE], na.rm = FALSE)
  valid_cols_sub <- intersect(cols, names(sub_full))
  sub_full[[sc]] <- rowSums(sub_full[, valid_cols_sub, drop = FALSE], na.rm = FALSE)
}

# run subscale LME
subscale_outcomes <- list(
  SSI        = list(cov = cov_liwc),
  liwc_death = list(cov = cov_liwc),
  F0final_sma_qregc3 = list(cov = cov_f0)
)

subscale_results <- list()
for (out in names(subscale_outcomes)) {
  cat("\n=== Subscales ->", out, "===\n")
  subscale_results[[out]] <- run_lme_batch(
    sub_full, names(subscales), out,
    subscale_outcomes[[out]]$cov
  )
  print(subscale_results[[out]][, c("feature","coef","SE","p","q_fdr")])
}

# ─────────────────────────────────────────────────────────────
# 7. Analysis 5: liwc_death / F0_qregc3 -> each SSI item (SSI1–19)
# ─────────────────────────────────────────────────────────────
ssi_items <- paste0("SSI", 1:19)

run_predictor_x_items <- function(data, predictor, cov_str, items,
                                   use_invnorm = FALSE) {
  data$Y_pred <- if (use_invnorm) invnorm(data[[predictor]])
                 else scale(data[[predictor]])[, 1]

  results <- lapply(items, function(item) {
    tmp <- data[, c("id", item, "Y_pred", "time_numeric",
                    "age", "sex", "Dx", "AP_dose", "edu_yrs")] |>
      na.omit()
    if (sd(tmp[[item]], na.rm=TRUE) == 0) return(NULL)

    tmp$Y <- scale(tmp[[item]])[, 1]
    formula_str <- paste0("Y ~ Y_pred + ", cov_str, " + (1 | id)")

    fit <- tryCatch(
      lmer(as.formula(formula_str), data = tmp, REML = TRUE,
           control = lmerControl(optimizer = "bobyqa")),
      error = function(e) NULL
    )
    if (is.null(fit)) return(NULL)

    coef_tbl <- as.data.frame(coef(summary(fit)))
    row      <- coef_tbl["Y_pred", ]
    data.frame(item = item,
               coef  = row$Estimate,
               SE    = row$`Std. Error`,
               CI_lo = row$Estimate - 1.96 * row$`Std. Error`,
               CI_hi = row$Estimate + 1.96 * row$`Std. Error`,
               p     = row$`Pr(>|t|)`,
               n     = nrow(tmp))
  })

  res <- bind_rows(results)
  res$q_fdr <- p.adjust(res$p, method = "BH")
  res[order(res$p), ]
}

cat("\n=== liwc_death -> SSI items ===\n")
res_liwc_ssi_items <- run_predictor_x_items(
  sub_full, "liwc_death", cov_liwc, ssi_items)
cat("FDR-sig:", sum(res_liwc_ssi_items$q_fdr < 0.05, na.rm=TRUE), "/19\n")
print(res_liwc_ssi_items[, c("item","coef","SE","p","q_fdr")])

cat("\n=== F0_qregc3 -> SSI items ===\n")
res_f0_ssi_items <- run_predictor_x_items(
  sub_full, "F0final_sma_qregc3", cov_f0, ssi_items)
cat("FDR-sig:", sum(res_f0_ssi_items$q_fdr < 0.05, na.rm=TRUE), "/19\n")
print(res_f0_ssi_items[, c("item","coef","SE","p","q_fdr")])

# ─────────────────────────────────────────────────────────────
# 8. Analysis 6: GLMM — self_inj / suicide_behavior
#    liwc_death / F0_qregc3 (non-baseline only)
# ─────────────────────────────────────────────────────────────
df_followup <- df[df$case_episode != "baseline", ]

run_glmm <- function(data, outcome, predictor, cov_str) {
  tmp <- data[, c("id", outcome, predictor, "time_numeric",
                  "age", "sex", "Dx", "AP_dose", "edu_yrs")] |>
    na.omit()
  tmp$pred_z  <- scale(tmp[[predictor]])[, 1]
  tmp$outcome <- as.integer(tmp[[outcome]])

  formula_str <- paste0("outcome ~ pred_z + ", cov_str, " + (1 | id)")

  fit <- tryCatch(
    glmer(as.formula(formula_str), data = tmp, family = binomial,
          control = glmerControl(optimizer = "bobyqa")),
    error = function(e) NULL
  )
  if (is.null(fit)) return(NULL)

  coef_tbl <- as.data.frame(coef(summary(fit)))
  row      <- coef_tbl["pred_z", ]

  data.frame(
    outcome   = outcome,
    predictor = predictor,
    log_OR    = row$Estimate,
    SE        = row$`Std. Error`,
    z         = row$`z value`,
    p         = row$`Pr(>|z|)`,
    OR        = exp(row$Estimate),
    OR_CI_lo  = exp(row$Estimate - 1.96 * row$`Std. Error`),
    OR_CI_hi  = exp(row$Estimate + 1.96 * row$`Std. Error`),
    n         = nrow(tmp),
    n_subj    = n_distinct(tmp$id)
  )
}

glmm_models <- list(
  list("self_inj",         "liwc_death",         cov_liwc),
  list("self_inj",         "F0final_sma_qregc3", cov_f0),
  list("suicide_behavior", "liwc_death",         cov_liwc),
  list("suicide_behavior", "F0final_sma_qregc3", cov_f0)
)

cat("\n=== GLMM: Self-harm outcomes (non-baseline) ===\n")
res_glmm <- bind_rows(lapply(glmm_models, function(m) {
  run_glmm(df_followup, m[[1]], m[[2]], m[[3]])
}))
print(res_glmm[, c("outcome","predictor","log_OR","SE","p","OR","OR_CI_lo","OR_CI_hi")])

# ─────────────────────────────────────────────────────────────
# 9. Save all results
# ─────────────────────────────────────────────────────────────
dir.create("results_R", showWarnings = FALSE)

write.csv(res_f0_ssi_in,       "results_R/lme_invnorm_F0_SSI.csv",       row.names=FALSE)
write.csv(res_liwc_ssi_in,     "results_R/lme_invnorm_LIWC_SSI.csv",     row.names=FALSE)
write.csv(res_scale_all,       "results_R/feature_scale_lme_invnorm.csv",row.names=FALSE)
write.csv(res_liwc_ssi_items,  "results_R/ssi_item_lme_liwc_death.csv",  row.names=FALSE)
write.csv(res_f0_ssi_items,    "results_R/ssi_item_lme_F0_qregc3.csv",   row.names=FALSE)
write.csv(res_glmm,            "results_R/selfharm_glmm.csv",            row.names=FALSE)

for (out in names(subscale_results)) {
  write.csv(subscale_results[[out]],
            paste0("results_R/subscale_lme_", out, ".csv"),
            row.names = FALSE)
}
for (out in names(item_results)) {
  write.csv(item_results[[out]],
            paste0("results_R/item_lme_", gsub("F0final_sma_","F0_",out), ".csv"),
            row.names = FALSE)
}

cat("\n✓ All results saved to results_R/\n")

# ─────────────────────────────────────────────────────────────
# 10. Quick Visualization (ggplot2)
# ─────────────────────────────────────────────────────────────

# Forest plot helper
forest_gg <- function(res, title, p_col="p", q_col="q_fdr",
                       feat_col="feature", coef_col="coef",
                       lo_col="CI_lo", hi_col="CI_hi",
                       p_thresh=0.10, clean_prefix="liwc_|F0final_sma_") {
  show <- res[res[[p_col]] < p_thresh, ]
  if (nrow(show) == 0) return(NULL)
  show <- show[order(show[[coef_col]]), ]
  show$label <- gsub(clean_prefix, "", show[[feat_col]]) |>
    gsub("_", " ", x=_) |> tools::toTitleCase()
  show$sig <- ifelse(show[[q_col]] < 0.05, "[FDR]",
               ifelse(show[[p_col]] < 0.05, "*", "."))
  show$label <- factor(show$label, levels = show$label)

  ggplot(show, aes(x = .data[[coef_col]], y = label,
                   xmin = .data[[lo_col]], xmax = .data[[hi_col]],
                   color = sig)) +
    geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.4) +
    geom_errorbar(width = 0.25, alpha = 0.75, orientation = "y") +
    geom_point(size = 2.5) +
    scale_color_manual(values = c("[FDR]" = "#C0392B", "*" = "#E67E22",
                                   "."     = "#F39C12"),
                       name = "Significance") +
    labs(x = "Standardized beta", y = NULL, title = title) +
    theme_classic(base_size = 11) +
    theme(plot.title = element_text(face = "bold"))
}

p1 <- forest_gg(res_liwc_ssi_in,
                "LIWC -> SSI (invnorm)",
                clean_prefix = "liwc_")
p2 <- forest_gg(res_f0_ssi_in,
                "F0 -> SSI (invnorm)",
                clean_prefix = "F0final_sma_")

if (!is.null(p1) && !is.null(p2)) {
  (p1 + p2 + plot_annotation(title = "Speech/Language Features Associated with SSI")) &
    theme(plot.title = element_text(face = "bold"))
  ggsave("results_R/forest_ssi_invnorm.pdf", width=14, height=9)
  cat("✓ forest_ssi_invnorm.pdf\n")
}

# Heatmap (pheatmap)
if (requireNamespace("pheatmap", quietly=TRUE)) {
  library(pheatmap)
  library(viridis)

  make_heatmap <- function(res, group_label, fname) {
    fdr_feats <- unique(res$feature[res$q_fdr < 0.05])
    nom_feats <- unique(res$feature[res$p    < 0.05])
    show_feats <- union(fdr_feats, nom_feats)
    if (length(show_feats) == 0) return(invisible(NULL))

    mat <- res |>
      filter(feature %in% show_feats) |>
      select(feature, outcome, coef) |>
      pivot_wider(names_from = outcome, values_from = coef) |>
      tibble::column_to_rownames("feature") |>
      as.matrix()

    rownames(mat) <- gsub("liwc_|F0final_sma_", "", rownames(mat)) |>
      gsub("_", " ", x=_)

    # annotation: FDR significance
    sig_mat <- res |>
      filter(feature %in% show_feats) |>
      mutate(sig = ifelse(q_fdr < 0.05, "*", "")) |>
      select(feature, outcome, sig) |>
      pivot_wider(names_from = outcome, values_from = sig,
                  values_fill = "") |>
      tibble::column_to_rownames("feature") |>
      as.matrix()
    rownames(sig_mat) <- gsub("liwc_|F0final_sma_", "", rownames(sig_mat)) |>
      gsub("_", " ", x=_)

    pheatmap(mat,
             display_numbers = sig_mat,
             color = colorRampPalette(c("#2980B9","white","#C0392B"))(100),
             breaks = seq(-max(abs(mat), na.rm=TRUE),
                           max(abs(mat), na.rm=TRUE),
                           length.out = 101),
             cluster_rows = TRUE, cluster_cols = FALSE,
             fontsize = 9,
             main = paste(group_label,
                          "features (invnorm) x Clinical Scales"),
             filename = fname, width = 7,
             height = max(5, nrow(mat) * 0.3 + 2))
    cat("✓", fname, "\n")
  }

  make_heatmap(res_f0_scale,   "F0",   "results_R/heatmap_F0_scales.pdf")
  make_heatmap(res_liwc_scale, "LIWC", "results_R/heatmap_LIWC_scales.pdf")
}
