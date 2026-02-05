# ==========================================
# BIPARTITE NETWORK ANALYSIS
# Adapted from Despard et al. 2025
# ==========================================

library(bipartite)
library(tidyverse)
library(tidyr)

all_correlations_classified <- read.csv("all_spearman_correlations_by_conditions.csv", header = T)

# ==========================================
# STEP 1: Prepare data for each condition
# ==========================================

# Function to create bipartite matrix (presence/absence)
# Following Despard: unweighted network of miRNAs and targets
create_bipartite_matrix_despard <- function(cor_data, 
                                            timepoint_val,
                                            treatment_val,
                                            cor_threshold = 0.0) {
  
  # Filter for this condition with strong correlations
  filtered_data <- cor_data %>%
    filter(timepoint == timepoint_val, 
           treatment == treatment_val,
           !is.na(cor_value),
           abs(cor_value) >= cor_threshold) %>%
    select(miRNA, mRNA) %>%
    distinct() %>%
    mutate(presence = 1)  # Unweighted (presence/absence)
  
  # Create matrix: rows = miRNAs (higher level), cols = mRNAs (lower level)
  bp_matrix <- filtered_data %>%
    pivot_wider(names_from = mRNA, 
                values_from = presence, 
                values_fill = 0) %>%
    column_to_rownames("miRNA") %>%
    as.matrix()
  
  return(bp_matrix)
}

# Create matrices for all conditions
cat("=== CREATING BIPARTITE MATRICES ===\n\n")

bp_TP0 <- create_bipartite_matrix_despard(all_correlations_classified, 
                                          "TP0", "Acclimation", 
                                          cor_threshold = 0.0)

bp_TP5_Ambient <- create_bipartite_matrix_despard(all_correlations_classified, 
                                                  "TP5", "Ambient", 
                                                  cor_threshold = 0.0)

bp_TP5_Heat <- create_bipartite_matrix_despard(all_correlations_classified, 
                                               "TP5", "Heat", 
                                               cor_threshold = 0.0)

bp_TP7_Ambient <- create_bipartite_matrix_despard(all_correlations_classified, 
                                                  "TP7", "Ambient", 
                                                  cor_threshold = 0.0)

bp_TP7_Heat <- create_bipartite_matrix_despard(all_correlations_classified, 
                                               "TP7", "Heat", 
                                               cor_threshold = 0.0)

# Store in list for easy iteration
bp_matrices_list <- list(
  "TP0_Acclimation" = bp_TP0,
  "TP5_Ambient" = bp_TP5_Ambient,
  "TP5_Heat" = bp_TP5_Heat,
  "TP7_Ambient" = bp_TP7_Ambient,
  "TP7_Heat" = bp_TP7_Heat
)

# Check dimensions
for (name in names(bp_matrices_list)) {
  cat(name, ":", nrow(bp_matrices_list[[name]]), "miRNAs x", 
      ncol(bp_matrices_list[[name]]), "mRNAs,",
      sum(bp_matrices_list[[name]]), "interactions\n")
}

# ==========================================
# STEP 2: Calculate species-level metrics
# Following Despard: degree, betweenness, d' (specialization)
# ==========================================

cat("\n=== CALCULATING SPECIES-LEVEL METRICS ===\n\n")

# Function to calculate metrics for one condition
calculate_mirna_metrics <- function(bp_matrix, condition_name) {
  
  cat("Processing", condition_name, "...\n")
  
  if (nrow(bp_matrix) == 0) {
    return(data.frame())
  }
  
  # Calculate degree and betweenness
  # level = "higher" means miRNAs (rows)
  metrics <- specieslevel(bp_matrix, 
                          level = "higher", 
                          index = c("degree", "betweenness"))
  
  # Calculate d' (specialization index)
  # dfun calculates specialization (d') for each species
  d_prime <- dfun(bp_matrix)
  
  # Combine results
  results <- data.frame(
    miRNA = rownames(metrics),
    condition = condition_name,
    degree = metrics[, "degree"],
    betweenness = metrics[, "betweenness"],
    dprime = d_prime$dprime[rownames(metrics)]
  )
  
  return(results)
}

# Calculate for all conditions
all_mirna_metrics <- map_dfr(names(bp_matrices_list), function(cond_name) {
  calculate_mirna_metrics(bp_matrices_list[[cond_name]], cond_name)
})

# Add timepoint and treatment columns
all_mirna_metrics <- all_mirna_metrics %>%
  mutate(
    timepoint = str_extract(condition, "TP[0-9]"),
    treatment = str_extract(condition, "(Acclimation|Ambient|Heat)")
  ) %>%
  select(condition, timepoint, treatment, miRNA, degree, betweenness, dprime)

cat("Calculated metrics for", nrow(all_mirna_metrics), "miRNA-condition combinations\n")

# ==========================================
# STEP 3: Summary statistics by condition
# ==========================================

cat("\n=== SUMMARY BY CONDITION ===\n\n")

metrics_summary <- all_mirna_metrics %>%
  group_by(condition, timepoint, treatment) %>%
  summarise(
    n_miRNAs = n(),
    mean_degree = mean(degree, na.rm = TRUE),
    median_degree = median(degree, na.rm = TRUE),
    max_degree = max(degree, na.rm = TRUE),
    mean_betweenness = mean(betweenness, na.rm = TRUE),
    mean_dprime = mean(dprime, na.rm = TRUE),
    median_dprime = median(dprime, na.rm = TRUE),
    .groups = "drop"
  )

print(metrics_summary)

# ==========================================
# STEP 4: Compare metrics across conditions
# (analogous to Despard comparing across miRNA categories)
# ==========================================

# Round dprime for display (like Despard)
all_mirna_metrics <- all_mirna_metrics %>%
  mutate(dprime_round = round(dprime, 2))

# Statistical comparison - are metrics different across timepoints?
# Using Kruskal-Wallis (non-parametric) since data may not be normal

cat("\n=== STATISTICAL COMPARISONS ===\n\n")

# Test if degree differs by timepoint (ambient only)
ambient_data <- all_mirna_metrics %>%
  filter(treatment %in% c("Acclimation", "Ambient"))

degree_test <- kruskal.test(degree ~ timepoint, data = ambient_data)
cat("Degree across timepoints (ambient): p =", 
    format.pval(degree_test$p.value, digits = 3), "\n")

dprime_test <- kruskal.test(dprime ~ timepoint, data = ambient_data)
cat("d' across timepoints (ambient): p =", 
    format.pval(dprime_test$p.value, digits = 3), "\n")

betweenness_test <- kruskal.test(betweenness ~ timepoint, data = ambient_data)
cat("Betweenness across timepoints (ambient): p =", 
    format.pval(betweenness_test$p.value, digits = 3), "\n")

# Test treatment effect within each timepoint
tp5_data <- all_mirna_metrics %>% filter(timepoint == "TP5")
tp7_data <- all_mirna_metrics %>% filter(timepoint == "TP7")

cat("\nTreatment effects:\n")
if (nrow(tp5_data) > 0) {
  tp5_test <- wilcox.test(degree ~ treatment, data = tp5_data)
  cat("TP5 degree (Ambient vs Heat): p =", 
      format.pval(tp5_test$p.value, digits = 3), "\n")
}

if (nrow(tp7_data) > 0) {
  tp7_test <- wilcox.test(degree ~ treatment, data = tp7_data)
  cat("TP7 degree (Ambient vs Heat): p =", 
      format.pval(tp7_test$p.value, digits = 3), "\n")
}

# ==========================================
# STEP 5: Visualizations (like Despard's plots)
# ==========================================

# Set up factor levels for ordering
all_mirna_metrics <- all_mirna_metrics %>%
  mutate(
    timepoint = factor(timepoint, levels = c("TP0", "TP5", "TP7")),
    treatment = factor(treatment, levels = c("Acclimation", "Ambient", "Heat"))
  )

# Plot 1: Degree distribution by condition
p_degree <- ggplot(all_mirna_metrics, 
                   aes(x = condition, y = degree, fill = timepoint)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "miRNA degree (number of targets) across conditions",
    x = "",
    y = "Degree (number of mRNA targets)",
    fill = "Timepoint"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_degree)
ggsave("mirna_degree_by_condition.pdf", width = 8, height = 6)

# Plot 2: Specialization (d') by condition
p_dprime <- ggplot(all_mirna_metrics,
                   aes(x = condition, y = dprime, fill = timepoint)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "miRNA specialization (d') across conditions",
    subtitle = "Higher d' = more specialized targeting",
    x = "",
    y = "Specialization index (d')",
    fill = "Timepoint"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_dprime)
ggsave("mirna_specialization_by_condition.pdf", width = 8, height = 6)

# Plot 3: Betweenness
p_betweenness <- ggplot(all_mirna_metrics,
                        aes(x = condition, y = betweenness, fill = timepoint)) +
  geom_boxplot() +
  theme_minimal() +
  scale_y_log10() +  # Log scale because betweenness can be very skewed
  labs(
    title = "miRNA betweenness centrality across conditions",
    subtitle = "Higher betweenness = more central in network",
    x = "",
    y = "Betweenness (log scale)",
    fill = "Timepoint"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_betweenness)
ggsave("mirna_betweenness_by_condition.pdf", width = 8, height = 6)

# Plot 4: Seasonal trends (ambient only)
seasonal_metrics <- all_mirna_metrics %>%
  filter(treatment %in% c("Acclimation", "Ambient")) %>%
  group_by(timepoint) %>%
  summarise(
    mean_degree = mean(degree),
    se_degree = sd(degree) / sqrt(n()),
    mean_dprime = mean(dprime),
    se_dprime = sd(dprime) / sqrt(n()),
    .groups = "drop"
  )

p_seasonal <- ggplot(seasonal_metrics, 
                     aes(x = timepoint, y = mean_degree, group = 1)) +
  geom_line(linewidth = 1.2, color = "steelblue") +
  geom_point(size = 4, color = "steelblue") +
  geom_errorbar(aes(ymin = mean_degree - se_degree, 
                    ymax = mean_degree + se_degree),
                width = 0.2) +
  theme_minimal() +
  labs(
    title = "Seasonal changes in mean miRNA degree",
    subtitle = "Ambient conditions, mean ± SE",
    x = "Timepoint",
    y = "Mean degree (number of targets)"
  )

print(p_seasonal)
ggsave("seasonal_degree_trend.pdf", width = 7, height = 5)

# ==========================================
# STEP 6: Identify key miRNAs
# ==========================================

# Top hub miRNAs (highest degree) in each condition
top_hubs <- all_mirna_metrics %>%
  group_by(condition) %>%
  slice_max(degree, n = 10) %>%
  ungroup() %>%
  arrange(condition, desc(degree))

cat("\n=== TOP 10 HUB miRNAs PER CONDITION ===\n\n")
print(top_hubs %>% select(condition, miRNA, degree, dprime_round))

# Most specialized miRNAs (highest d')
top_specialized <- all_mirna_metrics %>%
  group_by(condition) %>%
  slice_max(dprime, n = 10) %>%
  ungroup() %>%
  arrange(condition, desc(dprime))

cat("\n=== TOP 10 MOST SPECIALIZED miRNAs PER CONDITION ===\n\n")
print(top_specialized %>% select(condition, miRNA, dprime_round, degree))

# ==========================================
# STEP 7: Save results
# ==========================================

write_csv(all_mirna_metrics, "mirna_network_metrics_all_conditions.csv")
write_csv(metrics_summary, "network_metrics_summary.csv")
write_csv(top_hubs, "top_hub_mirnas.csv")
write_csv(top_specialized, "top_specialized_mirnas.csv")

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Files saved:\n")
cat("  - mirna_network_metrics_all_conditions.csv\n")
cat("  - network_metrics_summary.csv\n")
cat("  - top_hub_mirnas.csv\n")
cat("  - top_specialized_mirnas.csv\n")