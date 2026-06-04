rm(list = ls())
library(pacman)
p_load(
  openxlsx, tidyverse, cowplot, ggpubr, ggunchained, patchwork, otuSummary,
  lmerTest, lme4, nlme, ggh4x, vegan, rdacca.hp, ggtern, dplyr, ggplot2,
  Polychrome, car, emmeans, ggrepel, FactoMineR, factoextra, ggview,
  ggpmisc, piecewiseSEM, reshape2, MetBrewer, gRodon, emmeans, semEff
)

source("pipeline_code_upload.R")

#### Fig2 CUE, substrate traits, SUE and RAE ####
df <- openxlsx::read.xlsx("data_for_RAE.xlsx", sheet = 1) %>%
  dplyr::mutate(Management = factor(Management, levels = c("Conventional", "Conservation")))

cue.summary_df <- df %>%
  dplyr::group_by(Treat) %>%
  dplyr::summarise(
    CUE = mean(CUE),
    SUE_Glu = mean(SUE_Glu),
    SUE_Van = mean(SUE_Van),
    .groups = "drop"
  ); head(cue.summary_df)

# A. CUE
A.CUE_df <- df %>%
  dplyr::select(Treat, Management, Warm, CUE) %>%
  tidyr::pivot_longer(-c(Treat, Management, Warm), values_to = "value")

A.CUE_plot <- ggplot(A.CUE_df, aes(x = Management, y = value, fill = Warm, color = Warm, shape = Management)) +
  geom_rect(
    data = background_data,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
    fill = background_data$fill_color,
    alpha = 0.1,
    inherit.aes = FALSE
  ) +
  ggunchained::geom_split_violin(alpha = 0.6, trim = FALSE, width = 0.5, scale = "width") +
  stat_summary(
    fun = "mean",
    geom = "point",
    position = position_dodge(0.3),
    show.legend = FALSE,
    size = 3,
    color = "black",
    stroke = 1
  ) +
  geom_vline(xintercept = 1.5, linetype = "dashed", color = "black", linewidth = 0.6) +
  scale_fill_manual(values = cols1) +
  scale_color_manual(values = cols1) +
  scale_shape_manual(values = c(21, 23)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
  scale_x_discrete(expand = expansion(add = 0.2)) +
  coord_cartesian(ylim = c(0, 0.3)) +
  labs(y = "CUE", x = NULL, color = NULL, fill = NULL) +
  my_theme +
  theme(legend.position = "none", axis.title.x = element_blank())
A.CUE_plot

# B and C. Substrate traits
B.substrate_df <- df %>%
  dplyr::select(Treat, Management, Warm, Block, LCI, Root_exudation, DOC_SOC, MAOC_POC)

B.substrate.summary_df <- B.substrate_df %>%
  dplyr::group_by(Treat) %>%
  dplyr::summarise(
    dplyr::across(c(LCI, Root_exudation, DOC_SOC, MAOC_POC), mean),
    .groups = "drop"
  )

B.substrate.scaled_df <- B.substrate_df %>%
  dplyr::select(Root_exudation, DOC_SOC, MAOC_POC, LCI) %>%
  scale() %>%
  as.data.frame()

B.pca_result <- vegan::rda(B.substrate.scaled_df)
B.pca.importance_df <- summary(B.pca_result)$cont$importance %>%
  as.data.frame()

B.pca.sites_df <- vegan::scores(B.pca_result, display = "sites") %>%
  as.data.frame() %>%
  dplyr::mutate(
    Temperature = B.substrate_df$Warm,
    System = B.substrate_df$Management,
    Treat = B.substrate_df$Treat
  )

B.pca.vars_df <- vegan::scores(B.pca_result, display = "species") %>%
  as.data.frame() %>%
  tibble::rownames_to_column("varname")

B.substrate.pca_plot <- ggplot(B.pca.sites_df, aes(x = PC1, y = PC2)) +
  geom_segment(
    data = B.pca.vars_df,
    aes(x = 0, y = 0, xend = PC1, yend = PC2),
    arrow = arrow(length = unit(0.2, "cm")),
    color = "gray30"
  ) +
  geom_text(data = B.pca.vars_df, aes(x = PC1, y = PC2, label = varname), size = 4) +
  geom_point(aes(shape = System, fill = Temperature), size = 4, alpha = 1, stroke = 1) +
  scale_fill_manual(values = cols1) +
  scale_shape_manual(values = c(23, 21)) +
  labs(
    x = paste0("PC1 (", round(B.pca.importance_df[2, 1] * 100, 1), "%)"),
    y = paste0("PC2 (", round(B.pca.importance_df[2, 2] * 100, 1), "%)")
  ) +
  my_theme +
  theme(legend.position = "none"); B.substrate.pca_plot

C.pca.var_df <- FactoMineR::PCA(B.substrate.scaled_df, graph = FALSE)$var$contrib %>%
  as.data.frame() %>%
  tibble::rownames_to_column("rowname")

C.pca.contribution_df <- C.pca.var_df %>%
  dplyr::mutate(group = c("access", "access", "quality", "quality")) %>%
  dplyr::select(rowname, group, Dim.1, Dim.2) %>%
  tidyr::pivot_longer(-c(rowname, group)) %>%
  dplyr::group_by(name) %>%
  dplyr::mutate(rowname_ordered = factor(rowname, levels = rowname[order(value)])) %>%
  dplyr::ungroup()

C.pca.contribution_plot <- ggplot(C.pca.contribution_df, aes(x = rowname_ordered, y = value, fill = group)) +
  geom_bar(stat = "identity", color = "black", alpha = 1) +
  geom_text(aes(label = rowname), size = 3, hjust = -0.1) +
  coord_flip() +
  facet_wrap(~name, ncol = 1) +
  scale_y_continuous(limits = c(0, 55), breaks = seq(0, 50, 10)) +
  scale_fill_manual(values = rev(c("#ccaf9a", "grey80"))) +
  ylab("Contribution to PCA (%)") +
  my_theme +
  theme(
    panel.background = element_blank(),
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    axis.line.x = element_line(),
    axis.line.y = element_line(),
    plot.title = element_blank(),
    strip.text = element_blank()
  ); C.pca.contribution_plot

# D and E. SUE
D.SUE.Glu_df <- df %>%
  dplyr::select(Treat, Management, Warm, SUE_Glu) %>%
  tidyr::pivot_longer(-c(Treat, Management, Warm), values_to = "value")

D.SUE.Glu_plot <- ggplot(D.SUE.Glu_df, aes(x = Management, y = value, fill = Warm, color = Warm, shape = Management)) +
  geom_rect(
    data = background_data,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
    fill = background_data$fill_color,
    alpha = 0.1,
    inherit.aes = FALSE
  ) +
  ggunchained::geom_split_violin(alpha = 0.6, trim = FALSE, width = 1, scale = "width") +
  stat_summary(
    fun = "mean",
    geom = "point",
    position = position_dodge(0.3),
    show.legend = FALSE,
    size = 3,
    color = "black",
    stroke = 1
  ) +
  geom_vline(xintercept = 1.5, linetype = "dashed", color = "black", linewidth = 0.6) +
  scale_fill_manual(values = cols1) +
  scale_color_manual(values = cols1) +
  scale_shape_manual(values = c(21, 23)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
  scale_x_discrete(expand = expansion(add = 0.7)) +
  coord_cartesian(ylim = c(0.65, 0.95)) +
  labs(y = "SUE_Glu", x = NULL, color = NULL, fill = NULL) +
  my_theme +
  theme(legend.position = "none", axis.title.x = element_blank())
D.SUE.Glu_plot

E.SUE.Van_df <- df %>%
  dplyr::select(Treat, Management, Warm, SUE_Van) %>%
  tidyr::pivot_longer(-c(Treat, Management, Warm), values_to = "value")

E.SUE.Van_plot <- ggplot(E.SUE.Van_df, aes(x = Management, y = value, fill = Warm, color = Warm, shape = Management)) +
  geom_rect(
    data = background_data,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
    fill = background_data$fill_color,
    alpha = 0.1,
    inherit.aes = FALSE
  ) +
  ggunchained::geom_split_violin(alpha = 0.6, trim = FALSE, width = 1, scale = "width") +
  stat_summary(
    fun = "mean",
    geom = "point",
    position = position_dodge(0.3),
    show.legend = FALSE,
    size = 3,
    color = "black",
    stroke = 1
  ) +
  geom_vline(xintercept = 1.5, linetype = "dashed", color = "black", linewidth = 0.6) +
  scale_fill_manual(values = cols1) +
  scale_color_manual(values = cols1) +
  scale_shape_manual(values = c(21, 23)) +
  scale_y_continuous(limits = c(0.35, 0.9), labels = scales::number_format(accuracy = 0.01)) +
  scale_x_discrete(expand = expansion(add = 0.7)) +
  labs(y = "SUE_Van", x = NULL, color = NULL, fill = NULL) +
  my_theme +
  theme(legend.position = "none")
E.SUE.Van_plot

# F. RAE
F.growth.resp_df <- df %>%
  dplyr::select(Treat, Block, Management, Warm, CUE_G, CUE_R, SUE_Glu_G, SUE_Glu_R, SUE_Van_G, SUE_Van_R) %>%
  tidyr::pivot_longer(
    cols = dplyr::matches("_(G|R)$"),
    names_to = c("group", "flux"),
    names_pattern = "(.+)_(G|R)$",
    values_to = "value"
  ) %>%
  dplyr::group_by(Treat, Management, Warm, group, flux) %>%
  dplyr::summarise(mean_value = mean(value), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = flux, values_from = mean_value)

F.warming.ratio_df <- F.growth.resp_df %>%
  tidyr::pivot_wider(
    id_cols = c(Management, group),
    names_from = Warm,
    values_from = c(G, R),
    names_sep = "_"
  ) %>%
  dplyr::mutate(
    mean_G_ratio = G_Warming / G_Ambient,
    mean_R_ratio = R_Warming / R_Ambient
  ) %>%
  dplyr::select(Management, group, mean_G_ratio, mean_R_ratio)

F.G.R_plot <- ggplot(
  F.warming.ratio_df,
  aes(x = mean_G_ratio, y = mean_R_ratio, shape = group, fill = Management)
) +
  geom_point(size = 6, stroke = 1.5) +
  geom_hline(yintercept = 1.275, linetype = "longdash", color = "black") +
  geom_vline(xintercept = 1.275, linetype = "longdash", color = "black") +
  geom_abline(intercept = 0, slope = 1, linetype = "dotted", color = "black") +
  geom_abline(intercept = 2.55, slope = -1, linetype = "dotted", color = "black") +
  scale_shape_manual(values = c(21, 22, 23)) +
  scale_fill_manual(values = c("#404040", "#9d5d21")) +
  scale_x_continuous(limits = c(0.35, 2.2)) +
  scale_y_continuous(limits = c(0.35, 2.2)) +
  labs(x = "RR_Growth", y = "RR_Respiration") +
  my_theme +
  theme(legend.position = "none")
F.G.R_plot

F.RAE_df <- df %>%
  dplyr::select(Treat, Management, Block, Warm, SUE_Glu, SUE_Van) %>%
  dplyr::mutate(prefer = SUE_Glu / SUE_Van) %>%
  dplyr::select(-SUE_Glu, -SUE_Van) %>%
  tidyr::pivot_longer(-c(Treat, Management, Warm, Block))

F.RAE_plot <- df %>%
  dplyr::select(Treat, Management, Block, Warm, SUE_Glu, SUE_Van) %>%
  dplyr::mutate(prefer = SUE_Glu / SUE_Van) %>%
  dplyr::group_by(Management, Block) %>%
  dplyr::summarise(
    ratio_prefer = mean(prefer[Warm == "Warming"]) / mean(prefer[Warm == "Ambient"]),
    .groups = "drop"
  ) %>%
  dplyr::group_by(Management) %>%
  dplyr::summarise(
    mean_value = mean(ratio_prefer),
    se_value = sd(ratio_prefer) / sqrt(dplyr::n()),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    treatment = factor(
      paste(Management, "ratio_prefer", sep = "_"),
      levels = c("Conventional_ratio_prefer", "Conservation_ratio_prefer")
    )
  ) %>%
  ggplot(aes(x = treatment, y = mean_value, shape = treatment)) +
  geom_errorbar(aes(ymin = mean_value - se_value, ymax = mean_value + se_value), width = 0.2, linewidth = 0.8) +
  geom_point(size = 5, stroke = 1, fill = "white") +
  geom_hline(yintercept = 1, color = "grey50", linetype = "longdash", linewidth = 0.5) +
  scale_shape_manual(values = c(23, 21)) +
  scale_y_continuous(limits = c(0.95, 1.5)) +
  labs(y = "Warming effect", x = NULL) +
  my_theme +
  theme(legend.position = "none", axis.text.x = element_blank())
F.RAE_plot

# Figure assembly
Fig2.ABC_plot <- cowplot::plot_grid(
  A.CUE_plot,
  B.substrate.pca_plot,
  C.pca.contribution_plot,
  ncol = 3,
  align = "vh",
  axis = "tblr",
  rel_widths = c(2, 2, 2)
)

Fig2.DEF_plot <- cowplot::plot_grid(
  D.SUE.Glu_plot,
  E.SUE.Van_plot,
  (F.G.R_plot / F.RAE_plot) + patchwork::plot_layout(heights = c(2, 1)),
  ncol = 3,
  align = "vh",
  axis = "tblr",
  rel_widths = c(2, 2, 2)
)

Fig2.final_plot <- cowplot::plot_grid(
  Fig2.ABC_plot,
  Fig2.DEF_plot,
  nrow = 2,
  align = "hv",
  rel_heights = c(1, 1)
)
Fig2.final_plot


#### Fig3 Amplicon sequencing figures ####
df <- openxlsx::read.xlsx("data_for_RAE.xlsx", sheet = 2) %>%
  dplyr::mutate(
    Management = factor(Management, levels = c("Conventional", "Conservation"))
  ); head(df)

phylum.sample_order <- c(
  paste0("TN", 1:4),
  paste0("TW", 1:4),
  paste0("CN", 1:4),
  paste0("CW", 1:4)
)

df$Treat <- factor(df$Treat, levels = phylum.sample_order)

# A and B. Alpha diversity
bacteria.alpha_df <- df %>%
  dplyr::select(Treat, B_Richness, B_Shannon) %>%
  tidyr::pivot_longer(-Treat)

bacteria.alpha_plot <- ggplot(bacteria.alpha_df, aes(x = Treat, y = value)) +
  geom_bar(stat = "identity") +
  facet_wrap(~name, scales = "free") +
  labs(x = NULL) +
  ggh4x::facetted_pos_scales(
    y = list(
      name == "Richness" ~ scale_y_continuous(limits = c(1200, 2000), breaks = c(1200, 2000)),
      name == "Shannon" ~ scale_y_continuous(limits = c(8, 11), breaks = c(8, 11))
    )
  ) +
  theme_classic()
bacteria.alpha_plot

fungi.alpha_df <- df %>%
  dplyr::select(Treat, F_Richness, F_Shannon) %>%
  tidyr::pivot_longer(-Treat)

fungi.alpha_plot <- ggplot(fungi.alpha_df, aes(x = Treat, y = value)) +
  geom_bar(stat = "identity") +
  facet_wrap(~name, scales = "free") +
  labs(x = NULL) +
  ggh4x::facetted_pos_scales(
    y = list(
      name == "Richness" ~ scale_y_continuous(limits = c(300, 1000), breaks = c(300, 1000)),
      name == "Shannon" ~ scale_y_continuous(limits = c(4, 8), breaks = c(4, 8))
    )
  ) +
  theme_classic()
fungi.alpha_plot

# A and B. Bacterial species composition at phylum level
bac.phylum.comp_df <- df %>%
  dplyr::select(Treat, Acidobacteriota:Pseudomonadota) %>%
  tidyr::pivot_longer(
    cols = Acidobacteriota:Pseudomonadota,
    names_to = "ID",
    values_to = "Abundance"
  ) %>%
  tidyr::pivot_wider(
    names_from = Treat,
    values_from = Abundance,
    names_sort = FALSE
  )

bac.phylum.cols <- c(
  "#294b41", "#D9B5CF", "#3A9B3F", "#C7252D", "#28A9C9",
  "#F28E2B", "#7B5AA6", "#4F6FAE", "#8AA0A8", "#4D4D4D"
)

bac.phylum.top9_taxa <- c(
  "Actinomycetota", "Pseudomonadota", "Acidobacteriota",
  "Planctomycetota", "Bacillota", "Chloroflexota",
  "Bacteroidota", "Myxococcota", "Gemmatimonadota"
)

names(bac.phylum.cols) <- c(bac.phylum.top9_taxa, "B_Others")

plot.bac.phylum_comp <- function(sample.pattern, group.name) {
  bac.phylum.sample_order <- phylum.sample_order[
    stringr::str_detect(phylum.sample_order, sample.pattern)
  ]

  bac.phylum.sub_df <- bac.phylum.comp_df %>%
    dplyr::select(ID, dplyr::all_of(bac.phylum.sample_order))

  bac.phylum.long_df <- bac.phylum.sub_df %>%
    tidyr::pivot_longer(-ID, names_to = "Sample", values_to = "Abundance") %>%
    dplyr::group_by(Sample) %>%
    dplyr::mutate(Percent = Abundance / sum(Abundance, na.rm = TRUE) * 100) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      Sample = factor(Sample, levels = bac.phylum.sample_order),
      ID = factor(ID, levels = rev(c(bac.phylum.top9_taxa, "B_Others")))
    )

  ggplot(bac.phylum.long_df, aes(Sample, Percent, fill = ID)) +
    geom_col(width = 0.85, color = "white", linewidth = 0.15) +
    coord_flip(ylim = c(0, 100), clip = "off") +
    scale_fill_manual(values = bac.phylum.cols, breaks = names(bac.phylum.cols)) +
    scale_y_continuous(expand = c(0, 0), breaks = seq(0, 100, 25)) +
    labs(x = NULL, y = "Relative abundance (%)", fill = NULL, title = group.name) +
    theme_classic(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.text.y = element_text(color = "black"),
      axis.ticks.y = element_blank(),
      axis.line.y = element_blank(),
      legend.position = "bottom",
      legend.key.size = unit(0.35, "cm"),
      legend.text = element_text(size = 10)
    )
}

bac.phylum.CNCW_plot <- plot.bac.phylum_comp("^(CN|CW)", "CN/CW")
bac.phylum.TNTW_plot <- plot.bac.phylum_comp("^(TN|TW)", "TN/TW")

bac.phylum.combined_plot <- (bac.phylum.TNTW_plot / bac.phylum.CNCW_plot) +
  patchwork::plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
bac.phylum.combined_plot

# A and B. Fungal species composition at phylum level
fungi.phylum.comp_df <- df %>%
  dplyr::select(Treat, Ascomycota:F_Others) %>%
  tidyr::pivot_longer(
    cols = Ascomycota:F_Others,
    names_to = "ID",
    values_to = "Abundance"
  ) %>%
  tidyr::pivot_wider(
    names_from = Treat,
    values_from = Abundance,
    names_sort = FALSE
  )

fungi.phylum.cols <- c(
  "#2F80C9", "#D98627", "#8C77B5", "#4AB0A7", "#79B9E3",
  "#9E2428", "#51338D", "#9A6334", "#1D6264", "#D9D9D9"
)

fungi.phylum.top9_taxa <- c(
  "Ascomycota", "Basidiomycota", "Blastocladiomycota",
  "Chytridiomycota", "Glomeromycota", "Kickxellomycota",
  "Mortierellomycota", "Mucoromycota", "Olpidiomycota"
)

names(fungi.phylum.cols) <- c(fungi.phylum.top9_taxa, "F_Others")

plot.fungi.phylum_comp <- function(sample.pattern, group.name) {
  fungi.phylum.sample_order <- phylum.sample_order[
    stringr::str_detect(phylum.sample_order, sample.pattern)
  ]

  fungi.phylum.sub_df <- fungi.phylum.comp_df %>%
    dplyr::select(ID, dplyr::all_of(fungi.phylum.sample_order))

  fungi.phylum.long_df <- fungi.phylum.sub_df %>%
    tidyr::pivot_longer(-ID, names_to = "Sample", values_to = "Abundance") %>%
    dplyr::group_by(Sample) %>%
    dplyr::mutate(Percent = Abundance / sum(Abundance, na.rm = TRUE) * 100) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      Sample = factor(Sample, levels = fungi.phylum.sample_order),
      ID = factor(ID, levels = rev(c(fungi.phylum.top9_taxa, "F_Others")))
    )

  ggplot(fungi.phylum.long_df, aes(Sample, Percent, fill = ID)) +
    geom_col(width = 0.85, color = "white", linewidth = 0.15) +
    coord_flip(ylim = c(0, 100), clip = "off") +
    scale_fill_manual(values = fungi.phylum.cols, breaks = names(fungi.phylum.cols)) +
    scale_y_continuous(expand = c(0, 0), breaks = seq(0, 100, 25)) +
    labs(x = NULL, y = "Relative abundance (%)", fill = NULL, title = group.name) +
    theme_classic(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.text.y = element_text(color = "black"),
      axis.ticks.y = element_blank(),
      axis.line.y = element_blank(),
      legend.position = "bottom",
      legend.key.size = unit(0.35, "cm"),
      legend.text = element_text(size = 10)
    )
}

fungi.phylum.CNCW_plot <- plot.fungi.phylum_comp("^(CN|CW)", "CN/CW")
fungi.phylum.TNTW_plot <- plot.fungi.phylum_comp("^(TN|TW)", "TN/TW")

fungi.phylum.combined_plot <- (fungi.phylum.TNTW_plot / fungi.phylum.CNCW_plot) +
  patchwork::plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
fungi.phylum.combined_plot

# C-E. Life-history strategies based on amplicon sequencing
plot.box_trait <- function(data_df, y.var, y.lim = NULL, show.x.text = FALSE) {
  y.var <- rlang::enquo(y.var)

  trait_plot <- data_df %>%
    ggplot(aes(Management, !!y.var, fill = Warm, shape = Management)) +
    geom_rect(
      data = background_data,
      aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
      fill = background_data$fill_color,
      alpha = 0.1,
      inherit.aes = FALSE
    ) +
    geom_boxplot(
      alpha = 0.6,
      width = 0.5,
      outlier.shape = NA,
      coef = 2,
      position = position_dodge(width = 0.6)
    ) +
    stat_summary(
      fun = "mean",
      geom = "point",
      position = position_dodge(width = 0.6),
      show.legend = FALSE,
      size = 3,
      color = "black",
      stroke = 1
    ) +
    scale_fill_manual(values = cols1) +
    scale_shape_manual(values = c(21, 23)) +
    my_theme +
    theme(axis.title.x = element_blank(), legend.position = "none")

  if (!is.null(y.lim)) trait_plot <- trait_plot + coord_cartesian(ylim = y.lim)
  if (!show.x.text) trait_plot <- trait_plot + theme(axis.text.x = element_blank())

  trait_plot
}

rrn.copy_plot <- plot.box_trait(df, rrn_copy, show.x.text = FALSE) +
  scale_y_continuous(limits = c(2.2, 2.5))
rrn.copy_plot

B.rK_plot <- plot.box_trait(df, B_r_K, show.x.text = FALSE) +
  coord_cartesian(ylim = c(1.0, 1.5))
B.rK_plot

F.rK_plot <- plot.box_trait(df, F_r_K, show.x.text = TRUE) +
  scale_y_continuous(limits = c(10, 40))
F.rK_plot

life.history_plot <- rrn.copy_plot / B.rK_plot / F.rK_plot
life.history_plot

#### Fig4 Metagenome analysis ####
# B. functional profile
df <- read.xlsx("data_for_RAE.xlsx", sheet = 3) |> 
  mutate(Management = factor(Management, levels = c('Conventional', 'Conservation')),
         Warm = factor(Warm), AGS_Mb = AGS / 1e6); head(df)

A.C.ratio_plot <- ggplot(df, aes(x = Management, y = A_C_ratio, fill = Warm, shape = Management)) +
  geom_rect(
    data = background_data,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
    fill = background_data$fill_color,
    alpha = 0.15,
    inherit.aes = FALSE
  ) +
  geom_boxplot(
    alpha = .6,
    width = .55,
    outlier.shape = NA,
    coef = 2,
    position = position_dodge(.65),
    color = "black"
  ) +
  stat_summary(
    fun = "mean",
    geom = "point",
    position = position_dodge(.65),
    show.legend = FALSE,
    size = 3.2,
    color = "black",
    stroke = 1
  ) +
  geom_vline(xintercept = 1.5, linetype = "dashed", color = "black", linewidth = 0.6) +
  scale_fill_manual(values = cols1) +
  scale_shape_manual(values = c(21, 23)) +
  labs(x = NULL, y = "Anabolism/Catabolism", fill = NULL) +
  theme_cowplot() +
  theme(
    axis.text = element_text(size = 14),
    axis.title.y = element_text(size = 16),
    legend.position = "none"
  )
A.C.ratio_plot

Gene.rK_plot <- ggplot(df, aes(x = Management, y = Gene_r_K, fill = Warm, shape = Management)) +
  geom_rect(
    data = background_data,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
    fill = background_data$fill_color,
    alpha = 0.15,
    inherit.aes = FALSE
  ) +
  geom_boxplot(
    alpha = .6,
    width = .55,
    outlier.shape = NA,
    coef = 2,
    position = position_dodge(.65),
    color = "black"
  ) +
  stat_summary(
    fun = "mean",
    geom = "point",
    position = position_dodge(.65),
    show.legend = FALSE,
    size = 3.2,
    color = "black",
    stroke = 1
  ) +
  geom_vline(xintercept = 1.5, linetype = "dashed", color = "black", linewidth = 0.6) +
  scale_fill_manual(values = cols1) +
  scale_shape_manual(values = c(21, 23)) +
  labs(x = NULL, y = "L/R genes", fill = NULL) +
  theme_cowplot() +
  theme(
    axis.text = element_text(size = 14),
    axis.title.y = element_text(size = 16),
    legend.position = "none"
  )
Gene.rK_plot

functional.profile_plot <- A.C.ratio_plot / Gene.rK_plot
functional.profile_plot


# C. microbial life-history traits at contigs level
vars <- c("AGS_Mb" = "Average genome size (Mbp)", 
          "ACN" = "Average 16S copy number", 
          "CUBHE" = "CUBHE", 
          "d" = "Minimal doubling time (h-1)")

plots <- list()

for (v in names(vars)) {
  df_v <- df
  
  plots[[v]] <- ggplot(df_v, aes(x = Management, y = .data[[v]], fill = Warm, shape = Management)) +
    geom_rect(data = background_data, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), fill = background_data$fill_color, alpha = 0.15, inherit.aes = FALSE) +
    geom_boxplot(alpha = .6, width = .55, outlier.shape = NA, coef = 2, position = position_dodge(.65), color = "black") +
    stat_summary(fun = "mean", geom = "point", position = position_dodge(.65), show.legend = FALSE, size = 3.2, color = "black", stroke = 1) +
    geom_vline(xintercept = 1.5, linetype = "dashed", color = "black", linewidth = 0.6) +
    scale_fill_manual(values = cols1) +
    scale_shape_manual(values = c(21, 23)) +
    labs(x = NULL, y = vars[v], fill = NULL) +
    theme_cowplot() +
    theme(axis.text = element_text(size = 14), axis.title.y = element_text(size = 16), legend.position = "none") +
    {if(v == "CUBHE") coord_cartesian(ylim = c(0.70, 0.8))} # 特殊处理 CUBHE
}

plots$AGS_Mb; plots$ACN; plots$CUBHE; plots$d

(plots$AGS_Mb | plots$ACN) /
  (plots$CUBHE | plots$d)

# D. plot in https://itol.embl.de/ 

# E. microbial life-history traits at MAGs level
MAG.traits.long_df <- read.xlsx("data_for_RAE.xlsx", sheet = 4) %>%
  group_by(ID, Treat) %>%
  mutate(
    Block = paste0("B", row_number()),
    Sample = paste0(Treat, row_number())
  ) %>%
  ungroup() %>%
  mutate(
    Management = factor(Management, c("Conventional", "Conservation")),
    Warm = factor(Warm, c("Ambient", "Warming")),
    Treat = factor(Treat, c("TN", "TW", "NN", "NW")),
    Block = factor(Block)
  ); head(MAG.traits.long_df )

MAG.weighted.traits_df <- MAG.traits.long_df %>%
  group_by(Sample, Treat, Management, Warm, Block) %>%
  summarise(
    total_MAG_abundance = sum(abundance, na.rm = TRUE),
    CUBHE_weighted_mean = weighted.mean(CUBHE, abundance, na.rm = TRUE),
    Growth_proxy_weighted_mean = weighted.mean(d, abundance, na.rm = TRUE),
    tRNA_count_weighted_mean = weighted.mean(tRNA_count, abundance, na.rm = TRUE),
    tRNA_count_weighted_sum = sum(abundance * tRNA_count, na.rm = TRUE),
    .groups = "drop"
  )

MAG.traits.plot_df <- MAG.traits.long_df %>%
  select(ID, Sample, Treat, Management, Warm, Block, abundance, CUBHE, d, tRNA_count) %>%
  pivot_longer(c(CUBHE, d, tRNA_count), names_to = "Trait", values_to = "Value") %>%
  mutate(Trait = factor(Trait, levels = c("CUBHE", "d", "tRNA_count")))

mean.line_df <- MAG.traits.plot_df %>%
  group_by(Management, Warm, Trait) %>%
  summarise(mean_value = weighted.mean(Value, abundance, na.rm = TRUE), .groups = "drop")

trait.model_map_df <- tibble(
  Trait = factor(c("CUBHE", "d", "tRNA_count"), levels = c("CUBHE", "d", "tRNA_count")),
  model_var = c("CUBHE_weighted_mean", "Growth_proxy_weighted_mean", "tRNA_count_weighted_mean")
)

run_trait_lme <- function(model_var, trait_label) {
  mod <- lme(
    as.formula(paste0(model_var, " ~ Management * Warm")),
    random = ~1 | Block,
    data = MAG.weighted.traits_df,
    method = "ML"
  )

  anova_df <- anova(mod) %>%
    as.data.frame() %>%
    rownames_to_column("term") %>%
    mutate(Trait = trait_label, model_var = model_var)

  pair_df <- pairs(
    emmeans(mod, ~ Warm | Management),
    reverse = TRUE,
    adjust = "bonferroni"
  ) %>%
    as.data.frame() %>%
    mutate(Trait = trait_label, model_var = model_var)

  list(anova = anova_df, pairs = pair_df)
}

model.results <- pmap(
  list(trait.model_map_df$model_var, trait.model_map_df$Trait),
  run_trait_lme
)

trait.anova_df <- map_dfr(model.results, "anova")
trait.pairs_df <- map_dfr(model.results, "pairs")

pval.real_df <- trait.pairs_df %>%
  mutate(
    Trait = factor(Trait, levels = levels(MAG.traits.plot_df$Trait)),
    Management = factor(Management, levels = c("Conventional", "Conservation")),
    p_value = p.value,
    p_label = case_when(
      p_value < 0.001 ~ "P < 0.001",
      TRUE ~ paste0("P = ", signif(p_value, 2))
    )
  ) %>%
  select(Trait, Management, p_value, p_label, estimate, SE, df, t.ratio)

plot_one_trait_density <- function(trait_name, show_strip = FALSE) {
  dat_df <- MAG.traits.plot_df %>% filter(Trait == trait_name)
  mean_df <- mean.line_df %>% filter(Trait == trait_name)

  p <- ggplot(dat_df, aes(Value, fill = Warm, color = Warm)) +
    geom_rect(
      data = tibble(Management = factor("Conventional", levels = c("Conventional", "Conservation"))),
      aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
      fill = bg.cols["Conventional"],
      alpha = 0.18,
      inherit.aes = FALSE
    ) +
    geom_rect(
      data = tibble(Management = factor("Conservation", levels = c("Conventional", "Conservation"))),
      aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
      fill = bg.cols["Conservation"],
      alpha = 0.18,
      inherit.aes = FALSE
    ) +
    geom_density(aes(weight = abundance), alpha = 0.10, linewidth = 0.8, adjust = 1.15) +
    geom_vline(data = mean_df, aes(xintercept = mean_value, color = Warm), linetype = "dashed", linewidth = 0.75) +
    facet_grid(. ~ Management, scales = "free_x") +
    scale_fill_manual(values = cols1) +
    scale_color_manual(values = cols1) +
    scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) +
    labs(x = NULL, y = trait_name, fill = NULL, color = NULL) +
    theme_cowplot() +
    theme(
      axis.text = element_text(size = 9),
      axis.title.y = element_text(size = 11, face = "bold"),
      legend.position = "none",
      panel.spacing.x = unit(0.45, "lines")
    )

  if (show_strip) {
    p + theme(
      strip.background = element_rect(fill = "grey88", color = NA),
      strip.text = element_text(size = 11, face = "bold")
    )
  } else {
    p + theme(strip.background = element_blank(), strip.text = element_blank())
  }
}

plot_one_p <- function(trait_name, show_x = FALSE) {
  p_min <- 0.001
  p_max <- 1
  x_lab <- if (show_x) "p value" else NULL

  bg_p_df <- tibble(
    Management = factor(c("Conservation", "Conventional"), levels = c("Conservation", "Conventional")),
    ymin = c(0.5, 1.5),
    ymax = c(1.5, 2.5),
    fill_color = c(bg.cols["Conservation"], bg.cols["Conventional"])
  )

  p <- pval.real_df %>%
    filter(Trait == trait_name) %>%
    mutate(
      Management = factor(Management, levels = c("Conservation", "Conventional")),
      p_plot = pmin(pmax(p_value, p_min), p_max),
      p_label = case_when(
        p_value < 0.001 ~ "p < 0.001",
        TRUE ~ paste0("p = ", signif(p_value, 2))
      ),
      label_x = case_when(
        p_plot <= 0.003 ~ p_plot * 2.2,
        p_plot >= 0.5 ~ p_plot / 2.0,
        TRUE ~ p_plot * 1.35
      ),
      label_hjust = case_when(
        p_plot >= 0.5 ~ 1,
        TRUE ~ 0
      )
    ) %>%
    ggplot(aes(p_plot, Management, shape = Management)) +
    geom_rect(
      data = bg_p_df,
      aes(xmin = p_min, xmax = p_max, ymin = ymin, ymax = ymax),
      fill = bg_p_df$fill_color,
      alpha = 0.18,
      inherit.aes = FALSE
    ) +
    geom_vline(xintercept = 0.05, linetype = "dashed", color = "grey50", linewidth = 0.5) +
    geom_point(size = 3.3, color = "black", fill = "red4", stroke = 0.9) +
    geom_text(aes(x = label_x, label = p_label, hjust = label_hjust), size = 4.3, show.legend = FALSE) +
    scale_shape_manual(values = c("Conventional" = 21, "Conservation" = 23)) +
    scale_x_log10(
      limits = c(p_min, p_max),
      breaks = c(0.001, 0.01, 0.05, 0.10, 1),
      labels = c("0.001", "0.01", "0.05", "0.10", "1"),
      guide = guide_axis(check.overlap = TRUE)
    ) +
    labs(x = x_lab, y = NULL) +
    coord_cartesian(clip = "off") +
    theme_cowplot() +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid = element_blank(),
      legend.position = "none",
      plot.margin = margin(5.5, 45, 5.5, 5.5)
    )

  if (show_x) {
    p + theme(
      axis.text.x = element_text(size = 9),
      axis.ticks.x = element_line(),
      axis.title.x = element_text(size = 11)
    )
  } else {
    p + theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank()
    )
  }
}

# Figure assembly 
trait_levels <- levels(MAG.traits.plot_df$Trait)

density_plots <- purrr::imap(trait_levels, ~ plot_one_trait_density(.x, show_strip = .y == 1))
p_plots <- purrr::imap(trait_levels, ~ plot_one_p(.x, show_x = .y == length(trait_levels)))

row_plots <- purrr::map2(
  density_plots,
  p_plots,
  ~ .x + .y + patchwork::plot_layout(widths = c(5, 1.35))
)

life_strategy_density_plot <- patchwork::wrap_plots(row_plots, ncol = 1)
life_strategy_density_plot

# F. MAGs function 
mags.df <- read.xlsx("data_for_RAE.xlsx", 5) %>%
  dplyr::select(Anno, System, Type, Function)

cazy.type_levels <- c(
  "CBM42", "CBM48",
  "GH5", "GH31", "GH29", "GH32",
  "GH13", "GH3", "GH51", "GH54", "GH15", "GH9",
  "GH20", "GH43"
)

NW.cazy_df <- mags.df %>%
  dplyr::filter(Anno == "CAzyme", System == "NW") %>%
  dplyr::distinct(Type, Anno, .keep_all = TRUE) %>%
  dplyr::select(Anno, System, Type)

TW.cazy_df <- mags.df %>%
  dplyr::filter(Anno == "CAzyme", System == "TW") %>%
  dplyr::distinct(Type, Anno, .keep_all = TRUE) %>%
  dplyr::select(Anno, System, Type)

cazy.all_df <- dplyr::bind_rows(TW.cazy_df, NW.cazy_df)

cazy.matrix_df <- cazy.all_df %>%
  dplyr::mutate(Present = 1) %>%
  dplyr::distinct(System, Type, .keep_all = TRUE) %>%
  tidyr::pivot_wider(names_from = System, values_from = Present, values_fill = 0)

cazy.long_df <- cazy.matrix_df %>%
  dplyr::select(-Anno) %>%
  tidyr::pivot_longer(cols = -Type, names_to = "System", values_to = "Present") %>%
  dplyr::mutate(Type = factor(Type, levels = cazy.type_levels))

Cazyme.plot <- ggplot(cazy.long_df, aes(y = System, x = Type, fill = factor(Present))) +
  geom_tile(color = "white") +
  scale_fill_manual(
    values = c("0" = "grey90", "1" = "#d4675a"),
    name = "Presence",
    labels = c("Absent", "Present")
  ) +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    axis.text = element_text(color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )
Cazyme.plot

functi.df <- tibble(
  site = c("NW", "TW"),
  FDis = c(0.000000, 2.831448)
) # based on Functional Dispersion (FDis) from fundiversity package

FDis.plot <- ggplot(functi.df, aes(site, FDis)) +
  geom_col(width = 0.6) +
  labs(x = NULL) +
  theme_minimal()
FDis.plot

cazy_plot <- (Cazyme.plot | FDis.plot) + patchwork::plot_layout(widths = c(4, 1))
cazy_plot

kegg.type_levels <- c(
  "4.1.2.13", "3.1.3.11", "2.7.2.3", "1.2.1.12", "5.3.1.6",
  "6.3.4.3", "6.4.1.2", "6.4.1.3", "5.1.99.1", "5.4.99.2",
  "4.2.1.17", "2.3.1.9", "1.3.5.1", "1.3.5.4",
  "4.2.1.2", "1.1.1.35", "1.1.1.37", "2.7.9.2",
  "GT4", "GT2", "GT35", "GT20", "GT28", "GT39", "GT26", "GT51"
)

NW.kegg_df <- mags.df %>%
  dplyr::filter(Anno == "KEGG", System == "NW") %>%
  dplyr::distinct(Type, Anno, .keep_all = TRUE) %>%
  dplyr::select(Anno, System, Type)

TW.kegg_df <- mags.df %>%
  dplyr::filter(Anno == "KEGG", System == "TW") %>%
  dplyr::distinct(Type, Anno, .keep_all = TRUE) %>%
  dplyr::select(Anno, System, Type)

kegg.all_df <- dplyr::bind_rows(TW.kegg_df, NW.kegg_df)

kegg.matrix_df <- kegg.all_df %>%
  dplyr::mutate(Present = 1) %>%
  dplyr::distinct(System, Type, .keep_all = TRUE) %>%
  tidyr::pivot_wider(names_from = System, values_from = Present, values_fill = 0)

kegg.long_df <- kegg.matrix_df %>%
  dplyr::select(-Anno) %>%
  tidyr::pivot_longer(cols = -Type, names_to = "System", values_to = "Present") %>%
  dplyr::mutate(Type = factor(Type, levels = kegg.type_levels))

kegg.plot <- ggplot(kegg.long_df, aes(y = System, x = Type, fill = factor(Present))) +
  geom_tile(color = "white") +
  scale_fill_manual(
    values = c("0" = "grey90", "1" = "#8d792e"),
    name = "Presence",
    labels = c("Absent", "Present")
  ) +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    axis.text = element_text(color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )
kegg.plot

MAG.function_plot <- cazy_plot / kegg.plot
MAG.function_plot

#### Fig5 SEM analysis ####
df <- read.xlsx("data_for_RAE.xlsx", 6) %>%
  mutate(
    Temperature_scale = scale(Temperature),
    Substrate_quality = -1 * Substrate_quality,
    Substrate_available_scale = Substrate_available^2
  )

model_CUE <- psem(
  lm(CUE ~ Temperature_scale + RAE + Substrate_available + A_C_ratio + RAE, data = df),
  lm(RAE ~ Microbial_life + Substrate_quality + A_C_ratio, data = df),
  lm(A_C_ratio ~ Substrate_quality + Substrate_available, data = df),
  lm(Microbial_life ~ Till_m + Substrate_available + Substrate_quality, data = df),
  lm(Substrate_available ~ Till_m + Temperature_scale, data = df),
  lm(Substrate_quality ~ Till_m + Temperature_scale + Till_m:Temperature_scale, data = df)
)

model_CUE_summary <- summary(model_CUE)
model_CUE_fisherC <- fisherC(model_CUE)
model_CUE_AIC <- AIC(model_CUE, AIC.type = "loglik")
model_CUE_coefs <- coefs(model_CUE)
model_CUE_path_plot <- plot(model_CUE)

model_CUE_boot <- bootEff(
  model_CUE,
  R = 9999,
  catch.err = FALSE,
  parallel = "multicore",
  seed = 23
)

model_CUE_boot_eff <- semEff(model_CUE_boot)
model_CUE_boot_summary <- summary(model_CUE_boot_eff)

funGetEff <- function(Tp, Ef) {
  data.frame(effType = Tp, Effect = Ef, row.names = names(Ef)) %>%
    tibble::rownames_to_column(var = "Predictor")
}

get_effect_df <- function(response, filler_predictor = NULL, predictor_levels) {
  effRes <- semEff::getEff(model_CUE_boot_eff, response)
  dEf <- effRes[[response]]$Direct
  indEf <- effRes[[response]]$Indirect
  totalEf <- effRes[[response]]$Total

  combinedDf <- rbind(
    funGetEff("Direct", dEf),
    funGetEff("Indirect", indEf),
    funGetEff("Total", totalEf)
  ) %>%
    dplyr::filter(Predictor != "(Intercept)")

  if (!is.null(filler_predictor)) {
    combinedDf <- combinedDf %>%
      dplyr::bind_rows(
        tibble(
          Predictor = filler_predictor,
          effType = c("Direct", "Indirect", "Total"),
          Effect = 0
        )
      )
  }

  combinedDf %>%
    dplyr::mutate(Predictor = factor(Predictor, levels = predictor_levels))
}

plot_effect_df <- function(effect_df, show_legend = FALSE, show_x_text = TRUE) {
  bar_df <- effect_df %>%
    dplyr::filter(effType %in% c("Direct", "Indirect"))

  point_df <- effect_df %>%
    dplyr::filter(effType == "Total")

  p <- ggplot() +
    geom_col(data = bar_df, aes(x = Predictor, y = Effect, fill = effType), width = 0.7) +
    geom_point(data = point_df, aes(x = Predictor, y = Effect), size = 5, shape = 21, fill = "white", stroke = 1) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
    scale_fill_manual(values = c(Direct = "#4C72B0", Indirect = "#DD8452"), name = "Effect type") +
    scale_y_continuous(limits = c(-0.55, 0.55)) +
    labs(x = NULL, y = "Standardized effect size") +
    theme_cowplot(font_size = 20)

  if (show_legend) {
    p <- p + theme(legend.position = c(0.1, 0.3))
  } else {
    p <- p + theme(legend.position = "none")
  }

  if (show_x_text) {
    p + theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))
  } else {
    p + theme(axis.text.x = element_blank())
  }
}

CUE.effect_df <- get_effect_df(
  response = "CUE",
  filler_predictor = "Microbial.life.R1",
  predictor_levels = c(
    "Till.m", "Temperature.scale", "Till.m:Temperature.scale",
    "Substrate.available", "Substrate.quality", "Microbial.life.R1",
    "A.C.ratio", "RAE"
  )
)

CUE_plot <- plot_effect_df(CUE.effect_df, show_legend = FALSE, show_x_text = TRUE)
CUE_plot

RAE.effect_df <- get_effect_df(
  response = "RAE",
  filler_predictor = "Perference",
  predictor_levels = c(
    "Till.m", "Temperature.scale", "Till.m:Temperature.scale",
    "Substrate.available", "Substrate.quality", "Microbial.life.R1",
    "A.C.ratio", "Perference"
  )
)

RAE_plot <- plot_effect_df(RAE.effect_df, show_legend = TRUE, show_x_text = FALSE)
RAE_plot

SEM_effect_plot <- RAE_plot / CUE_plot
SEM_effect_plot
