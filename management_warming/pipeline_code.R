rename <- dplyr::rename
cols1 <- c("royalblue4", "red4")
cols2 <- c("royalblue4", "red4", "royalblue4", "red4")
cols_t <- c("#404040", "#9d5d21")
bg.cols <- c("Conventional" = "grey", "Conservation" = "#a76825")

my_theme <- theme_cowplot() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 14),
    strip.text = element_text(size = 14)
  )

background_data <- data.frame(
  Tillage = c("Conventional", "Conservation"),
  xmin = c(0.5, 1.5),
  xmax = c(1.5, 2.5),
  fill_color = c("grey", "#a76825")
)

background_data_1 <- data.frame(
  xmin = c(0, 3.5),
  xmax = c(3.5, 7),
  fill_color = c("grey", "#a76825")
)

# Calculate Hedges' g and confidence intervals for the tillage comparison.
calculate_hedges_g_tillage <- function(data) {
  TN_values <- data$Value[data$Treat == "TN"]
  TW_values <- data$Value[data$Treat == "TW"]
  
  result <- esc_mean_sd(
    grp2m = mean(TN_values), grp1sd = sd(TN_values), grp1n = length(TN_values),
    grp1m = mean(TW_values), grp2sd = sd(TW_values), grp2n = length(TW_values),
    es.type = "g"
  )
  
  return(c(Hedges_g = result$es, CI_Lower = result$ci.lo, CI_Upper = result$ci.hi))
}

# Calculate Hedges' g and confidence intervals for the no-tillage comparison.
calculate_hedges_g_notillage <- function(data) {
  TN_values <- data$Value[data$Treat == "NN"]
  TW_values <- data$Value[data$Treat == "NW"]
  
  result <- esc_mean_sd(
    grp2m = mean(TN_values), grp1sd = sd(TN_values), grp1n = length(TN_values),
    grp1m = mean(TW_values), grp2sd = sd(TW_values), grp2n = length(TW_values),
    es.type = "g"
  )
  
  return(c(Hedges_g = result$es, CI_Lower = result$ci.lo, CI_Upper = result$ci.hi))
}


# Correct OTU abundances using rrnDB copy numbers across taxonomic levels.
rco <- function(otu, classifer, rrnDB) {
  
  hang1 = length(rownames(otu))
  a= c(6,5,4,3)
  b= c(7,6,5,4)
  levels= c("genus","family","order","class")
  whole.res = c()
  
  for (q in 1:4){
    spe = classifer[(classifer[,a[q]] != "Unclassified") &  (classifer[,b[q]] == "Unclassified"),]
    rspe = rrnDB[ rrnDB[,2]==levels[q],c(3,9)]
    
    name.spe = rownames(spe)
    name.otu = rownames(otu)
    name.match = match(name.spe,name.otu)
    match.otu = otu[name.match,]
    otu.spe = cbind(spe[,a[q]],match.otu)
    
    # Assign -1 when a taxon is missing from rrnDB at the current level.
    whole = data.frame(matrix(data=NA,
                              nrow=length(rownames(otu.spe)),
                              ncol=length(colnames(otu.spe))),stringsAsFactors=FALSE)
    
    for (i in 1:length(rownames(otu.spe))){
      mat = match(as.character(otu.spe[i,1]),as.character(rspe[,1]))
      if(!is.na(mat)){
        each = cbind(rspe[mat,2],otu.spe[i,-1])
      } else{
        each = cbind(-1,otu.spe[i,-1])
      }
      whole[i,] = each
    }
    spe.res = cbind(taxa=otu.spe[,1],level=rep(levels[q],length(rownames(otu.spe))),whole)
    rownames(spe.res) = rownames(otu.spe)
    
    whole.res = rbind(whole.res,spe.res)

    # Remove processed OTUs so later levels only handle unresolved entries.
    diffotu = setdiff(rownames(otu),rownames(otu.spe))
    otu = otu[diffotu,]
  }
  whole.res = whole.res[complete.cases(whole.res),]
  colnames(whole.res) = c("taxa","level","CopyNumber",colnames(otu))
  
  # Check whether the processed and unresolved OTUs still match the input size.
  hang2 = length(rownames(whole.res))+length(rownames(otu))
  
  if (hang1==hang2){
    print("Well done!")}
  
  # Divide OTU abundances by copy number after dropping unresolved taxa.
  simply.res = whole.res[whole.res[,3]!= -1,]
  correct = simply.res[,-c(1:3)] / simply.res[,3]
  correct.table = as.data.frame(cbind(simply.res[,1:2],correct))
  
  list(whole.res=whole.res,correct.table=correct.table)
}


# Compute common alpha-diversity indices, with optional phylogenetic diversity.
alpha_diversity <- function(x, tree = NULL) {
  observed_species <- estimateR(x)[1, ]
  Chao1 <- estimateR(x)[2, ]
  ACE <- estimateR(x)[4, ]
  Shannon <- diversity(x, index = 'shannon', base = 2)
  Simpson <- diversity(x, index = 'simpson')
  goods_Coverage <- 1 - rowSums(x == 1) / rowSums(x)
  
  Shannon <- sprintf("%0.4f", Shannon)
  Simpson <- sprintf("%0.4f", Simpson)
  goods_Coverage <- sprintf("%0.4f", goods_Coverage)
  
  result <- data.frame(observed_species, ACE, Chao1, Shannon, Simpson, goods_Coverage)
  
  if (!is.null(tree)) {
    PD_whole_tree <- pd(x, tree, include.root = FALSE)[, 1]
    result <- cbind(result, PD_whole_tree = PD_whole_tree)
  }
  
  return(result)
}


# Build a directed graph from coefficient relationships.
build_graph <- function(coefs_df) {
  graph <- list()
  for (i in 1:nrow(coefs_df)) {
    from <- coefs_df$Predictor[i]
    to <- coefs_df$Response[i]
    effect <- coefs_df$effect[i]
    if (!is.null(graph[[from]])) {
      graph[[from]] <- rbind(graph[[from]], data.frame(to = to, effect = effect, stringsAsFactors = FALSE))
    } else {
      graph[[from]] <- data.frame(to = to, effect = effect, stringsAsFactors = FALSE)
    }
  }
  return(graph)
}

# Recursively enumerate all paths between two nodes in the graph.
find_all_paths <- function(graph, start, end, visited = character()) {
  if (start == end) {
    return(list(c(end)))
  }
  if (!start %in% names(graph)) {
    return(list())
  }
  visited <- c(visited, start)
  paths <- list()
  for (i in 1:nrow(graph[[start]])) {
    next_node <- graph[[start]]$to[i]
    if (!(next_node %in% visited)) {
      sub_paths <- find_all_paths(graph, next_node, end, visited)
      for (sp in sub_paths) {
        paths <- c(paths, list(c(start, sp)))
      }
    }
  }
  return(paths)
}

# Multiply edge effects along a single graph path.
path_effect <- function(path, graph) {
  eff <- 1
  for (i in 1:(length(path)-1)) {
    from <- path[i]
    to <- path[i+1]
    edge <- graph[[from]]
    eff <- eff * edge$effect[edge$to == to]
  }
  return(eff)
}

# Summarize direct, indirect, and total effects among all variable pairs.
compute_all_effects <- function(graph, variables) {
  results <- data.frame(
    predictor = character(),
    outcome = character(),
    direct_effect = numeric(),
    indirect_effect = numeric(),
    total_effect = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (pred in variables) {
    for (outc in variables) {
      if (pred != outc) {
        paths <- find_all_paths(graph, pred, outc)
        if (length(paths) > 0) {
          direct_effect <- 0
          indirect_effect <- 0
          for (p in paths) {
            eff <- path_effect(p, graph)
            if (length(p) == 2) {
              direct_effect <- direct_effect + eff
            } else {
              indirect_effect <- indirect_effect + eff
            }
          }
          total_effect <- direct_effect + indirect_effect
          results <- rbind(results, data.frame(
            predictor = pred,
            outcome = outc,
            direct_effect = direct_effect,
            indirect_effect = indirect_effect,
            total_effect = total_effect,
            stringsAsFactors = FALSE
          ))
        }
      }
    }
  }
  return(results)
}

# Fit an LMM by treatment and return compact mean ± SE summaries with letters.
get_lmm_table <- function(dat){
  m <- lmer(value ~ treatment + (1 | block), data = dat)
  emm <- emmeans(m, ~ treatment)
  letters <- cld(emm, Letters = letters, adjust = "tukey", reversed = TRUE) %>%
    as.data.frame() %>%
    transmute(treatment, letter = str_remove_all(.group, " "))
  
  dat %>%
    group_by(treatment) %>%
    summarise(mean = mean(value, na.rm = TRUE), se = sd(value, na.rm = TRUE) / sqrt(n()), n = n(), .groups = "drop") %>%
    left_join(letters, by = "treatment") %>%
    mutate(result = sprintf("%.2f ± %.2f%s", mean, se, letter)) %>%
    dplyr::select(treatment, result)
}

# Run gRodon growth prediction for MAG gene sets with basic QC checks.
run_grodon_mags <- function(ffn_file){
  ID <- basename(ffn_file) %>% str_remove("\\.ffn$")
  genes <- readDNAStringSet(ffn_file)
  
  highly_expressed <- grepl("ribosomal protein", names(genes), ignore.case = TRUE) &
    !grepl("methyl|hydroxy", names(genes), ignore.case = TRUE)
  
  CDS_n <- length(genes)
  HEG_n <- sum(highly_expressed)
  
  if(CDS_n < 100) return(tibble(ID, status = "too_few_CDS", CDS = CDS_n, HEG = HEG_n))
  if(HEG_n < 1) return(tibble(ID, status = "no_ribosomal_protein", CDS = CDS_n, HEG = HEG_n))
  
  pred <- tryCatch(
    predictGrowth(genes, highly_expressed, mode = "partial"),
    error = function(e) e
  )
  
  if(inherits(pred, "error")){
    return(tibble(ID, status = paste0("gRodon_error: ", pred$message), CDS = CDS_n, HEG = HEG_n))
  }
  
  tibble(ID, status = "ok", CDS = CDS_n, HEG = HEG_n) %>%
    bind_cols(as_tibble_row(as.list(unlist(pred))))
}

# Run gRodon growth prediction for contig gene sets with read and model error handling.
run_grodon_contigs <- function(ffn_file){
  ID <- basename(ffn_file) %>% str_remove("\\.ffn$")
  
  genes <- tryCatch(
    readDNAStringSet(ffn_file),
    error = function(e) e
  )
  
  if(inherits(genes, "error")){
    return(tibble(ID = ID, status = paste0("read_error: ", genes$message),
                  CDS = NA_integer_, HEG = NA_integer_))
  }
  
  highly_expressed <- grepl("ribosomal protein", names(genes), ignore.case = TRUE)
  
  CDS_n <- length(genes)
  HEG_n <- sum(highly_expressed)
  
  pred <- tryCatch(
    predictGrowth(genes, highly_expressed, mode = "metagenome_v2"),
    error = function(e) e
  )
  
  if(inherits(pred, "error")){
    return(tibble(ID = ID, status = paste0("gRodon_error: ", pred$message),
                  CDS = CDS_n, HEG = HEG_n))
  }
  
  tibble(ID = ID, status = "ok", CDS = CDS_n, HEG = HEG_n) %>%
    bind_cols(as_tibble_row(as.list(unlist(pred))))
}

# Plot weighted trait-density distributions by warming and tillage treatment.
plot_one_trait_density <- function(trait_name, show_strip = FALSE){
  dat_df <- MAG.traits.plot_df %>% filter(Trait == trait_name)
  mean_df <- mean.line_df %>% filter(Trait == trait_name)
  
  p <- ggplot(dat_df, aes(Value, fill = Warm, color = Warm)) +
    geom_rect(data = tibble(Tillage = factor("Conventional", levels = c("Conventional", "Conservation"))),
              aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
              fill = bg.cols["Conventional"], alpha = 0.18, inherit.aes = FALSE) +
    geom_rect(data = tibble(Tillage = factor("Conservation", levels = c("Conventional", "Conservation"))),
              aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
              fill = bg.cols["Conservation"], alpha = 0.18, inherit.aes = FALSE) +
    geom_density(aes(weight = abundance), alpha = 0.10, linewidth = 0.8, adjust = 1.15) +
    geom_vline(data = mean_df, aes(xintercept = mean_value, color = Warm), linetype = "dashed", linewidth = 0.75) +
    facet_grid(. ~ Tillage, scales = "free_x") +
    scale_fill_manual(values = cols1) +
    scale_color_manual(values = cols1) +
    scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) +
    labs(x = NULL, y = trait_name, fill = NULL, color = NULL) +
    theme_cowplot() +
    theme(axis.text = element_text(size = 9),
          axis.title.y = element_text(size = 11, face = "bold"),
          legend.position = "none",
          panel.spacing.x = unit(0.45, "lines"))
  
  if(show_strip){
    p + theme(strip.background = element_rect(fill = "grey88", color = NA),
              strip.text = element_text(size = 11, face = "bold"))
  } else {
    p + theme(strip.background = element_blank(), strip.text = element_blank())
  }
}

# Plot treatment-specific p-values on a log-scaled horizontal layout.
plot_one_p <- function(trait_name, show_x = FALSE){
  p_min <- 0.001
  p_max <- 1
  x_lab <- if(show_x) "p value" else NULL
  
  bg_p_df <- tibble(
    Tillage = factor(c("Conservation", "Conventional"),
                     levels = c("Conservation", "Conventional")),
    ymin = c(0.5, 1.5),
    ymax = c(1.5, 2.5),
    fill_color = c(bg.cols["Conservation"], bg.cols["Conventional"])
  )
  
  p <- pval.real_df %>%
    filter(Trait == trait_name) %>%
    mutate(
      Tillage = factor(Tillage, levels = c("Conservation", "Conventional")),
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
    ggplot(aes(p_plot, Tillage, shape = Tillage)) +
    geom_rect(
      data = bg_p_df,
      aes(xmin = p_min, xmax = p_max, ymin = ymin, ymax = ymax),
      fill = bg_p_df$fill_color,
      alpha = 0.18,
      inherit.aes = FALSE
    ) +
    geom_vline(
      xintercept = 0.05,
      linetype = "dashed",
      color = "grey50",
      linewidth = 0.5
    ) +
    geom_point(size = 3.3, color = "black", fill = "red4", stroke = 0.9) +
    geom_text(
      aes(x = label_x, label = p_label, hjust = label_hjust),
      size = 4.3,
      show.legend = FALSE
    ) +
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
  
  if(show_x){
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



