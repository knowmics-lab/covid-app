read.mutation.data <- function(input.file) {
  cache.path <- paste0(input.file, ".rds")
  if (file.exists(cache.path)) {
    return(readRDS(cache.path))
  } else {
    mutation.data <- fread(input.file, header = T, check.names = F, data.table = F)
    mutation.data <- mutation.data[, -ncol(mutation.data)]
    mutation.data$month <- sub("_21", " 2021", mutation.data$month)
    mutation.data$month <- sub("_22", " 2022", mutation.data$month)
    Sys.setlocale("LC_TIME", "C")
    dates <- as.Date(paste0("1", mutation.data$month), format = "%d%b %Y")
    mutation.data <- mutation.data[order(dates), ]
    mutation.data$month <- factor(mutation.data$month, levels = unique(mutation.data$month))
    mutation.data$clade <- factor(mutation.data$clade)
    saveRDS(mutation.data, cache.path)
    return(mutation.data)
  }
}

read_clades_correlations <- function(input_file) {
  cache_path <- paste0(input_file, ".rds")
  if (file.exists(cache_path)) {
    return(readRDS(cache_path))
  } else {
    clade_correlations <- fread(input_file,
      header = TRUE, check.names = FALSE, data.table = FALSE
    )
    saveRDS(clade_correlations, cache_path)
    return(clade_correlations)
  }
}

read_cached <- function(fn, cache_file) {
  if (file.exists(cache_file)) {
    return(readRDS(cache_file))
  } else {
    data <- fn()
    saveRDS(data, cache_file)
    return(data)
  }
}

compute.temporal.rates <- function(mutation.data) {
  temporal.rates <- mutation.data %>%
    group_by(month) %>%
    summarise(across(all_of(colnames(mutation.data)[-(1:3)]),
      list(freq = ~ sum(.x), samples = ~ length(.x)),
      .names = "{.fn}_{.col}"
    ))
  return(temporal.rates)
}

compute.temporal.mutation.rates <- function(temporal.rates, mutation.set) {
  temporal.mutation.set.rates <- data.frame(
    mutation = character(), month = character(), frequency = numeric(),
    samples = numeric(), rate = numeric()
  )
  for (mutation in mutation.set)
  {
    temporal.mutation.rates <- data.frame(
      mutation = rep(mutation, nrow(temporal.rates)),
      month = temporal.rates$month,
      frequency = unname(unlist(temporal.rates[, paste0("freq_", mutation)])),
      samples = unname(unlist(temporal.rates[, paste0("samples_", mutation)]))
    )
    temporal.mutation.rates$rate <- temporal.mutation.rates$frequency / temporal.mutation.rates$samples
    temporal.mutation.rates$rate <- as.numeric(percent(temporal.mutation.rates$rate, accuracy = 0.01, suffix = ""))
    temporal.mutation.set.rates <- rbind(temporal.mutation.set.rates, temporal.mutation.rates)
  }
  temporal.mutation.set.rates$mutation <- factor(temporal.mutation.set.rates$mutation)
  return(temporal.mutation.set.rates)
}

make.temporal.plot <- function(temporal.mutation.set.rates, mutation.set) {
  temporal.plot <- ggplot(temporal.mutation.set.rates, aes(x = month, y = rate, color = mutation, group = mutation)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    scale_y_continuous(breaks = seq(0, 100, 10)) +
    scale_color_discrete(name = "Mutations") +
    labs(x = "Month", y = "Mutation rate (%)") +
    theme_bw() +
    theme(
      axis.text.x = element_text(size = 15, angle = 90, color = "black"),
      axis.text.y = element_text(size = 15, color = "black"),
      axis.title.x = element_text(size = 20, face = "bold", margin = margin(t = 20)),
      axis.title.y = element_text(size = 20, face = "bold", margin = margin(r = 20)),
      legend.text = element_text(size = 15),
      legend.title = element_text(size = 15),
      axis.line.x = element_line(color = "black", linewidth = 0.5),
      axis.line.y = element_line(color = "black", linewidth = 0.5),
      axis.ticks.length = unit(.25, "cm"),
      legend.text.align = 0
    )
  return(temporal.plot)
}

compute.clade.rates <- function(mutation.data) {
  clade.rates <- mutation.data %>%
    group_by(clade) %>%
    summarise(across(all_of(colnames(mutation.data)[-(1:3)]),
      list(freq = ~ sum(.x), samples = ~ length(.x)),
      .names = "{.fn}_{.col}"
    ))
  return(clade.rates)
}

compute.pie.mutation.rates <- function(clade.rates, mutation.set) {
  pie.data <- data.frame(
    mutation = character(), clade = character(), cladeFreq = numeric(),
    cladeSamples = numeric(), cladeRate = numeric(), absRate = numeric()
  )
  for (mutation in mutation.set)
  {
    clade.pie.data <- data.frame(
      mutation = rep(mutation, nrow(clade.rates)), clade = clade.rates$clade,
      cladeFreq = unname(unlist(clade.rates[, paste0("freq_", mutation)])),
      cladeSamples = unname(unlist(clade.rates[, paste0("samples_", mutation)]))
    )
    clade.pie.data <- clade.pie.data[clade.pie.data$cladeFreq != 0, ]
    clade.pie.data$cladeRate <- clade.pie.data$cladeFreq / clade.pie.data$cladeSamples
    clade.pie.data$cladeRate <- percent(clade.pie.data$cladeRate, accuracy = 0.01, suffix = "%")
    clade.pie.data$absRate <- clade.pie.data$cladeFreq / sum(clade.pie.data$cladeFreq)
    clade.pie.data$absRate <- percent(clade.pie.data$absRate, accuracy = 0.01, suffix = "%")
    pie.data <- rbind(pie.data, clade.pie.data)
  }
  return(pie.data)
}

make.clade.plot <- function(pie.data, mutation.set) {
  pie.plot.list <- list()
  for (mutation in mutation.set)
  {
    mutation.pie.data <- pie.data[pie.data$mutation == mutation, ] %>%
      mutate(
        csum = rev(cumsum(rev(cladeFreq))),
        pos = cladeFreq / 2 + lead(csum, 1),
        pos = if_else(is.na(pos), cladeFreq / 2, pos)
      )

    mutation.pie.plot <- ggplot(mutation.pie.data, aes(x = "", y = cladeFreq, fill = clade)) +
      geom_bar(stat = "identity", width = 1, color = "black") +
      coord_polar("y", start = 0) +
      scale_fill_brewer(palette = "Spectral") +
      geom_label_repel(
        data = mutation.pie.data,
        aes(y = pos, label = paste0("AR: ", absRate, ", CR: ", cladeRate)),
        size = 3.5, nudge_x = 1, show.legend = FALSE
      ) +
      theme_void() +
      theme(
        legend.text = element_text(size = 15), legend.title = element_blank(),
        legend.position = "right", legend.spacing.y = unit(0.2, "cm")
      ) +
      guides(fill = guide_legend(byrow = F)) +
      ggtitle(label = paste0("\n", mutation)) +
      theme(plot.title = element_text(size = 17, hjust = 0.5, face = "bold"), plot.margin = unit(c(0, 0, 0, 0), "cm"))
    # mutation.pie.plot
    pie.plot.list[[mutation]] <- mutation.pie.plot
  }
  if (length(pie.plot.list) == 4) {
    nrow <- 2
    ncol <- 2
  } else {
    nrow <- ceiling(length(pie.plot.list) / 2)
    ncol <- min(length(pie.plot.list), 2)
  }
  final.plot <- ggarrange(plotlist = pie.plot.list, nrow = nrow, ncol = ncol, legend = "right")
  final.plot <- annotate_figure(final.plot, top = text_grob("AR: Absolute Rate, CR: Clade Rate", face = "bold", size = 13))
  # final.plot <- subplot(pie.plot.list,nrows=nrow,margin=0.05)
  final.plot
  return(final.plot)
}

compute.temporal.pair.rates <- function(mutation.data, mutation.source, mutation.dest, corr.plot.opt) {
  if ("Use clades" %in% corr.plot.opt) {
    temporal.pair.rates <- mutation.data[, c("clade", mutation.source, mutation.dest)]
    temporal.pair.rates <- temporal.pair.rates %>% group_by(clade)
  } else {
    temporal.pair.rates <- mutation.data[, c("month", mutation.source, mutation.dest)]
    temporal.pair.rates <- temporal.pair.rates %>% group_by(month)
  }
  temporal.pair.rates <- summarise(temporal.pair.rates, across(all_of(colnames(temporal.pair.rates)[-1]),
    ~ sum(.x) / length(.x),
    .names = "{.col}"
  ))
  temporal.pair.rates <- as.data.frame(temporal.pair.rates[temporal.pair.rates[, 2] != 0 | temporal.pair.rates[, 3] != 0, , drop = F])
  temporal.pair.rates[, 2] <- as.numeric(percent(temporal.pair.rates[, 2], accuracy = 0.01, suffix = ""))
  temporal.pair.rates[, 3] <- as.numeric(percent(temporal.pair.rates[, 3], accuracy = 0.01, suffix = ""))
  return(temporal.pair.rates)
}

make.correlation.plot <- function(temporal.pair.rates, corr.plot.opt) {
  mut.rate.plot <- ggplot(temporal.pair.rates, aes(x = temporal.pair.rates[, 2], y = temporal.pair.rates[, 3])) +
    geom_point(size = 2, position = position_jitter(h = 0.01, w = 0.01)) +
    # geom_smooth(formula = y ~ x,method="glm") +
    geom_text_repel(label = temporal.pair.rates[, 1], max.overlaps = 100, segment.color = "transparent", size = 5) +
    scale_x_continuous(breaks = seq(0, 100, 10), limits = c(-5, 105)) +
    scale_y_continuous(breaks = seq(0, 100, 10), limits = c(-5, 105)) +
    labs(x = paste0("Mutation rate ", names(temporal.pair.rates)[2], " (%)"), y = paste0("Mutation rate ", names(temporal.pair.rates[3]), " (%)")) +
    theme_bw() +
    theme(
      axis.text.x = element_text(size = 15, color = "black"),
      axis.text.y = element_text(size = 15, color = "black"),
      axis.title.x = element_text(size = 20, face = "bold", margin = margin(t = 20)),
      axis.title.y = element_text(size = 20, face = "bold", margin = margin(r = 20)),
      axis.line.x = element_line(color = "black", linewidth = 0.5),
      axis.line.y = element_line(color = "black", linewidth = 0.5),
      axis.ticks.length = unit(.25, "cm"),
      legend.text.align = 0
    )
  if ("Show regression line" %in% corr.plot.opt) {
    mut.rate.plot <- mut.rate.plot + geom_smooth(formula = y ~ x, method = "glm")
  }
  return(mut.rate.plot)
}



mutation.data <- read.mutation.data("data/MutationMatrix.txt")
clade.correlations <- read_clades_correlations("data/corrClades.txt")
temporal.rates <- read_cached(
  fn = function() (compute.temporal.rates(mutation.data)),
  cache_file = "data/temporalRates.rds"
)
clade.rates <- read_cached(
  fn = function() (compute.clade.rates(mutation.data)),
  cache_file = "data/cladeRates.rds"
)
