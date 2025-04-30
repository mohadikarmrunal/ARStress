source(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), "init.R"))

###############################
#
#   Load raw files
#
###############################

load(file = "./02 RData/analysis.RData")
load(file = "./02 RData/pvals_study2.RData")

##############################
#Function to calculate significance stars from p-value

# Function to convert p-value to stars
get.significance.stars <- function(p_value) {
  if (p_value <= 0.001) {
    return("***")
  } else if (p_value <= 0.01) {
    return("**")
  } else if (p_value <= 0.05) {
    return("*")
  } else if (p_value <= 0.10) {
    return("+")  
  } else {
    return("n.s.")
  }
}

# Function to summarize the study data
summarize.study.data <- function(df, measure) {
  df %>%
    group_by(condition.label, resource) %>%
    summarize(
      mean = mean(.data[[measure]], na.rm = TRUE),
      std.err = sd(.data[[measure]], na.rm = TRUE) / sqrt(n()),
      sd = sd(.data[[measure]], na.rm = TRUE),
      n = n(),
      .groups = "drop"
    )
}

# Function to add hypothesis annotations for mean plot (arrows, texts)
add.mean.hypothesis.annotations <- function(df, sigstars, measure.label) {
  ymax <- max(df$mean, na.rm = TRUE) * 1.4
  ytext <- max(df$mean, na.rm = TRUE) * 1.5
  
  list(
    # DIGITAL H1a
    geom_segment(data = subset(df, condition.label == "DIGITAL"),
                 aes(x = 1, xend = 1.9, y = ymax, yend = ymax),
                 color = cbPalette[6], linewidth = 0.75),
    geom_segment(data = subset(df, condition.label == "DIGITAL"),
                 aes(x = 1, xend = 1, y = ymax, yend = mean[resource == "lack" & condition.label == "DIGITAL"]),
                 color = cbPalette[6], linewidth = 0.75, arrow = arrow_single),
    geom_segment(data = subset(df, condition.label == "DIGITAL"),
                 aes(x = 1.9, xend = 1.9, y = ymax, yend = mean[resource == "gain" & condition.label == "DIGITAL"]),
                 color = cbPalette[6], linewidth = 0.75, arrow = arrow_single),
    geom_text(data = subset(df, condition.label == "DIGITAL"),
              aes(x = 1.45, y = ytext, label = paste0("H1a ", sigstars[[paste0("h1a.", measure.label)]])),
              color = cbPalette[6], size = 5, family = "latex"),
    
    # DIGITAL H1b
    geom_segment(data = subset(df, condition.label == "DIGITAL"),
                 aes(x = 2.1, xend = 3, y = ymax, yend = ymax),
                 color = cbPalette[6], linewidth = 0.75),
    geom_segment(data = subset(df, condition.label == "DIGITAL"),
                 aes(x = 2.1, xend = 2.1, y = ymax, yend = mean[resource == "gain" & condition.label == "DIGITAL"]),
                 color = cbPalette[6], linewidth = 0.75, arrow = arrow_single),
    geom_segment(data = subset(df, condition.label == "DIGITAL"),
                 aes(x = 3, xend = 3, y = ymax, yend = mean[resource == "loss" & condition.label == "DIGITAL"]),
                 color = cbPalette[6], linewidth = 0.75, arrow = arrow_single),
    geom_text(data = subset(df, condition.label == "DIGITAL"),
              aes(x = 2.55, y = ytext, label = paste0("H1b ", sigstars[[paste0("h1b.", measure.label)]])),
              color = cbPalette[6], size = 5, family = "latex"),
    
    # TRADITIONAL H2a
    geom_segment(data = subset(df, condition.label == "TRADITIONAL"),
                 aes(x = 1, xend = 1.9, y = ymax, yend = ymax),
                 color = cbPalette[6], linewidth = 0.75),
    geom_segment(data = subset(df, condition.label == "TRADITIONAL"),
                 aes(x = 1, xend = 1, y = ymax, yend = mean[resource == "lack" & condition.label == "TRADITIONAL"]),
                 color = cbPalette[6], linewidth = 0.75, arrow = arrow_single),
    geom_segment(data = subset(df, condition.label == "TRADITIONAL"),
                 aes(x = 1.9, xend = 1.9, y = ymax, yend = mean[resource == "gain" & condition.label == "TRADITIONAL"]),
                 color = cbPalette[6], linewidth = 0.75, arrow = arrow_single),
    geom_text(data = subset(df, condition.label == "TRADITIONAL"),
              aes(x = 1.45, y = ytext, label = paste0("H2a ", sigstars[[paste0("h2a.", measure.label)]])),
              color = cbPalette[6], size = 5, family = "latex"),
    
    # TRADITIONAL H2b
    geom_segment(data = subset(df, condition.label == "TRADITIONAL"),
                 aes(x = 2.1, xend = 3, y = ymax, yend = ymax),
                 color = cbPalette[6], linewidth = 0.75),
    geom_segment(data = subset(df, condition.label == "TRADITIONAL"),
                 aes(x = 2.1, xend = 2.1, y = ymax, yend = mean[resource == "gain" & condition.label == "TRADITIONAL"]),
                 color = cbPalette[6], linewidth = 0.75, arrow = arrow_single),
    geom_segment(data = subset(df, condition.label == "TRADITIONAL"),
                 aes(x = 3, xend = 3, y = ymax, yend = mean[resource == "loss" & condition.label == "TRADITIONAL"]),
                 color = cbPalette[6], linewidth = 0.75, arrow = arrow_single),
    geom_text(data = subset(df, condition.label == "TRADITIONAL"),
              aes(x = 2.55, y = ytext, label = paste0("H2b ", sigstars[[paste0("h2b.", measure.label)]])),
              color = cbPalette[6], size = 5, family = "latex")
  )
}

# Function to create mean plot
create.mean.plot <- function(summary.df, sigstars, y.label, measure.label) {
  ggplot(summary.df, aes(x = resource, y = mean, fill = condition.label)) +
    geom_bar(stat = "identity", color = cbPalette[4], position = position_dodge(width = 0.9), alpha = 0.5) +
    scale_fill_manual(values = c("DIGITAL" = cbPalette[2], "TRADITIONAL" = cbPalette[2]), guide = "none") +
    geom_errorbar(aes(ymin = mean - std.err, ymax = mean + std.err),
                  width = 0.2, position = position_dodge(width = 0.9)) +
    labs(x = "", y = y.label) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          strip.background = element_blank(),
          strip.placement = "outside",
          strip.text.x = element_text(size = 20, face = "bold"),
          axis.line = element_line(colour = "black"),
          plot.title = element_text(size = 26, face = "bold", hjust = 0.5),
          text = element_text(size = 24, family = "latex")) +
    facet_grid(. ~ condition.label, switch = "x") +
    add.mean.hypothesis.annotations(summary.df, sigstars, measure.label)
}


################################
#
#
#   FIGURES
#
#
################################

# Define color and pattern palettes globally
cbPalette <- c( "#F9F9F6", "#D5D5D5","#BABABA", "#3B3B3B", "#636363", "#0072B2", "#005293", "#CC79A7")

# Define arrow globally
arrow_single <- arrow(length = unit(0.02, "npc"), type = "closed")
arrow_double <- arrow(length = unit(0.015, "npc"), type = "open", ends = "both", angle = 90)

################################
#
#   STUDY 2
#
################################

# Create readable labels
study2.final.df <- study2.final.df %>%
  mutate(condition.label = ifelse(condition == "Digital", "DIGITAL", "TRADITIONAL"),
         resource = factor(resource, levels = c("lack", "gain", "loss")))

# Apply to whole list
sigstars.study2 <- lapply(pvals.study2, get.significance.stars)

############################
#
# H1a: The gain of digital resources reduces stress; 
# H1b: the loss of digital resources increases stress;
# H2a: The gain of traditional resources increases stress; 
# H2b: The loss of traditional resources increases stress; 
#
############################
# Cortisol
study2.cortisol.mean.df <- summarize.study.data(study2.final.df, "AUCi.cortisol")  # Summarize mean and SE for plotting

#Plot mean
p.study2.cortisol.mean <- create.mean.plot(study2.cortisol.mean.df, sigstars.study2, expression(AUCi[cortisol]), "cortisol")
  
print(p.study2.cortisol.mean)

############################
# Perceived
study2.perceived.mean.df <- summarize.study.data(study2.final.df, "AUCi.perceived")  # Summarize mean and SE for plotting

#Plot mean
p.study2.perceived.mean <- create.mean.plot(study2.perceived.mean.df, sigstars.study2, expression(AUCi[perceived]), "perceived")

print(p.study2.perceived.mean)

############################
#
# H1c: The decrease in stress due to the gain of digital resources is smaller than the increase in stress due to the loss of digital resources. 
# H2c: The increase in stress due to the gain of traditional resources is smaller than the increase in stress due to the loss of traditional resources. 
# H3: The stress reduction associated with the gain of digital resources is smaller than the stress increase associated with the gain of traditional resources.
# H4: The stress increase associated with the loss of digital resources is larger than the stress increase associated with the loss of traditional resources. 
#
############################
# Cortisol
study2.cortisol.delta.df <- study2.final.df %>%
  filter(resource %in% c("lack", "gain", "loss")) %>%
  select(participant.id, condition.label, resource, AUCi.cortisol) %>%
  pivot_wider(names_from = resource, values_from = AUCi.cortisol) %>%
  mutate(
    delta.gain = lack - gain,
    delta.loss = loss - gain
  ) %>% 
  group_by(condition.label) %>%
  reframe(
    mean.delta.gain = mean(delta.gain, na.rm = TRUE),
    std.err.delta.gain = sd(delta.gain, na.rm = TRUE) / sqrt(n()),
    sd.delta.gain = sd(delta.gain, na.rm = TRUE),
    n.delta.gain = n(),
    
    mean.delta.loss = mean(delta.loss, na.rm = TRUE),
    std.err.delta.loss = sd(delta.loss, na.rm = TRUE) / sqrt(n()),
    sd.delta.loss = sd(delta.loss, na.rm = TRUE),
    n.delta.loss = n(),
    
    .groups = "drop"
  )

# Compute comparison bars directly from study2.cortisol.df
study2.cortisol.delta.coordinate.df <- study2.cortisol.mean.df %>%
  filter(resource %in% c("lack", "gain", "loss")) %>%
  select(condition.label, resource, mean, std.err) %>%
  pivot_wider(names_from = resource, values_from = c(mean, std.err)) %>%
  mutate(
    # Calculate difference bar positions (for lack vs gain)
    diffbar.lack.gain.xmin = 1.25,
    diffbar.lack.gain.xmax = 1.75,
    diffbar.lack.gain.ymin = pmin(mean_lack, mean_gain),
    diffbar.lack.gain.ymax = pmax(mean_lack, mean_gain),
    
    # Calculate difference bar positions (for gain vs loss)
    diffbar.gain.loss.xmin = 2.25,
    diffbar.gain.loss.xmax = 2.75,
    diffbar.gain.loss.ymin = pmin(mean_gain, mean_loss),
    diffbar.gain.loss.ymax = pmax(mean_gain, mean_loss)
  ) %>%
  left_join(
    study2.cortisol.delta.df %>%
      select(condition.label,
             se.lack.gain = std.err.delta.gain,
             se.gain.loss = std.err.delta.loss),
    by = "condition.label"
  )

study2.cortisol.mean.df <- study2.cortisol.mean.df %>%
  mutate(
    x = factor(paste(resource, condition.label, sep = "_"),
               levels = c("lack_DIGITAL", "gain_DIGITAL", "loss_DIGITAL",
                          "lack_TRADITIONAL", "gain_TRADITIONAL", "loss_TRADITIONAL")),
    x.label = c("lack", "gain\nDIGITAL", "loss", "lack", "gain\nTRADITIONAL", "loss")  # 6 in total
  )

# Plot delta
p.study2.cortisol.delta <-  
  #ggplot(study2.cortisol.mean.df, aes(x = resource, y = mean, fill = condition.label)) +
  ggplot(study2.cortisol.mean.df,
         aes(x = x, y = mean, fill = condition.label)) +
  
  # Standard bar plot (no error bars)
  geom_bar(stat = "identity", 
           color = cbPalette[2], 
           position = position_dodge(width = 0.9), 
           alpha = 0.5) +
  
  scale_fill_manual(values = c("DIGITAL" = cbPalette[1], "TRADITIONAL" = cbPalette[1]), guide = "none") +
  
  scale_x_discrete(labels = setNames(study2.cortisol.mean.df$x.label, #Labels
                                     study2.cortisol.mean.df$x)) +
  
  labs(x = "", y = expression(AUCi[cortisol])) +
  
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text.x = element_text(size = 20, face = "bold"), 
        axis.line = element_line(colour = "black"),
        plot.title = element_text(size = 26, face = "bold", hjust = 0.5),
        text = element_text(size = 24, family = "latex")) +

  #H1c
  #Difference bar (lack to gain)
  geom_rect(data = subset(study2.cortisol.delta.coordinate.df, condition.label == "DIGITAL"),
            aes(xmin = 1.25, xmax = 1.75,
                ymin = diffbar.lack.gain.ymin, ymax = diffbar.lack.gain.ymax),
            fill = cbPalette[2], color = "black", inherit.aes = FALSE) +
  geom_errorbar(data = subset(study2.cortisol.delta.coordinate.df, condition.label == "DIGITAL"),
                aes(x = (1.25 + 1.75)/2,
                    ymin = diffbar.lack.gain.ymax - se.lack.gain,
                    ymax = diffbar.lack.gain.ymax + se.lack.gain),
                width = 0.1, inherit.aes = FALSE) +
  #Difference bar (gain to loss) 
  geom_rect(data = subset(study2.cortisol.delta.coordinate.df, condition.label == "DIGITAL"),
            aes(xmin = 2.25, xmax = 2.75,
                ymin = diffbar.gain.loss.ymin, ymax = diffbar.gain.loss.ymax),
            fill = cbPalette[2], color = "black", inherit.aes = FALSE) + 
  geom_errorbar(data = subset(study2.cortisol.delta.coordinate.df, condition.label == "DIGITAL"),
                aes(x = (2.25 + 2.75)/2,
                    ymin = diffbar.gain.loss.ymax - se.gain.loss,
                    ymax = diffbar.gain.loss.ymax + se.gain.loss),
                width = 0.1, inherit.aes = FALSE) +
  #label
  geom_segment(data = subset(study2.cortisol.delta.coordinate.df, condition.label == "DIGITAL"),
               aes( 
                 x = 1.5, xend = 1.5, 
                 y = max(diffbar.lack.gain.ymax) * 1.4, yend = diffbar.lack.gain.ymax[condition.label == "DIGITAL"]),
               color = cbPalette[6], linewidth = 0.75, 
               arrow = arrow_single) +
  geom_segment(data = subset(study2.cortisol.delta.coordinate.df, condition.label == "DIGITAL"),
               aes( 
                 x = 1.5, xend = 2.5, 
                 y = max(diffbar.lack.gain.ymax) * 1.4, yend = max(diffbar.lack.gain.ymax) * 1.4),
               color = cbPalette[6], linewidth = 0.75) +
  geom_segment(data = subset(study2.cortisol.delta.coordinate.df, condition.label == "DIGITAL"),
               aes( 
                 x = 2.5, xend = 2.5, 
                 y = max(diffbar.lack.gain.ymax) * 1.4, yend = diffbar.gain.loss.ymax[condition.label == "DIGITAL"]),
               color = cbPalette[6], linewidth = 0.75, 
               arrow = arrow_single) +
  geom_text(data = subset(study2.cortisol.delta.coordinate.df, condition.label == "DIGITAL"),
            aes(x = 2, y = max(diffbar.lack.gain.ymax) * 1.5, 
                label = paste("H2c", sigstars.study2$h1c.cortisol)),
            color = cbPalette[6], size = 5, family = "latex") +
  
  #H2c
  #Difference bar (lack to gain)
  geom_rect(data = subset(study2.cortisol.delta.coordinate.df, condition.label == "TRADITIONAL"),
            aes(xmin = 4.25, xmax = 4.75,
                ymin = diffbar.lack.gain.ymin, ymax = diffbar.lack.gain.ymax),
            fill = cbPalette[2], color = "black", inherit.aes = FALSE) +
  geom_errorbar(data = subset(study2.cortisol.delta.coordinate.df, condition.label == "TRADITIONAL"),
                aes(x = (4.25 + 4.75)/2,
                    ymin = diffbar.lack.gain.ymax - se.lack.gain,
                    ymax = diffbar.lack.gain.ymax + se.lack.gain),
                width = 0.1, inherit.aes = FALSE) +
  #Difference bar (gain to loss) 
  geom_rect(data = subset(study2.cortisol.delta.coordinate.df, condition.label == "TRADITIONAL"),
            aes(xmin = 5.25, xmax = 5.75,
                ymin = diffbar.gain.loss.ymin, ymax = diffbar.gain.loss.ymax),
            fill = cbPalette[2], color = "black", inherit.aes = FALSE) + 
  geom_errorbar(data = subset(study2.cortisol.delta.coordinate.df, condition.label == "TRADITIONAL"),
                aes(x = (5.25 + 5.75)/2,
                    ymin = diffbar.gain.loss.ymax - se.gain.loss,
                    ymax = diffbar.gain.loss.ymax + se.gain.loss),
                width = 0.1, inherit.aes = FALSE) +
  #label
  geom_segment(data = subset(study2.cortisol.delta.coordinate.df, condition.label == "TRADITIONAL"),
               aes( 
                 x = 4.5, xend = 4.5, 
                 y = max(diffbar.lack.gain.ymax) * 1.4, yend = diffbar.lack.gain.ymax[condition.label == "TRADITIONAL"]),
               color = cbPalette[6], linewidth = 0.75, 
               arrow = arrow_single) +
  geom_segment(data = subset(study2.cortisol.delta.coordinate.df, condition.label == "TRADITIONAL"),
               aes( 
                 x = 4.5, xend = 5.5, 
                 y = max(diffbar.lack.gain.ymax) * 1.4, yend = max(diffbar.lack.gain.ymax) * 1.4),
               color = cbPalette[6], linewidth = 0.75) +
  geom_segment(data = subset(study2.cortisol.delta.coordinate.df, condition.label == "TRADITIONAL"),
               aes( 
                 x = 5.5, xend = 5.5, 
                 y = max(diffbar.lack.gain.ymax) * 1.4, yend = diffbar.gain.loss.ymax[condition.label == "TRADITIONAL"]),
               color = cbPalette[6], linewidth = 0.75, 
               arrow = arrow_single) +
  geom_text(data = subset(study2.cortisol.delta.coordinate.df, condition.label == "TRADITIONAL"),
            aes(x = 5, y = max(diffbar.lack.gain.ymax) * 1.5, 
                label = paste("H2c", sigstars.study2$h2c.cortisol)),
            color = cbPalette[6], size = 5, family = "latex") +
  
  # H3: lack-gain (DIGITAL vs TRADITIONAL)
  geom_segment(aes(x = 1.5, xend = 4.5, y = 0.38, yend = 0.38),
               color = cbPalette[7], linewidth = 0.75, inherit.aes = FALSE) +
  geom_segment(aes(x = 1.5, xend = 1.5, y = 0.38, yend = study2.cortisol.mean.df$mean[study2.cortisol.mean.df$x == "lack_DIGITAL"]),
               color = cbPalette[7], linewidth = 0.75, arrow = arrow_single, inherit.aes = FALSE) +
  geom_segment(aes(x = 4.5, xend = 4.5, y = 0.38, yend = study2.cortisol.mean.df$mean[study2.cortisol.mean.df$x == "lack_TRADITIONAL"]),
               color = cbPalette[7], linewidth = 0.75, arrow = arrow_single, inherit.aes = FALSE) +
  geom_text(aes(x = 3, y = 0.40,
                label = paste0("H3 ", sigstars.study2$h3.cortisol)),
            color = cbPalette[7], size = 5, family = "latex", inherit.aes = FALSE) +
  
  # H4: loss-gain (DIGITAL vs TRADITIONAL)
  geom_segment(aes(x = 2.5, xend = 5.5, y = 0.46, yend = 0.46),
               color = cbPalette[7], linewidth = 0.75, inherit.aes = FALSE) +
  geom_segment(aes(x = 2.5, xend = 2.5, y = 0.46, yend = study2.cortisol.mean.df$mean[study2.cortisol.mean.df$x == "loss_DIGITAL"]),
               color = cbPalette[7], linewidth = 0.75, arrow = arrow_single, inherit.aes = FALSE) +
  geom_segment(aes(x = 5.5, xend = 5.5, y = 0.46, yend = study2.cortisol.mean.df$mean[study2.cortisol.mean.df$x == "loss_TRADITIONAL"]),
               color = cbPalette[7], linewidth = 0.75, arrow = arrow_single, inherit.aes = FALSE) +
  geom_text(aes(x = 4, y = 0.48,
                label = paste0("H4 ", sigstars.study2$h4.cortisol)),
            color = cbPalette[7], size = 5, family = "latex", inherit.aes = FALSE)

print(p.study2.cortisol.delta)

##################
# Perceived
study2.perceived.delta.df <- study2.final.df %>%
  filter(resource %in% c("lack", "gain", "loss")) %>%
  select(participant.id, condition.label, resource, AUCi.perceived) %>%
  pivot_wider(names_from = resource, values_from = AUCi.perceived) %>%
  mutate(
    delta.gain = lack - gain,
    delta.loss = loss - gain
  ) %>% 
  group_by(condition.label) %>%
  reframe(
    mean.delta.gain = mean(delta.gain, na.rm = TRUE),
    std.err.delta.gain = sd(delta.gain, na.rm = TRUE) / sqrt(n()),
    sd.delta.gain = sd(delta.gain, na.rm = TRUE),
    n.delta.gain = n(),
    
    mean.delta.loss = mean(delta.loss, na.rm = TRUE),
    std.err.delta.loss = sd(delta.loss, na.rm = TRUE) / sqrt(n()),
    sd.delta.loss = sd(delta.loss, na.rm = TRUE),
    n.delta.loss = n(),
    
    .groups = "drop"
  )

# Compute comparison bars directly from study2.perceived.df
study2.perceived.delta.coordinate.df <- study2.perceived.mean.df %>%
  filter(resource %in% c("lack", "gain", "loss")) %>%
  select(condition.label, resource, mean, std.err) %>%
  pivot_wider(names_from = resource, values_from = c(mean, std.err)) %>%
  mutate(
    # Calculate difference bar positions (for lack vs gain)
    diffbar.lack.gain.xmin = 1.25,
    diffbar.lack.gain.xmax = 1.75,
    diffbar.lack.gain.ymin = pmin(mean_lack, mean_gain),
    diffbar.lack.gain.ymax = pmax(mean_lack, mean_gain),
    
    # Calculate difference bar positions (for gain vs loss)
    diffbar.gain.loss.xmin = 2.25,
    diffbar.gain.loss.xmax = 2.75,
    diffbar.gain.loss.ymin = pmin(mean_gain, mean_loss),
    diffbar.gain.loss.ymax = pmax(mean_gain, mean_loss)
  ) %>%
  left_join(
    study2.perceived.delta.df %>%
      select(condition.label,
             se.lack.gain = std.err.delta.gain,
             se.gain.loss = std.err.delta.loss),
    by = "condition.label"
  )

study2.perceived.mean.df <- study2.perceived.mean.df %>%
  mutate(
    x = factor(paste(resource, condition.label, sep = "_"),
               levels = c("lack_DIGITAL", "gain_DIGITAL", "loss_DIGITAL",
                          "lack_TRADITIONAL", "gain_TRADITIONAL", "loss_TRADITIONAL")),
    x.label = c("lack", "gain\nDIGITAL", "loss", "lack", "gain\nTRADITIONAL", "loss")  # 6 in total
  )

# Plot delta
p.study2.perceived.delta <-  
  ggplot(study2.perceived.mean.df,
         aes(x = x, y = mean, fill = condition.label)) +
  
  # Standard bar plot (no error bars)
  geom_bar(stat = "identity", 
           color = cbPalette[2], 
           position = position_dodge(width = 0.9), 
           alpha = 0.5) +
  
  scale_fill_manual(values = c("DIGITAL" = cbPalette[1], "TRADITIONAL" = cbPalette[1]), guide = "none") +
  
  scale_x_discrete(labels = setNames(study2.perceived.mean.df$x.label, #Labels
                                     study2.perceived.mean.df$x)) +
  
  labs(x = "", y = expression(AUCi[perceived])) +
  
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text.x = element_text(size = 20, face = "bold"), 
        axis.line = element_line(colour = "black"),
        plot.title = element_text(size = 26, face = "bold", hjust = 0.5),
        text = element_text(size = 24, family = "latex")) +
  
  #H1c
  #Difference bar (lack to gain)
  geom_rect(data = subset(study2.perceived.delta.coordinate.df, condition.label == "DIGITAL"),
            aes(xmin = 1.25, xmax = 1.75,
                ymin = diffbar.lack.gain.ymin, ymax = diffbar.lack.gain.ymax),
            fill = cbPalette[2], color = "black", inherit.aes = FALSE) +
  geom_errorbar(data = subset(study2.perceived.delta.coordinate.df, condition.label == "DIGITAL"),
                aes(x = (1.25 + 1.75)/2,
                    ymin = diffbar.lack.gain.ymax - se.lack.gain,
                    ymax = diffbar.lack.gain.ymax + se.lack.gain),
                width = 0.1, inherit.aes = FALSE) +
  #Difference bar (gain to loss) 
  geom_rect(data = subset(study2.perceived.delta.coordinate.df, condition.label == "DIGITAL"),
            aes(xmin = 2.25, xmax = 2.75,
                ymin = diffbar.gain.loss.ymin, ymax = diffbar.gain.loss.ymax),
            fill = cbPalette[2], color = "black", inherit.aes = FALSE) + 
  geom_errorbar(data = subset(study2.perceived.delta.coordinate.df, condition.label == "DIGITAL"),
                aes(x = (2.25 + 2.75)/2,
                    ymin = diffbar.gain.loss.ymax - se.gain.loss,
                    ymax = diffbar.gain.loss.ymax + se.gain.loss),
                width = 0.1, inherit.aes = FALSE) +
  #label
  geom_segment(data = subset(study2.perceived.delta.coordinate.df, condition.label == "DIGITAL"),
               aes( 
                 x = 1.5, xend = 1.5, 
                 y = max(diffbar.lack.gain.ymax)+1.5, yend = diffbar.lack.gain.ymax[condition.label == "DIGITAL"]),
               color = cbPalette[6], linewidth = 0.75, 
               arrow = arrow_single) +
  geom_segment(data = subset(study2.perceived.delta.coordinate.df, condition.label == "DIGITAL"),
               aes( 
                 x = 1.5, xend = 2.5, 
                 y = max(diffbar.lack.gain.ymax)+1.5, yend = max(diffbar.lack.gain.ymax)+1.5),
               color = cbPalette[6], linewidth = 0.75) +
  geom_segment(data = subset(study2.perceived.delta.coordinate.df, condition.label == "DIGITAL"),
               aes( 
                 x = 2.5, xend = 2.5, 
                 y = max(diffbar.lack.gain.ymax)+1.5, yend = diffbar.gain.loss.ymax[condition.label == "DIGITAL"]),
               color = cbPalette[6], linewidth = 0.75, 
               arrow = arrow_single) +
  geom_text(data = subset(study2.perceived.delta.coordinate.df, condition.label == "DIGITAL"),
            aes(x = 2, y = max(diffbar.lack.gain.ymax)+1.1, 
                label = paste("H2c", sigstars.study2$h1c.perceived)),
            color = cbPalette[6], size = 5, family = "latex") +
  
  #H2c
  #Difference bar (lack to gain)
  geom_rect(data = subset(study2.perceived.delta.coordinate.df, condition.label == "TRADITIONAL"),
            aes(xmin = 4.25, xmax = 4.75,
                ymin = diffbar.lack.gain.ymin, ymax = diffbar.lack.gain.ymax),
            fill = cbPalette[2], color = "black", inherit.aes = FALSE) +
  geom_errorbar(data = subset(study2.perceived.delta.coordinate.df, condition.label == "TRADITIONAL"),
                aes(x = (4.25 + 4.75)/2,
                    ymin = diffbar.lack.gain.ymax - se.lack.gain,
                    ymax = diffbar.lack.gain.ymax + se.lack.gain),
                width = 0.1, inherit.aes = FALSE) +
  #Difference bar (gain to loss) 
  geom_rect(data = subset(study2.perceived.delta.coordinate.df, condition.label == "TRADITIONAL"),
            aes(xmin = 5.25, xmax = 5.75,
                ymin = diffbar.gain.loss.ymin, ymax = diffbar.gain.loss.ymax),
            fill = cbPalette[2], color = "black", inherit.aes = FALSE) + 
  geom_errorbar(data = subset(study2.perceived.delta.coordinate.df, condition.label == "TRADITIONAL"),
                aes(x = (5.25 + 5.75)/2,
                    ymin = diffbar.gain.loss.ymax - se.gain.loss,
                    ymax = diffbar.gain.loss.ymax + se.gain.loss),
                width = 0.1, inherit.aes = FALSE) +
  #label
  geom_segment(data = subset(study2.perceived.delta.coordinate.df, condition.label == "TRADITIONAL"),
               aes( 
                 x = 4.5, xend = 4.5, 
                 y = max(diffbar.lack.gain.ymax)+1.5, yend = diffbar.lack.gain.ymax[condition.label == "TRADITIONAL"]),
               color = cbPalette[6], linewidth = 0.75, 
               arrow = arrow_single) +
  geom_segment(data = subset(study2.perceived.delta.coordinate.df, condition.label == "TRADITIONAL"),
               aes( 
                 x = 4.5, xend = 5.5, 
                 y = max(diffbar.lack.gain.ymax)+1.5, yend = max(diffbar.lack.gain.ymax)+1.5),
               color = cbPalette[6], linewidth = 0.75) +
  geom_segment(data = subset(study2.perceived.delta.coordinate.df, condition.label == "TRADITIONAL"),
               aes( 
                 x = 5.5, xend = 5.5, 
                 y = max(diffbar.lack.gain.ymax)+1.5, yend = diffbar.gain.loss.ymax[condition.label == "TRADITIONAL"]),
               color = cbPalette[6], linewidth = 0.75, 
               arrow = arrow_single) +
  geom_text(data = subset(study2.perceived.delta.coordinate.df, condition.label == "TRADITIONAL"),
            aes(x = 5, y = max(diffbar.lack.gain.ymax)+1.1, 
                label = paste("H2c", sigstars.study2$h2c.perceived)),
            color = cbPalette[6], size = 5, family = "latex") +
  
  # H3: lack-gain (DIGITAL vs TRADITIONAL)
  geom_segment(aes(x = 1.5, xend = 4.5, y = 10.5, yend = 10.5),
               color = cbPalette[7], linewidth = 0.75, inherit.aes = FALSE) +
  geom_segment(aes(x = 1.5, xend = 1.5, y = 10.5, yend = study2.perceived.mean.df$mean[study2.perceived.mean.df$x == "lack_DIGITAL"]),
               color = cbPalette[7], linewidth = 0.75, arrow = arrow_single, inherit.aes = FALSE) +
  geom_segment(aes(x = 4.5, xend = 4.5, y = 10.5, yend = study2.perceived.mean.df$mean[study2.perceived.mean.df$x == "lack_TRADITIONAL"]),
               color = cbPalette[7], linewidth = 0.75, arrow = arrow_single, inherit.aes = FALSE) +
  geom_text(aes(x = 3, y = 11,
                label = paste0("H3 ", sigstars.study2$h3.perceived)),
            color = cbPalette[7], size = 5, family = "latex", inherit.aes = FALSE) +
  
  # H4: loss-gain (DIGITAL vs TRADITIONAL)
  geom_segment(aes(x = 2.5, xend = 5.5, y = 12, yend = 12),
               color = cbPalette[7], linewidth = 0.75, inherit.aes = FALSE) +
  geom_segment(aes(x = 2.5, xend = 2.5, y = 12, yend = study2.perceived.mean.df$mean[study2.perceived.mean.df$x == "loss_DIGITAL"]),
               color = cbPalette[7], linewidth = 0.75, arrow = arrow_single, inherit.aes = FALSE) +
  geom_segment(aes(x = 5.5, xend = 5.5, y = 12, yend = study2.perceived.mean.df$mean[study2.perceived.mean.df$x == "loss_TRADITIONAL"]),
               color = cbPalette[7], linewidth = 0.75, arrow = arrow_single, inherit.aes = FALSE) +
  geom_text(aes(x = 4, y = 12.5,
                label = paste0("H4 ", sigstars.study2$h4.perceived)),
            color = cbPalette[7], size = 5, family = "latex", inherit.aes = FALSE)

print(p.study2.perceived.delta)

# Save the plot to a PDF file
ggsave(file.path(artwork.dir, "p.study2.cortisol.mean.pdf"), plot = p.study2.cortisol.mean, width = 20, height = 20, units = "cm") 
ggsave(file.path(artwork.dir, "p.study2.perceived.mean.pdf"), plot = p.study2.perceived.delta, width = 20, height = 20, units = "cm")

ggsave(file.path(artwork.dir, "p.study2.cortisol.delta.pdf"), plot = p.study2.cortisol.delta, width = 20, height = 20, units = "cm") 
ggsave(file.path(artwork.dir, "p.study2.perceived.delta.pdf"), plot = p.study2.perceived.delta, width = 20, height = 20, units = "cm")
