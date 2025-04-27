source(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), "init.R"))


###############################
#
#   Load raw files
#
###############################

load(file = "./02 RData/analysis.RData")
load(file = "./02 RData/pvals_study1.RData")

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
  } else {
    return("n.s.")
  }
}

# Apply to whole list
sigstars.study1 <- lapply(pvals.study1, get.significance.stars)


################################
#
#
#   FIGURES
#
#
################################

# Define color and pattern palettes globally
cbPalette <- c( "#F9F9F6", "#D5D5D5","#BABABA", "#3B3B3B", "#636363", "#0072B2", "#D55E00", "#CC79A7")

# Define arrow globally
arrow_single <- arrow(length = unit(0.02, "npc"), type = "closed")
arrow_double <- arrow(length = unit(0.015, "npc"), type = "open", ends = "both", angle = 90)

# Create readable labels
study1.final.df <- study1.final.df %>%
  mutate(condition.label = ifelse(condition == "Digital", "DIGITAL", "TRADITIONAL"),
         resource = factor(resource, levels = c("lack", "gain", "loss")))

############################
#
# H1a: The gain of digital resources reduces stress; 
# H1b: the loss of digital resources increases stress;
# H2a: The gain of traditional resources increases stress; 
# H2b: The loss of traditional resources increases stress; 
#
############################
# Cortisol
# Summarize mean and SE for plotting
study1.cortisol.df <- study1.final.df %>%
  group_by(condition.label, resource) %>%
  summarize(
    mean = mean(AUCi.cortisol, na.rm = TRUE),
    std.err = sd(AUCi.cortisol, na.rm = TRUE) / sqrt(n()),
    sd = sd(AUCi.cortisol, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

#Plot mean
p.study1.cortisol.mean <-  
  ggplot(study1.cortisol.df, aes(x = resource, y = mean, fill = condition.label)) + #
  geom_bar(stat = "identity", 
           color = cbPalette[4], 
           position = position_dodge(width = 0.9), 
           alpha = 0.5)  +
  scale_fill_manual(values = c("DIGITAL" = cbPalette[2], "TRADITIONAL" = cbPalette[2]), guide = "none") +

  geom_errorbar(aes(ymin = mean - std.err, ymax = mean + std.err), 
                width = 0.2, 
                position = position_dodge(width = 0.9)) +
  
  labs(x = "", y = expression(AUCi[cortisol])) + # , title = "Mean cortisol levels")
  
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
  
  #Annotations for DIGITAL
  #H1a
  geom_segment(data = subset(study1.cortisol.df, condition.label == "DIGITAL"),
               aes( 
                 x = 1, xend = 1, 
                 y = max(study1.cortisol.df$mean) * 1.4, yend = mean[resource == "lack" & condition.label == "DIGITAL"] * 1.375),
               color = cbPalette[6], size = 0.75, 
               arrow = arrow_single) +
  geom_segment(data = subset(study1.cortisol.df, condition.label == "DIGITAL"),
               aes( 
                 x = 1, xend = 1.9, 
                 y = max(study1.cortisol.df$mean) * 1.4, yend = max(study1.cortisol.df$mean) * 1.4),
               color = cbPalette[6], size = 0.75) +
  geom_segment(data = subset(study1.cortisol.df, condition.label == "DIGITAL"),
               aes( 
                 x = 1.9, xend = 1.9, 
                 y = max(study1.cortisol.df$mean) * 1.4, yend = mean[resource == "gain" & condition.label == "DIGITAL"] * -0.25),
               color = cbPalette[6], size = 0.75, 
               arrow = arrow_single) +
  geom_text(data = subset(study1.cortisol.df, condition.label == "DIGITAL"),
            aes(x = 1.45, y = max(study1.cortisol.df$mean) * 1.5, 
                label = paste("H1a", sigstars.study1$h1a.cortisol)),
            color = cbPalette[6], size = 5, family = "latex") +
  #H1b
  geom_segment(data = subset(study1.cortisol.df, condition.label == "DIGITAL"),
               aes( 
                 x = 2.1, xend = 2.1, 
                 y = max(study1.cortisol.df$mean) * 1.4, yend = mean[resource == "gain" & condition.label == "DIGITAL"] * -0.25),
               color = cbPalette[6], size = 0.75, 
               arrow = arrow_single) +
  geom_segment(data = subset(study1.cortisol.df, condition.label == "DIGITAL"),
               aes( 
                 x = 2.1, xend = 3, 
                 y = max(study1.cortisol.df$mean) * 1.4, yend = max(study1.cortisol.df$mean) * 1.4),
               color = cbPalette[6], size = 0.75) +
  geom_segment(data = subset(study1.cortisol.df, condition.label == "DIGITAL"),
               aes( 
                 x = 3, xend = 3, 
                 y = max(study1.cortisol.df$mean) * 1.4, yend = mean[resource == "loss" & condition.label == "DIGITAL"] * 1.5),
               color = cbPalette[6], size = 0.75, 
               arrow = arrow_single) +
  geom_text(data = subset(study1.cortisol.df, condition.label == "DIGITAL"),
            aes(x = 2.55, y = max(study1.cortisol.df$mean) * 1.5, 
                label = paste("H1b", sigstars.study1$h1b.cortisol)),
            color = cbPalette[6], size = 5, family = "latex") +
  
  #Annotations for TRADITIONAL
  #H2a
  geom_segment(data = subset(study1.cortisol.df, condition.label == "TRADITIONAL"),
               aes( 
                 x = 1, xend = 1, 
                 y = max(study1.cortisol.df$mean) * 1.4, yend = mean[resource == "lack" & condition.label == "TRADITIONAL"] * 1.325),
               color = cbPalette[6], size = 0.75, 
               arrow = arrow_single) +
  geom_segment(data = subset(study1.cortisol.df, condition.label == "TRADITIONAL"),
               aes( 
                 x = 1, xend = 1.9, 
                 y = max(study1.cortisol.df$mean) * 1.4, yend = max(study1.cortisol.df$mean) * 1.4),
               color = cbPalette[6], size = 0.75) +
  geom_segment(data = subset(study1.cortisol.df, condition.label == "TRADITIONAL"),
               aes( 
                 x = 1.9, xend = 1.9, 
                 y = max(study1.cortisol.df$mean) * 1.4, yend = mean[resource == "gain" & condition.label == "TRADITIONAL"] * -0.25),
               color = cbPalette[6], size = 0.75, 
               arrow = arrow_single) +
  geom_text(data = subset(study1.cortisol.df, condition.label == "TRADITIONAL"),
            aes(x = 1.45, y = max(study1.cortisol.df$mean) * 1.5, 
                label = paste("H2a", sigstars.study1$h2a.cortisol)),
            color = cbPalette[6], size = 5, family = "latex") +
  #H2b
  geom_segment(data = subset(study1.cortisol.df, condition.label == "TRADITIONAL"),
               aes( 
                 x = 2.1, xend = 2.1, 
                 y = max(study1.cortisol.df$mean) * 1.4, yend = mean[resource == "gain" & condition.label == "TRADITIONAL"] * -0.25),
               color = cbPalette[6], size = 0.75, 
               arrow = arrow_single) +
  geom_segment(data = subset(study1.cortisol.df, condition.label == "TRADITIONAL"),
               aes( 
                 x = 2.1, xend = 3, 
                 y = max(study1.cortisol.df$mean) * 1.4, yend = max(study1.cortisol.df$mean) * 1.4),
               color = cbPalette[6], size = 0.75) +
  geom_segment(data = subset(study1.cortisol.df, condition.label == "TRADITIONAL"),
               aes( 
                 x = 3, xend = 3, 
                 y = max(study1.cortisol.df$mean) * 1.4, yend = mean[resource == "loss" & condition.label == "TRADITIONAL"] * 1.5),
               color = cbPalette[6], size = 0.75, 
               arrow = arrow_single) +
  geom_text(data = subset(study1.cortisol.df, condition.label == "TRADITIONAL"),
            aes(x = 2.55, y = max(study1.cortisol.df$mean) * 1.5, 
                label = paste("H2b", sigstars.study1$h2b.cortisol)),
            color = cbPalette[6], size = 5, family = "latex")
  
print(p.study1.cortisol.mean)

##############
# Perceived
# Summarize mean and SE for plotting
study1.perceived.df <- study1.final.df %>%
  group_by(condition.label, resource) %>%
  summarize(
    mean = mean(AUCi.perceived, na.rm = TRUE),
    std.err = sd(AUCi.perceived, na.rm = TRUE) / sqrt(n()),
    sd = sd(AUCi.perceived, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

#Plot
p.study1.perceived.mean <-  
  ggplot(study1.perceived.df, aes(x = resource, y = mean, fill = condition.label)) + #
  geom_bar(stat = "identity", 
           color = cbPalette[4], 
           position = position_dodge(width = 0.9), 
           alpha = 0.5)  +
  scale_fill_manual(values = c("DIGITAL" = cbPalette[2], "TRADITIONAL" = cbPalette[2]), guide = "none") +
  
  geom_errorbar(aes(ymin = mean - std.err, ymax = mean + std.err), 
                width = 0.2, 
                position = position_dodge(width = 0.9)) +
  
  labs(x = "", y = expression(AUCi[perceived])) + # , title = "Mean perceived levels")
  
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
  
  #Annotations for DIGITAL
  #H1a
  geom_segment(data = subset(study1.perceived.df, condition.label == "DIGITAL"),
               aes( 
                 x = 1, xend = 1, 
                 y = max(study1.perceived.df$mean) * 1.4, yend = mean[resource == "lack" & condition.label == "DIGITAL"] * 1.25),
               color = cbPalette[6], size = 0.75, 
               arrow = arrow_single) +
  geom_segment(data = subset(study1.perceived.df, condition.label == "DIGITAL"),
               aes( 
                 x = 1, xend = 1.9, 
                 y = max(study1.perceived.df$mean) * 1.4, yend = max(study1.perceived.df$mean) * 1.4),
               color = cbPalette[6], size = 0.75) +
  geom_segment(data = subset(study1.perceived.df, condition.label == "DIGITAL"),
               aes( 
                 x = 1.9, xend = 1.9, 
                 y = max(study1.perceived.df$mean) * 1.4, yend = mean[resource == "gain" & condition.label == "DIGITAL"] * 4),
               color = cbPalette[6], size = 0.75, 
               arrow = arrow_single) +
  geom_text(data = subset(study1.perceived.df, condition.label == "DIGITAL"),
            aes(x = 1.45, y = max(study1.perceived.df$mean) * 1.5, 
                label = paste("H1a", sigstars.study1$h1a.perceived)),
            color = cbPalette[6], size = 5, family = "latex") +
  #H1b
  geom_segment(data = subset(study1.perceived.df, condition.label == "DIGITAL"),
               aes( 
                 x = 2.1, xend = 2.1, 
                 y = max(study1.perceived.df$mean) * 1.4, yend = mean[resource == "gain" & condition.label == "DIGITAL"] * 4),
               color = cbPalette[6], size = 0.75, 
               arrow = arrow_single) +
  geom_segment(data = subset(study1.perceived.df, condition.label == "DIGITAL"),
               aes( 
                 x = 2.1, xend = 3, 
                 y = max(study1.perceived.df$mean) * 1.4, yend = max(study1.perceived.df$mean) * 1.4),
               color = cbPalette[6], size = 0.75) +
  geom_segment(data = subset(study1.perceived.df, condition.label == "DIGITAL"),
               aes( 
                 x = 3, xend = 3, 
                 y = max(study1.perceived.df$mean) * 1.4, yend = mean[resource == "loss" & condition.label == "DIGITAL"] * 1.6),
               color = cbPalette[6], size = 0.75, 
               arrow = arrow_single) +
  geom_text(data = subset(study1.perceived.df, condition.label == "DIGITAL"),
            aes(x = 2.55, y = max(study1.perceived.df$mean) * 1.5, 
                label = paste("H1b", sigstars.study1$h1b.perceived)),
            color = cbPalette[6], size = 5, family = "latex") +
  
  #Annotations for TRADITIONAL
  #H2a
  geom_segment(data = subset(study1.perceived.df, condition.label == "TRADITIONAL"),
               aes( 
                 x = 1, xend = 1, 
                 y = max(study1.perceived.df$mean) * 1.4, yend = mean[resource == "lack" & condition.label == "TRADITIONAL"] * 1.3),
               color = cbPalette[6], size = 0.75, 
               arrow = arrow_single) +
  geom_segment(data = subset(study1.perceived.df, condition.label == "TRADITIONAL"),
               aes( 
                 x = 1, xend = 1.9, 
                 y = max(study1.perceived.df$mean) * 1.4, yend = max(study1.perceived.df$mean) * 1.4),
               color = cbPalette[6], size = 0.75) +
  geom_segment(data = subset(study1.perceived.df, condition.label == "TRADITIONAL"),
               aes( 
                 x = 1.9, xend = 1.9, 
                 y = max(study1.perceived.df$mean) * 1.4, yend = mean[resource == "gain" & condition.label == "TRADITIONAL"] * 2.25),
               color = cbPalette[6], size = 0.75, 
               arrow = arrow_single) +
  geom_text(data = subset(study1.perceived.df, condition.label == "TRADITIONAL"),
            aes(x = 1.45, y = max(study1.perceived.df$mean) * 1.5, 
                label = paste("H2a", sigstars.study1$h2a.perceived)),
            color = cbPalette[6], size = 5, family = "latex") +
  #H2b
  geom_segment(data = subset(study1.perceived.df, condition.label == "TRADITIONAL"),
               aes( 
                 x = 2.1, xend = 2.1, 
                 y = max(study1.perceived.df$mean) * 1.4, yend = mean[resource == "gain" & condition.label == "TRADITIONAL"] * 2.25),
               color = cbPalette[6], size = 0.75, 
               arrow = arrow_single) +
  geom_segment(data = subset(study1.perceived.df, condition.label == "TRADITIONAL"),
               aes( 
                 x = 2.1, xend = 3, 
                 y = max(study1.perceived.df$mean) * 1.4, yend = max(study1.perceived.df$mean) * 1.4),
               color = cbPalette[6], size = 0.75) +
  geom_segment(data = subset(study1.perceived.df, condition.label == "TRADITIONAL"),
               aes( 
                 x = 3, xend = 3, 
                 y = max(study1.perceived.df$mean) * 1.4, yend = mean[resource == "loss" & condition.label == "TRADITIONAL"] * 2.25),
               color = cbPalette[6], size = 0.75, 
               arrow = arrow_single) +
  geom_text(data = subset(study1.perceived.df, condition.label == "TRADITIONAL"),
            aes(x = 2.55, y = max(study1.perceived.df$mean) * 1.5, 
                label = paste("H2b", sigstars.study1$h2b.perceived)),
            color = cbPalette[6], size = 5, family = "latex")

print(p.study1.perceived.mean)

############################
#
# H1c: The decrease in stress due to the gain of digital resources is smaller than the increase in stress due to the loss of digital resources. 
# H2c: The increase in stress due to the gain of traditional resources is smaller than the increase in stress due to the loss of traditional resources. 
#
############################

# Cortisol
# Plot delta
p.study1.cortisol.delta <-  
  ggplot(study1.cortisol.df, aes(x = resource, y = mean, fill = condition.label)) + #
  geom_bar(stat = "identity", 
           color = cbPalette[4], 
           position = position_dodge(width = 0.9), 
           alpha = 0.5)  +
  scale_fill_manual(values = c("DIGITAL" = cbPalette[2], "TRADITIONAL" = cbPalette[2]), guide = "none") +
  
  geom_errorbar(aes(ymin = mean - std.err, ymax = mean + std.err), 
                width = 0.2, 
                position = position_dodge(width = 0.9)) +
  
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
  
  facet_grid(. ~ condition.label, switch = "x") +
  
  #Annotations for DIGITAL
  geom_segment(data = subset(study1.cortisol.df, condition.label == "DIGITAL"),
               aes( 
                 x = 1.5, xend = 1.5, 
                 y = mean[resource == "lack" & condition.label == "DIGITAL"], yend = mean[resource == "gain" & condition.label == "DIGITAL"]),
               color = cbPalette[5], size = 1, 
               arrow = arrow_double) +
  geom_segment(data = subset(study1.cortisol.df, condition.label == "DIGITAL"),
               aes( 
                 x = 2.5, xend = 2.5, 
                 y = mean[resource == "gain" & condition.label == "DIGITAL"], yend = mean[resource == "loss" & condition.label == "DIGITAL"]),
               color = cbPalette[5], size = 1,
               arrow = arrow_double) +
  #H1c
  geom_segment(data = subset(study1.cortisol.df, condition.label == "DIGITAL"),
               aes( 
                 x = 1.5, xend = 1.5, 
                 y = max(study1.cortisol.df$mean) * 1.2, yend = mean[resource == "lack" & condition.label == "DIGITAL"]+0.0125),
               color = cbPalette[6], size = 0.75, 
               arrow = arrow_single) +
  geom_segment(data = subset(study1.cortisol.df, condition.label == "DIGITAL"),
               aes( 
                 x = 1.5, xend = 2.5, 
                 y = max(study1.cortisol.df$mean) * 1.2, yend = max(study1.cortisol.df$mean) * 1.2),
               color = cbPalette[6], size = 0.75) +
  geom_segment(data = subset(study1.cortisol.df, condition.label == "DIGITAL"),
               aes( 
                 x = 2.5, xend = 2.5, 
                 y = max(study1.cortisol.df$mean) * 1.2, yend = mean[resource == "loss" & condition.label == "DIGITAL"]+0.0125),
               color = cbPalette[6], size = 0.75, 
               arrow = arrow_single) +
  geom_text(data = subset(study1.cortisol.df, condition.label == "DIGITAL"),
            aes(x = 2, y = max(study1.cortisol.df$mean*1.1), 
                label = paste("H1c", sigstars.study1$h1c.cortisol)),
            color = cbPalette[6], size = 5, family = "latex") +
  
  #Annotations for TRADITIONAL
  geom_segment(data = subset(study1.cortisol.df, condition.label == "TRADITIONAL"),
               aes( 
                 x = 1.5, xend = 1.5, 
                 y = mean[resource == "lack" & condition.label == "TRADITIONAL"], yend = mean[resource == "gain" & condition.label == "TRADITIONAL"]),
               color = cbPalette[5], size = 1, 
               arrow = arrow_double) +
  geom_segment(data = subset(study1.cortisol.df, condition.label == "TRADITIONAL"),
               aes( 
                 x = 2.5, xend = 2.5, 
                 y = mean[resource == "gain" & condition.label == "TRADITIONAL"], yend = mean[resource == "loss" & condition.label == "TRADITIONAL"]),
               color = cbPalette[5], size = 1,
               arrow = arrow_double) +
  #H2c
  geom_segment(data = subset(study1.cortisol.df, condition.label == "TRADITIONAL"),
               aes( 
                 x = 1.5, xend = 1.5, 
                 y = max(study1.cortisol.df$mean) * 1.2, yend = mean[resource == "lack" & condition.label == "TRADITIONAL"]+0.0125),
               color = cbPalette[6], size = 0.75, 
               arrow = arrow_single) +
  geom_segment(data = subset(study1.cortisol.df, condition.label == "TRADITIONAL"),
               aes( 
                 x = 1.5, xend = 2.5, 
                 y = max(study1.cortisol.df$mean) * 1.2, yend = max(study1.cortisol.df$mean) * 1.2),
               color = cbPalette[6], size = 0.75) +
  geom_segment(data = subset(study1.cortisol.df, condition.label == "TRADITIONAL"),
               aes( 
                 x = 2.5, xend = 2.5, 
                 y = max(study1.cortisol.df$mean) * 1.2, yend = mean[resource == "loss" & condition.label == "TRADITIONAL"]+0.0125),
               color = cbPalette[6], size = 0.75, 
               arrow = arrow_single) +
  geom_text(data = subset(study1.cortisol.df, condition.label == "TRADITIONAL"),
            aes(x = 2, y = max(study1.cortisol.df$mean)*1.1, 
                label = paste("H2c", sigstars.study1$h2c.cortisol)),
            color = cbPalette[6], size = 5, family = "latex")

print(p.study1.cortisol.delta)


# Delta Cortisol
# Plot delta
p.study1.perceived.delta <-  
  ggplot(study1.perceived.df, aes(x = resource, y = mean, fill = condition.label)) + #
  geom_bar(stat = "identity", 
           color = cbPalette[4], 
           position = position_dodge(width = 0.9), 
           alpha = 0.5)  +
  scale_fill_manual(values = c("DIGITAL" = cbPalette[2], "TRADITIONAL" = cbPalette[2]), guide = "none") +
  
  geom_errorbar(aes(ymin = mean - std.err, ymax = mean + std.err), 
                width = 0.2, 
                position = position_dodge(width = 0.9)) +
  
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
  
  facet_grid(. ~ condition.label, switch = "x") +
  
  #Annotations for DIGITAL
  geom_segment(data = subset(study1.perceived.df, condition.label == "DIGITAL"),
               aes( 
                 x = 1.5, xend = 1.5, 
                 y = mean[resource == "lack" & condition.label == "DIGITAL"], yend = mean[resource == "gain" & condition.label == "DIGITAL"]),
               color = cbPalette[5], size = 1, 
               arrow = arrow_double) +
  geom_segment(data = subset(study1.perceived.df, condition.label == "DIGITAL"),
               aes( 
                 x = 2.5, xend = 2.5, 
                 y = mean[resource == "gain" & condition.label == "DIGITAL"], yend = mean[resource == "loss" & condition.label == "DIGITAL"]),
               color = cbPalette[5], size = 1,
               arrow = arrow_double) +
  #H1c
  geom_segment(data = subset(study1.perceived.df, condition.label == "DIGITAL"),
               aes( 
                 x = 1.5, xend = 1.5, 
                 y = max(study1.perceived.df$mean) * 1.3, yend = mean[resource == "lack" & condition.label == "DIGITAL"] * 1.05),
               color = cbPalette[6], size = 0.75, 
               arrow = arrow_single) +
  geom_segment(data = subset(study1.perceived.df, condition.label == "DIGITAL"),
               aes( 
                 x = 1.5, xend = 2.5, 
                 y = max(study1.perceived.df$mean) * 1.3, yend = max(study1.perceived.df$mean) * 1.3),
               color = cbPalette[6], size = 0.75) +
  geom_segment(data = subset(study1.perceived.df, condition.label == "DIGITAL"),
               aes( 
                 x = 2.5, xend = 2.5, 
                 y = max(study1.perceived.df$mean) * 1.3, yend = mean[resource == "loss" & condition.label == "DIGITAL"] * 1.2),
               color = cbPalette[6], size = 0.75, 
               arrow = arrow_single) +
  geom_text(data = subset(study1.perceived.df, condition.label == "DIGITAL"),
            aes(x = 2, y = max(study1.perceived.df$mean) * 1.5, 
                label = paste("H1c", sigstars.study1$h1c.perceived)),
            color = cbPalette[6], size = 5, family = "latex") +
  
  #Annotations for TRADITIONAL
  geom_segment(data = subset(study1.perceived.df, condition.label == "TRADITIONAL"),
               aes( 
                 x = 1.5, xend = 1.5, 
                 y = mean[resource == "lack" & condition.label == "TRADITIONAL"], yend = mean[resource == "gain" & condition.label == "TRADITIONAL"]),
               color = cbPalette[5], size = 1, 
               arrow = arrow_double) +
  geom_segment(data = subset(study1.perceived.df, condition.label == "TRADITIONAL"),
               aes( 
                 x = 2.5, xend = 2.5, 
                 y = mean[resource == "gain" & condition.label == "TRADITIONAL"], yend = mean[resource == "loss" & condition.label == "TRADITIONAL"]),
               color = cbPalette[5], size = 1,
               arrow = arrow_double) +
  #H1c
  geom_segment(data = subset(study1.perceived.df, condition.label == "TRADITIONAL"),
               aes( 
                 x = 1.5, xend = 1.5, 
                 y = max(study1.perceived.df$mean) * 1.3, yend = mean[resource == "lack" & condition.label == "TRADITIONAL"] * 1.05),
               color = cbPalette[6], size = 0.75, 
               arrow = arrow_single) +
  geom_segment(data = subset(study1.perceived.df, condition.label == "TRADITIONAL"),
               aes( 
                 x = 1.5, xend = 2.5, 
                 y = max(study1.perceived.df$mean) * 1.3, yend = max(study1.perceived.df$mean) * 1.3),
               color = cbPalette[6], size = 0.75) +
  geom_segment(data = subset(study1.perceived.df, condition.label == "TRADITIONAL"),
               aes( 
                 x = 2.5, xend = 2.5, 
                 y = max(study1.perceived.df$mean) * 1.3, yend = mean[resource == "loss" & condition.label == "TRADITIONAL"] * 1.2),
               color = cbPalette[6], size = 0.75, 
               arrow = arrow_single) +
  geom_text(data = subset(study1.perceived.df, condition.label == "TRADITIONAL"),
            aes(x = 2, y = max(study1.perceived.df$mean) * 1.5, 
                label = paste("H2c", sigstars.study1$h2c.perceived)),
            color = cbPalette[6], size = 5, family = "latex")

print(p.study1.perceived.delta)

# Save the plot to a PDF file
ggsave(file.path(artwork.dir, "p.study1.cortisol.mean.pdf"), plot = p.study1.cortisol.mean, width = 20, height = 20, units = "cm") 
ggsave(file.path(artwork.dir, "p.study1.perceived.mean.pdf"), plot = p.study1.perceived.delta, width = 20, height = 20, units = "cm")

ggsave(file.path(artwork.dir, "p.study1.cortisol.delta.pdf"), plot = p.study1.cortisol.delta, width = 20, height = 20, units = "cm") 
ggsave(file.path(artwork.dir, "p.study1.perceived.delta.pdf"), plot = p.study1.perceived.delta, width = 20, height = 20, units = "cm")

# Cortisol
# Plot delta
# 1. Reshape data
digital.df <- study1.final.df %>% filter(condition == "Digital")

digital.wide <- digital.df %>%
  select(participant.id, resource, AUCi.cortisol) %>%
  pivot_wider(names_from = resource, values_from = AUCi.cortisol)

# 2. Calculate deltas
digital.wide <- digital.wide %>%
  mutate(
    lack_minus_gain = lack - gain,
    loss_minus_gain = loss - gain
  )

# 3. Summarize
delta.summary <- digital.wide %>%
  summarize(
    mean_lack_minus_gain = mean(lack_minus_gain, na.rm = TRUE),
    se_lack_minus_gain = sd(lack_minus_gain, na.rm = TRUE) / sqrt(n()),
    
    mean_loss_minus_gain = mean(loss_minus_gain, na.rm = TRUE),
    se_loss_minus_gain = sd(loss_minus_gain, na.rm = TRUE) / sqrt(n())
  )

# 4. Prepare plotting dataframe
delta.plot.df <- tibble(
  contrast = c("Lack-Gain", "Loss-Gain"),
  mean = c(delta.summary$mean_lack_minus_gain, delta.summary$mean_loss_minus_gain),
  se = c(delta.summary$se_lack_minus_gain, delta.summary$se_loss_minus_gain)
)

# 5. Plot
p.delta <- ggplot(delta.plot.df, aes(x = contrast, y = mean)) +
  geom_bar(stat = "identity", fill = cbPalette[2], color = "black", alpha = 0.5) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) +
  labs(x = "", y = "Delta AUCi cortisol (lack-gain or loss-gain)") +
  theme_minimal() +
  theme(text = element_text(size = 24, family = "serif"))

print(p.delta)

