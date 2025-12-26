# Erstellung eines LinkedIn-Hintergrunds -------------------------------------------

library(tidyverse, quietly = T) # Data Cleaning, Pipes und Visualisierung
library(see)                    # Theme für Abbildung
library(haven)                  # Einlesen von dta-Dateien
library(ggridges)               # für geom_density_ridges()

# Einlesen des Datensatzes --------------------------------------------------

# GLES 2025, ZA10100
GLES2025 <- read_dta("GLES2025.dta") 

# Links-Rechts-Selbsteinstufung
table(GLES2025$q37) 

# Zweitstimme Bundestagswahl
table(GLES2025$q114ba) 


# Data Cleaning und Wrangling ---------------------------------------------

plot_gles <- GLES2025 %>% 
  # Umbenennung der Variablen
  rename(vote = q114ba, 
         lr = q37) %>% 
  select(lr, vote) %>% 
  # Umkodierungen
  mutate(vote = case_when(vote == 4 ~ "SPD", 
                          vote == 1 ~ "CDU/CSU",
                          vote == 6 ~ "Die Grünen",
                          vote == 5 ~ "FDP",
                          vote == 322 ~ "AfD",
                          vote == 7 ~ "Die Linke",
                          vote == 392 ~ "BSW")) %>% 
  # Entfernen fehlender Werte
  filter(lr > 0, 
         vote != "NA") %>% 
  # Berechnung der Mittelwerte (Links-Rechts-Selbsteinstufung nach Parteiwahl)
  group_by(vote) %>% 
  mutate(mean_lr = mean(lr)) %>% 
  ungroup() 



# Festlegung der Parteifarben
party_colors <- c(
  "SPD"        = "#E3000F",
  "CDU/CSU"    = "#000000", 
  "Die Grünen" = "#46962b",
  "FDP"        = "#ffed00",
  "AfD"        = "#009ee0",
  "Die Linke"  = "#be3075",
  "BSW"        = "#7a2b58")



# Dark Mode Plot ----------------------------------------------------------

plot_gles %>% 
  mutate(vote = fct_reorder(vote, mean_lr)) %>% 
  ggplot(aes(x = lr, y = vote, fill = vote)) +
  geom_density_ridges(
    scale = 3, 
    rel_min_height = 0.01,
    color = "white",       
    linewidth = 0.6,       
    alpha = 0.9,           
    quantile_lines = TRUE, 
    quantile_fun = mean,   
    vline_color = "white",
    vline_width = 0.8       
  ) +
  scale_fill_manual(values = party_colors) +
  labs(
    title = 'Left-Right Self-Identification by Party Vote',
    caption = "Source: GLES 2025, ZA10100 | Noah Schulz",
    x = NULL,
    y = NULL
  ) +
  theme_abyss() + 
  scale_x_continuous(
    expand = c(0.01, 0), 
    breaks = c(1, 6, 11), 
    labels = c("Left\n(1)", "Center\n(6)", "Right\n(11)")
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "white"),
    axis.text.y = element_text(size = 11, face = "bold", color = "white"),
    axis.text.x = element_text(size = 11, face = "bold", color = "white"),
    panel.grid.major.x = element_line(color = "gray20", linewidth = 0.1),
    plot.caption = element_text(size = 9, color = "white",face = "bold",
                                hjust = 1, 
                                vjust = -3),
    plot.margin = margin(t = 10, r = 20, b = 20, l = 20)
  )



# Plot mit Navy Blue Hintergrund ------------------------------------------


plot_gles %>% 
  mutate(vote = fct_reorder(vote, mean_lr)) %>% 
  ggplot(aes(x = lr, y = vote, fill = vote)) +
  geom_density_ridges(
    scale = 3, 
    rel_min_height = 0.01,
    color = "white",       
    linewidth = 0.6,       
    alpha = 0.9,           
    quantile_lines = T, 
    quantile_fun = mean,   
    vline_color = "white",
    vline_width = 0.8) +
  scale_fill_manual(values = party_colors) +
  labs(
    title = 'Left-Right Self-Identification by Party Vote',
    caption = "Source: GLES 2025, ZA10100 | Noah Schulz",
    x = NULL,
    y = NULL
  ) +
  theme_abyss() + 
  scale_x_continuous(
    expand = c(0.01, 0), 
    breaks = c(1, 6, 11), 
    labels = c("Left\n(1)", "Center\n(6)", "Right\n(11)")
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "white"),
    axis.text.y = element_text(size = 11, face = "bold", color = "white"),
    axis.text.x = element_text(size = 11, face = "bold", color = "white"),
    panel.grid.major.x = element_line(color = "gray20", linewidth = 0.1),
    plot.caption = element_text(size = 9, color = "white",face = "bold",
                                hjust = 1, 
                                vjust = -3),
    panel.background = element_rect(fill = "#002b5e", color = NA),
    plot.background = element_rect(fill = "#002b5e", color = NA),
    plot.margin = margin(t = 10, r = 20, b = 20, l = 20)
  )



# Plot mit Light Background -----------------------------------------


# Individualisierung der Linienfarben
plot_gles_light <- plot_gles %>% 
  mutate(
    vote = fct_reorder(vote, mean_lr),
    # vline und Außenlinie nur bei Union weiß
    line_col = ifelse(vote == "CDU/CSU", "white", "black"),
    color_cdu = ifelse(vote =="CDU/CSU", "white", "grey80"))


plot_gles_light %>% 
  ggplot(aes(x = lr, y = vote, fill = vote)) +
  geom_density_ridges(
    aes(vline_color = line_col,
        color = color_cdu), 
    scale = 3, 
    rel_min_height = 0.01,
    linewidth = 0.5,       
    alpha = 0.9,           
    quantile_lines = TRUE, 
    quantile_fun = mean,   
    vline_width = 0.8       
  ) +
  scale_discrete_identity(aesthetics = c("vline_color", "color")) + 
  
  scale_fill_manual(values = party_colors) +
  labs(
    title = 'Left-Right Self-Identification by Party Vote',
    caption = "Source: GLES 2025, ZA10100 | Noah Schulz",
    x = NULL,
    y = NULL
  ) +
  theme_modern() + 
  scale_x_continuous(
    expand = c(0.01, 0), 
    breaks = c(1, 6, 11), 
    labels = c("Left\n(1)", "Center\n(6)", "Right\n(11)")
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "#222222"),
    axis.text.y = element_text(size = 11, face = "bold", color = "#222222"),
    axis.text.x = element_text(size = 11, face = "bold", color = "#222222"),
    panel.grid.major.x = element_line(color = "gray90", linewidth = .2),
    panel.grid.major.y = element_line(color = "gray70", linewidth = .3),
    plot.caption = element_text(size = 9, 
                                color = "black", 
                                face = "bold",
                                hjust = 1, 
                                vjust = -3),
    plot.margin = margin(t = 10, r = 20, b = 10, l = 20),
    axis.line.x = element_line(color = "black"))


