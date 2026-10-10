library(tidyverse)
library(psych)
library(patchwork)
library(janitor)
library(ggthemes)
library(gt)


#gganimate
#ggthemes
#ggsignif
#ggpubr
Complete_ADHD_Knowledge <- read_csv("./Complete_ADHD_Knowledge.csv", na = "NA")

Complete_ADHD_Knowledge <- Complete_ADHD_Knowledge |>   #removing people who didn't answer anything
  filter(Knowledge_Total > 0)

#Removing countries with n < 20 for each role
Complete_Knowledge_high_n <- Complete_ADHD_Knowledge |> 
  group_by(Country_Residence) |> 
  filter(!(Role %in% "teacher" & sum(Role %in% "teacher") < 20),
         !(Role %in% "parent"  & sum(Role %in% "parent")  < 20)) |> 
  ungroup()

#Remember to add relevant footnotes about the "inconsistent" Canada percentages

#Academic Visualizations

# Hmisc must be installed once for mean_cl_normal (install.packages("Hmisc")), but you don't need to load it

# Changes applied to every figure (marked CHANGED below):
#  - light grey bars (grey75) so the black error bars are visible on both halves
#  - thinner error bar lines (linewidth) and narrower caps (width)
#  - the same fixed y-range (0-80) everywhere, so the error bars fit and the figures compare
#  - larger axis text


# ---------- FIGURE 1: T&T parents vs. teachers ----------
fig1 <- Complete_Knowledge_high_n |> 
  filter(Country_Residence == "Trinidad & Tobago") |> 
  ggplot(aes(x = Role, y = Knowledge_Corr_Percent)) +
  geom_bar(stat = "summary", fun = "mean",
           fill = "grey75", width = 0.6) +                  # CHANGED: light fill, narrower bars
  stat_summary(geom = "errorbar", fun.data = mean_cl_normal,
               width = 0.2, linewidth = 0.6) +              # CHANGED: thinner lines, smaller caps
  scale_x_discrete(labels = c(parent = "Parent", teacher = "Teacher")) +
  coord_cartesian(ylim = c(0, 80)) +                        # CHANGED: fixed 0-80 range
  theme_hc() +
  labs(title = "ADHD Knowledge: Trinidad & Tobago",
       x = NULL, y = "ADHD Knowledge: % Correct") +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 13),                  # CHANGED: no 45-degree rotation, bigger text
    axis.text.y = element_text(size = 11)
  )


# ---------- FIGURE 2: T&T with Canada as reference ----------
fig2 <- Complete_Knowledge_high_n |> 
  filter(Country_Residence %in% c("Trinidad & Tobago", "Canada"),
         Data_source == "Ronald") |>                        # NOTE: Canada's means differ slightly from Figs 3-4 because of this filter
  mutate(Country_Residence = factor(Country_Residence,
                                    levels = c("Trinidad & Tobago", "Canada"))) |> 
  ggplot(aes(x = Country_Residence, y = Knowledge_Corr_Percent)) +
  geom_bar(stat = "summary", fun = "mean",
           fill = "grey75", width = 0.7) +                  # CHANGED: light fill
  stat_summary(geom = "errorbar", fun.data = mean_cl_normal,
               width = 0.2, linewidth = 0.6) +              # CHANGED: thinner lines, smaller caps
  facet_wrap(~Role,
             labeller = labeller(Role = c(parent = "Parent", teacher = "Teacher"))) +
  coord_cartesian(ylim = c(0, 80)) +                        # CHANGED: fixed 0-80 range
  theme_hc() +
  labs(title = "ADHD Knowledge: Trinidad & Tobago and Canada",
       x = NULL, y = "ADHD Knowledge: % Correct") +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 12),                  # CHANGED: no rotation (two bars per panel)
    axis.text.y = element_text(size = 11),
    strip.background = element_blank(),
    strip.text = element_text(size = 14, face = "bold"),
    panel.spacing = unit(1.5, "lines")                      # CHANGED: more space between panels
  )


# ---------- FIGURE 3: Parents, all countries ----------
fig3 <- Complete_Knowledge_high_n |> 
  filter(Role == "parent") |> 
  mutate(Country_Residence = fct_reorder(Country_Residence, Knowledge_Corr_Percent,
                                         .fun = mean, na.rm = TRUE)) |> 
  ggplot(aes(x = Country_Residence, y = Knowledge_Corr_Percent)) +
  geom_bar(stat = "summary", fun = "mean",
           fill = "grey75", width = 0.8) +                  # CHANGED: light fill
  stat_summary(geom = "errorbar", fun.data = mean_cl_normal,
               width = 0.3, linewidth = 0.6) +              # CHANGED: thinner lines
  coord_cartesian(ylim = c(0, 80)) +                        # CHANGED: axis now covers the error bars
  theme_hc() +
  labs(title = "Parent ADHD Knowledge",
       x = NULL, y = "ADHD Knowledge: % Correct") +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 12),  # CHANGED: bigger labels
    axis.text.y = element_text(size = 11)
  )


# ---------- FIGURE 4: Teachers, all countries ----------
fig4 <- Complete_Knowledge_high_n |> 
  filter(Role == "teacher") |> 
  mutate(Country_Residence = fct_reorder(Country_Residence, Knowledge_Corr_Percent,
                                         .fun = mean, na.rm = TRUE)) |> 
  ggplot(aes(x = Country_Residence, y = Knowledge_Corr_Percent)) +
  geom_bar(stat = "summary", fun = "mean",
           fill = "grey75", width = 0.8) +                  # CHANGED: light fill
  stat_summary(geom = "errorbar", fun.data = mean_cl_normal,
               width = 0.3, linewidth = 0.6) +              # CHANGED: thinner lines
  coord_cartesian(ylim = c(0, 80)) +                        # CHANGED: same range as Fig 3
  theme_hc() +
  labs(title = "Teacher ADHD Knowledge",
       x = NULL, y = "ADHD Knowledge: % Correct") +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 12),  # CHANGED: bigger labels
    axis.text.y = element_text(size = 11)
  )


# ---------- FIGURE 5: Parents and teachers side by side ----------
# Reuses fig3 and fig4 and adds only what differs for the combined version.

fig3_comb <- fig3 +
  labs(title = "Parents") +                                 # CHANGED: shorter panel title (fixes the cut-off title)
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.margin = margin(5.5, 5.5, 5.5, 25)                 # CHANGED: extra left margin so "Trinidad & Tobago" isn't clipped
  )

fig4_comb <- fig4 +
  labs(title = "Teachers", y = NULL) +                      # CHANGED: no y title (shared with parent panel)
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.y = element_blank(),                          # CHANGED: hide y-axis; both panels share 0-80
    axis.ticks.y = element_blank()
  )

fig5 <- (fig3_comb | fig4_comb) +                           # CHANGED: parentheses so the settings apply to BOTH plots
  plot_layout(widths = c(14.5, 8)) +                        # CHANGED: panel widths ~ number of bars (13 + y-axis space, 8)
  plot_annotation(
    title = "ADHD Knowledge by Country",
    theme = theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5))
  )


fig1
fig2
fig5


means_tab <- Complete_Knowledge_high_n |> 
  mutate(Country_Residence = fct_reorder(Country_Residence, Knowledge_Corr_Percent,
                                         .fun = mean, na.rm = TRUE)) |> 
  tabyl(Country_Residence, Role) |> 
  adorn_totals(where = c("col", "row"))     

means_gt <- gt(means_tab)   #use describe() to generate other stats like se to append to final gt table. 

#Non-academic Visualizations

#T&T parents and teachers as the initial focus
#Assign after polishing up

pal <- c("Trinidad & Tobago" = "#CE1126",
         "Canada"            = "#333333",
         "Other"             = "#BDBDBD")

p_TT_only <- Complete_Knowledge_high_n |> 
  filter(Country_Residence == "Trinidad & Tobago") |> 
  ggplot(aes(x = Role, y = Knowledge_Corr_Percent)) +
  geom_bar(stat = "summary", fun = "mean",
           fill = pal[["Trinidad & Tobago"]], width = 0.6) +
  stat_summary(
    geom = "text", fun = "mean",
    aes(label = sprintf("%1.0f%%", after_stat(y))),
    vjust = -0.5, size = 5, fontface = "bold"
  ) +
  scale_x_discrete(labels = c(parent = "Parent", teacher = "Teacher")) +
  coord_cartesian(ylim = c(0, 80), clip = "off") +
  theme_hc() +
  labs(title = "ADHD Knowledge: Trinidad & Tobago",
       x = NULL, y = "ADHD Knowledge: % Correct") +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 11)
  )

p_TT_only

#T&T with Canada as reference point
#Comparison with Canada (original dataset)
p_TT_Can <- Complete_Knowledge_high_n |> 
  filter(Country_Residence %in% c("Trinidad & Tobago", "Canada"),
         Data_source == "Ronald") |>
  mutate(Country_Residence = factor(Country_Residence,
                                    levels = c("Trinidad & Tobago", "Canada"))) |> 
  ggplot(aes(x = Country_Residence, y = Knowledge_Corr_Percent,
             fill = Country_Residence)) +
  geom_bar(stat = "summary", fun = "mean", width = 0.7) +
  stat_summary(
    geom = "text", fun = "mean",
    aes(label = sprintf("%1.0f%%", after_stat(y))),
    vjust = -0.5, size = 5, fontface = "bold"
  ) +
  facet_wrap(~Role,
             labeller = labeller(Role = c(parent = "Parent", teacher = "Teacher"))) +
  scale_fill_manual(values = pal, guide = "none") +
  coord_cartesian(ylim = c(0, 80), clip = "off") +
  theme_hc() +
  labs(title = "ADHD Knowledge: Trinidad & Tobago and Canada",
       x = NULL, y = "ADHD Knowledge: % Correct") +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 11),
    strip.background = element_blank(),
    strip.text = element_text(size = 14, face = "bold"),
    panel.spacing = unit(1.5, "lines")
  )

p_TT_Can

#Need to highlight only T&T and Canada
highlight <- c("Trinidad & Tobago", "Canada")



#All countries parent, teacher, then combined with patchwork


#Style suggestions for all-country figures from Claude


#Parent version
p_parent <- Complete_Knowledge_high_n |> 
  filter(Role == "parent") |> 
  mutate(
    Country_Residence = fct_reorder(Country_Residence, Knowledge_Corr_Percent,
                                    .fun = mean, na.rm = TRUE),
    Highlight = if_else(Country_Residence %in% highlight,
                        as.character(Country_Residence), "Other")
  ) |> 
  ggplot(aes(x = Country_Residence, y = Knowledge_Corr_Percent, fill = Highlight)) +
  geom_bar(stat = "summary", fun = "mean", width = 0.8) +
  stat_summary(
    data = ~ filter(.x, Highlight != "Other"),
    geom = "text", fun = "mean",
    aes(label = sprintf("%1.0f%%", after_stat(y))),
    vjust = -0.5, size = 4.5, fontface = "bold"
  ) +
  scale_fill_manual(values = pal, guide = "none") +
  coord_cartesian(ylim = c(0, 80), clip = "off") +
  theme_hc() +
  labs(title = "Parent ADHD Knowledge", x = NULL, y = "ADHD Knowledge: % Correct") +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 12),
    axis.text.y = element_text(size = 11)
  )
p_parent

#Teacher version
p_teacher <- Complete_Knowledge_high_n |> 
  filter(Role == "teacher") |> 
  mutate(
    Country_Residence = fct_reorder(Country_Residence, Knowledge_Corr_Percent,
                                    .fun = mean, na.rm = TRUE),
    Highlight = if_else(Country_Residence %in% highlight,
                        as.character(Country_Residence), "Other")
  ) |> 
  ggplot(aes(x = Country_Residence, y = Knowledge_Corr_Percent, fill = Highlight)) +
  geom_bar(stat = "summary", fun = "mean", width = 0.8) +
  stat_summary(
    data = ~ filter(.x, Highlight != "Other"),
    geom = "text", fun = "mean",
    aes(label = sprintf("%1.0f%%", after_stat(y))),
    vjust = -0.5, size = 4.5, fontface = "bold"
  ) +
  scale_fill_manual(values = pal, guide = "none") +
  coord_cartesian(ylim = c(0, 80), clip = "off") +
  theme_hc() +
  labs(title = "Teacher ADHD Knowledge", x = NULL, y = NULL) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 12),
    axis.text.y = element_blank(),    # shared y-axis: only the parent panel shows it
    axis.ticks.y = element_blank()
  )
p_teacher

#Side by side
p_par_teach <- (p_parent | p_teacher) +
  plot_layout(widths = c(14.5, 8)) + 
  plot_annotation(title = "ADHD Knowledge by Country", 
                  theme = theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5)))

p_par_teach

ggsave("adhd_knowledge_side_by_side.png", p_par_teach,
       width = 14, height = 6.5, dpi = 300)
