library(readxl)
library(dplyr)
library(ggplot2)

df <- read_excel(
  "Waring_Table3_Phase2_Data.xlsx",
  sheet = "Long_Format"
)

# Set experimental order
df <- df %>%
  mutate(
    Condition = factor(
      Condition,
      levels = c("BG", "AC", "AC/AF", "AF")
    )
  )

# Colors for ion generators
ig_colors <- c(
  "IG 1"  = "#1565C0",
  "IG 3"  = "#2E7D32",
  "IG 4"  = "#F57C00",
  "IG 5A" = "#7B1FA2",
  "IG 5B" = "#C62828"
)





p_pm <- ggplot(
  df %>% filter(!is.na(`PM (#/cm3)`)),
  aes(
    x = Condition,
    y = `PM (#/cm3)`,
    group = `Air Cleaner`,
    color = `Air Cleaner`
  )
) +
  
  geom_line(
    linewidth = 1.2
  ) +
  
  geom_point(
    size = 4
  ) +
  
  scale_y_log10(
    breaks = c(100, 200, 500, 1000, 2000, 3000)
  ) +
  
  scale_color_manual(
    values = ig_colors
  ) +
  
  labs(
    title = "Particle concentration",
    subtitle = "Steady-state total particle number (4.61–157 nm)",
    x = NULL,
    y = expression(
      "Particle concentration (#/cm"^3*")"
    ),
    color = NULL
  ) +
  
  theme_classic(base_size = 16) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 20
    ),
    plot.subtitle = element_text(
      size = 12,
      color = "grey35"
    ),
    axis.text = element_text(
      size = 13
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 14
    ),
    legend.position = "top"
  )

p_pm




p_o3 <- ggplot(
  df %>% filter(!is.na(`O3 (ppb)`)),
  aes(
    x = Condition,
    y = `O3 (ppb)`,
    group = `Air Cleaner`,
    color = `Air Cleaner`
  )
) +
  
  geom_line(
    linewidth = 1.2
  ) +
  
  geom_point(
    size = 4
  ) +
  
  scale_color_manual(
    values = ig_colors
  ) +
  
  labs(
    title = expression(bold(O[3]~" concentration")),
    subtitle = "Steady-state chamber ozone",
    x = NULL,
    y = expression(O[3]~"(ppb)"),
    color = NULL
  ) +
  
  theme_classic(base_size = 16) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 20
    ),
    plot.subtitle = element_text(
      size = 12,
      color = "grey35"
    ),
    axis.text = element_text(
      size = 13
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 14
    ),
    legend.position = "none"
  )

p_o3




library(patchwork)

final_plot <- p_pm + p_o3 +
  plot_annotation(
    title = "Ion generators promoted secondary particle formation in the presence of terpenes"
  ) &
  theme(
    plot.title = element_text(
      face = "bold"
    )
  )

final_plot
