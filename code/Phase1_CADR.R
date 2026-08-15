library(readxl)
library(dplyr)
library(ggplot2)
library(forcats)
library(grid)

# -----------------------------
# 1. Read data
# -----------------------------

df <- read_excel(
  "Waring_Table1_CADR_Data.xlsx",
  sheet = "Table1_Data"
)

# -----------------------------
# 2. Prepare data
# -----------------------------

df <- df %>%
  mutate(
    Cleaner_Type = case_when(
      grepl("HEPA", `Air Cleaner`) ~ "HEPA",
      `Air Cleaner` == "ESP" ~ "Electrostatic precipitator",
      TRUE ~ "Ion generator"
    ),
    
    # Arrange from highest to lowest CADR
    `Air Cleaner` = fct_reorder(
      `Air Cleaner`,
      `CADR Mean (m³/h)`
    ),
    
    # Text for labels
    CADR_label = paste0(
      `CADR Mean (m³/h)`,
      " ± ",
      `CADR SD (m³/h)`
    )
  )

# -----------------------------
# 3. Colors and shapes
# -----------------------------

cleaner_colors <- c(
  "Electrostatic precipitator" = "#1565C0",
  "HEPA" = "#2E9B3F",
  "Ion generator" = "#7B1FA2"
)

cleaner_shapes <- c(
  "Electrostatic precipitator" = 16,
  "HEPA" = 17,
  "Ion generator" = 15
)

# -----------------------------
# 4. Create plot
# -----------------------------

p <- ggplot(
  df,
  aes(
    x = `CADR Mean (m³/h)`,
    y = `Air Cleaner`,
    color = Cleaner_Type,
    shape = Cleaner_Type
  )
) +
  
  # Light horizontal reference lines
  geom_hline(
    yintercept = 1:5,
    color = "grey85",
    linewidth = 0.45,
    linetype = "dashed"
  ) +
  
  # Mean ± 1 SD
  geom_errorbarh(
    aes(
      xmin = `CADR Mean (m³/h)` - `CADR SD (m³/h)`,
      xmax = `CADR Mean (m³/h)` + `CADR SD (m³/h)`
    ),
    height = 0.10,
    linewidth = 1.15
  ) +
  
  # Mean
  geom_point(
    size = 4.8
  ) +
  
  # ------------------------------------------------
# FIXED VALUE COLUMN
# ------------------------------------------------

geom_text(
  aes(
    x = 445,
    label = CADR_label,
    color = Cleaner_Type
  ),
  hjust = 1,
  size = 5.2,
  fontface = "bold",
  show.legend = FALSE
) +
  
  scale_color_manual(
    values = cleaner_colors
  ) +
  
  scale_shape_manual(
    values = cleaner_shapes
  ) +
  
  # Plot data only to ~400, but reserve room for labels
  scale_x_continuous(
    breaks = seq(0, 400, 100),
    limits = c(0, 450),
    expand = c(0, 0)
  ) +
  
  labs(
    x = expression("Mean size-resolved CADR (m"^3*"/h)"),
    y = NULL,
    color = NULL,
    shape = NULL
  ) +
  
  theme_classic(base_size = 16) +
  
  theme(
    
    # Y-axis device names
    axis.text.y = element_text(
      size = 16,
      face = "bold",
      color = "grey25",
      margin = margin(r = 8)
    ),
    
    # X-axis
    axis.text.x = element_text(
      size = 13,
      color = "grey25"
    ),
    
    axis.title.x = element_text(
      size = 16,
      face = "bold",
      margin = margin(t = 8)
    ),
    
    # Axis lines
    axis.line = element_line(
      linewidth = 0.8,
      color = "black"
    ),
    
    axis.ticks = element_line(
      linewidth = 0.7
    ),
    
    # Legend
    legend.position = "top",
    
    legend.direction = "horizontal",
    
    legend.text = element_text(
      size = 12
    ),
    
    legend.key.width = unit(0.8, "cm"),
    
    legend.spacing.x = unit(0.25, "cm"),
    
    # Give figure breathing room
    plot.margin = margin(
      t = 8,
      r = 15,
      b = 8,
      l = 8
    )
  )

p





