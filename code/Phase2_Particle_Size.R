library(readxl)
library(dplyr)
library(ggplot2)
library(forcats)

size_df <- read_excel(
  "Waring_Table3_Phase2_Data.xlsx",
  sheet = "Table3_Data"
)

size_df <- size_df %>%
  mutate(
    `Air Cleaner` = factor(
      `Air Cleaner`,
      levels = rev(c(
        "IG 1", "IG 3", "IG 4",
        "IG 5A", "IG 5B"
      ))
    )
  )

ggplot(
  size_df,
  aes(y = `Air Cleaner`)
) +
  
  # Significant particle-size range
  geom_segment(
    aes(
      x = `Net Formation Min (nm)`,
      xend = `Net Formation Max (nm)`,
      yend = `Air Cleaner`,
      color = `Air Cleaner`
    ),
    linewidth = 5,
    lineend = "round"
  ) +
  
  # Starting point
  geom_point(
    aes(
      x = `Net Formation Min (nm)`,
      color = `Air Cleaner`
    ),
    size = 3
  ) +
  
  # Ending point
  geom_point(
    aes(
      x = `Net Formation Max (nm)`,
      color = `Air Cleaner`
    ),
    size = 3
  ) +
  
  # Range labels
  geom_text(
    aes(
      x = `Net Formation Max (nm)` + 5,
      label = paste0(
        `Net Formation Min (nm)`,
        "–",
        `Net Formation Max (nm)`,
        " nm"
      )
    ),
    hjust = 0,
    size = 4.5,
    fontface = "bold",
    show.legend = FALSE
  ) +
  
  scale_x_continuous(
    limits = c(0, 190),
    breaks = c(0, 50, 100, 150)
  ) +
  
  labs(
    title = "Particle-size range with significant net formation",
    subtitle = "AC/AF compared with BG (α < 0.05)",
    x = "Particle diameter (nm)",
    y = NULL
  ) +
  
  guides(color = "none") +
  
  theme_classic(base_size = 15) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 18
    ),
    
    plot.subtitle = element_text(
      size = 12,
      color = "grey35"
    ),
    
    axis.text.y = element_text(
      face = "bold",
      size = 13
    ),
    
    axis.text.x = element_text(
      size = 12
    ),
    
    axis.title.x = element_text(
      face = "bold",
      size = 13
    )
  )



