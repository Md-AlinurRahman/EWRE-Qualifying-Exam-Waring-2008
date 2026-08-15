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
