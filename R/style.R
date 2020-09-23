source('R/PNAD/commuting_pnad_0_libraries.R')

aop_style <- function() {
  font <- "Helvetica"
  
  ggplot2::theme(
    
    text = element_text(family = font, colour = "#808080", size = 10),
    
    # Titles
    # Font, size, type, colour, lineheight, margin, for the chart's title, subtitle, caption
    plot.title = ggtext::element_markdown(
      lineheight = 1.5, family = font, size = 11, colour = "#323232"
    ),
    plot.subtitle = ggtext::element_markdown(
      lineheight = 1.5, colour = "#575757", family = font, size = 9,
      margin = margin(t = 0., r = 0, b = 0.25, l = 0, unit = 'cm')
    ),
    plot.title.position = "plot",
    plot.caption = ggtext::element_markdown(
      lineheight = 1, family = font, size = 8, colour = "#808080", hjust = 0,
      margin = margin(t = 0.25, unit = 'cm')
    ),
    plot.caption.position = "plot",
    
    # Legend
    # Legend is set to be excluded. However, in case it is needed, the code below sets its configuration. May need aditional manual tweaking
    legend.position = "none",
    legend.background = ggplot2::element_blank(),
    legend.title = ggtext::element_markdown(size = 8, colour = "#575757"),
    #legend.text = ggtext::element_markdown(size = 10, colour = "#808080"),
    legend.key = element_blank(),
    
    # Axis
    # Formats axis text, ticks, line and titles. Axis titles are formated, but can be excluded with axis.title.x or y = element_blank()
    axis.text = element_markdown(size = 8, colour = '#808080'),
    axis.ticks = element_blank(),
    axis.line.x = element_line(size = 0.5, color = "grey"),
    axis.line.y = element_blank(),
    axis.title.y = element_markdown(
      size = 8, 
      margin = margin(r = 0.25, unit = 'cm'), 
      lineheight = 0.5,
      colour = "#575757",
      hjust = 1
    ),
    axis.title.x = element_markdown(
      size = 8, 
      margin = margin(t = 0.25, b = 0, unit = 'cm'), 
      lineheight = 0.5,
      colour = "#575757",
      hjust = 1
    ),
    
    # Panel
    # Format panel grid, border, spacing, background. Aditional manual tweking may be necessary
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.spacing.x = unit(0, "cm"),
    panel.spacing.y = unit(-0.2, "cm"),
    plot.background = ggplot2::element_rect(fill = NA),
    panel.background = ggplot2::element_rect(fill = NA),
    
    # Strip
    # Format strips
    strip.placement = "outside",
    strip.background = ggplot2::element_rect(fill = NA),
    strip.text = element_text(size = 8, face = "plain", colour = "#575757", hjust = 0),
    
    # Margin
    # Format plot.margin. Adjust if necessary
    plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")
    
  )
}
