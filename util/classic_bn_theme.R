# classic bn plot theme with no colors, lines delineated by types and serif font

# Lucas Watson

# load libraries ----------------------------------------------------------

# dependencies include showtext
library(showtext)

# add TNR
font_add(family = 'Times New Roman', regular = 'times.ttf')

# make font display pretty
showtext_opts(dpi = 300)
showtext_auto()

# function ----------------------------------------------------------------

classic_bn_theme <- function() {
  theme_bw() + 
    theme(
      axis.line = element_line(color = 'black', linewidth = 1.1), 
      axis.ticks = element_line(color = 'black', linewidth = 1), 
      panel.border = element_rect(fill = 'white', color = NA), 
      strip.background = element_rect(fill = 'white', color = NA),
      panel.grid = element_blank(),
      text = element_text(family = 'Times New Roman', size = 10), 
      title = element_text(face = 'bold', size = 12)
    )
}
