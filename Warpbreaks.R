library(tidyverse)
data("warpbreaks")

View(warpbreaks)
str(warpbreaks)

break_summary <- 
  warpbreaks |> 
    group_by(wool, tension) |>
    summarise(
      mean_breaks = mean(breaks),
      sd_breaks = sd(breaks)
    ) |>
  
    mutate(lower_bound = mean_breaks - sd_breaks,
           upper_bound = mean_breaks + sd_breaks)
  
ggplot(
  data = break_summary,
  aes(
    x = tension,
    y = mean_breaks,
    fill = wool)
  ) +
      geom_col(position = "dodge") +
      geom_errorbar(
        aes(
          ymin = lower_bound, 
          ymax = upper_bound
        ),
          position = position_dodge(width = 0.9),
          width = 0.2,
          colour = "#40382F",
          linewidth = 0.6
      ) +
  scale_fill_manual(
    name = "Wool type",
    values = c(
      "A" = "#D9A092",
      "B" = "#9AAA9A"
    ),
    labels = c(
      "A" = "Wool A",
      "B" = "Wool B"
    )
) +
  labs(
    title = "THE BREAKING POINT",
    subtitle = "Does tension matter more than wool type?",
    x = "Thread tension",
    y = "Mean number of breaks",
    caption = "Bars show mean ± 1 SD (n = 9 per group)."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(
      fill = "#F3EEE5", colour = NA
    ),
    panel.background = element_rect(
      fill = "#F3EEE5", colour = NA
    ),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(
      colour = "#D9D0C4", linewidth = 0.4
    ),
    axis.title = element_text(
      colour = "#40382F", face = "bold"
    ),
    axis.text = element_text(
      colour = "#51483F"
    ),
    plot.title = element_text(
      size = 22, face = "bold", colour = "#40382F"
    ),
    plot.subtitle = element_text(
      size = 12, colour = "#76695D"
    ),
    legend.position = "top",
    legend.title = element_text(
      face = "bold", colour = "#40382F"
    ),
    legend.text = element_text(
      colour = "#51483F"
    ),
    plot.caption = element_text(
      colour = "#76695D", size = 9
    )
  )  
