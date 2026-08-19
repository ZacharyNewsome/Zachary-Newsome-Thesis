# =========================================================
# CHAPTER 2 – EXPERIMENT 1
# FEMALE HOST SELECTION BEHAVIOR
#
# Statistical analyses and figure generation for
# overwintered and first-generation females in 2025 and 2026.
# =========================================================


# =========================================================
# 1. REQUIRED PACKAGES
# =========================================================

# Install required packages if they are not already installed.
# These commands only need to be run once and are therefore
# commented out by default.

# install.packages("tidyverse")
# install.packages("patchwork")


# Load required packages.

library(tidyverse)
library(patchwork)

# =========================================================
# 2. DEFINE COMMON TREATMENT ORDERS AND FIGURE SETTINGS
# =========================================================

# Treatment order used for chi-square analyses.
choice_treatment_order <- c(
  "Low",
  "Medium",
  "High"
)

# Treatment order used in the pie-chart figure.
figure_treatment_order <- c(
  "High",
  "Medium",
  "Low",
  "No Choice"
)

# Grayscale fills used in the final figure.
treatment_fills <- c(
  "High" = "grey45",
  "Medium" = "grey75",
  "Low" = "grey95",
  "No Choice" = "grey20"
)


# =========================================================
# 3. IMPORT DATA
# =========================================================

ow_2025 <- read.csv(
  "SP25 Obj 1 Ethovision Data for R csv.csv"
)

fg_2025 <- read.csv(
  "SU25 First Gen Data for R.csv"
)

ow_2026 <- read.csv(
  "SP26 OW Ethovision Data.csv"
)

fg_2026 <- read.csv(
  "SP26 First Gen Ethovision Data.csv"
)


# =========================================================
# 4. 2025 OVERWINTERED FEMALES – CHI-SQUARE TEST
# =========================================================

# Retain only females that made a treatment choice.

ow_2025_choice <- ow_2025 %>%
  filter(
    Made_Choice == "Yes"
  ) %>%
  mutate(
    Chosen_Treatment = factor(
      Chosen_Treatment,
      levels = choice_treatment_order
    )
  )

# Count selections among Low, Medium, and High treatments.

ow_2025_counts <- table(
  ow_2025_choice$Chosen_Treatment
)

ow_2025_counts

# Chi-square goodness-of-fit test.
# Null hypothesis: females select each of the three
# moisture treatments with equal probability.

ow_2025_chisq <- chisq.test(
  ow_2025_counts,
  p = rep(
    1 / length(choice_treatment_order),
    length(choice_treatment_order)
  )
)

ow_2025_chisq


# =========================================================
# 5. 2025 FIRST-GENERATION FEMALES – CHI-SQUARE TEST
# =========================================================

fg_2025_choice <- fg_2025 %>%
  filter(
    Made_Choice == "Yes"
  ) %>%
  mutate(
    Chosen_Treatment = factor(
      Chosen_Treatment,
      levels = choice_treatment_order
    )
  )

fg_2025_counts <- table(
  fg_2025_choice$Chosen_Treatment
)

fg_2025_counts

fg_2025_chisq <- chisq.test(
  fg_2025_counts,
  p = rep(
    1 / length(choice_treatment_order),
    length(choice_treatment_order)
  )
)

fg_2025_chisq


# =========================================================
# 6. 2026 OVERWINTERED FEMALES – CHI-SQUARE TEST
# =========================================================

ow_2026_choice <- ow_2026 %>%
  filter(
    Made_Choice == "Yes"
  ) %>%
  mutate(
    Chosen_Treatment = factor(
      Chosen_Treatment,
      levels = choice_treatment_order
    )
  )

ow_2026_counts <- table(
  ow_2026_choice$Chosen_Treatment
)

ow_2026_counts

ow_2026_chisq <- chisq.test(
  ow_2026_counts,
  p = rep(
    1 / length(choice_treatment_order),
    length(choice_treatment_order)
  )
)

ow_2026_chisq


# =========================================================
# 7. 2026 FIRST-GENERATION FEMALES – CHI-SQUARE TEST
# =========================================================

fg_2026_choice <- fg_2026 %>%
  filter(
    Made_Choice == "Yes"
  ) %>%
  mutate(
    Chosen_Treatment = factor(
      Chosen_Treatment,
      levels = choice_treatment_order
    )
  )

fg_2026_counts <- table(
  fg_2026_choice$Chosen_Treatment
)

fg_2026_counts

fg_2026_chisq <- chisq.test(
  fg_2026_counts,
  p = rep(
    1 / length(choice_treatment_order),
    length(choice_treatment_order)
  )
)

fg_2026_chisq


# =========================================================
# 8. PREPARE DATA FOR FIGURE
# =========================================================

ow_2025$Chosen_Treatment <- factor(
  ow_2025$Chosen_Treatment,
  levels = figure_treatment_order
)

fg_2025$Chosen_Treatment <- factor(
  fg_2025$Chosen_Treatment,
  levels = figure_treatment_order
)

ow_2026$Chosen_Treatment <- factor(
  ow_2026$Chosen_Treatment,
  levels = figure_treatment_order
)

fg_2026$Chosen_Treatment <- factor(
  fg_2026$Chosen_Treatment,
  levels = figure_treatment_order
)


# =========================================================
# 9. SUMMARIZE 2025 OVERWINTERED FEMALES
# =========================================================

summary_2025_OW <- ow_2025 %>%
  count(
    Chosen_Treatment,
    .drop = FALSE
  ) %>%
  mutate(
    perc = 100 * n / sum(n),
    
    label = if_else(
      n > 0,
      paste0(
        round(
          perc,
          1
        ),
        "%"
      ),
      ""
    ),
    
    label_color = if_else(
      Chosen_Treatment == "No Choice",
      "white",
      "black"
    )
  )


# =========================================================
# 10. SUMMARIZE 2025 FIRST-GENERATION FEMALES
# =========================================================

summary_2025_FG <- fg_2025 %>%
  count(
    Chosen_Treatment,
    .drop = FALSE
  ) %>%
  mutate(
    perc = 100 * n / sum(n),
    
    label = if_else(
      n > 0,
      paste0(
        round(
          perc,
          1
        ),
        "%"
      ),
      ""
    ),
    
    label_color = if_else(
      Chosen_Treatment == "No Choice",
      "white",
      "black"
    )
  )


# =========================================================
# 11. SUMMARIZE 2026 OVERWINTERED FEMALES
# =========================================================

summary_2026_OW <- ow_2026 %>%
  count(
    Chosen_Treatment,
    .drop = FALSE
  ) %>%
  mutate(
    perc = 100 * n / sum(n),
    
    label = if_else(
      n > 0,
      paste0(
        round(
          perc,
          1
        ),
        "%"
      ),
      ""
    ),
    
    label_color = if_else(
      Chosen_Treatment == "No Choice",
      "white",
      "black"
    )
  )


# =========================================================
# 12. SUMMARIZE 2026 FIRST-GENERATION FEMALES
# =========================================================

summary_2026_FG <- fg_2026 %>%
  count(
    Chosen_Treatment,
    .drop = FALSE
  ) %>%
  mutate(
    perc = 100 * n / sum(n),
    
    label = if_else(
      n > 0,
      paste0(
        round(
          perc,
          1
        ),
        "%"
      ),
      ""
    ),
    
    label_color = if_else(
      Chosen_Treatment == "No Choice",
      "white",
      "black"
    )
  )


# =========================================================
# 13. FUNCTION FOR MAKING EACH PIE CHART
# =========================================================

make_pie_chart <- function(data) {
  
  ggplot(
    data,
    aes(
      x = "",
      y = n,
      fill = Chosen_Treatment
    )
  ) +
    
    geom_col(
      width = 1,
      color = "black",
      linewidth = 0.5
    ) +
    
    coord_polar(
      theta = "y"
    ) +
    
    geom_text(
      aes(
        label = label,
        color = label_color
      ),
      position = position_stack(
        vjust = 0.5
      ),
      size = 3.4,
      fontface = "bold",
      show.legend = FALSE
    ) +
    
    scale_fill_manual(
      name = "Treatment",
      values = treatment_fills,
      breaks = figure_treatment_order,
      drop = FALSE
    ) +
    
    scale_color_identity() +
    
    labs(
      x = NULL,
      y = NULL
    ) +
    
    theme_void(
      base_size = 14
    ) +
    
    theme(
      aspect.ratio = 1,
      
      plot.margin = margin(
        t = 5,
        r = 5,
        b = 5,
        l = 5
      )
    )
}


# =========================================================
# 14. CREATE THE FOUR PIE CHARTS
# =========================================================

pie_2025_OW <- make_pie_chart(
  summary_2025_OW
)

pie_2025_FG <- make_pie_chart(
  summary_2025_FG
)

pie_2026_OW <- make_pie_chart(
  summary_2026_OW
)

pie_2026_FG <- make_pie_chart(
  summary_2026_FG
)


# =========================================================
# 15. COLUMN LABELS
# =========================================================

column_OW <- ggplot() +
  
  annotate(
    "text",
    x = 0.5,
    y = 0.5,
    label = "Overwintered Females",
    size = 5,
    fontface = "bold"
  ) +
  
  xlim(
    0,
    1
  ) +
  
  ylim(
    0,
    1
  ) +
  
  theme_void()


column_FG <- ggplot() +
  
  annotate(
    "text",
    x = 0.5,
    y = 0.5,
    label = "First-Generation Females",
    size = 5,
    fontface = "bold"
  ) +
  
  xlim(
    0,
    1
  ) +
  
  ylim(
    0,
    1
  ) +
  
  theme_void()


# =========================================================
# 16. ROW LABELS
# =========================================================

row_2025 <- ggplot() +
  
  annotate(
    "text",
    x = 0.5,
    y = 0.5,
    label = "2025",
    size = 5.5,
    fontface = "bold"
  ) +
  
  xlim(
    0,
    1
  ) +
  
  ylim(
    0,
    1
  ) +
  
  theme_void()


row_2026 <- ggplot() +
  
  annotate(
    "text",
    x = 0.5,
    y = 0.5,
    label = "2026",
    size = 5.5,
    fontface = "bold"
  ) +
  
  xlim(
    0,
    1
  ) +
  
  ylim(
    0,
    1
  ) +
  
  theme_void()


# =========================================================
# 17. COMBINE INTO ONE FOUR-PANEL FIGURE
# =========================================================

combined_pie_plot <-
  
  plot_spacer() +
  column_OW +
  column_FG +
  
  row_2025 +
  pie_2025_OW +
  pie_2025_FG +
  
  row_2026 +
  pie_2026_OW +
  pie_2026_FG +
  
  plot_layout(
    design = "
    ABC
    DEF
    GHI
    ",
    
    widths = c(
      0.16,
      1,
      1
    ),
    
    heights = c(
      0.16,
      1,
      1
    ),
    
    guides = "collect"
  ) &
  
  theme(
    legend.position = "right",
    
    legend.title = element_text(
      size = 12,
      face = "bold"
    ),
    
    legend.text = element_text(
      size = 11
    ),
    
    legend.key.size = unit(
      0.8,
      "cm"
    )
  )


# =========================================================
# 18. DISPLAY FINAL FIGURE
# =========================================================

combined_pie_plot
