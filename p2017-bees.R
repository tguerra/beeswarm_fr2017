library(tidyverse)
library(ggbeeswarm)
library(ggthemes)


# Importing 2017 runoff results ---------------------------------------------------------------------


f <- "Presidentielle_2017_Resultats_Communes_Tour_2_c.xls"
d <- readxl::read_excel(f, skip = 3, guess_max = 10^5)

v <- c(
  "Département", "Code postal", "Commune",
  "Inscrits", "Votants", "Exprimés",
  "Candidat", "% Exprimés"
)
v <- purrr::map_dfr(seq(21, 28, by = 7), function(x) {
  d[, c(1, 3:5, 8, 16, x, x + 4) ] %>% 
    purrr::set_names(v)
}) %>% 
  mutate(
    Département = str_pad(Département, 2, "left", pad = "0"),
    `Code postal` = str_pad(`Code postal`, 3, "left", pad = "0"),
    `Code postal` = str_c(Département, `Code postal`)
  ) %>% 
  filter(!is.na(`% Exprimés`)) %>% 
  tidyr::pivot_wider(names_from = Candidat, values_from = `% Exprimés`)

v2 <- v %>% 
  mutate(gagnant = ifelse(MACRON > `LE PEN`, "Macron", "Le Pen")) %>%
  mutate(votes_perc = Votants/sum(Votants)) %>% 
  #mutate(margin17 = ifelse(gagnant == "Macron", MACRON - 50, `LE PEN` - 50)) %>%
  mutate(margin2017 = `LE PEN` - MACRON) %>%
  mutate(abs_margin2017 = abs(margin2017)) %>%
  mutate(cat = cut_number(Inscrits, n = 4))
  
  
# Visualization -----------------------------------------------------------
# Based on https://gist.github.com/favstats/c569b92aa0906dc3aead1f3eb5242c8c

v2 %>%
  filter(Inscrits >= 2000) %>%
  mutate(votes_perc = Votants/sum(Votants)) %>% 
  ggplot(aes(y = margin2017, x= 1, color = margin2017, size = votes_perc, alpha = abs_margin2017)) + 
  geom_quasirandom(method = "tukeyDense") +
  scale_color_gradient2("Margins", 
                        low = "#ff0803", 
                        mid = "purple", 
                        high = "#0000ff") +
  scale_y_continuous(labels = c("95% Macron", "0%", "45% Le Pen"),
                     breaks = c(-95, 0, 45)) +
  scale_alpha(range = c(0.4, 1)) +
  scale_size(range = c(0.65, 12)) +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "\nMunicipality Margins in the 2017 French Election", 
       caption = glue::glue("\n\nColor reflects by which margin the municipality was won: red = greater Macron margin; blue = greater Le Pen margin. \nThe size of the bubble reflects the municipality share of votes of the national total vote count.\n\nElectoral Results: F. Briatte. Based on @favstats code\n"), 
       title = "French 2017 Second Round Presidential Election - Results by Municipality") +
  ggthemes::theme_fivethirtyeight() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_blank(), plot.caption = element_text(hjust = 0)) 
  
