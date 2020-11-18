library(tidyverse)
library(ggbeeswarm)
library(ggthemes)

f_2012 <- "P2012_Tour_2_c.xls"
d_2012 <- readxl::read_excel(f_2012, skip = 0, guess_max = 10^5)

v_2012 <- c(
  "Département", "Code postal", "Commune",
  "Inscrits", "Votants", "Exprimés",
  "Candidat", "% Exprimés"
)
v_2012 <- purrr::map_dfr(seq(17, 23, by = 6), function(x) {
  d_2012[, c(1, 3:5, 8, 13, x, x + 4) ] %>% 
    purrr::set_names(v_2012)
}) %>% 
  mutate(
    Département = str_pad(Département, 2, "left", pad = "0"),
    `Code postal` = str_pad(`Code postal`, 3, "left", pad = "0"),
    `Code postal` = str_c(Département, `Code postal`)
  ) %>% 
  filter(!is.na(`% Exprimés`)) %>% 
  tidyr::pivot_wider(names_from = Candidat, values_from = `% Exprimés`)

v2012def <- v_2012 %>% 
  mutate(gagnant = ifelse(HOLLANDE > SARKOZY, "Hollande", "Sarkozy")) %>%
  #mutate(votes_perc = Votants/sum(Votants)) %>% 
  mutate(margin2012 = SARKOZY - HOLLANDE) %>%
  mutate(abs_margin2012 = abs(margin2012))


v2012def %>%
  filter(Inscrits >= 2000) %>%
  mutate(votes_perc = Votants/sum(Votants)) %>% 
  ggplot(aes(y = margin2012, x= 1, color = margin2012, size = votes_perc, alpha = abs_margin2012)) + 
  geom_quasirandom(method = "tukeyDense") +
  scale_color_gradient2("Margins", 
                        low = "#ff0803", 
                        mid = "purple", 
                        high = "#0000ff") +
  scale_y_continuous(labels = c("50% Hollande", "0%", "50% Sarkozy"),
                     breaks = c(-50, 0, 50)) +
  scale_alpha(range = c(0.3, 1)) +
  scale_size(range = c(0.65, 12)) +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "\nMunicipality Margins in the 2012 French Election", 
       caption = glue::glue("\n\nColor reflects by which margin the municipality was won: red = greater Hollande margin; blue = greater Sarkozy margin. \nThe size of the bubble reflects the municipality share of votes of the national total vote count.\n\nElectoral Results: F. Briatte. Based on @favstats code\n"), 
       title = "French 2012 Second Round Presidential Election - Results by Municipality") +
  ggthemes::theme_fivethirtyeight() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_blank(), plot.caption = element_text(hjust = 0)) 
