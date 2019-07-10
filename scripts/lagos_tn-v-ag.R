library(LAGOSNE)
library(ggplot2)
library(dplyr)

lg <- lagosne_load()

dt <- dplyr::filter(lg$iws.lulc,
                    lagoslakeid %in% lg$epi_nutr$lagoslakeid) %>%
  group_by(lagoslakeid) %>%
  dplyr::select(matches("2011_pct_8"), lagoslakeid) %>%
  left_join(dplyr::select(lg$epi_nutr, lagoslakeid, tn)) %>%
  summarize_all(median, na.rm = TRUE) %>%
  dplyr::filter(!is.na(tn)) %>%
  ungroup() %>% rowwise() %>%
  mutate(ag_pct = sum(iws_nlcd2011_pct_81,
                      iws_nlcd2011_pct_82, na.rm = TRUE))

gg_out <- ggplot(data = dt) +
  geom_point(aes(x = ag_pct, y = tn),
             alpha = 0.7, shape = 20) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 1994), expand = c(0,0)) +
  cowplot::theme_cowplot() +
  theme(plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent")) +
  xlab("Ag land use (%)") + ylab("Total Nitrogen")

ggsave("images/tn_vs_ag.png", gg_out, bg = "transparent")
