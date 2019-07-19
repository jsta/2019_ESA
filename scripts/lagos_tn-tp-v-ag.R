library(LAGOSNE)
library(ggplot2)
library(dplyr)

lg <- lagosne_load()

dt <- dplyr::filter(lg$iws.lulc,
                    lagoslakeid %in% lg$epi_nutr$lagoslakeid) %>%
  group_by(lagoslakeid) %>%
  dplyr::select(matches("2011_pct_8"), lagoslakeid) %>%
  left_join(dplyr::select(lg$epi_nutr, lagoslakeid, tn, tp)) %>%
  summarize_all(median, na.rm = TRUE) %>%
  dplyr::filter(!is.na(tn)) %>%
  ungroup() %>% rowwise() %>%
  mutate(ag_pct = sum(iws_nlcd2011_pct_81,
                      iws_nlcd2011_pct_82, na.rm = TRUE)) %>%
  dplyr::select(ag_pct, tn, tp, lagoslakeid) %>%
  tidyr::gather(key = "key", value = "value", -lagoslakeid, -ag_pct) %>%
  dplyr::filter((value < 1994 & key == "tn") | (value < 200 & key == "tp" & value != 100)) %>%
  mutate(key = factor(toupper(key), levels = c("TP", "TN")))

(gg_out <- ggplot(data = dt) +
  geom_point(aes(x = ag_pct, y = value),
             alpha = 0.7, shape = 20) +
  scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) +
  cowplot::theme_cowplot() +
  theme(plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent"),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold")) +
  facet_wrap(~key, scales = "free") +
  xlab("Ag land use (%)") + ylab(""))

gg_out <- cowplot::plot_grid(gg_out, NULL, rel_widths = c(1, 0.2))

ggsave("images/tn_vs_ag.png", gg_out, bg = "transparent",
       width = 5.5, height = 2.4)
