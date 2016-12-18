# Sõnavõttude emotsioon - neg vs pos
sonavottude_emotsioon <- stenogrammid %>%
    # igale sõnavõtule indeks külge
    mutate(indeks = row_number(),
           tekst_2 = tekst) %>%
    # leia kõik sõnad lausetest
    unnest_tokens(word, tekst_2) %>%
    #eemalda nn stoppsõnad
    anti_join(stop_words_est, by = c("word" = "sona")) %>%
    filter(!str_detect(word, "[0-9]")) %>%
    # joini juurde sõnade emotsiooni kirjeldavad tunnused
    inner_join(sonade_emotsioon_est %>%
                   distinct(sona, emotsioon_nr, emotsioon_sona), by = c("word" = "sona")) %>%
    # riigikoguliikmete ja sõnavõttude lõikes emotsioon (negatiivsed sõnad pluss positiivsed)
    group_by(nimi, indeks, tekst) %>%
    summarise(sonavotu_emotsioon = sum(emotsioon_nr),
              sonavotu_pos_neg = (sum(ifelse(emotsioon_nr == -1, 1, NA), na.rm = TRUE)) /
              (sum(ifelse(emotsioon_nr == 1, 1, NA), na.rm = TRUE)),
              neg = sum(ifelse(emotsioon_nr == -1, -1, NA), na.rm = TRUE),
              pos = sum(ifelse(emotsioon_nr == 1, 1, NA), na.rm = TRUE)) %>%
    ungroup()

# joonista graafik
ggplot() +
    # positiivsed laused sinisega
    geom_bar(data = konede_emotsioon %>%
                 filter(lause_emotsioon > 0), aes(indeks, lause_emotsioon), 
             alpha = 0.8, stat = "identity", show.legend = FALSE, fill = "#377eb8") +
    # negatiivsed laused punasega
    geom_bar(data = konede_emotsioon %>%
                 filter(lause_emotsioon < 0), aes(indeks, lause_emotsioon), 
             alpha = 0.8, stat = "identity", show.legend = FALSE, fill = "#e41a1c") +
    labs(title = "President Ilvese aastapäeva kõnede emotsioon",
         subtitle = "Tulbad näitavad positiivsete ja neatiivsete sõnade osakaalu lauses.\nSinine tulp tähendab, et ülekaalus on positiivsed sõnad ja punane, et negatiivsed.",
         x = NULL, y = "lause emotsioon") + 
    facet_wrap(~aasta, ncol = 2, scales = "free_x") +
    theme_tufte() +
    theme(panel.grid = element_blank(),
          axis.text = element_text(size = 9),
          text = element_text(family = "Chivo"),
          plot.title = element_text(size = 14, face = "bold"),
          strip.text = element_text(size = 11),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank())