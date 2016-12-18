# Riigikoguliikmete populaarsuse hindamiseks pärin igaühe Wikipedia lehe külastuste arvu
# ajavahemik 30.03.2015 - 22.11.2016

library(dplyr)
library(purrr)
library(stringr)

load(file = "data/stenogrammid.Rdata")

# Riigikogulaste nimekiri, kes on vähemalt 25 korda sõna võtnud
riigikogu_liikmed <- stenogrammid %>% 
    filter(amet == "Riigikogu liige") %>% 
    count(nimi) %>% 
    filter(n > 25) %>% 
    distinct(nimi) %>% 
    .$nimi

# funktsioon wikipedia artiklite vaatamiste pärimiseks
# iga isiku kohta vaatamiste arv kokku
wikipedia_lehe_vaatamisi <- function(x){
    library(pageviews)
    tryCatch(
        {
            df <- article_pageviews(project = "et.wikipedia", article = x,
                                    start = as.Date('2015-03-30'), end = as.Date("2016-11-22"),
                                    user_type = c("user"), platform = c("desktop", "mobile-web"))
            
            kokku <- df %>% 
                group_by(article) %>% 
                summarise(wiki_vaatamisi = sum(views, na.rm = TRUE)) %>% 
                rename(nimi = article) %>% 
                mutate(nimi = str_replace_all(nimi, "_", " "))
            
            return(kokku)
        }, error = function(e) NULL
    )
}

# tulemused kokku
riigikogulaste_populaarsus <- map_df(riigikogu_liikmed, wikipedia_lehe_vaatamisi)

# salvesta tulemus
save(riigikogulaste_populaarsus, file = "output/riigikogulaste_populaarsus.RData")