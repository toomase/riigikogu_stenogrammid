# Kraabi Riigikogu 13. koosseisu stenogrammmid seisuga 22.11.2016
# Leia stenogrammmidest Riigikogu liikmetele kõige iseloomulikumad teemad
library(rvest)
library(dplyr)
library(stringr)
library(lubridate)
library(purrr)

# url-d, kust saab omakorda kätte stenogrammide lingid
# kraabi esimesed 10 lehte, kuna seal on 13. koosseisu osa olemas
stenogrammide_avalehe_urlid <- str_c("http://stenogrammid.riigikogu.ee/?page=",
                                    seq(from = 1, to = 10, by = 1))

# funktsioon, mis kraabib stenorammide kuupäeva ja lingi
kraabi_istungite_nimed_urlid <- function(x){
    
    leht <- read_html(x)
    
    kuupaev <- leht %>%
        html_nodes(".width-17") %>%
        html_text() %>%
        str_extract(., "^.+(?= /)") %>%
        dmy(.)
    
    pealkiri <- leht %>%
        html_nodes(".width-78") %>%
        html_text()
    
    url <- leht %>%
        html_nodes(".istungLink") %>%
        html_attr("href")
    
    Sys.sleep(1)
        
    stenogrammide_nimekiri <- tibble(kuupaev, pealkiri, url)
}

# kraabi 13. koosseisu stenogrammide nimekiri
stenogrammide_nimekiri <- map_df(stenogrammide_avalehe_urlid, kraabi_istungite_nimed_urlid)

# filtreeri välja ainult 13 koosseisu stenogrammid (alates 30.03.2015)
stenogrammide_nimekiri_13_koosseis <- stenogrammide_nimekiri %>%
    filter(kuupaev >= dmy("30.03.2015"))

# funktsioon, mis kraabib juba konkreetsed stenogrammid teksti ja esinejaga
kraabi_stenogrammid <- function(x){
    tryCatch(
        {
            leht <- read_html(x)
            
            isik <- leht %>%
                html_nodes(".syndmus strong") %>%
                html_text()
            
            tekst <- leht %>%
                html_nodes(".text") %>%
                html_text()
            
            Sys.sleep(1)
            
            stenogrammid <- tibble(x, isik, tekst)
            
            return(stenogrammid)
        }, error = function(e) NULL
    )
}

# kraabi stenogrammid
stenogrammid_raw <- map_df(stenogrammide_nimekiri_13_koosseis$url, 
                           kraabi_stenogrammid)

# töötle stenogramme
stenogrammid <- stenogrammid_raw %>%
    # lisa kuupäev ja pealkiri
    left_join(stenogrammide_nimekiri_13_koosseis, by = c("x" = "url")) %>%
    # eralda esinejast nimi ja amet
    mutate(nimi = str_extract(isik, "([[:alpha:]]|-|Toomas Hendrik)+\\s([[:alpha:]]|-)+$"),
           amet = str_replace_all(isik, nimi, ""),
           amet = ifelse(!str_detect(amet, "\\w"), "Riigikogu liige", str_trim(amet))) %>%
    select(kuupaev, pealkiri, url = x, nimi, amet, tekst)

# salvesta stenogramm edasiseks analüüsiks
save(stenogrammid, file = "data/stenogrammid.Rdata")