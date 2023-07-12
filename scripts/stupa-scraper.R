# Packages laden
library(tidyverse)
library(dplyr)
library(stringr)
library(tidyr)
library(rvest)
library(pdftools)
library(rlang)

# Workspace leeren
rm(list = ls())

# Funktion, die von einer Webseite alle Links extrahiert, die bestimmtes Muster erfüllen
get_links = function(link, pattern) {
  links = read_html(
    link
  ) %>% html_elements("a") %>%
    html_attr("href")
  links = as.data.frame(links) %>%
    filter(
      str_detect(links, pattern)
    )
  colnames(links) = "url"
  return(links)
}

# Legislaturen
legislaturen = c("arch59/Antr59", "arch60/antr60", "antr61")

for(k in 1:length(legislaturen)) {
  # Links zu den Seiten mit den Anträgen der jeweiligen Sitzungen
  sitzung_name = ifelse(
    legislaturen[k] == "arch59/Antr59",
    "Sitzung",
    "sitzung"
  )
  sitzungen = unique(
    get_links(
      link = paste0(
        "https://www.uni-giessen.de/de/org/ssv/stupa/",
        legislaturen[k]
      ),
      pattern = sitzung_name
    )
  )
  
  # Links zu den Anträgen an sich extrahieren
  urls = as.data.frame(matrix(nrow = 0, ncol = 2))
  for(i in 1:nrow(sitzungen)) {
    links = get_links(
      link = sitzungen[i, ],
      pattern = sitzungen[i, ]
    )
    links$sitzung = sub(".*antr61/", "", sitzungen[i, ])
    urls = rbind(urls, links)
  }
  rm(i, links)
  
  # nach Download-Links zu den Antragsdateien filtern
  download_links = character()
  for(i in 1:nrow(urls)) {
    links = get_links(
      link = urls["url"][i, ],
      pattern = "@@download"
    )
    download_links = append(download_links, unique(links))
  }
  rm(i, links)
  download_links = as.data.frame(unlist(download_links))
  colnames(download_links) = "url"

  # mit den Download-Links die Dateien herunterladen
  sitzungen$sitzung = sub(".*antr61/", "", sitzungen$url)
  download_links$sitzung = NA
  for(i in 1:nrow(download_links)) {
    for(j in 1:nrow(sitzungen)) {
      # Spalte mit der Sitzung ergänzen, in der der Antrag eingebracht wurde
      download_links["sitzung"][i, ] = (
        sitzungen %>%
          filter(
            str_detect(
              download_links["url"][i, ],
              sitzung
            ) == T
          )
      )$sitzung

      # Ordner für die jeweilige Sitzung erstellen
      dir.create(
        file.path(
          "download_files",
          legislaturen[k],
          sitzungen$sitzung[j]
          ),
        showWarnings = F
      )
    }

    # Dateinamen erzeugen
    file = enc2native(
      strsplit(
        download_links["url"][i, ], "@@download/file/"
      )[[1]][2]
    )
    file = gsub("[^[:alnum:][:blank:];.,_-]", "", file)
    file = gsub("U00..", "", file)
    file = gsub("U03..", "", file)

    # Download der Dateien (mit Error Handling)
    tryCatch(
      # try
      download.file(
        url = download_links["url"][i, ],
        destfile = enc2utf8(
          file.path(
            "download_files",
            legislaturen[k],
            download_links$sitzung[i],
            file
          )
        ),
        quiet = F,
        mode = "wb"
      ),
      # catch
      error = function(e) print(
        paste(
          file,
          'did not work out'
        )
      )
    )
  }
}
rm(i, j, k, sitzungen, download_links, file, legislaturen, sitzung_name)