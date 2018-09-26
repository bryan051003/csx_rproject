library(rvest)

#IMDb Top Rated TV Shows
page.source <- read_html("https://www.imdb.com/chart/toptv/?ref_=nv_tvv_250")

# title
title <- html_nodes(page.source,".titleColumn a")
# year
year <- html_nodes(page.source,".secondaryInfo")
# rate
rate <- html_nodes(page.source,"#main strong")

# title
tv.title <- html_text(title)
# year
tv.year <- html_text(year)
# rate
tv.rate <- html_text(rate)

tv.df <- data.frame(tv.title, tv.year, tv.rate)
colnames(tv.df) <- c("title", "year", "rate")
View(tv.df)


