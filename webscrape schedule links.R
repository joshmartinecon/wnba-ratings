
library(rvest)

### web scrape box score links
teams <- c("atl", "chi", "con", "dal", "gs", "ind", "lv", "la", "min", "ny", "phx", "por", "sea", "tor", "wsh")
y <- list()

### scrape the urls for each team's schedule
for(i in teams){
  
  ### start
  x <- read_html(paste0("https://www.espn.com/wnba/team/schedule/_/name/", i))
  
  x %>% 
    html_table() %>%
    as.data.frame() -> z
  colnames(z) <- z[2,]
  z <- z[c(-1:-2),1:3]
  last <- as.numeric(rownames(z)[grepl("Preseason", z$DATE)])
  z <- z[1:(last-1),]
  z <- z[grepl(", ", z$DATE),]
  
  z$DATE %>%
    str_remove("^[A-Za-z]{3},\\s*") %>%       # Remove "Fri," etc.
    paste("2026") %>%                         # Add year
    as.Date(format = "%B %d %Y") -> z$DATE  
  
  ### links
  x %>%
    html_elements("a") %>%
    html_attr("href") -> a
  
  ### game links
  b <- unique(a[grepl("gameId", a)])
  b <- b[!grepl("preview", b)]
  
  ### save
  y[[length(y)+1]] <- data.frame(
    team = i,
    opponent = z$OPPONENT,
    date = z$DATE,
    link = b[1:nrow(z)]
  )
  
  ### rest
  Sys.sleep(5)
}

### compile and save data
x <- as.data.frame(do.call(rbind, y))
x <- x[,-1:-2]
x <- x[!duplicated(x),]
x <- x[order(x$date, x$link),]
setwd("/home/jmartin/Desktop/Dropbox/github/wnba-ratings")
write.csv(x, "game links.csv", row.names = F)