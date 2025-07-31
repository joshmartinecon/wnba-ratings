
library(rvest)
library(dplyr)

### clear data environment
# rm(list=ls())

### Negate function
'%ni%' <- Negate('%in%')

### web scrape box score links
teams <- c("atl", "chi", "conn", "dal", "gs", "ind", "lv", "la", "min", "ny", "phx", "sea", "wsh")
y <- list()

### loop
for(i in teams){
  
  ### start
  x <- read_html(paste0("https://www.espn.com/wnba/team/schedule/_/name/", i))
  
  x %>% 
    html_table() %>%
    as.data.frame() -> z
  colnames(z) <- z[2,]
  z <- z[c(-1:-2),]
  z <- z[1:((1:nrow(z))[grepl("Regular|Cup", z[,1])]-1),]
  
  ### links
  x %>%
    html_elements("a") %>%
    html_attr("href") -> a
  
  ### game links
  b <- unique(a[grepl("gameId", a)])
  b <- b[!grepl("preview", b)]
  
  ### box score links
  a <- gsub("game/_/", "boxscore/_/", b)
  a <- sub("/[^/]*$", "", a)
  
  y[[length(y)+1]] <- a[1:nrow(z)]
  
  Sys.sleep(5)
}

### game scrape
a <- unique(unlist(y))
b <- list()

for(i in 1:length(a)){
  
  ### start
  y <- read_html(a[i]) %>% 
    html_table()
  
  ### team 1
  x <- cbind(as.data.frame(y[2]), as.data.frame(y[3]))
  colnames(x) <- x[1,]
  x <- x[-1,]
  x <- data.frame(
    player = sub("([A-Z][a-z]+ [A-Z][a-z]+).*", "\\1", x[,1]),
    mp = ifelse(x[,2] == "DNP-COACH'S DECISION", 0, x[,2]),
    team = as.data.frame(y[1])[1,1]
  )
  x <- x[!grepl("bench|team", x$player) & x$player != "" & !grepl("DNP", x$mp),]
  
  ### team 2
  z <- cbind(as.data.frame(y[4]), as.data.frame(y[5]))
  colnames(z) <- z[1,]
  z <- z[-1,]
  z <- data.frame(
    player = sub("([A-Z][a-z]+ [A-Z][a-z]+).*", "\\1", z[,1]),
    mp = ifelse(z[,2] == "DNP-COACH'S DECISION", 0, z[,2]),
    team = as.data.frame(y[1])[2,1]
  )
  z <- z[!grepl("bench|team", z$player) & z$player != "" & !grepl("DNP", z$mp),]
  
  ### combine
  b[[length(b)+1]] <- rbind(x, z)
  Sys.sleep(5)
  cat(round(i/length(a), 2), "\r")
}

### compile and clean data
y <- as.data.frame(do.call(rbind, b))

z <- as.data.frame(table(paste(y$player, y$team)))
x <- aggregate(as.numeric(mp) ~ player + team, y, sum)
x <- data.frame(
  player = x[,1],
  team = x[,2],
  g = z$Freq[match(paste(x$player, x$team), z$Var1)],
  mp = x[,3]
)
x$mp_g <- round(x$mp / x$g, 2)
x[x$player == "JJ QuinerlyJ. Quinerly#11",1] <- "JJ Quinerly"
x[x$player == "Teaira Mc",1] <- "Teaira McCowan"
x[x$player == "Aari Mc",1] <- "Aari McDonald"
x[x$player == "A'ja WilsonA. Wilson#22",1] <- "A'ja Wilson"
x[x$player == "Kayla Mc",1] <- "Kayla McBride"
x[x$player == "Megan Mc",1] <- "Megan McConnell"
x$player[x$player == "Monique Akoa"] <- "Monique Akoa Makani"
x$player[x$player == "Sarah Ashlee"] <- "Sarah Ashlee Barker"
x$player[x$player == "Myisha Hines"] <- "Myisha Hines-Allen"
x$player[x$player == "Leila Lacan"] <- "Leïla Lacan"
x$player[x$player == "Olivia Nelson"] <- "Olivia Nelson-Ododa"
x$player[x$player == "Olivia Nelson"] <- "Olivia Nelson-Ododa"
x$player[x$player == "Te-Hina Paopao"] <- "Te-Hina PaoPao"
x$player[x$player == "Janelle Salaun"] <- "Janelle Salaün"
x$player[x$player == "Hailey Van"] <- "Hailey Van Lith"
x$player[x$player == "Shatori Walker"] <- "Shatori Walker-Kimbrough"

x$team <- ifelse(x$team == "CONN", "CON",
                 ifelse(x$team == "GS", "GSV",
                        ifelse(x$team == "LA", "LAS",
                               ifelse(x$team == "LV", "LVA",
                                      ifelse(x$team == "NY", "NYL",
                                             ifelse(x$team == "PHX", "PHO",
                                                    ifelse(x$team == "WSH", "WAS", x$team)))))))

### save 
setwd("C:/Users/jmart/OneDrive/Desktop/GitHub/wnba-ratings")
# write.csv(x, "minutes played.csv", row.names = F)

##### player data and analysis #####

### webscrape primary data
x <- read_html("https://www.basketball-reference.com/wnba/years/2025_advanced.html")

### links
x %>%
  html_elements("a") %>%
  html_attr("href") -> a

## team links
b <- unique(a[grepl("wnba/teams/", a)])
b <- b[grepl("2025", b)]

## player links
a <- a[grepl("wnba/players/", a)]
a <- a[grepl(".html", a)]
a <- gsub("/wnba/players/", "", a)
a <- gsub(".html", "", a)

## tables
x %>%
  html_table() %>%
  as.data.frame() -> y
y <- y[y$Player != "Player",]
y$links <- a[(length(a)-nrow(y)+1):length(a)]

### create data frame
y <- data.frame(
  player = y$Player,
  link = y$links,
  team = y$Team,
  g = y$G,
  mp = y$MP,
  per = y$PER,
  rtg = as.numeric(y$ORtg) - as.numeric(y$DRtg),
  ws = y$WS
)

### keep players on their current rosters
z <- list()
for(i in 1:length(b)){
  z[[length(z)+1]] <- data.frame(
    id = read_html(paste0("https://www.basketball-reference.com", b[i])) %>%
      html_element("#roster") %>%
      html_elements("a") %>%
      html_attr("href") %>%
      gsub("^/wnba/players/", "", .) %>%
      gsub(".html", "", .),
    team = b[i] %>%
      gsub("^/wnba/teams/", "", .) %>%
      gsub("/2025.html$", "", .)
  )
  Sys.sleep(5)
  cat(paste(i, "out of", length(b)), "\r")
}
z <- as.data.frame(do.call(rbind, z))
y <- y[paste(y$link, y$team) %in% paste(z$id, z$team),]

## remove players with little data
y <- subset(y, as.numeric(y$mp) >= 40)
y[,4:8] <- lapply(y[,4:8], as.numeric)

## supplement minutes played measure
x <- read.csv("minutes played.csv")
y$mp_g <- x$mp_g[match(paste(y$player, y$team),
                       paste(x$player, x$team))]

### create measure of minutes played per game, scale by team
z <- aggregate(mp_g ~ team, y, sum)
z$adj <- z[,2] / (40*5)
y$mp_g_team <- y$mp_g / z$adj[match(y$team, z$team)]

### standardize playing-time weighted efficiency variables
for(i in 6:8){
  y[,i] <- y[,i] * y$mp_g
  y[,i] <- (y[,i] - mean(y[,i])) / sd(y[,i])
}
y$score <- rowMeans(y[,6:8])

### video-game style rating (logged)
y$rating <- 60 + (asinh(y$score) - min(asinh(y$score)))/
                    (max(asinh(y$score)) - min(asinh(y$score))) * (100-60)

### web scrape injuries
w <- read_html("https://www.espn.com/wnba/injuries") %>%
  html_elements("tbody tr") %>%
  lapply(function(row) {
    cols <- row %>% html_elements("td")
    data.frame(
      Name = cols[1] %>% html_element("a") %>% html_text(),
      POS = cols[2] %>% html_text(),
      ReturnDate = cols[3] %>% html_text(),
      Status = cols[4] %>% html_element("span") %>% html_text(),
      Comment = cols[5] %>% html_text(),
      stringsAsFactors = FALSE
    )
  }) %>%
  bind_rows()
w <- w[w$Status == "Out",]

### full squad playing time
teams <- unique(y$team)
x <- list()
for(i in teams){
  z <- y[y$team == i,]
  cdf_fn <- ecdf(z$rating)
  z$mp_g_star <- cdf_fn(z$rating)*max(y$mp_g)
  z$mp_g_star <- z$mp_g_star / sum(z$mp_g_star) * 200
  rule <- z$mp_g_star > max(y$mp_g)
  if(TRUE %in% rule){
    allocate_mins <- sum(z$mp_g_star[rule] - max(y$mp_g))
    z$mp_g_star <- ifelse(!rule, z$mp_g_star + cdf_fn(z$rating) / sum(cdf_fn(z$rating)) * allocate_mins, max(y$mp_g))
    z$mp_g_star <- z$mp_g_star / sum(z$mp_g_star) * 200
  }
  x[[length(x)+1]] <- z
}
y <- as.data.frame(do.call(rbind, x))

### current squad playing time
teams <- unique(y$team)
x <- list()
for(i in teams){
  z <- y[y$team == i,]
  z <- z[z$player %ni% w$Name,]
  z$mp_g_team <- z$mp_g / sum(z$mp_g) * 200 
  cdf_fn <- ecdf(z$rating)
  z$mp_g_star <- cdf_fn(z$rating)*max(y$mp_g)
  z$mp_g_star <- z$mp_g_star / sum(z$mp_g_star) * 200
  rule <- z$mp_g_star > max(y$mp_g)
  if(TRUE %in% rule){
    allocate_mins <- sum(z$mp_g_star[rule] - max(y$mp_g))
    z$mp_g_star <- ifelse(!rule, z$mp_g_star + cdf_fn(z$rating) / sum(cdf_fn(z$rating)) * allocate_mins, max(y$mp_g))
    z$mp_g_star <- z$mp_g_star / sum(z$mp_g_star) * 200
  }
  x[[length(x)+1]] <- z
}
x <- as.data.frame(do.call(rbind, x))

### current team rating
z <- as.data.frame(aggregate(rating*mp_g_team ~ team, x, sum))
z <- data.frame(
  team = z[,1],
  current_rating = z[,2] / 200
)
r <- z

### full team, best rotation rating
z <- as.data.frame(aggregate(rating*mp_g_star ~ team, y, sum))
z <- data.frame(
  team = z[,1],
  current_better_rating = z[,2] / 200
)
r$best_rating <- z[match(r$team, z$team),2]

### full team, best rotation rating
z <- as.data.frame(aggregate(rating*mp_g_team ~ team, y, sum))
z <- data.frame(
  team = z[,1],
  current_better_rating = z[,2] / 200
)
r$rotation_rating <- z[match(r$team, z$team),2] - r$best_rating
ecdf_fn <- ecdf(r$rotation_rating)
r$rotation_rating <- ecdf_fn(r$rotation_rating)

### simplify data
r[,2:4] <- round(r[,2:4], 1)
colnames(r)[2:3] <- c("rating", "strength")
r <- r[order(-r$rating),]

### add back in injured players
z <- y[y$link %ni% x$link,]
z$mp_g_team <- z$mp_g_star <- 0
x <- rbind(x, z)
x <- data.frame(
  player = x$player,
  team = x$team,
  mp_g = round(x$mp_g_team),
  mp_g_star = round(x$mp_g_star),
  min_diff = 0,
  rating = x$rating
)
x$min_diff <- x$mp_g_star - x$mp_g
x <- x[order(-x$rating),]
x$rating <- round(x$rating)

##### save work #####

setwd("C:/Users/jmart/OneDrive/Desktop/GitHub/wnba-ratings")
# write.csv(x, "WNBA_Ratings_and_Rotations.csv", row.names = F)
# write.csv(r, "WNBA_Team_Ratings.csv", row.names = F)
# write.csv(Sys.time(), "last_updated.csv")
