
library(rvest)

### clear data enviornment
# rm(list=ls())

### Negate funciton
'%ni%' <- Negate('%in%')

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
y <- subset(y, y$mp >= 40)

### create measure of minutes played per game, scale by team
y[,4:8] <- lapply(y[,4:8], as.numeric)
z <- aggregate(mp / g ~ team, y, sum)
z$adj <- z[,2] / (40*5)
y$mp_g <- y$mp / y$g / z$adj[match(y$team, z$team)]

### standardize playing-time weighted efficiency variables
for(i in 6:8){
  y[,i] <- y[,i] * y$mp_g
  y[,i] <- (y[,i] - mean(y[,i])) / sd(y[,i])
}
y$score <- rowMeans(y[,6:8])

### cor(playing time, rating) seems non-linear
y$mp2 <- y$mp_g*y$mp_g
y <- y[order(y$mp_g),]

### store regression coefs for later
lm1 <- lm(y$score ~ y$mp_g + y$mp2)
a <- coef(lm1)[1]
b <- coef(lm1)[2]
c <- coef(lm1)[3]

### predicted rating, based on playing time
y$pred <- predict(lm1)

### difference between actual score and prediction
y$diff <- y$score - y$pred

### video-game style rating (logged)
y$rating <- round(60 + (asinh(y$score) - min(asinh(y$score)))/
                    (max(asinh(y$score)) - min(asinh(y$score))) * (100-60), 1)

## visalize expected score and playing time
plot(y$mp_g, y$score)
lines(y$mp_g, y$pred, lwd = 4, lty = 2)

### webscrape playing time
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

### assign playing time based on productivity
teamz <- unique(y$team)
x <- list()
for(i in teamz){
  
  # filter by team
  z <- y[y$team == i,]
  
  # remove injured players
  z <- z[z$player %ni% w$Name,]
  
  # expected minutes
  minz <- (-1*b + sqrt(b^2 - 4*c*(a - z$score))) / (2*c)
  
  # lower and upper bounds on minutes
  minz <- ifelse(is.na(minz), min(y$mp_g), minz)
  minz <- pmin(pmax(minz, min(y$mp_g)), max(y$mp_g))
  
  # sums to 200 (5 players, 40 mins)
  z$mp_g_star <- minz / sum(minz) * 200
  
  # can't play more than some amount
  rulez <- z$mp_g_star > max(y$mp_g)
  
  # reallocate playing time if rule-breaker
  while(TRUE %in% rulez){
    xtra <- sum(z$mp_g_star[rulez] - max(y$mp_g))
    
    z$mp_g_star <- ifelse(rulez, max(y$mp_g), z$mp_g_star)
    
    z$mp_g_star[!rulez] <- z$mp_g_star[!rulez] + 
      z$mp_g_star[!rulez] / sum(z$mp_g_star[!rulez]) * xtra
    
    rulez <- z$mp_g_star > max(y$mp_g)
  }
  
  # store team
  x[[length(x)+1]] <- z
}
x <- as.data.frame(do.call(rbind, x))

## difference between optimal and current playing time
x$min_diff <- x$mp_g_star - x$mp_g

## throw injured players back into data
z <- y[y$player %ni% x$player,]
z$player <- paste0(z$player, "+")
z$mp_g <- 0
z$mp_g_star <- 0
z$min_diff <- 0
x <- rbind(x, z)

### sort by score
x <- x[order(-x$score),]
row.names(x) <- 1:nrow(x)

### simplify data
y <- x[,c(1, 3, 9, 15:16, 14)]

##### team data and analysis #####

### playing-time weighted team rating
z <- aggregate(cbind(rating*mp_g, mp_g) ~ team, y, sum)
z$rating <- z[,2] / z[,3]

## optimal playing-time weighted team rating
z_star <- aggregate(cbind(rating*mp_g_star, mp_g_star) ~ team, y, sum)
z_star$rating <- z_star[,2] / z_star[,3]

### simplify data
z <- data.frame(
  team = z$team,
  rating = round(z$rating, 1),
  rating_star = round(z_star$rating, 1)
)
## sory by rating
z <- z[order(-z$rating_star),]

##### save work #####

setwd("C:/Users/jmart/OneDrive/Desktop/GitHub/wnba-ratings/Data")
y[,3:6] <- apply(y[,3:6], 2, round)
# write.csv(y, "WNBA_Ratings_and_Rotations.csv", row.names = F)
# write.csv(z, "WNBA_Team_Ratings.csv", row.names = F)
# saveRDS(Sys.time(), "last_updated.RDS")
