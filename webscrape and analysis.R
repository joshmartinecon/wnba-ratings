
library(rvest)
library(dplyr)
library(stringr)

### clear data environment
# rm(list=ls())

### Negate function
'%ni%' <- Negate('%in%')

##### step 1: team schedules #####

x <- read.csv("game links.csv")

##### step 2: game-score prod. metric #####

dates <- as.Date(unique(x$date))
dates <- data.frame(
  date = dates,
  year = lubridate::year(dates),
  month = lubridate::month(dates),
  day = lubridate::day(dates)
)
dates$month <- ifelse(nchar(dates$month) == 1, paste0("0", dates$month), dates$month)
gs <- read.csv("game scores.csv")
gs$date <- as.Date(gs$date)
dates <- dates[dates$date > max(gs$date) & as.Date(dates$date) < Sys.Date(),]

z <- list()
for(i in 1:nrow(dates)){
  link <- paste0("https://www.basketball-reference.com/wnba/friv/dailyleaders.fcgi?month=",
                 dates$month[i],"&day=",dates$day[i],"&year=",dates$year[i],"&type=all")
  x <- read_html(link)
  x %>%
    html_table() %>%
    as.data.frame() -> y
  x %>%
    html_elements("a") %>%
    html_attr("href") -> a
  a <- a[grepl("wnba/players/", a)]
  a <- a[grepl(".html", a)]
  a <- gsub("/wnba/players/", "", a)
  a <- gsub(".html", "", a)
  y <- subset(y, y$Rk != "Rk")
  y <- data.frame(
    date = as.Date(paste0(dates$year[i], "-", dates$month[i], "-", dates$day[i])),
    player = y$Player,
    id = a,
    mp = y$MP,
    gmsc = y$GmSc
  )
  mp <- as.data.frame(do.call(rbind, strsplit(y$mp, ":")))
  y$mp <- as.numeric(mp$V1) + as.numeric(mp$V2)/60
  y$gmsc_pm <- as.numeric(y$gmsc) / y$mp
  z[[length(z)+1]] <- y
  if(nrow(dates) > 1){
    Sys.sleep(10)
  }
}
y <- as.data.frame(do.call(rbind, z))
gs <- rbind(gs, y)
write.csv(gs, "game scores.csv", row.names = F)

##### step 3: minutes played (DNPs are important) #####

### game scrape
y <- read.csv("minutes played.csv")
gl <- read.csv("game links.csv")
a <- gl$link[gl$date > max(y$date) & gl$date < Sys.Date()]
b <- list()
for(i in 1:length(a)){
  
  ### start
  x <- read_html(gsub("/game/", "/boxscore/", a[i]))
  
  ### table
  x %>%
    html_table() -> y
  
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
  
  x <- data.frame(
    player = c(x[,1], z[,1]),
    mp = as.numeric(c(x[,2], z[,2])),
    team = c(x[,3], z[,3])
  )
  x$date <- gl$date[match(a[i], gl$link)]
  
  if(TRUE %in% is.na(as.Date(x$date))){
    break
  }
  
  ### combine
  b[[length(b)+1]] <- x
  Sys.sleep(5)
  cat(round(i/length(a), 2), "\r")
}

### compile and clean save data
y <- as.data.frame(do.call(rbind, b))
z <- data.frame(
  team = c("AcesLV", "DreamATL", "FeverIND", "FirePOR", "LibertyNY", "LynxMIN", "MercuryPHX", "MysticsWSH", 
           "SkyCHI", "SparksLA", "StormSEA", "SunCON", "TempoTOR", "ValkyriesGS","WingsDAL"),
  abr = c("LV", "ATL", "IND", "POR", "NY", "MIN", "PHX", "WSH", "CHI", "LA", "SEA", "CON", "TOR", "GS", "DAL")
)
y$team <- ifelse(y$team %in% z$team, z$abr[match(y$team, z$team)], y$team)

x <- read.csv("minutes played.csv")
x <- data.frame(
  player = x$player,
  mp = as.numeric(x$mp),
  team = x$team,
  date = as.Date(x$date)
)
y <- rbind(x, y)

ot2 <- unique(paste(y$team, y$date)[as.numeric(y$mp) > 45])
ot <- unique(paste(y$team, y$date)[as.numeric(y$mp) > 40])
ot <- ot[ot %ni% ot2]
if(length(ot) > 0 | length(ot2) > 0){
  y$mp[paste(y$team, y$date) %in% ot] <- round(y$mp[paste(y$team, y$date) %in% ot] * (40/45))
  y$mp[paste(y$team, y$date) %in% ot2] <- round(y$mp[paste(y$team, y$date) %in% ot2] * (40/50)) 
}
write.csv(y, "minutes played.csv")

##### step 4: minutes per game #####

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
x$player[x$player == "Sarah Ashlee"] <- "Sarah Ashlee Barker"
x$player[x$player == "Myisha Hines"] <- "Myisha Hines-Allen"
x$player[x$player == "Olivia Nelson"] <- "Olivia Nelson-Ododa"
x$player[x$player == "Te-Hina Paopao"] <- "Te-Hina PaoPao"
x$player[x$player == "Janelle Salaun"] <- "Janelle Salaün"
x$player[x$player == "Hailey Van"] <- "Hailey Van Lith"
x$player[x$player == "Shatori Walker"] <- "Shatori Walker-Kimbrough"
x$player[x$player == "Anastasiia Olairi"] <- "Anastasiia Kosu"
x$player[x$player == "Laura Juskaite"] <- "Laura Juškaitė"
x$player[x$player == "Noemie Brochant"] <- "Noémie Brochant"
x$player[x$player == "Frieda Buhner"] <- "Frieda Bühner"
x$player[x$player == "Raquel Carrera"] <- "Raquel Carrera Quintana"
x$player[x$player == "Emma Cechova"] <- "Emma Čechová"
x$player[x$player == "Angela Dugalic"] <- "Angela Dugalić"
x$player[x$player == "Luisa Geiselsoder"] <- "Luisa Geiselsöder"
x$player[x$player == "Eliska Hamzova"] <- "Eliška Hamzová"
x$player[x$player == "Flau'jae JohnsonF. Johnson#4"] <- "Flau'jae Johnson"
x$player[x$player == "Sika Kone"] <- "Sika Koné"
x$player[x$player == "Betnijah Laney"] <- "Betnijah Laney-Hamilton"
x$player[x$player == "Charlisse Leger"] <- "Charlisse Leger-Walker"
x$player[x$player == "Cotie Mc"] <- "Cotie McMahon"
x$player[x$player == "Jovana Nogic"] <- "Jovana Nogić"
x$player[x$player == "Cheyenne Parker"] <- "Cheyenne Parker-Tyus"
x$player[x$player == "Katie Lou"] <- "Katie Lou Samuelson"
x$player[x$player == "Marta Suarez"] <- "Marta Suárez"
x$player[x$player == "Alex Wilson"] <- "Ally Wilson"
x$player[x$player == "Grace Van"] <- "Grace VanSlooten"
x$player[x$player == "Juste Jocyte"] <- "Justė Jocytė"
x$player[x$player == "Monique Akoa"] <- "Monique Akoa Makani"
x$player[x$player == "Alicia Florez"] <- "Alicia Flórez"
x$player[x$player == "Leila Lacan"] <- "Leïla Lacan"

x$team <- ifelse(x$team == "CONN", "CON",
                 ifelse(x$team == "GS", "GSV",
                        ifelse(x$team == "LA", "LAS",
                               ifelse(x$team == "LV", "LVA",
                                      ifelse(x$team == "NY", "NYL",
                                             ifelse(x$team == "PHX", "PHO",
                                                    ifelse(x$team == "WSH", "WAS", x$team)))))))
write.csv(x, "minutes played per.csv", row.names = F)

##### step 5: webscrape bbref #####

### web-scrape primary data
x <- read_html("https://www.basketball-reference.com/wnba/years/2026_advanced.html")

### links
x %>%
  html_elements("a") %>%
  html_attr("href") -> a

## team links
b <- unique(a[grepl("wnba/teams/", a)])
b <- b[grepl("2026", b)]

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
  ws = y$WS.40
)

## match in game score
gs$product <- as.numeric(gs$gmsc) * gs$mp
a <- aggregate(cbind(product, mp) ~ player + id, gs, sum)
a$gmsc <- a$product / a$mp
y$gmsc <- a$gmsc[match(y$link, a$id)]

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
      gsub("/2026.html$", "", .)
  )
  Sys.sleep(5)
  cat(paste(i, "out of", length(b)), "\r")
}
z <- as.data.frame(do.call(rbind, z))
y <- y[paste(y$link, y$team) %in% paste(z$id, z$team),]

## convert variables to numbers
y[,4:9] <- lapply(y[,4:9], as.numeric)

## supplement minutes played measure
x <- read.csv("minutes played per.csv")
y$mp_g <- x$mp_g[match(paste(y$player, y$team),
                       paste(x$player, x$team))]

##### step 6: scale playing time by team #####

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

### scale minutes by team
teams <- unique(y$team)
x <- list()
for(i in teams){
  
  z <- y[y$team == i,]
  z <- z[z$mp_g > 4,]
  z <- z[order(-z$mp, -z$mp_g, -z$g),]
  
  z1 <- z[z$player %in% w$Name,]
  z2 <- z[z$player %ni% w$Name,]
  
  z <- z2[1:ifelse(nrow(z2) > 8, 8, nrow(z2)),]
  z$mp_g_team <- z$mp_g / sum(z$mp_g) * 200
  
  while (any(z$mp_g_team > max(y$mp_g))) {
    rule <- z$mp_g_team > max(y$mp_g)
    allocate_mins <- sum(z$mp_g_team[rule] - max(y$mp_g))
    z$mp_g_team <- ifelse(
      !rule,
      z$mp_g_team + z$mp_g_team / sum(z$mp_g_team) * allocate_mins,
      max(y$mp_g)
    )
    z$mp_g_team <- z$mp_g_team / sum(z$mp_g_team) * 200
  }
  
  if(nrow(z1) > 0){
    z1$mp_g_team <- 0
    x[[length(x)+1]] <- rbind(z, z1)
  }else{
    x[[length(x)+1]] <- z
  }
  
}
y <- as.data.frame(do.call(rbind, x))

### standardize playing-time weighted efficiency variables
for(i in 6:9){
  y[,i] <- y[,i] * y$mp_g
  y[,i] <- (y[,i] - mean(y[,i])) / sd(y[,i])
}
y$score <- rowMeans(y[,6:9])

### video-game style rating (logged)
y$rating <- 60 + (asinh(y$score) - min(asinh(y$score)))/
                    (max(asinh(y$score)) - min(asinh(y$score))) * (99-60)

### full squad playing time
z <- data.frame(
  mp = y$mp_g,
  rtg = y$rating,
  rtg2 = y$rating^2
)
lm1 <- lm(mp ~ rtg, z)
lm2 <- lm(mp ~ rtg + rtg2, z)
y$mp_g_star <- ifelse(y$rating > median(y$rating), predict(lm2), predict(lm1))
a <- y[order(y$rating),]
a$median <- 0
for(i in 1:nrow(a)){
  a$median[i] <- ifelse(a$rating[i] > median(a$rating), 1, 0)
}
a$row <- 1:nrow(a)
num <- min(a$row[a$median == 1])
diff <- (a$mp_g_star[num-1] - a$mp_g_star[num])
y$mp_g_star <- ifelse(y$rating > median(y$rating), y$mp_g_star + diff, y$mp_g_star)
plot(y$rating, y$mp_g_star)

## expected minutes
teams <- unique(y$team)
x <- list()
for(i in teams){
  
  z <- y[y$team == i,]
  z$mp_g_star <- z$mp_g_star + z$mp_g_star / sum(z$mp_g_star) * (200 - sum(z$mp_g_star))
  
  while (any(z$mp_g_star > max(y$mp_g))) {
    rule <- z$mp_g_star > max(y$mp_g)
    allocate_mins <- sum(z$mp_g_star[rule] - max(y$mp_g))
    z$mp_g_star <- ifelse(
      !rule,
      z$mp_g_star + z$mp_g_star / sum(z$mp_g_star) * allocate_mins,
      max(y$mp_g)
    )
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
  z <- z[z$mp_g_team > 0,]
  z$mp_g_star <- z$mp_g_star + z$mp_g_star / sum(z$mp_g_star) * (200 - sum(z$mp_g_star))
  
  while (any(z$mp_g_star > max(y$mp_g))) {
    rule <- z$mp_g_star > max(y$mp_g)
    allocate_mins <- sum(z$mp_g_star[rule] - max(y$mp_g))
    z$mp_g_star <- ifelse(
      !rule,
      z$mp_g_star + z$mp_g_star / sum(z$mp_g_star) * allocate_mins,
      max(y$mp_g)
    )
    z$mp_g_star <- z$mp_g_star / sum(z$mp_g_star) * 200
  }
  
  x[[length(x)+1]] <- z
}
x <- as.data.frame(do.call(rbind, x))

##### step 7: team ratings #####

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
z <- as.data.frame(aggregate(abs(mp_g_star - mp_g_team) ~ team, x, sum))
z <- data.frame(
  team = z[,1],
  rotation_rating = z[,2]
)
z[,2] <- ((max(y$mp_g_team) * 5) - z[,2]) / (max(y$mp_g_team) * 5) * 100
r$rotation_rating <- z[match(r$team, z$team),2]

### simplify data
r[,2:4] <- round(r[,2:4], 1)
colnames(r)[2:3] <- c("rating", "strength")
r <- r[order(-r$rating, -r$strength, -r$rotation_rating),]

### add back in injured players
z <- y[y$link %ni% x$link,]
if(nrow(z) > 0){
  z$mp_g_team <- z$mp_g_star <- 0
  z$player <- paste0(z$player, "+")
  x <- rbind(x, z)
}
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

r
head(x)

##### step 8: save output #####

write.csv(x, "WNBA_Ratings_and_Rotations.csv", row.names = F)
write.csv(r, "WNBA_Team_Ratings.csv", row.names = F)
write.csv(Sys.time(), "last_updated.csv")
