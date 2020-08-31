library(splitstackshape)
library(plyr)
library(ggplot2)

# clear the workspace
rm(list=ls())

setwd("/Users/dylanblokhuis/dev/ds/assignment/")
movies <- read.csv("movies.csv", header = TRUE, stringsAsFactors = FALSE)

keep_columns <- c("movie_title", "title_year", "imdb_score", "genres")
movies <- movies[keep_columns]

# Check missing title_year
table(sum(is.na(movies$title_year)))
# result: 108

# removing 108 records
movies <- subset(movies, title_year != "")

# data is now ready for analysis
save(movies, file="movies_edited.rdata")

View(movies)

# ----- Statistische berekeningen ----- #

# Wat is de kans dat een film boven de 7 haalt op de imdb score? (kansberekening)
kans_hoger_dan_7 <- pnorm(7, mean=mean(movies$imdb_score), sd=sd(movies$imdb_score), lower = FALSE) * 100
# P(x >= 7)
kans_hoger_dan_7

# Hoeveel procent van de films hebben over 7 gehaald in imdb score? (centrummaten)
frequentie_tabel <- table((movies$imdb_score) / length(movies$imdb_score) * 100)
procent_hoger_dan_7 <- mean(frequentie_tabel > 7) * 100
procent_hoger_dan_7

# Wat is de variantie van de imdb score van films gemaakt na 2006? (spreidingsmaat)
after_2006 <- subset(movies, title_year >= 2006)
var(after_2006$imdb_score)

# ----- Einde Statistische berekeningen ----- #


# ----- Data visualisatie ----- #

# Is er sprake van normaalverdeling bij de imdb scores van films?
jpeg('score_normaalverdeling.jpg', width = 600)
hist(movies$imdb_score,
     main = "Is er sprake van normaalverdeling bij de imdb scores van films?",
     xlab = "Score",
     ylab = "Frequentie",
     col = "skyblue")
dev.off()

# Hoeveel films zijn er per jaar gemaakt?
frequency_movies_per_year <- plyr::count(movies, "title_year")
jpeg('films_per_jaar.jpg', width = 900)
plot <- ggplot(data=frequency_movies_per_year, aes(title_year, freq, group=1)) + 
        geom_line() +   # maak de lijn op de plot
        geom_point() +  # maak de puntjes op de plot
        labs(x = "Frequentie", y = "Film titels", title = "Hoeveel films zijn er per jaar gemaakt?")
print(plot)
dev.off()

# Hoeveel films zijn er per genre?

# split all genres into rows
movies.splitted <- splitstackshape::cSplit(movies, "genres", sep = "|", direction = "long")
movies.genres <- plyr::count(movies.splitted, "genres")

jpeg('films_per_genre.jpg', width = 900)
plot <- ggplot(data = movies.genres, aes(x = freq, y = reorder(genres, freq))) +
        geom_bar(stat = 'identity') +
        labs(x = "Frequentie", y = "Genres", title = "Hoeveel films zijn er per genre?")
print(plot)
dev.off()

# ----- Einde Data visualisatie ----- #


# ----- Machine learning ----- #

gemiddelde_scores_per_jaar <- aggregate(imdb_score ~ title_year, data = movies, FUN = mean)
jpeg('gemiddelde_scores_per_jaar.jpg', width = 900)

result <- lm(imdb_score ~ title_year, data = gemiddelde_scores_per_jaar)

plot(gemiddelde_scores_per_jaar,
     main = "Gemiddelde scores per jaar",
     xlab = "Film titels",
     ylab = "IMDb scores")

abline(result, col="blue")
dev.off()


# Wat is de voorspelling voor de volgende 5 jaar?
prediction <- predict(result, data.frame(title_year = c(2017:2021)))
prediction

# ----- Einde Machine learning ----- #





