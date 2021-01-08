

###Jack Ferreri R Shiny Project Pre Processing

library(tidyverse)
library(lubridate)

#Import Data
moviesdf = read.csv("./IMDb_movies.csv", stringsAsFactors = FALSE)
ratingsdf = read.csv("./IMDb_ratings.csv", stringsAsFactors = FALSE)

#Join Data Sets
imdb_df = inner_join(moviesdf, ratingsdf, by = 'imdb_title_id')

#Drop unused columns
drop_cols = c('original_title', 'date_published', 'writer', 
              'production_company', 'description', 'avg_vote', 
              'votes', 'budget', 'metascore',
              'reviews_from_users', 'reviews_from_critics', 
              'votes_10', 'votes_9', 'votes_8', 'votes_7', 'votes_6', 
              'votes_5', 'votes_4', 'votes_3', 'votes_2', 'votes_1',
              "allgenders_0age_votes", "allgenders_18age_votes",
              "allgenders_30age_votes", "allgenders_45age_votes",
              "males_allages_votes", "males_0age_votes",
              "males_18age_votes", "males_30age_votes",
              "males_45age_votes", "females_allages_votes",
              "females_0age_votes", "females_18age_votes", "females_45age_votes",
              "top1000_voters_votes", "us_voters_votes", "non_us_voters_votes")
imdb_df = imdb_df[, !(colnames(imdb_df) %in% drop_cols), drop=FALSE]

#Drop observations before 2005 (Analysis is on movies released with the last 15 years)
imdb_df = imdb_df %>% filter(year >= 2005 & year <= 2019)

#Clean Year Column
unique(imdb_df$year)
imdb_df$year = gsub('TV Movie ', '', imdb_df$year)
imdb_df$year = as.numeric(imdb_df$year)

#Review Dataset
summary(imdb_df)

#Convert usa_gross_income column to integers
imdb_df$usa_gross_income = gsub('[$] ', '', imdb_df$usa_gross_income)
imdb_df$usa_gross_income = as.numeric(imdb_df$usa_gross_income)

#Convert worldwide_gross_income to integers
colnames(imdb_df)[11] = 'worldwide_gross_income'
imdb_df$worldwide_gross_income = gsub('[$] ', '', imdb_df$worldwide_gross_income)
imdb_df$worldwide_gross_income = as.numeric(imdb_df$worldwide_gross_income)

#Create movie_length column and create values short, medium, long
imdb_df$movie_length = imdb_df$duration

imdb_df$movie_length[imdb_df$movie_length < 90] = 1
imdb_df$movie_length[imdb_df$movie_length >= 90 & imdb_df$movie_length <= 120] = 2
imdb_df$movie_length[imdb_df$movie_length > 120] = 3

imdb_df$movie_length[imdb_df$movie_length == 1] = 'short'
imdb_df$movie_length[imdb_df$movie_length == '2'] = 'medium'
imdb_df$movie_length[imdb_df$movie_length == '3'] = 'long'

#Create new df for genre analysis
genre_df = separate_rows(imdb_df, 'genre', sep = ',')
genre_df$genre = trimws(genre_df$genre)

  #Remove genres where count = 1
  genre_df %>% 
    group_by(genre) %>% 
    count()
  genre_df = genre_df[!(genre_df$genre %in% c('Documentary', 'News', 'Reality-TV')),]

#Create new df for country analysis
country_df = separate_rows(imdb_df, 'country', sep = ',')
country_df$country = trimws(country_df$country)

  #Drop missing values
  country_df = country_df[!(country_df$country == ''),]

  #Include only countries with over 100+ movies (23 countries)
  country_500 = country_df %>% 
    group_by(country) %>% 
    count() %>% 
    filter(n >= 500) %>% 
    select(country)

    country_vector = as_vector(country_500)
    country_df = country_df %>% filter(country %in% country_vector)

#Create new df for director analysis
director_df = separate_rows(imdb_df, 'director', sep = ',')
director_df$director = trimws(director_df$director)
  
  #Drop missing values
  director_df = director_df[!(director_df$director == ''),]

  #Include only directors with 5+ U.S. movies (191 directors)
  director_5 = director_df %>% 
    group_by(director) %>%
    filter(country == 'USA') %>% 
    count() %>%
    filter(n >= 5) %>%
    arrange(desc(n)) %>% 
    select(director)
  
  director_vector = as_vector(director_5)
  director_df = director_df %>% filter(director %in% director_vector)
  
#Create new df for actor analysis
actors_df = separate_rows(imdb_df, 'actors', sep = ',')
actors_df$actors = trimws(actors_df$actors)

  #Drop missing values
  actors_df = actors_df[!(actors_df$actors == ''),]

  #Include only actors with 20+ U.S. movies (1569 actors)
  actors_20 = actors_df %>% 
      group_by(actors) %>%
      filter(country == 'USA') %>% 
      count() %>%
      filter(n >= 10) %>%
      arrange(desc(n)) %>% 
      select(actors)
  
    actors_vector = as_vector(actors_20)
    actors_df = actors_df %>% filter(actors %in% actors_vector)

#Export dataframes to CSVs
write.csv(genre_df, "genre.csv", row.names = TRUE)
write.csv(country_df, "country.csv", row.names = TRUE)
write.csv(actors_df, "actors.csv", row.names = TRUE)
write.csv(director_df, "director.csv", row.names = TRUE)
write.csv(imdb_df, "imdb.csv", row.names = TRUE)
