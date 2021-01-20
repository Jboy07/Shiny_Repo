# shiny_repo

What Types of Movies Will Maximize Subscriptions?

This Shiny app was designed to explore the most popular types of movies in recent years. The motivation for this project was to identify the types of movies streaming services may wish to acquire or produce to seek to maximize subscriptions.

Analysis is based on data obtained from the IMDb movies extensive dataset from Kaggle, which in turn was scraped from the popular movie website IMDb. For purposes of this app, popularity is measured by viewers' IMDb ratings (on a scale of 1-10) and worldwide gross income (USD).

The main source code for the app can be found in the ui.R, server.R, and global.R files. The Preprocessing.R file shows how the original dataset was cleaned and manipulated in order to analyze different variables (Genre, Duration, Actors & Directors, Country) within the app.

While the original dataset includes titles dating back to the early 1900s, this analysis only considered titles released since 2005. I wished to focus on how viewers feel about movies released in recent years as streaming has become increasingly popular. I suspect that viewer preferences have changed in recent years due to the increasing number of convenient ways viewers are able to watch movies.

The app is divided into the following pages/tabs:

Home: Project overview.
Genre/Duration/Actors & Directors/Country: Data visualizations showing the movie popularity trends in these respective categories.
Data: The cleaned dataset for the user to explore.
Notes: Links to my blog page, LinkedIn and GitHub.

The app also includes a “Date Range” slider which allows the user to view the data output within a certain date range (in years).


Link to the Shiny App Here: https://jboy.shinyapps.io/jf_shiny_project/

This project was created during the NYC Data Science Academy Fall 2020 Bootcamp.
