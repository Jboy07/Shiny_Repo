
shinyServer(function(input, output) {
    
    ### Home Tab ###
    
    output$titles <- renderValueBox({
        total <- nrow(imdb)
        text <- 'Movie Titles'
        valueBox(total, subtitle = text, icon = icon("list"), color = 'yellow')
    })
    
    output$years <- renderValueBox({
        valueBox('2005 to 2019', subtitle = 'Release Date', 
                icon = icon("clock"), color = 'yellow')
    })
    
    output$votes <- renderValueBox({
        total <- '~440 Million'
        text <- 'Total Viewer Ratings'
        valueBox(total, subtitle = text, icon = icon("thumbs-up"), color = 'yellow')
    })
    
    ### Genre Tab ### 
    
    output$genre_rating <- renderPlot(
        genre %>% 
            filter(year == input$daterange) %>% 
            group_by(genre) %>%
            summarise(mean = mean(mean_vote, na.rm = TRUE)) %>% 
            arrange(mean) %>%
            mutate(genre = factor(genre, levels = unique(genre))) %>%
            ggplot() +
            geom_bar(aes(x=mean, y=genre), stat='identity',
                     color = 'black', fill = 'orange') +
            labs(title='Average Rating') +
            coord_cartesian(xlim = c(4, 7)) +
            theme_wsj() + 
            theme(plot.title = element_text(hjust = 0.5))
    )
    
    output$genre_income <- renderPlot(
        genre %>%
            filter(year == input$daterange) %>% 
            group_by(genre) %>%
            summarise(mean = mean(worldwide_gross_income, na.rm = TRUE)) %>% 
            arrange(mean) %>%
            mutate(genre = factor(genre, levels = unique(genre))) %>%
            ggplot() +
            geom_bar(aes(x=mean, y=genre), stat='identity',
                     color = 'black', fill = 'green4') +
            labs(title='Average Income') +
            theme_wsj() + 
            theme(plot.title = element_text(hjust = 0.5))
    )
    
    output$genre_distribution <- renderPlot(
        genre %>%     
            filter(year == input$daterange) %>% 
            ggplot() +
            geom_boxplot(aes(x=mean_vote, y=genre), fill = 'dodgerblue') +
            labs(title='Rating Spread') +
            theme_wsj() + 
            theme(plot.title = element_text(hjust = 0.5))
    )
    
    output$genre_trend <- renderPlot(
        genre %>%
            group_by(genre, year) %>% 
            summarise(mean = mean(mean_vote, na.rm = TRUE)) %>% 
            ggplot(aes(x = year, y = mean)) + 
            geom_line() +
            facet_wrap(~ genre) +
            labs(title='Average Rating by Year') +
            scale_y_continuous(breaks=seq(1,10,1)) +
            theme_wsj() + 
            theme(plot.title = element_text(hjust = 0.5))
    )
    
    ### Duration Tab ### 
    
    output$duration_mean <- renderPlot(
        imdb %>%
            group_by(year) %>%
            summarise(mean = mean(duration, na.rm = TRUE)) %>% 
            ggplot(aes(x = year, y = mean)) + 
            geom_line() +
            theme_wsj() +
            ggtitle('Average Duration By Year') +
            theme(plot.title = element_text(hjust = 0.5))
    )
    
    output$duration_v_rating <- renderPlot(
        imdb %>%
            filter(year == input$daterange) %>%
            ggplot(aes(x=duration, y=mean_vote)) +
            geom_point(alpha = 0.1) +
            geom_smooth(method=lm) +
            labs(title='Rating Distribution') +
            coord_cartesian(xlim = c(40, 200), ylim = c(1, 10)) +
            theme_wsj() + 
            theme(plot.title = element_text(hjust = 0.5))
    )
    
    output$duration_rating <- renderPlot(
        imdb %>%
            filter(year == input$daterange) %>% 
            group_by(movie_length) %>%
            summarise(mean = mean(mean_vote, na.rm = TRUE)) %>% 
            arrange(mean) %>%
            ggplot() +
            geom_bar(aes(x=movie_length, y=mean, color=movie_length), stat='identity',
                     color = 'black', fill = 'orange') +
            labs(title='Average Rating') +
            coord_cartesian(ylim = c(5, 7)) +
            theme_wsj() + 
            theme(plot.title = element_text(hjust = 0.5))
    )
    
    output$duration_income <- renderPlot(
        imdb %>% 
            filter(year == input$daterange) %>% 
            group_by(movie_length) %>%
            summarise(mean = mean(worldwide_gross_income, na.rm = TRUE)) %>% 
            arrange(mean) %>%
            ggplot(aes(x=movie_length, y=mean)) +
            geom_bar(aes(x=movie_length, y=mean), stat='identity',
                     color = 'black', fill = 'green4') +
            labs(title='Average Income') +
            theme_wsj() + 
            theme(plot.title = element_text(hjust = 0.5))
    )
    
    output$duration_distribution <- renderPlot(
        imdb %>% 
            filter(year == input$daterange) %>%
            ggplot() +
            geom_boxplot(aes(x=movie_length, y=mean_vote), fill = 'dodgerblue') +
            labs(title='Rating Spread') +
            coord_cartesian(ylim = c(1, 10)) +
            theme_wsj() + 
            theme(plot.title = element_text(hjust = 0.5))
    )
    
    output$duration_rating_trend <- renderPlot(
        imdb %>% 
            group_by(year, movie_length) %>%
            summarise(mean = mean(mean_vote, na.rm = TRUE)) %>% 
            ggplot(aes(x = year, y = mean)) + 
            geom_line(aes(color = movie_length)) +
            theme_wsj() +
            ggtitle('Average Rating by Year') +
            theme(plot.title = element_text(hjust = 0.5))
    )
    
    output$duration_income_trend <- renderPlot(
        imdb %>% 
            group_by(year, movie_length) %>%
            summarise(mean = mean(worldwide_gross_income, na.rm = TRUE)) %>%
            ggplot(aes(x = year, y = mean)) + 
            geom_line(aes(color = movie_length)) +
            theme_wsj() +
            ggtitle('Average Income by Year') +
            theme(plot.title = element_text(hjust = 0.5))
    )
    
    ### Actors & Directors Tab ### 
    
    output$actors_rating <- renderPlot(
        actors %>% 
            filter(year == input$daterange) %>%
            group_by(actors) %>%
            summarise(mean = mean(mean_vote, na.rm = TRUE)) %>% 
            arrange(mean) %>%
            mutate(actors = factor(actors, levels = unique(actors))) %>%
            top_n(20) %>% 
            ggplot() +
            geom_bar(aes(x=mean, y=actors), stat='identity',
                     color = 'black', fill = 'orange') +
            labs(title='Average Rating: Actor') +
            coord_cartesian(xlim = c(5, 10)) +
            theme_wsj()
    )
    
    output$actors_income <- renderPlot(
        actors %>% 
            filter(year == input$daterange) %>%
            group_by(actors) %>%
            summarise(mean = mean(worldwide_gross_income, na.rm = TRUE)) %>% 
            arrange(mean) %>%
            mutate(actors = factor(actors, levels = unique(actors))) %>%
            top_n(20) %>% 
            ggplot() +
            geom_bar(aes(x=mean, y=actors), stat='identity',
                     color = 'black', fill = 'green4') +
            labs(title='Average Income: Actor') +
            theme_wsj()
    )
    
    output$director_rating <- renderPlot(
        director %>%
            filter(year == input$daterange) %>%
            group_by(director) %>%
            summarise(mean = mean(mean_vote, na.rm = TRUE)) %>% 
            arrange(mean) %>%
            mutate(director = factor(director, levels = unique(director))) %>%
            top_n(20) %>% 
            ggplot() +
            geom_bar(aes(x=mean, y=director), stat='identity',
                     color = 'black', fill = 'orange') +
            labs(title='Average Rating: Director') +
            coord_cartesian(xlim = c(5, 10)) +
            theme_wsj()
    )
    
    output$director_income <- renderPlot(
        director %>%
            filter(year == input$daterange) %>%
            group_by(director) %>%
            summarise(mean = mean(worldwide_gross_income, na.rm = TRUE)) %>% 
            arrange(mean) %>%
            mutate(director = factor(director, levels = unique(director))) %>%
            top_n(20) %>% 
            ggplot() +
            geom_bar(aes(x=mean, y=director), stat='identity',
                     color = 'black', fill = 'green4') +
            labs(title='Average Income: Director') +
            theme_wsj()
    )
    
    ### Country Tab ### 
    
    output$country_rating <- renderPlot(
        country %>% 
            filter(year == input$daterange) %>%
            group_by(country) %>%
            summarise(mean = mean(mean_vote, na.rm = TRUE)) %>% 
            arrange(mean) %>%
            mutate(country = factor(country, levels = unique(country))) %>%
            ggplot() +
            geom_bar(aes(x=mean, y=country), stat='identity',
                     color = 'black', fill = 'orange') +
            labs(title='Average Rating') +
            coord_cartesian(xlim = c(4, 7)) +
            theme_wsj() + 
            theme(plot.title = element_text(hjust = 0.5))
    )
    
    output$country_income <- renderPlot(
        country %>% 
            filter(year == input$daterange) %>%
            group_by(country) %>%
            summarise(mean = mean(worldwide_gross_income, na.rm = TRUE)) %>% 
            arrange(mean) %>%
            mutate(country = factor(country, levels = unique(country))) %>%
            ggplot() +
            geom_bar(aes(x=mean, y=country), stat='identity',
                     color = 'black', fill = 'green4') +
            labs(title='Average Income') +
            theme_wsj() + 
            theme(plot.title = element_text(hjust = 0.5))
    )
    
    output$country_trend <- renderPlot(
        country %>% 
            group_by(country, year) %>% 
            summarise(mean = mean(mean_vote, na.rm = TRUE)) %>% 
            ggplot(aes(x = year, y = mean)) + 
            geom_line() +
            facet_wrap(~ country) +
            labs(title='Average Rating by Year') +
            scale_y_continuous(breaks=seq(1,10,1)) +
            theme_wsj() + 
            theme(plot.title = element_text(hjust = 0.5))
    )
    
    ### Data Tab ### 
    
    output$table <- DT::renderDataTable({
        DT::datatable(
            imdb,
            filter = 'top', extensions = c('Buttons', 'Scroller'),
            options = list(scrollY = 600,
                           scrollX = 50,
                           deferRender = TRUE,
                           scroller = TRUE,
                           buttons = list('excel'),
                           dom = 'lBfrtip',
                           fixedColumns = TRUE), 
            rownames = FALSE) 
    })
    
})
