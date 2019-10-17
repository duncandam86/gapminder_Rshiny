library(shiny)
library(plotly)
library(DT)
library(ggplot2)
library(dplyr)

shinyServer(function(input, output){
  #=======================================Render Overview tab =================================
  #Render in tab visualization
  data <- gapminder 
  #rendering first plot
  output$t1_plot <- renderPlotly({
    if(input$t1_year != 'All'){
      data <- data %>%
        filter(year %in% input$t1_year)
    }
    #input$axis returns string so need to use
    p <- ggplot(data, aes_string(x=input$t1_xaxis, 
                                 y=input$t1_yaxis,
                                 size = data$pop)) + 
      geom_point(aes(text = country, color = continent, year = year)) +
      theme(axis.title = element_text(size = 11),
            legend.title = element_blank(),
            panel.background = element_blank(),
            panel.grid = element_line(colour = 'gray')) 
    
    if(input$t1_xaxis == 'gdpPercap'){
      p <- p + 
        xlab('GPD per capita')
    }
    if(input$t1_xaxis == 'lifeExp'){
      p <- p + 
        xlab('Life Expectancy') 
    }
    if(input$t1_xaxis == 'pop'){
      p <- p + 
        xlab('Population')
    }
    if(input$t1_yaxis == 'pop'){
      p <- p + 
        ylab('Population')
    }
    if(input$t1_yaxis == 'gdpPercap'){
      p <- p + 
        ylab('GDP per capita')
    }
    if(input$t1_yaxis == 'lifeExp'){
      p <- p + 
        ylab('Life Expectancy')
    }
    if (input$t1_xaxis == 'gdpPercap'){
      p <- p + scale_x_log10()
    }
    if (input$t1_yaxis == 'gdpPercap'){
      p <- p + scale_y_log10()
    }
    
    if(input$fit){
      p <- p+geom_smooth(
        stat = 'smooth',
        method = 'lm'
      )
    }
    
    #make ggplot interactive
    ggplotly(p, tooltip = c('x','y','size','text','year'))
  })
  
  #rendering second plot
  data_pop <- data %>%
    group_by(continent,year) %>%
    summarize(meanPop = mean(pop)/1000000) 
  
  output$t2_plot <- renderPlotly({
    if(input$t1_year != 'All'){
      data_pop <- data_pop %>%
        filter(year %in% input$t1_year)
    }
    
    #input$axis returns string so need to use
    p1 <- ggplot(data_pop, aes(x = year, 
                               y = meanPop)) + 
      geom_col(aes(fill = continent, 
                   color = continent)) +
      ylab('Avg.Population (mil)') +
      coord_flip()+
      theme(axis.title.x = element_text(size =  11),
            axis.title.y = element_blank(),
            axis.line = element_line(color = 'gray'),
            legend.title = element_blank(),
            legend.position = 'none',
            panel.background = element_blank()) +
      scale_x_discrete(limits =c(unique(data_pop$year))) 
    #make ggplot interactive
    ggplotly(p1, tooltip = c('x','y'))
  })
  
  #rendering third plot
  data_life <- data %>%
    group_by(continent,year) %>%
    summarize(meanlifeExp = mean(lifeExp)) 
  
  output$t3_plot <- renderPlotly({
    if(input$t1_year != 'All'){
      data_life <- data_life %>%
        filter(year %in% input$t1_year)
    }
    
    #input$axis returns string so need to use
    p2 <- ggplot(data_life, aes(x=year, 
                                y=meanlifeExp)) + 
      geom_line(aes(fill = continent, 
                    color = continent)) +
      geom_point(aes(fill = continent, 
                     color = continent)) +
      ylab('Avg.Life Expectancy') +
      theme(axis.title.y = element_text(size =  11),
            axis.title.x = element_blank(),
            axis.line = element_line(color = 'gray'),
            legend.title = element_blank(),
            panel.background = element_blank()) 
    
    #make ggplot interactive
    ggplotly(p2, tooltip = c('x','y'))
  })
  
  #rendering box plot
  #rendering third plot
  data_gdp <- data %>%
    group_by(continent,year) %>%
    summarize(meanGDP = mean(gdpPercap)) 
  
  output$t4_plot <- renderPlotly({
    if(input$t1_year != 'All'){
      data_gdp <- data_gdp %>%
        filter(year %in% input$t1_year)
    }
    
    #input$axis returns string so need to use
    p3 <- ggplot(data_gdp, aes(x=year, 
                               y=meanGDP)) + 
      geom_col(aes(fill = continent,
                   color = continent),
               position = position_dodge(4),
               alpha = .4)+
      ylab('Avg.GDP per capita') +
      theme(axis.title.y = element_text(size =  11),
            axis.title.x = element_blank(),
            legend.title = element_blank(),
            panel.background = element_blank(),
            panel.grid.major.y = element_line(color = 'gray')) 
    #scale_x_discrete(limits =c(data_gdp$year)) +
    #scale_y_log10()
    
    #make ggplot interactive
    ggplotly(p3, tooltip = c('x','y','country'))
  })
  
  #rendering table
  output$table_output1 <- renderDataTable({
    if(input$t1_year != 'All'){
      data <- data %>%
        filter(year %in% input$t1_year)
    }
    
    datatable(data,
              options = list(pageLength = 11),
              rownames = FALSE
    ) %>%
      formatStyle(colnames(data),color = 'black')
  })
  
  
  #=======================================Render continent tab =================================
  output$cont_plot <- renderPlotly({
    data_cont <- data %>%
      filter(continent == input$cont, year == input$cont_year)
    
    p4 <- ggplot(data_cont, aes_string(x=input$cont_xaxis, 
                                       y=input$cont_yaxis)) + 
      geom_point(aes(country = country, size = pop, color = 'colors')) +
      scale_color_manual(name ="Val", values=c(colors = input$cont_col))+
      theme(legend.position = 'none',
            panel.background = element_blank(),
            panel.grid = element_line(color = 'gray'))
    
    if(input$cont_xaxis == 'gdpPercap'){
      p4 <- p4 + 
        xlab('GPD per Capita')
    }
    if(input$cont_xaxis == 'lifeExp'){
      p4 <- p4 + 
        xlab('Life Expectancy')
    }
    if(input$cont_xaxis == 'pop'){
      p4 <- p4 + 
        xlab('Population')
    }
    if(input$cont_yaxis == 'pop'){
      p4 <- p4 + 
        ylab('Population')
    }
    if(input$cont_yaxis == 'gdpPercap'){
      p4 <- p4 + 
        ylab('GDP per Capita')
    }
    if(input$cont_yaxis == 'lifeExp'){
      p4 <- p4 + 
        ylab('Life Expectancy')
    }
    if (input$cont_xaxis == 'gdpPercap'){
      p4 <- p4 + scale_x_log10()
    }
    if (input$cont_yaxis == 'gdpPercap'){
      p4 <- p4 + scale_y_log10()
    }
    
    if(input$cont_fit){
      p4 <- p4 + geom_smooth(
        stat = 'smooth',
        method = 'lm'
      )
    }
    #make ggplot interactive
    ggplotly(p4, tooltip = c('x','y','country'))
  })
  
  #render piechart
  data_react <- reactive({
    data %>%
      mutate(gdp = gdpPercap * pop) %>%
      filter(year == input$cont_year, continent == input$cont)
  })
  
  output$pie_pop <- renderPlotly({
    p5 <- plot_ly(data_react(), labels = ~ country, values = ~pop, type = 'pie',
                  textposition = 'inside', 
                  textinfo = 'label',
                  showlegend = FALSE,
                  opacity = 0.75,
                  marker = list(line = list(color = 'white', width =0.5))
    ) %>%
      layout(title  = sprintf('%s Population in %s', input$cont, input$cont_year),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showtickLabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showtickLabels = FALSE ))
    p5
  })
  
  #render life expectancy pie
  output$pie_life <- renderPlotly({
    p6 <- ggplot(data_react(), aes(x = country, y = lifeExp)) +
      geom_bar(stat = 'identity', aes (fill = country, color = country, alpha = .75))+
      ggtitle(sprintf('%s Life Expectancy in %s', input$cont, input$cont_year)) +
      theme(legend.position = 'none',
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks= element_blank(),
            panel.background = element_blank())
    ggplotly(p6, tooltip = c('x','y'))
  })
  
  output$pie_gdp <- renderPlotly({
    #render GDP pie
    p7 <- plot_ly(data_react(), labels = ~ country, values = ~ gdp,
                  textposition = 'inside', 
                  textinfo = 'label',
                  showlegend = FALSE,
                  opacity = 0.75,
                  marker = list(line = list(color = 'white', width =0.5))
    ) %>%
      add_pie(hole = 0.6)%>%
      layout(title  = sprintf('%s GDP in %s', input$cont, input$cont_year),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showtickLabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showtickLabels = FALSE ))
    
    p7
  })
  
  #rendering table
  output$cont_table <- renderDataTable({
    datatable(data_react(),
              options = list(pageLength = 10),
              rownames = FALSE
    ) %>%
      formatStyle(colnames(data_react()),color = 'black')
  })
  
  #==================================Rendering Country Tab================
  data_react_2 <- reactive({
    data %>%
      mutate (gdp = gdpPercap * pop ) %>%
      filter(country %in% input$count, year %in% input$count_year)
  })
  
  output$count_plot_1 <- renderPlotly({
    
    p8 <- ggplot (data_react_2(), aes_string(y = input$count_axis))+
      geom_bar(aes(x= year, fill= country), position = 'dodge', stat = 'identity', alpha =.6) +
      theme(legend.title = element_blank(),
            axis.title.x = element_blank(),
            panel.background = element_blank(),
            panel.grid.major = element_line(colour = 'gray')) 
    if(input$count_axis == 'gdpPercap'){
      p8 <- p8 + ylab('GDP per Capita') 
    }
    
    if(input$count_axis == 'lifeExp'){
      p8 <- p8 + ylab('Life Expectancy')
    }
    if(input$count_axis == 'pop'){
      p8 <- p8 + ylab('Population')
    }
    if(input$count_axis == 'gdp'){
      p8 <- p8 + ylab('Growth Domestic Product')
    }
    ggplotly(p8, tooltip= c('x','y'))
  })
  
  output$count_text <- renderText({
    if(input$count_axis == 'gdpPercap'){
      text <- 'GDP per Capita'  
    }
    if(input$count_axis == 'lifeExp'){
      text <-'Life Expectancy'
    }
    if(input$count_axis == 'pop'){
      text <- 'Population'
    }
    if(input$count_axis == 'gdp'){
      text <- 'Growth Domestic Product'
    }
    text
  })
  
  output$count_plot_2 <- renderPlotly({
    
    p9 <- ggplot(data_react_2(), aes(x = year, fill=country, color = country, alpha = 0.6)) +
      geom_point(aes_string(y=input$count_axis)) +
      geom_line(aes_string(y=input$count_axis)) +
      theme(legend.title = element_blank(),
            axis.title = element_blank(),
            panel.background = element_blank(),
            strip.background = element_blank(),
            panel.grid = element_line(colour = 'gray'),
            strip.text.x = element_text(size = rel(1.2))) +
      facet_wrap(~country, nrow = 3) 
    
    ggplotly(p9, tooltip = c('x','y'))
    
  })
  
  output$count_table <- renderDataTable({
    datatable(data_react_2(),
              options = list(pageLength = 10),
              rownames = FALSE) %>%
      formatStyle(colnames(data_react_2()),color = 'black')
  })
})