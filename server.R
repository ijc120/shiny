
shinyServer(function(input,output, session){

  output$map1 <- renderGvis({
    gvisGeoChart(filter(newdf,year==input$Year), locationvar = "state", colorvar = input$measure1,
                 options=list(region="US", 
                              displayMode="regions", 
                              resolution="provinces",
                              width=400, height=200,
                              colors="['#603913']"))
    
  })
  output$map2 <- renderGvis({
    gvisGeoChart(filter(newdf,year==input$Year), locationvar = "state", colorvar = input$measure2,
                 options=list(region="US", 
                              displayMode="regions", 
                              resolution="provinces",
                              width=400, height=200,
                              colors="['#990099']"))
    
  })
  
  corr_data <- reactive({
    c1 <- cat_filter(newdf,input$Year,input$measure1)
    c2 <- cat_filter(newdf,input$Year,input$measure2)
    if(nrow(c1)==0 | (nrow(c2)==0)){
      return (0)
    }else{return (cbind(c1,c2))}
    
  })
  
  corr_bw_measures<-reactive({
    m1<-cat_filter(newdf,input$Year,input$measure1)
    m2<-cat_filter(newdf,input$Year,input$measure2)
    if(nrow(m1)==0 | ((m2)==0)){
      return (0)
    }else{return (cor(m1,m2))}
  })
  output$corr_statement <- renderText({corr_bw_measures()})
  
  output$corr_1 <- renderGvis({
    if (corr_data() == 0){
      return (NULL)}
    gvisScatterChart(corr_data(),
                     options=list(width= 800,height= 500,
                                  title="Correlation between the two measures",
                                  legend="{position:'none'}",
                                  hAxis="{title:'Behavior Factors'}",
                                  vAxis="{title:'Outcome'}",
                                  colorAxis="{colors:['#91BFDB', '#FC8D59']}"))
  })
  
  output$corr_2 <- renderGvis({
    gvisScatterChart(corr_data2(),
                     options=list(width= 800,height= 500,
                                  title="Correlation between the two measures",
                                  legend="{position:'none'}",
                                  hAxis="{title:'Measure 3'}",
                                  vAxis="{title:'Measure 4'}"))
  })
  output$heat = renderPlotly({
    plot_ly(x = nms, y = nms, z = correlation,
            key = correlation, type = "heatmap", source = "heatplot", colors = "RdYlGn",
            height = 600) %>%
      colorbar(limits = c(-1, 1)) %>% 
      layout(xaxis = list(title = ""),
             yaxis = list(title = ""))
  })
  output$corrs <- renderPlotly({
    s <- event_data(event = "plotly_click", source = "heatplot")
    if (length(s)) {
      vars <- c(s[["x"]], s[["y"]])
      d <- setNames(measures[vars], c("x", "y"))
      # yhat <- fitted(lm(y ~ x, data = d))
      plot_ly(d, x = ~x) %>% 
        add_markers(y = ~y) %>% 
        # add_lines(y = ~yhat) %>% 
        layout(xaxis = list(title = s[["x"]]),
               yaxis = list(title = s[["y"]]),
               showlegend = FALSE)
    } else{
      plotly_empty()
    }
  })
  filternewdf= reactive({
    slope <- 2.666051223553066e-05
    newdf$size <- sqrt(newdf$Totalpop * slope)
    newdf %>% filter(year==input$Year2) %>% select(year,state,Median_Household_Income,Totalpop,a=input$measure4,size) %>% unique()
  })
  output$bubble <- renderPlotly({
    colors <- c('#4AC6B7', '#1972A4', '#965F8A', '#FF7070', '#C61951')
    plot_ly(filternewdf(),x=~Median_Household_Income,
            y=~ a, color=~state, size=~size ,colors = colors,
            type = 'scatter', mode = 'markers', 
            marker = list(symbol = 'circle', sizemode = 'diameter',
                          line = list(width = 2, color = '#FFFFFF')),
            text = ~paste('State:', state, '<br>Median Household Income:', Median_Household_Income,'<br>Factor:',a,
                          '<br>Population:', Totalpop)) %>%
      layout(title = 'Median Household Income vs. Health Factor',
             xaxis = list(title = 'Median Houshold Income INFLATION-ADJUSTED DOLLARS ($USD)',
                          gridcolor = 'rgb(255, 255, 255)',
                          range = c(25000, 95000),
                          zerolinewidth = 1,
                          ticklen = 5,
                          gridwidth = 2),
             yaxis = list(title = input$measure4,
                          gridcolor = 'rgb(255, 255, 255)',
                          range = c(min(filternewdf() %>% select(a))-5,max(filternewdf() %>% select(a))+5),
                          zerolinewidth = 1,
                          ticklen = 5,
                          gridwith = 2),
             paper_bgcolor = 'rgb(243, 243, 243)',
             plot_bgcolor = 'rgb(243, 243, 243)')
  })
  output$bar <- renderPlotly({
    geodf <- filter(geodata,year==input$Year3&state==input$State1&cat==input$measure3)
    if(nrow(geodf)==0){return(NULL)
      }else{return(
    plot_ly(geodf,
            x= ~subcat, y= ~Value,type = 'bar', text= ~Value,textposition = 'auto',
       marker = list(color = 'rgb(158,202,225)')) %>%
      layout(xaxis = list(title = "URBANICITY", tickangle = -45),
             yaxis = list(title = ""),
             margin = list(b = 10),barmode = 'group')
      )}
    
  })
  output$bar2 <- renderPlotly({
    gendf <- filter(gendata,year==input$Year3&state==input$State1&cat==input$measure3)
    if(nrow(gendf)==0){return(NULL)
      }else{return(
    plot_ly(gendf,
            x= ~subcat, y= ~Value,type = 'bar', text= ~Value,textposition = 'auto',
            marker = list(color = 'rgb(158,202,225)')) %>%
      layout(xaxis = list(title = "GENDER", tickangle = -45),
             yaxis = list(title = ""),
             margin = list(b = 10),barmode = 'group')
      )}
    
  })
  output$bar3 <- renderPlotly({
    edudf <- filter(edudata,year==input$Year3&state==input$State1&cat==input$measure3)
    if (nrow(edudf)==0){return (NULL)
      }else{return( 
    plot_ly(edudf,
            x= ~subcat, y= ~Value,type = 'bar', text= ~Value,textposition = 'auto',
            marker = list(color = 'rgb(158,202,225)')) %>%
      layout(xaxis = list(title = "EDUCATION", tickangle = -45),
             yaxis = list(title = ""),
             margin = list(b = 10),barmode = 'group')
      )}
    
  })
  output$bar4 <- renderPlotly({
    incomedf <- filter(incomedata,year==input$Year3&state==input$State1&cat==input$measure3)
    if(nrow(incomedf)==0){return(NULL)
      }else{return(
    plot_ly(incomedf,
            x= ~subcat, y= ~Value,type = 'bar', text= ~Value,textposition = 'auto',
            marker = list(color = 'rgb(158,202,225)')) %>%
      layout(xaxis = list(title = "INCOME", tickangle = -45),
             yaxis = list(title = ""),
             margin = list(b = 10),barmode = 'group')
      )}
  })
  output$bar5 <- renderPlotly({
    agedf <- filter(agedata,year==input$Year3&state==input$State1&cat==input$measure3)
    if(nrow(agedf)==0){return(NULL)
      }else{return(
    plot_ly(agedf,
            x= ~subcat, y= ~Value,type = 'bar', text= ~Value,textposition = 'auto',
            marker = list(color = 'rgb(158,202,225)')) %>%
      layout(xaxis = list(title = "AGE", tickangle = -45),
             yaxis = list(title = ""),
             margin = list(b = 10),barmode = 'group')
    
      )}
  })
  output$trend <- renderPlotly({
    yeardf <- filter(newdf,year==input$Year3&cat==input$measure3&subcat=="All")
    if(nrow(yeardf)==0){return(NULL)
      }else{return(
    plot_ly(yeardf,
            x= ~state, y= ~Value, type='bar',
            marker = list(color = '#1972A4'))
      )} 
     })


})
  
