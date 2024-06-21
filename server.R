function(input,output) {
  
  final_data <- reactive({
    reg_qt<-subset(imdb, 
                   genres==input$genres & 
                     language==input$language)
    
    return(reg_qt)
  })
  output$summary<-renderPrint(
    movies %>%
      summary()
  )
  
  output$share<-renderDataTable({
    
    DT::datatable(final_data(),filter = 'none', selection = "none", rownames= TRUE,
                  options = list(scrollX = TRUE,
                                 scrollY = TRUE,
                                 paging = FALSE,
                                 dom="t",
                                 columnDefs = list(
                                   list(className = 'dt-center',
                                        targets = "_all")),
                                 scrollCollapse = TRUE,
                                 tabIndex = 1,
                                 ordering = FALSE,
                                 searching = FALSE,
                                 dom = 'Bfrtip'
                  ))
    
    
  })
  
  output$bubble<-renderPlot({ 
    data<-final_data()
    graph <- ggplot(data,aes(x=title_year,y=language,size=country,color=imdb_score))+geom_point(alpha=6)
   
   
    
    return(graph)
    
    
  }) 
  output$bar=renderPlot({
    data<-final_data()
    print(data)
    graph_bar<-ggplot(data,aes(x=imdb_score,y=movie_title,fill=country))+
      geom_bar(stat="identity", width = 0.8)+labs(title = "IMDB Scores by Movie Title and Country",x="imdb_score",y="movie_title", fill = "Country")+
      theme(plot.title = element_text(hjust = 0.5))
    return(graph_bar)
    ggplotly(graph_bar)
    
  })
 
}



  
