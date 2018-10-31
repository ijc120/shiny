
shinyUI(
  navbarPage(
    theme = shinytheme("flatly"),
    'Health Matters',
             navbarMenu("About",
                        tabPanel(title='Intro',
                                 fluidPage(
                                   fluidRow(
                                     column(12, includeMarkdown("test.md"))
                                   )
                                 )),
                        tabPanel(title='Appendix',
                                 fluidPage(
                                   column(12, includeMarkdown('appe.md'))
                                 )),
                        tabPanel(title='Contact',
                                 fluidPage(
                                   column(12, includeMarkdown('contact.md'))
                                 ))
                        ),
             tabPanel("Unhealthy Behaviors and Outcomes By Year",
                      fluidPage(
                        sidebarLayout(
                          sidebarPanel(
                            fluidRow(h5("Select the year and two health factors to see the coorelation accross U.S."),
                              column(6, selectizeInput(inputId="Year",
                                                       label="Choose an year",
                                                       choices= year_lis))
                            ),

                            fluidRow(
                              column(6, selectizeInput(inputId="measure1",
                                                       label="Choose a behavioral risk factor",
                                                       choices= factor_lis))
                            ),
                            
                            fluidRow(
                              column(6, selectizeInput(inputId="measure2",
                                                       label="Choose an outcome",
                                                       choices= result_lis))
                            )
                    
                            ),
                          mainPanel(
                            fluidRow(
                              column(6, htmlOutput("map1")),
                              column(6, htmlOutput("map2"))
                            ),
                            fluidRow(h4("The correlation between measures is :"),
                                     textOutput("corr_statement") ),
                            fluidRow(
                              column(6,htmlOutput("corr_1"))
                            )
                            )
                        ) 
                      )
                      ),
             tabPanel("Behaviors & Outcomes",
                      fluidPage(
                            fluidRow(
                              column(6,
                                h5("Click a correlation on the heatmap to view scatter plot."),
                                plotlyOutput("heat")),
                              column(6,
                              plotlyOutput("corrs"))
                      )
                            
                          
                                   )
                 ),
            tabPanel(
              "Median Annual Income vs Health Factors",
              fluidPage(
                  column(12,plotlyOutput('bubble',height = "100%")),
                  fluidRow(
                  column(2, selectizeInput(inputId="Year2", label="Choose an year",choices= year_lis)),
                  column(2, selectizeInput(inputId="measure4",
                                           label="Choose a behavioral risk factor",choices= measure_lis)))
              )
            ),
            tabPanel(
              "Subpopulation",
              fluidPage(
                    fluidRow(
                      column(3, selectizeInput(inputId="Year3",
                                               label="Choose an year",
                                               choices= year_lis)),
                      column(3, selectizeInput(inputId="measure3",
                                               label="Choose a measurement",
                                               choices= measure_lis))
                    ),
                    fluidRow(
                      column(12,
                             box(width=NULL,
                                 plotlyOutput('trend')))
                      
                    ),
                    fluidRow(
                      column(6, selectizeInput(inputId="State1",
                                               label="Choose a state",
                                               choices= state_lis),
                             "*Percentage of adults *Per 100,000 population"
                             )),
                    fluidRow(
                      column(2,
                             box(width=NULL, height=150,
                                 plotlyOutput('bar')
                             )
                             ),
                      column(2,
                             box(width=NULL, height=150,
                                 plotlyOutput('bar2')
                             )
                      ),
                      column(2,
                             box(width=NULL, height=150,
                                 plotlyOutput('bar5')
                             )
                      ),
                      column(3,
                             box(width=NULL, height=550,
                                 plotlyOutput('bar3')
                             )
                      ),
                      column(3,
                             box(width=NULL, height=550,
                                 plotlyOutput('bar4'))
                      )
                      

                  )
                )

             ),
      tabPanel('Resources',
               fluidPage(
                 column(12, includeMarkdown('resource.md'))
               ))    
  )
)
    

 

 



