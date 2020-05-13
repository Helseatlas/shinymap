test_that("tab_server", { 
  
  # filtrert data
  filtered_data <- readRDS("data/filtered_data_3.rds")
  
  
  
  shiny::testServer(tab_server, {
    
    # a) input data er som forventet
    #expect_equal(ncol(filtered_data), )
    #expect_equal(colnames(data()), c("", ""))
    
    
    # b) generere plot uten feilmelding
    output$plot_histogram
    
    
  } , args = list(data = filtered_data)
  
  
  )
  
}
)