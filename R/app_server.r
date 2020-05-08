#' Server side logic of the application
#'
#' @param input shiny input components
#' @param output shiny output components
#' @param session the shiny session parameter
#'
#' @return ignored
#' @export
app_server <- function(input, output, session) {
    healthatlas_data <- get_data() # nolint
    config <- get_config() # nolint

    # Run the select_server shiny module
    # User pick atlas and sample
    # Module returns name of choosen atlas and data from choosen sample
    filtered_data <- select_server("test1",
                  language = shiny::reactive(input$language),
                  healthatlas_data = healthatlas_data,
                  config = config
                  )

    if (!exists("git_hash")) {
      git_hash <- NULL
    }

    if (!exists("github_repo")) {
      github_repo <- NULL
    }

    if (isTRUE(getOption("shiny.testmode"))) {
      # Load static/dummy data if this is a test run
      healthatlas_data <- helseatlas::testdata
      healthatlas_map <- helseatlas::testmap
    }

    output$pick_language <- shiny::renderUI({
        shinyWidgets::radioGroupButtons(
          size = "sm",
          direction = "vertical",
          status = "default",
          inputId = "language",
          label = "",
          choices = c("NO" = "nb",
                      "EN" = "en"
          ),
          selected = "nb"
        )
    })

    atlas_data <- shiny::reactive({
      shiny::req(input$language)
      if (input$language == "nb") {
        num_atlas <- 1
      } else if (input$language == "en") {
        num_atlas <- 2
      }

      # Return the data for a given atlas.
      if (!is.data.frame(healthatlas_data)) {
        if (is.null(input$atlas)) {
          return(NULL)
        } else {
          return(healthatlas_data[[input$atlas]][[num_atlas]])
        }
      } else {
        return(healthatlas_data)
      }
    })

    atlas_map <- shiny::reactive({
      shiny::req(filtered_data)
      if (!exists("healthatlas_map")) {
        return(sf::st_transform(healthatlas_data[[filtered_data$atlas()]][["map"]], 32633))
      } else {
        return(sf::st_transform(healthatlas_map, 32633))
      }
    })


    output$git_version <- shiny::renderUI({
      if (!is.null(git_hash)) {
        if (is.null(github_repo)) {
          version_num <- substr(git_hash, 1, 8)
        } else {
          version_num <- paste0("<a href='https://github.com/",
                                github_repo,
                                "/tree/",
                                git_hash,
                                "'>",
                                substr(git_hash, 1, 8),
                                "</a>")
        }
        # Hash on web page, if given
        return(shiny::HTML(paste0("Version: ", version_num)))
      }
    })

    output$title <- shiny::renderUI({
      shiny::req(input$language)
      if (!exists("webpage_title") || is.null(webpage_title)) {
        # Define the atlas title, if not defined
        webpage_title <- config$title[[input$language]]
      }

      return(shiny::HTML(paste0("<h1>", webpage_title, "</h1>")))
    })

    output$plot_map <- leaflet::renderLeaflet({
      shiny::req(filtered_data)
      map <- helseatlas::make_map(map = atlas_map(), data = filtered_data$data())
      return(map)
    })

    output$plot_histogram <- shiny::renderPlot({
      shiny::req(filtered_data)
      plot <- helseatlas::plot_variation(
        input_data = filtered_data$data(),
        xlab = config$plot$xlab[[input$language]],
        ylab = "TBA"
      )
      return(plot)
    }
    , height = 800, width = 600)

    output$make_table <- shiny::renderTable({
      shiny::req(filtered_data)
      data_to_tabulate <- filtered_data$data()

      # Return null if data in invalid
      if (is.null(nrow(data_to_tabulate)) || nrow(data_to_tabulate) == 0) {
        return(NULL)
      }

      tabular_data <- data.frame(data_to_tabulate$area_name)
      colnames(tabular_data) <- c(config$plot$xlab[[input$language]])
      value_name <- as.character(unique(data_to_tabulate$type))
      tabular_data[value_name] <- data_to_tabulate$value
      numerator_name <- as.character(unique(data_to_tabulate$numerator_name))
      tabular_data[numerator_name] <- data_to_tabulate$numerator
      denominator_name <- as.character(unique(data_to_tabulate$denominator_name))
      tabular_data[denominator_name] <- data_to_tabulate$denominator
      # Sort data
      tabular_data <- tabular_data[order(tabular_data[, 2], na.last = TRUE, decreasing = TRUE), ]
      # Format numbers
      tabular_data[, -1] <- sapply(tabular_data[, -1],
                                   FUN = function(x) format(x,
                                                            digits = 2,
                                                            decimal.mark = config$num$decimal[[input$language]],
                                                            big.mark = config$num$big[[input$language]]
                                                            )
                                   )
      return(tabular_data)
    }
    , align = "lrrr")

  output$app_info <- shiny::renderUI({
    shiny::actionButton(
      inputId = "app_info",
      label = "",
      icon = shiny::icon("info")
    )
  })

  shiny::observeEvent(input$app_info, {
    shinyalert::shinyalert(title = config$info$title[[input$language]],
                           text = version_info(),
                           type = "",
                           closeOnEsc = TRUE, closeOnClickOutside = TRUE,
                           html = TRUE,
                           confirmButtonText =
                             sample(config$info$action_button$no_opt_out_ok[[input$language]], 1))
  })
}
