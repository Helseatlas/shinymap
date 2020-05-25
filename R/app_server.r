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
    selection <- select_server("atlas",
                               language = shiny::reactive(input$language),
                               data = healthatlas_data,
                               config = config
                               )

    if (!exists("git_hash")) {
      git_hash <- NULL
    }

    if (!exists("github_repo")) {
      github_repo <- NULL
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

    tab_server("plots",
               data = selection$data,
               map = shiny::reactive(healthatlas_data[[selection$atlas()]]$map),
               config = config,
               language = shiny::reactive(input$language)
               )

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

  output$desc_text <- shiny::renderUI({
    if ("description" %in% colnames(selection$data())) {
      selected_theme <- selection$data()
      desc <- selected_theme$description
      shiny::HTML(as.character(desc[1]))
    }
  })
}
