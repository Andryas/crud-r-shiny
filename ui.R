bslib::page_fluid(
    style = "padding: 15vw;",
    shinyjs::useShinyjs(),
    reactable.extras::reactable_extras_dependency(),
    bslib::card(
        bslib::card_header(
            class = "d-flex justify-content-between",
            shiny::actionButton(
                inputId = "create_modal",
                label = "",
                icon = shiny::icon("plus"),
                class = "btn-primary",
                width = "auto"
            ),
            shiny::h2("Typology")
        ),
        bslib::card_body(
            reactable::reactableOutput("table"),
            shiny::uiOutput("update")
        )
    )
)