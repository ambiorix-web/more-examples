box::use(
  htmltools[
    tags,
    tagList,
  ],
  . /
    page[
      Page,
    ],
  . /
    components[
      Button,
      TextInput,
      TextAreaInput,
      RadioButtonInput,
    ],
)

#' Contact page UI
#'
#' @export
UI <- function() {
  content <- tags$div(
    class = "container",
    tags$div(
      class = "card my-5 border-0 shadow",
      tags$div(
        class = "card-body p-5",
        tags$h3(
          class = "card-title mb-3",
          "Contact Us"
        ),
        tags$form(
          action = "/contact",
          method = "post",
          tags$div(
            class = "row mb-3",
            tags$div(
              class = "col-12 col-md-6",
              TextInput(
                id = "full_name",
                label = "Full Name",
                required = NA
              )
            ),
            tags$div(
              class = "col-12 col-md-6",
              TextInput(
                id = "email",
                label = "Email",
                type = "email",
                required = NA
              )
            )
          ),
          tags$div(
            class = "mb-3",
            tags$p(
              class = "mb-1",
              "Type of Inquiry"
            ),
            RadioButtonInput(
              id = "inquiry_type",
              choices = list(
                general = "General Inquiry",
                price = "Price Inquiry"
              ),
              selected = "General Inquiry",
              alignment = "inline"
            )
          ),
          tags$div(
            class = "mb-3",
            TextAreaInput(
              id = "message",
              label = "Message",
              required = NA
            )
          ),
          tags$div(
            class = "d-grid",
            Button(
              label = "Submit",
              type = "submit",
              class = "btn-primary"
            )
          )
        )
      )
    )
  )

  Page(content)
}
