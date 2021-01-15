dataSelect <- reactiveValues(type = "all")

DragableChartOutput <- function(inputId, width = "100%", height = "100%") {
  style <- sprintf(
    fmt = "width: %s; height: %s;",
    validateCssUnit(width),
    validateCssUnit(height)
  )

  tagList(
    includeScript("www/d3.min.js"),
    includeScript("www/ChartRendering.js"),

    div(
      id = inputId,
      class = "Dragable",
      style = style
    )
  )
}

# To be called from server.R
renderDragableChart <- function(expr, env = parent.frame(),
                                quoted = FALSE, labels = LETTERS,
                                width = NULL, sameval = FALSE) {
  installExprFunction(
    expr = expr,
    name = "data",
    eval.env = env,
    quoted = quoted
  )

  installExprFunction(
    expr = sameval,
    name = "sameval",
    eval.env = env,
    quoted = quoted
  )

  function() {
    data <- lapply(
      X = seq(length(data())),
      FUN = function(idx){
        list(x = labels[idx], y = data()[idx])
      }
    )

    list(
      data = data,
      width = width,
      sameval = sameval()
    )
  }
}