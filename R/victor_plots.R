plot_los <- function(data, department) {

  shiny::validate(shiny::need(
    is.data.frame(data) &&
      "LoS" %in% names(data) &&
      any(is.finite(data$LoS)),
    paste("No length-of-stay data for", department)
  ))

  data <- data[is.finite(data$LoS), , drop = FALSE]

  ggplot2::ggplot(data, ggplot2::aes(x = LoS)) +
    ggplot2::geom_histogram(
      binwidth = 1,
      boundary = 0,
      closed = "left",
      fill = "lightskyblue",
      colour = "white"
    ) +
    ggplot2::geom_density(
      ggplot2::aes(y = ggplot2::after_stat(count)),
      colour = "steelblue",
      linewidth = 1,
      trim = TRUE
    ) +
    ggplot2::labs(
      title = paste(department, "length of stay"),
      x = "Length of stay (days)",
      y = "Number of patients"
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      text = ggplot2::element_text(colour = "black"),
      axis.text.x = ggplot2::element_text(
        colour = "black", size = 11
      ),
      axis.text.y = ggplot2::element_text(
        colour = "black", size = 11
      ),
      axis.title = ggplot2::element_text(colour = "black"),
      plot.background = ggplot2::element_rect(
        fill = "white", colour = NA
      )
    )
}


plot_occupancy <- function(history, forecast, department) {

  ggplot2::ggplot() +
    ggplot2::geom_ribbon(
      data = forecast,
      ggplot2::aes(x = date, ymin = lbCI, ymax = ubCI),
      fill = "steelblue",
      alpha = 0.2
    ) +
    ggplot2::geom_line(
      data = history,
      ggplot2::aes(
        x = date, y = occupancy, colour = "Historical"
      ),
      linewidth = 0.8
    ) +
    ggplot2::geom_point(
      data = history,
      ggplot2::aes(
        x = date, y = occupancy, colour = "Historical"
      )
    ) +
    ggplot2::geom_line(
      data = forecast,
      ggplot2::aes(
        x = date, y = avg, colour = "Predicted"
      ),
      linewidth = 0.8
    ) +
    ggplot2::geom_point(
      data = forecast,
      ggplot2::aes(
        x = date, y = avg, colour = "Predicted"
      )
    ) +
    ggplot2::scale_colour_manual(
      values = c(
        Historical = "gray27",
        Predicted = "orange"
      )
    ) +
    ggplot2::scale_x_date(
      date_breaks = "2 weeks",
      date_labels = "%d %b"
    ) +
    ggplot2::labs(
      title = paste(department, "bed occupancy"),
      x = "Date",
      y = "Number of patients",
      colour = NULL,
      caption = "Shaded area: prediction confidence interval"
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      text = ggplot2::element_text(colour = "black"),
      axis.text.x = ggplot2::element_text(
        colour = "black",
        size = 11,
        angle = 45,
        hjust = 1
      ),
      axis.text.y = ggplot2::element_text(
        colour = "black", size = 11
      ),
      axis.title = ggplot2::element_text(colour = "black"),
      legend.text = ggplot2::element_text(
        colour = "black", size = 11
      ),
      legend.position = "bottom",
      plot.background = ggplot2::element_rect(
        fill = "white", colour = NA
      )
    )
}
