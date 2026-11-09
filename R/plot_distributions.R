plot_hospital_distributions <- function(
    h_risk = data.frame(
      AgeL = c(0, 30, 70),
      AgeU = c(30, 70, 111),
      HRisk = c(0.03, 0.08, 0.15)
    ),
    time_to_admit_mean = 7,
    time_to_admit_sd = 2.1,
    sources = c("Home", "Other hospital", "Nursing home"),
    source_prob = c(0.55, 0.45, 0.05),
    gender = c("M", "F"),
    gender_prob = c(0.49, 0.51)
) {
  percent <- function(x) sprintf("%g%%", 100 * x)

  plot_theme <- ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(colour = "#222222"),
      plot.title = ggplot2::element_text(face = "bold")
    )

  bar_plot <- function(labels, values, title, xlab, ylab,
                       fill, upper, digits = 0) {
    data <- data.frame(
      category = factor(labels, levels = labels),
      probability = values,
      label = sprintf("%.*f%%", digits, 100 * values)
    )

    ggplot2::ggplot(
      data, ggplot2::aes(x = category, y = probability)
    ) +
      ggplot2::geom_col(width = 0.65, fill = fill, colour = "black") +
      ggplot2::geom_text(ggplot2::aes(label = label), vjust = -0.4) +
      ggplot2::scale_y_continuous(
        labels = percent,
        limits = c(0, upper),
        expand = ggplot2::expansion(mult = c(0, 0))
      ) +
      ggplot2::labs(title = title, x = xlab, y = ylab) +
      plot_theme
  }

  # AgeU is exclusive: 30 means the first group ends at age 29.
  age_labels <- paste(h_risk$AgeL, h_risk$AgeU - 1, sep = "–")

  # Convert source weights to probabilities for display.
  source_share <- source_prob / sum(source_prob)

  days <- seq(0, time_to_admit_mean + 4 * time_to_admit_sd, length.out = 500)
  admission_time <- data.frame(
    days = days,
    density = stats::dnorm(days, time_to_admit_mean, time_to_admit_sd)
  )

  time_plot <- ggplot2::ggplot(
    admission_time, ggplot2::aes(x = days, y = density)
  ) +
    ggplot2::geom_area(fill = "darkorange", alpha = 0.2) +
    ggplot2::geom_line(colour = "darkorange", linewidth = 1) +
    ggplot2::geom_vline(
      xintercept = time_to_admit_mean,
      linetype = "dashed"
    ) +
    ggplot2::labs(
      title = "Time to hospital admission",
      subtitle = sprintf(
        "Dashed line: mean = %g days; SD = %g days",
        time_to_admit_mean, time_to_admit_sd
      ),
      x = "Time (days)",
      y = "Probability density"
    ) +
    plot_theme
  case_age_groups <- c(
    "00-19", "20-39", "40-59",
    "60-79", "80-99", "100-110"
  )
  case_age_probs <- c(0.15, 0.20, 0.20, 0.25, 0.19, 0.01)

  stopifnot(isTRUE(all.equal(sum(case_age_probs), 1)))
  list(age = bar_plot(
    case_age_groups, case_age_probs,
    title = "Age distribution of cases",
    xlab = "Age group (years)",
    ylab = "Probability",
    fill = "steelblue",
    upper = 0.30
  ),
    risk = bar_plot(
      age_labels, h_risk$HRisk, "Hospitalisation risk by age",
      "Age group (years)", "Hospitalisation risk",
      "steelblue", upper = 0.20
    ),
    time = time_plot,
    source = bar_plot(
      sources, source_share, "Patient source distribution",
      "Source", "Probability",
      "seagreen", upper = 0.65, digits = 1
    ),
    gender = bar_plot(
      gender, gender_prob, "Gender distribution",
      "Gender", "Probability",
      "mediumpurple", upper = 0.65
    )
  )
}
