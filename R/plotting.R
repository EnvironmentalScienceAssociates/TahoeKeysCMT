#' Create custom bar plot
#'
#' @export
#'
#'

custom_bar_plot <- function(data, x, y, fill, xlab, ylab, title, ylim = NULL, ybreaks = waiver(),
                            fill_name, fill_values, fill_labels = NULL,
                            show_legend = TRUE, legend_position = NULL,
                            facets = FALSE, facet_var = NULL, facet_scales = NULL,
                            errorbar = FALSE, ymin = NULL, ymax = NULL,
                            nutrients = FALSE, rw = NULL, rw_labels = NULL, text_group = NULL){
  # most arguments are passed as strings

  p = ggplot2::ggplot(data, ggplot2::aes(x = .data[[x]])) +
    ggplot2::geom_bar(ggplot2::aes(fill = .data[[fill]], y = .data[[y]]),
                      stat = "identity", col = "black", linewidth = 0.1,
                      position = ggplot2::position_dodge2(preserve = "single", width = 0.9),
                      show.legend = show_legend)

  if (errorbar){
    p = p +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = .data[[ymin]], ymax = .data[[ymax]], group = .data[[fill]]), linewidth = 0.3,
                             position = ggplot2::position_dodge2(preserve = "single", width = 0.9, padding = 0.5))
  }

  if (facets){
    p = p +
      ggplot2::facet_wrap(~.data[[facet_var]], strip.position = "bottom", ncol = 1, scales = facet_scales)
  }

  if (nutrients){
    p = p +
      ggplot2::geom_text(ggplot2::aes(y = det_text_y, label = det_text, group = .data[[text_group]]), vjust = 0, size = 2,
                         position = ggplot2::position_dodge2(preserve = "single", width = 0.9))

    if (!is.null(rw) & !is.na(rw)){
      p = p +
        ggplot2::geom_hline(aes(yintercept = rw, linetype = rw_labels), color = "969696") + #receiving water limit for analyte
        ggplot2::scale_linetype_manual(values = "solid") +
        # orders compliance in same place
        ggplot2::guides(fill = ggplot2::guide_legend(order = 1),
                        linetype = ggplot2::guide_legend(order = 2))
    }
  }

  p  +
    ggplot2::labs(x = xlab, y = ylab, title = title) +
    ggplot2::scale_fill_manual(name = fill_name, labels = fill_labels, values = fill_values) +
    ggplot2::coord_cartesian(ylim = ylim) +
    ggplot2::scale_y_continuous(breaks = ybreaks, expand = c(0, 0)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", color = NA),
                   axis.line = ggplot2::element_line(colour = "grey92"), # matches panel.grid color inherited from theme_minimal
                   axis.ticks = ggplot2::element_line(color = "grey92"),
                   panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.minor.x = ggplot2::element_blank(),
                   panel.grid.minor.y = ggplot2::element_blank(),
                   legend.position = legend_position)
}
