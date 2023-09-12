#' Plots the standard curve, samples, and plate map
#'
#' @param data data from formatVictor
#' @param fit standard curve model
#' @param dilution dilution factor for samples
#'
#' @return
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
plotBCA <- function(data, fit, dilution=1)
{
  p1 <- plotPlateMap(data)
  p2 <- plotStandardCurve(data, fit, dilution)

  num_conditions <- length(unique(data$samples$condition)) + 1
  multi_plot <- ggpubr::ggarrange(p1,p2,
                                  nrow=2,
                                  heights = c(1, 2*+ceiling(num_conditions / 2))
                                  )

  print(multi_plot)

}

.regEq <- function(lmObj, dig) {
  coef = c("x")
  if (length(names(lmObj$coef)[-1]) == 2)
  {
    coef = c("x", "x^2")
  }
  paste0(
     c(round(lmObj$coef[1], dig), round(sign(lmObj$coef[-1])*lmObj$coef[-1], dig)),
     c("", rep("*", length(lmObj$coef)-1)),
     paste0(c("", coef), c(ifelse(sign(lmObj$coef)[-1]==1," + "," - "), "")),
     collapse=""
   )
}

plotPlateMap <- function(data) {
  ht <- ComplexHeatmap::Heatmap(data$measurements,
                                cluster_rows = F,
                                cluster_columns = F,
                                column_names_rot = 0,
                                row_names_side = "left",
                                column_names_side = "top",
                                col = colorRampPalette(RColorBrewer::brewer.pal(n = 7, name = "Purples"))(100),
                                cell_fun = function(j, i, x, y, width, height, fill) {
                                  grid.text(stringr::str_c(data$plate_map[i, j], "\n", format(data$measurements[i,j], digits=3)),
                                            x, y, gp = gpar(fontsize = 6))
                                },
                                show_heatmap_legend = F
                                )

  p <- grid.grabExpr(draw(ht), height=2, width=3.5)
  return(p)

  #gb_heatmap = grid.grabExpr(draw(ht_list), height=3.5, width=2.5)
}

plotStandardCurve <- function(data, fit, dilution=1)
{
  fit_data <- data.frame(concentration = seq(0,max(data$standards$concentration),length.out=100))
  fit_data <- cbind(fit_data, investr::predFit(fit, fit_data, interval="confidence", level=0.95))
  confidence_bounds_standard <- data.frame(x = c(fit_data$concentration, rev(fit_data$concentration)),
                                           y = c(fit_data$lwr, rev(fit_data$upr)))

  # annotations
  Rsquared <- sprintf("%.4f", summary(fit)$adj.r.squared)

  # determine if model is multi regression or nonlinear
  multi <- length(names(fit$coef)[-1]) > 1

  # take average
  sample_data_final <- data$samples %>%
    dplyr::group_by(.data$condition)

  if (!multi) {
    # inverse predict linear
    sample_data_final <- sample_data_final %>%
      dplyr::summarize(descr = as.data.frame(investr::calibrate(fit, y0 = value, interval = "inversion", level = 0.95)[1:3])) %>%
      tidyr::unpack(cols = descr)
  } else {
    # inverse predict nonlinear
    sample_data_final <- sample_data_final %>%
      dplyr::summarize(descr = as.data.frame(investr::invest(fit, y0 = value, interval = "inversion", level = 0.95)[1:3])) %>%
      tidyr::unpack(cols = descr)
  }

  annotation_data <- data.frame(label = c(stringr::str_c("y * ' = ' * ", .regEq(fit,4)),
                                          stringr::str_c("italic(R) ^ 2 * ' = ' * ", Rsquared)),
                                condition = "Standard",
                                x = 0,
                                y = c(max(data$standards$value),
                                      max(data$standards$value)*0.95))
  sample_annotations <- data.frame(label = c(stringr::str_c("Estimate: ",
                                                            format(sample_data_final$estimate, digits=3),
                                                            " * ", dilution, "x dilution = ",
                                                            format(sample_data_final$estimate*dilution, digits=3), " mg/mL"),
                                             stringr::str_c("95% CI: [",
                                                            format(sample_data_final$lower, digits=3), ", ",
                                                            format(sample_data_final$upper, digits=3), "] ",
                                                            " * ", dilution, "x = [",
                                                            format(sample_data_final$lower*dilution, digits=3),", ",
                                                            format(sample_data_final$upper*dilution, digits=3),
                                                            "] mg/mL")),
                                   condition = sample_data_final$condition,
                                   x = 0,
                                   y = c(max(data$standards$value),
                                         max(data$standards$value)*0.95))
  #annotation_data <- rbind(annotation_data, sample_annotations)



  p <- (ggplot2::ggplot(data$standards, ggplot2::aes(x=concentration, y=value))
        + ggplot2::geom_rect(data=sample_data_final,
                             fill="#fbb4ae",
                             alpha=0.5,
                             ggplot2::aes(NULL, NULL, xmin=lower, ymin=-Inf, xmax=upper, ymax=Inf))
        + ggplot2::geom_vline(data = sample_data_final, ggplot2::aes(xintercept = estimate), linetype=3)
        + ggplot2::geom_hline(data = data$samples, ggplot2::aes(yintercept = value), linetype=3)
        + ggplot2::geom_polygon(data = confidence_bounds_standard, ggplot2::aes(x=x, y=y), fill="#b3cde3", alpha=0.5)
        + ggplot2::geom_line(data = fit_data, ggplot2::aes(y=fit), color="grey10")

        + ggplot2::geom_point(data = data$standards %>% dplyr::select(-condition), shape = 21, color="grey10")

        + ggplot2::geom_text(data = annotation_data, ggplot2::aes(x=x, y=y, label=label), parse=T, hjust=0, vjust=0, size=3)
        + ggplot2::geom_text(data = sample_annotations, ggplot2::aes(x=x, y=y, label=label), parse=F, hjust=0, vjust=0, size=3)

        + ggplot2::facet_wrap(~ factor(condition, levels=c("Standard", levels(factor(data$samples$condition)))),
                              scales="free_x")

        + ggplot2::ggtitle("BCA Standard Curve")
        + ggplot2::labs(color='Sample')
        + ggplot2::ylab(stringr::str_c("Absorbance (", data$wavelength, ")"))
        + ggplot2::xlab("Concentration (mg/mL)")

        + ggplot2::theme(panel.background = ggplot2::element_blank(),
                         panel.grid.minor = ggplot2::element_blank(),
                         panel.grid.major = ggplot2::element_line(colour="grey95"),
                         axis.line.x.bottom = ggplot2::element_line(colour="grey10"),
                         axis.line.y.left = ggplot2::element_line(colour="grey10"),
                         aspect.ratio = 1)
  )

  return(p)
}
