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
plotBCA <- function(data, fit, dilution=1, desired_mg = 1, desired_units = "uL")
{
  sampleCI <- getCI(data, fit, dilution=dilution, desired_mg=desired_mg, desired_units=desired_units)
  p1 <- plotPlateMap(data)
  p2 <- plotStandardCurve(data, fit, dilution, sampleCI)
  p3 <- plotConcentrationTable(sampleCI, fit, desired_units, desired_mg)

  num_conditions <- length(unique(data$samples$condition)) + 1
  multi_plot <- ggpubr::ggarrange(p3,p1,p2,
                                  nrow=3,
                                  heights = c(1, 1, 2*+ceiling(num_conditions / 2))
                                  )

  multi_plot <- ggpubr::annotate_figure(multi_plot,
                                        top = ggpubr::text_grob(str_c(data$path, "\n",
                                                                "Measured on: ", data$measurement_started_date),
                                                                color = "grey10", face = "bold", size = 10),
                                        bottom = ggpubr::text_grob(str_c("Plate Type: ", data$plate_type,
                                                                          ", Plate Format: ", data$plate_format,
                                                                          ", Wavelength: ", data$wavelength, "\n",
                                                                          "Protocol: ", data$protocol_name,
                                                                          ", Created by: ", data$protocol_created_by,
                                                                          ", Created on: ", data$protocol_created_date, "\n"),
                                                                   color = "grey10", face = "bold", size = 10))

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
                                  grid::grid.text(stringr::str_c(data$plate_map[i, j], "\n", format(data$measurements[i,j], digits=3)),
                                            x, y, gp = grid::gpar(fontsize = 6))
                                },
                                show_heatmap_legend = F
                                )

  p <- grid::grid.grabExpr(ComplexHeatmap::draw(ht))
  return(p)
}

plotStandardCurve <- function(data, fit, dilution=1, sampleCI)
{
  fit_data <- data.frame(concentration = seq(0,max(data$standards$concentration),length.out=100))
  fit_data <- cbind(fit_data, investr::predFit(fit, fit_data, interval="confidence", level=0.95))
  confidence_bounds_standard <- data.frame(x = c(fit_data$concentration, rev(fit_data$concentration)),
                                           y = c(fit_data$lwr, rev(fit_data$upr)))

  # annotations
  Rsquared <- sprintf("%.4f", summary(fit)$adj.r.squared)

  # determine if model is multi regression or nonlinear
  multi <- length(names(fit$coef)[-1]) > 1

  annotation_data <- data.frame(label = c(stringr::str_c("y * ' = ' * ", .regEq(fit,4)),
                                          stringr::str_c("italic(R) ^ 2 * ' = ' * ", Rsquared)),
                                condition = "Standard",
                                x = 0,
                                y = c(max(data$standards$value),
                                      max(data$standards$value)*0.95))
  sample_annotations <- data.frame(label = c(stringr::str_c("Estimate: ",
                                                            format(sampleCI$estimate, digits=3),
                                                            " * ", dilution, "x dilution = ",
                                                            format(sampleCI$`Corrected estimate`, digits=3), " mg/mL"),
                                             stringr::str_c("95% CI: [",
                                                            format(sampleCI$lower, digits=3), ", ",
                                                            format(sampleCI$upper, digits=3), "] ",
                                                            " * ", dilution, "x = [",
                                                            format(sampleCI$`Corrected lower`, digits=3),", ",
                                                            format(sampleCI$`Corrected upper`, digits=3),
                                                            "] mg/mL")),
                                   condition = sampleCI$condition,
                                   x = 0,
                                   y = c(max(data$standards$value),
                                         max(data$standards$value)*0.95))
  #annotation_data <- rbind(annotation_data, sample_annotations)



  p <- (ggplot2::ggplot(data$standards, ggplot2::aes(x=concentration, y=value))
        + ggplot2::geom_rect(data=sampleCI,
                             fill="#fbb4ae",
                             alpha=0.5,
                             ggplot2::aes(NULL, NULL, xmin=lower, ymin=-Inf, xmax=upper, ymax=Inf))
        + ggplot2::geom_vline(data = sampleCI, ggplot2::aes(xintercept = estimate), linetype=3)
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


plotConcentrationTable <- function(data, fit, unit = "uL", desired_mg = 1)
{
  names(data) <- tools::toTitleCase(names(data))
  data <- data %>%
    dplyr::relocate(`Estimate`, .after = `Upper`) %>%
    dplyr::relocate(`Corrected Estimate`, .after = `Corrected Upper`) %>%
    dplyr::mutate_if(is.numeric, round, 3)
  names(data)[ncol(data)] <- stringr::str_c("Volume (", unit, ") for ", desired_mg, " mg")
  p <- gridExtra::tableGrob(data)
  return(p)
}
