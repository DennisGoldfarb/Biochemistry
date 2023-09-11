#' Plots the standard curve, samples, and plate map
#'
#' @param data data from formatVictor
#' @param fit standard curve model
#'
#' @return
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
plotBCA <- function(data, fit)
{
  fit_data <- data.frame(value = seq(0,max(data$standards$value),length.out=100))
  fit_data$concentration <- predict(fit, fit_data)

  # predict
  data$samples$concentration <- predict(fit, data$samples)

  # annotations
  Rsquared <- sprintf("%.4f", summary(fit)$adj.r.squared)

  # take average
  sample_data_final <- data$samples %>%
    dplyr::group_by(.data$condition) %>%
    dplyr::summarize(concentration = mean(concentration))

  p <- (ggplot2::ggplot(data$standards, aes(y=concentration, x=value))
        + geom_line(data = fit_data, color="grey10")
        + geom_point(color="grey10")
        + geom_point(data = data$samples, aes(color=condition))

        + annotate("text", x=0, y=max(data$standards$value), hjust = 0, vjust=0,
                   label = stringr::str_c("y * ' = ' * ", .regEq(fit,4)), parse=T, size = 3)
        + annotate("text", x=0, y=max(data$standards$value)*0.95, hjust = 0, vjust=0,
                   label = stringr::str_c("italic(R) ^ 2 * ' = ' * ", Rsquared), parse=T, size = 3)

        + ggtitle("BCA Standard Curve")
        + labs(color='Sample')
        + xlab("Flourescence")
        + ylab("Concentration (mg/mL)")

        + theme(panel.background = element_blank(),
                panel.grid.minor = element_blank(),
                panel.grid.major = element_line(colour="grey95"),
                axis.line.x.bottom = element_line(colour="grey10"),
                axis.line.y.left = element_line(colour="grey10"),
                aspect.ratio = 1)
        )

  print(p)
}

.regEq <- function(lmObj, dig) {
  paste0(
     c(round(lmObj$coef[1], dig), round(sign(lmObj$coef[-1])*lmObj$coef[-1], dig)),
     c("", rep("*", length(lmObj$coef)-1)),
     paste0(c("", names(lmObj$coef)[-1]), c(ifelse(sign(lmObj$coef)[-1]==1," + "," - "), "")),
     collapse=""
   )
}
