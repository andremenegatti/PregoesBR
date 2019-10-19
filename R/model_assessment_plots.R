model_assessment_plots <- function(summary_df) {
  par(mfrow = c(2, 2))
  plot(summary_df$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
  plot(summary_df$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")
  points(which.max(summary_df$adjr2),
         summary_df$adjr2[which.max(summary_df$adjr2)],
         col = "red", cex = 2, pch = 20)
  plot(summary_df$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
  points(which.min(summary_df$cp ),
         summary_df$cp[which.min(summary_df$cp )],
         col = "red",cex = 2, pch = 20)
  plot(summary_df$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
  points(which.min(summary_df$bic),
         summary_df$bic[which.min(summary_df$bic)],
         col = "red", cex = 2, pch = 20)
}
