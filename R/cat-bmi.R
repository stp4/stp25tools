#'  BMI: WHO   (kg/m2)
#'
#'      Very severely underweight 15
#'      Severely underweight 15-16
#'      Underweight 16-18.5
#'      Normal (healthy weight) 18.5-25
#'      Overweight 25-30
#'      Obese Class I (Moderately obese) 30-35
#'      Obese Class II (Severely obese) 35-40
#'      Obese Class III (Very severely obese) 40
#' 
#'
#' @param x vector
#' @param breaks,labels an cut 
#' @param n anzahl der BMI-Kategorien default = 4 Underweight,        Normal,    Overweight, Obese Class I 
#' 
#' @export
#' @examples 
#' 
#' table(cat_bmi(c(10, 15, 16, 18.5, 25, 30, 35, 40, 45)))
#' table(cat_bmi(c(10, 15, 16, 18.5, 25, 30, 35, 40, 45), n=4))
#' table(cat_bmi(c(10, 15, 16, 18.5, 25, 30, 35, 40, 45), n=5))
#' table(cat_bmi(c(10, 15, 16, 18.5, 25, 30, 35, 40, 45), n=6))
#' 
cat_bmi <- function(x,
                    breaks = c(-Inf, 15, 16, 18.5, 25, 30, 35, 40, Inf),
                    labels = c(
                      "Very severely underweight",
                      "Severely underweight",
                      "Underweight",
                      "Normal",
                      "Overweight",
                      "Obese Class I",
                      "Obese Class II",
                      "Obese Class III"
                    ),
                    n = 4) {
  if (n == 3)
    cut(x, breaks[c(1, 4:5,  9)], labels[c(3:5)])
  else if (n == 4)
    cut(x,  breaks[c(1, 4:6,  9)],  labels[c(3:6)])
  else if (n == 5)
    cut(x, breaks[c(1, 3:6,  9)], labels[c(2:6)])
  else if (n == 6)
    cut(x, breaks[c(1, 3:7,  9)], labels[c(2:7)])
  else if (n == 7)
    cut(x, breaks[c(1, 2:7,  9)], labels[c(1:7)])
  else
    cut(x, breaks, labels)
  
  
}

