#' Derive the local minimum of a third degree polynomial
#'
#' Takes four polynomial parameters as input and derives the local minimum
#' if it exists.
#'
#' @param coef polynomial coefficients
#' @param ...
#'
#' @return position of the local minimum, NA if it does not exist
#' @export
#'
#' @examples
#'
#' trough(c(-1, -4, 3, 1))


trough <- function (coef, ...) {
  UseMethod("trough", coef)
}

#' @rdname trough
#' @export

trough.numeric <-
  function(coef = c(-.2, -.5, .2, .7)) {
    if(length(coef) != 4) stop("the uncanny valley trough polynomial requires exactly four parameters")
    poly <- polynom::polynomial(coef)
    dpoly <- deriv(poly)
    ddpoly <- deriv(dpoly)
    points <- solve(dpoly)
    pt_dir <- as.function(ddpoly)(points)
    if(!(any(is.complex(pt_dir)))){
      points[pt_dir > 0]
    }else{
      NA
    }
  }

#' @rdname trough
#' @export

trough.matrix <-
  function(coef, ...) plyr::aaply(as.matrix(coef), .margins = 1, trough)

#' @rdname trough
#' @export

trough.data.frame <- function(coef) trough(as.matrix(coef))


# as.function(polynomial(c(-1, -2, -3, -4)))
# c <- c(-1,-2,3,4)
# m <- matrix(c(c, -.1,-.2,.3,.4), nrow = 2, byrow = T)
#
# class(c)
# class(m)
#
# trough(c)
# trough(m)


fn_uncanny <-
  function (coef, ...) {
    UseMethod("fn_uncanny", coef)
  }

fn_uncanny.numeric <-
  function(coef) {
    if(length(coef) != 5) stop("not the correct number of parameters,
                               four coefficients and x required")
    coef[1] +
      coef[2] * coef[5] +
      coef[3] * coef[5]^2 +
      coef[4] * coef[,5]^3
  }


fn_uncanny.matrix <-
  function(coef) {
    if(ncol(coef) != 5) stop("not the correct number of columns,
                             four coefficients and x required")
    coef[,1] +
      coef[,2] * coef[,5] +
      coef[,3] * coef[,5]^2 +
      coef[,4] * coef[,5]^3
  }


fn_uncanny.data.frame <-
  function(coef) {
    fn_uncanny(as.matrix(coef))
  }

dp_str <-
  function(coef){
    print(str(coef))
  }

# ##
#
# fn_maxlike <-
#   function(coef) {
#     coef_1 = cbind(coef, 0)
#     fn_uncanny(as.matrix(coef_1))
#   }
#
## use this to beautify rstanarm parameter names from polynomial regression


recode_poly_par <-
  function(P){
    P_out <-
      P_1 %>%
      mutate(parameter = recode(parameter,
                                "poly(huMech, 3)3"  = "huMech3",
                                "poly(huMech, 3)2"  = "huMech2",
                                "poly(huMech, 3)1"  = "huMech1",
                                "Intercept" = "huMech0"),
             fixef = recode(fixef,
                            "poly(huMech, 3)3"  = "huMech3",
                            "poly(huMech, 3)2"  = "huMech2",
                            "poly(huMech, 3)1"  = "huMech1",
                            "Intercept" = "huMech0"))
    class(P_out) <- class(P)
    P_out
  }

str_recode_poly <-
  function(P) {
    P <- str_replace(P, "poly\\(huMech, 3\\)", "huMech")
    P <- str_replace(P, "Intercept", "huMech0")
    P
  }

recode_poly_par <-
  function(P){
    P_out <-
      P_1 %>%
      mutate(parameter = str_recode_poly(parameter),
             fixef = str_recode_poly(fixef))
    class(P_out) <- class(P)
    P_out
  }


par.poly <- function(P){
  P_out <-  P %>%
    tidyr::extract(fixef,
                   into = c("par_poly"),
                   regex = "(huMech.)",
                   remove = F) %>%
    class(P_out) <- class(P)
    P_out
}

# trough.tbl_post <-
#   function(P){
#     P <- as_data_frame(P_1)
#     P_mat <-
#       P %>%
#       filter(str_detect(par_poly, "huMech")) %>%
#       select(iter, Condition, par_poly, value) %>%
#       spread(key = par_poly, value = value)
#   }
#   P_1 %>%  ## copying huMech0 to get a complete column set
#   filter()
#   bind_rows()
