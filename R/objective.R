
#' @title Map
#'
#' @description
#' Describes a function (domain) -> (codomain)
#'
#' domain, codomain: ParamSet
#' fun: function. takes 1 list argument and returns a list, with positions and names agreeing with
#' domain, codomain
#'
#' @export
OkMap = R6Class("OkMap",
  public = list(
    fun = NULL,
    domain = NULL,
    codomain = NULL,
    initialize = function(fun, domain, codomain) {
      assert_param_set(domain)
      assert_param_set(codomain)
      assert_function(fun, null.ok = TRUE)
      private$.fun = fun
      self$domain = domain
      self$codomain = codomain
      assert_list(codomain$values, len = 0)
    }
  ),
  active = list(
    fun = function(val) {
      if (!missing(val)) {
        stop("fun is read-only")
      }
      private$.fun
    },
    safe_fun = function(val) {
      if (!missing(val)) {
        stop("safe_fun is read-only")
      }
      function(x) {
        assert_list(x, names = "unique", len = self$domain$length)
        assert_names(names(x), identical.to = self$domain$ids())
        ret = self$fun(x)
        assert_list(ret, names = "unique", len = self$codomain$length)
        assert_names(names(ret), identical.to = self$codomain$ids())
        ret
      }
    }
  ),
  private = list(
    .fun = NULL,
  )
)

#' @title Parameterized Map
#'
#' @description
#' Like OkMap, only it has an additional param_set. Set values here to
#' change the behaviour of fun.
#'
#' fun must be given as a function of two arguments. The second is set from
#' the param_set. $fun will then only have one parameter.
OkParamMap = R6Class("OkParamMap",
  public = list(
    param_set = NULL,

    initialize = function(fun, domain, codomain, param_set) {
      assert_param_set(param_set)
      self$param_set = param_set
      super$initialize(fun, domain, codomain)
    }
  ),
  active = list(
    fun = function(val) {
      if (!missing(val)) {
        stop("fun is read-only")
      }
      function(x) {
        private$.fun(x, self$param_set$values)
      }
    }
  )
)

forbid = function(what, msg) {
  if (any(what)) {
    stopf(msg, which(what))
  }
}

OkObjective = R6Class("OkObjective",
  inherits = OkMap,
  public = list(
    initialize = function(fun, domain, codomain) {
      assert_param_set(codomain)
      minimize = map_lgl(codomain$tags, function(x) "minimize" %in% x)
      maximize = map_lgl(codomain$tags, function(x) "maximize" %in% x)
      objective = map_lgl(codomain$tags, function(x) "objective" %in% x)
      forbid(minimize & maximize, "Trying minimize and maximize both given for cod index %s")
      forbid((minimize | maximize) & ~objective, "Trying minimize and maximize something that is not an objective for cod index %s")
      forbid(objective & !minimize & !maximize, "Cod index %s are objective with no direction")
      if (!any(objective)) {
        stop("At least one objective must be minimized or maximized")
      }
      super$initialize(fun, domain, codomain)
    }
  )
)

