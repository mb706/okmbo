

OkMbo = R6Class("OkMbo",
  public = list(
    initialize = function(objective) {
      assert_r6(objective, "OkObjective")
    },
    suggest = function(df, suggest_num = 1) {
      # if suggest_num > 1: how do we do multipoint?
      # do we do some kind of skalarizing before modeling?
      #
    }
  )
)
