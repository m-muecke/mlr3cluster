#' @title Hierarchical DBSCAN (HDBSCAN)
#'
#' @name mlr_learners_clust.hdbscan
#' @include LearnerClust.R
#' @include aaa.R
#'
#' A [LearnerClust] for hierarchical DBSCAN implemented in [dbscan::hdbscan()].
#' @description
#' The predict method uses [dbscan::predict.hdbscan()] to compute the
#' cluster memberships for new data.
#' @templateVar id clust.hdbscan
#'
#' @template learner
#' @template example
#'
#' @export
LearnerClustHDBSCAN = R6Class("LearnerClustHDBSCAN",
  inherit = LearnerClust,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ps(
        minPts = p_int(lower = 0L, tags = c("required", "train")),
        gen_hdbscan_tree = p_lgl(default = FALSE, tags = "train"),
        gen_simplified_tree = p_lgl(default = FALSE, tags = "train")
      )

      super$initialize(
        id = "clust.dbscan",
        param_set = ps,
        feature_types = c("logical", "integer", "numeric"),
        predict_types = "partition",
        properties = c("partitional", "exclusive", "complete"),
        packages = "dbscan",
        man = "mlr3cluster::mlr_learners_clust.hdbscan",
        label = "Density-Based Clustering"
      )
    }
  ),

  private = list(
    .train = function(task) {
      pv = self$param_set$get_values(tags = "train")
      m = invoke(dbscan::hdbscan, x = task$data(), .args = pv)
      m = set_class(
        list(cluster = m$cluster, minPts = m$minPts, data = task$data(), dist = m$dist),
        c("hdbscan", "dbscan")
      )
      if (self$save_assignments) {
        self$assignments = m$cluster
      }

      return(m)
    },

    .predict = function(task) {
      partition = predict(self$model, newdata = task$data(), self$model$data)
      PredictionClust$new(task = task, partition = partition)
    }
  )
)

learners[["clust.hdbscan"]] = LearnerClustHDBSCAN
