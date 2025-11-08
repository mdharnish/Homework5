## HW5 Class/Methods

setClass(
  Class = "sparse_numeric",
  slots = c(
    value = "numeric",
    pos = "integer",
    length = "integer"
  )
)

setValidity("sparse_numeric", function(object) {
  errors <- character()
  if (!(length(object@length) == 1L && !is.na(object@length) && object@length > 0L))
    errors <- c(errors, "slot `length` must be one positive integer.")
  if (length(object@value) != length(object@pos))
    errors <- c(errors, "slots `value` and `pos` must be equal in size.")
  if (anyNA(object@pos) || anyNA(object@value))
    errors <- c(errors, "`pos` and `value` must not contain NA.")
  if (length(object@pos) && (min(object@pos) < 1L || max(object@pos) > object@length))
    errors <- c(errors, "slot `pos` contains an index outside 1..length.")
  if (length(object@pos) > 1L && !all(diff(object@pos) > 0L))
    errors <- c(errors, "`pos` must be strictly increasing (no repeats).")
  if (length(object@value) && any(object@value == 0))
    errors <- c(errors, "stored `value` must exclude zeros.")
  if (length(errors)) errors else TRUE
})

setAs("numeric", "sparse_numeric", function(from) {
  idx <- which(from != 0)
  new("sparse_numeric",
      value  = unname(as.numeric(from[idx])),
      pos    = as.integer(idx),
      length = as.integer(length(from)))
})

setAs("sparse_numeric", "numeric", function(from) {
  res <- numeric(from@length)
  if (length(from@pos)) res[from@pos] <- from@value
  res
})

sn_vec <- function(x) as(x, "sparse_numeric")

setGeneric("sparse_add", function(x, y, ...) standardGeneric("sparse_add"))
setGeneric("sparse_sub", function(x, y, ...) standardGeneric("sparse_sub"))
setGeneric("sparse_mult", function(x, y, ...) standardGeneric("sparse_mult"))
setGeneric("sparse_crossprod", function(x, y, ...) standardGeneric("sparse_crossprod"))

setMethod("sparse_add", signature("sparse_numeric","sparse_numeric"),
          function(x, y, ...) {
            if (x@length != y@length) stop("vectors must share the same `length` slot")
            if (!length(x@pos) && !length(y@pos))
              return(new("sparse_numeric", value = numeric(), pos = integer(), length = x@length))
            agg_pos <- c(x@pos, y@pos)
            agg_val <- c(x@value, y@value)
            s <- tapply(agg_val, agg_pos, sum)
            keep <- s != 0
            new("sparse_numeric",
                value = as.numeric(s[keep]),
                pos   = as.integer(names(s)[keep]),
                length = x@length)
          })

setMethod("sparse_sub", signature("sparse_numeric","sparse_numeric"),
          function(x, y, ...) {
            if (x@length != y@length) stop("vectors must share the same `length` slot")
            if (!length(x@pos) && !length(y@pos))
              return(new("sparse_numeric", value = numeric(), pos = integer(), length = x@length))
            agg_pos <- c(x@pos, y@pos)
            agg_val <- c(x@value, -y@value)
            s <- tapply(agg_val, agg_pos, sum)
            keep <- s != 0
            new("sparse_numeric",
                value = as.numeric(s[keep]),
                pos   = as.integer(names(s)[keep]),
                length = x@length)
          })

setMethod("sparse_mult", signature("sparse_numeric","sparse_numeric"),
          function(x, y, ...) {
            if (x@length != y@length) stop("vectors must share the same `length` slot")
            if (!length(x@pos) || !length(y@pos))
              return(new("sparse_numeric", value = numeric(), pos = integer(), length = x@length))
            common <- x@pos[x@pos %in% y@pos]
            if (!length(common))
              return(new("sparse_numeric", value = numeric(), pos = integer(), length = x@length))
            idx_x <- match(common, x@pos)
            idx_y <- match(common, y@pos)
            v <- x@value[idx_x] * y@value[idx_y]
            new("sparse_numeric",
                value = v,
                pos   = as.integer(common),
                length = x@length)
          })

setMethod("sparse_crossprod", signature("sparse_numeric","sparse_numeric"),
          function(x, y, ...) {
            if (x@length != y@length) stop("vectors must share the same `length` slot")
            if (!length(x@pos) || !length(y@pos)) return(0)
            common <- x@pos[x@pos %in% y@pos]
            if (!length(common)) return(0)
            idx_x <- match(common, x@pos)
            idx_y <- match(common, y@pos)
            sum(x@value[idx_x] * y@value[idx_y])
          })

setMethod("+", signature(e1="sparse_numeric", e2="sparse_numeric"),
          function(e1, e2) sparse_add(e1, e2))

setMethod("-", signature(e1="sparse_numeric", e2="sparse_numeric"),
          function(e1, e2) sparse_sub(e1, e2))

setMethod("*", signature(e1="sparse_numeric", e2="sparse_numeric"),
          function(e1, e2) sparse_mult(e1, e2))

setMethod("show", "sparse_numeric", function(object) {
  nnz <- length(object@pos)
  cat("{",
      "\"class\":\"sparse_numeric\", ",
      "\"len\":", object@length, ", ",
      "\"nnz\":", nnz, ", ",
      "\"pos\":[", paste(object@pos, collapse=","), "], ",
      "\"val\":[", paste(format(object@value), collapse=","), "]",
      "}\n", sep = "")
})

setMethod("plot", signature(x="sparse_numeric", y="sparse_numeric"),
          function(x, y, ...) {
            if (x@length != y@length)
              stop(sprintf("length mismatch detected — x has %d slots, y has %d", x@length, y@length))
            match_pts <- intersect(x@pos, y@pos)
            plot(NA, xlim = c(1, x@length), ylim = c(0, 1),
                 xlab = "Index", ylab = "",
                 main = if (length(match_pts))
                   sprintf("Shared non-zero indices (%d total)", length(match_pts))
                 else
                   "No shared non-zero entries found",
                 ...)
            if (length(match_pts))
              points(match_pts, rep(1, length(match_pts)), pch = 19)
          })

setMethod("abs", signature(x="sparse_numeric"),
          function(x) {
            new("sparse_numeric",
                value  = abs(x@value),
                pos    = x@pos,
                length = x@length)
          })