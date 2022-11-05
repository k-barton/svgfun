range.vgshape <-
function (..., na.rm = FALSE, finite = FALSE)  {
    #if(!missing(na.rm)) warning("'na.rm' argument is ignored")
    #if(!missing(finite)) warning("'finite' argument is ignored")
    .xylimits(...elt(1L))
}

range.vgcontainer <-
function (..., na.rm = FALSE, finite = FALSE)  {
    x <- rapply(...elt(1L), classes =  "vgshape", .xylimits, how = "unlist")
    x <- sapply(split(x, rep(1L:2L, each = 2L)), range)
    dimnames(x) <- list(c("min", "max"), c("x", "y"))
    x
}

.xylimits <-
function(x) apply(x, 2L, range, na.rm = TRUE)


# parses "url(#ID)" -> "ID"
cssurl <-
function(x) {
    m <- regexec("^\\s*url\\(\\s*([\"']*)#(.+)\\1\\s*\\)\\s*$", x,
        perl = TRUE, ignore.case = TRUE)
    ss <- !vapply(m, function(x) length(x) == 1L && x == -1L, logical(1L))
    a <- sapply(regmatches(x[ss], m[ss]), "[", 3L)
    attr(a, "pos") <- which(ss)
    a
}

segmentlength <-
function(path) {
    k <- cumsum(attr(path, "seglen"))
    br <- cumsum(is.na(path[, 1L]))
    k <- k + br[k]
    apply(embed(k, 2L), 1L, function(i) {
        xy <- path[seq.int(i[2], i[1]), , drop = FALSE]
        if(anyNA(xy)) return(NA)
        xy <- matrix(apply(xy, 2L, diff), ncol = 2L)
        sum(sqrt(rowSums(xy^2)))
    })
}

#===============================================================================

# Calculate transform matrix for "use" objects. Combines translation with x,y
# attributes with "transform". Returns diag(3) if no transformations defined.
gettransmat <-
function(props) {
    Tmat <- diag(3L)
    if(has(props, "geometry"))
        Tmat <- transmat(translate = xmatch(props$geometry, c("x", "y"), 0))
    if(has(props, "transform"))
        Tmat <- Tmat %*% props$transform
    Tmat
}


.messageCtor <-
function(verbose) {
    if(isTRUE(verbose)) {
        function(x, ...) message(sprintf(x, ...))
    } else function(...) {}
}


getDef <-
function(path, name) {
    defenv <- attr(path, "defs")
    if(!is.environment(defenv) ||
       !exists(name, envir = defenv, inherits = FALSE))
         return(NULL)
    get(name, attr(path, "defs"), inherits = FALSE) 
}

wsstrip <-
function(x) gsub("(^\\s+|\\s+$)", "", x, perl = TRUE)


xmatch <-
function(x, nm, dflt = vector(mode(x), length(x)),
    fun = identity, pos = FALSE, ...) {
    k <- match(names(x), nm, nomatch = 0L)
    if(length(dflt) != length(nm))
        dflt <- rep_len(dflt, length(nm))
    if(any(k != 0L))
        dflt[k] <- fun(x[k != 0L], ...)
    names(dflt) <- nm
    if(isTRUE(pos)) attr(dflt, "pos") <- k
    dflt
}


treeapply <-
function(x, fun, ...) {
    .local <- function(x, fun, ...) {
       if(is.list(x)) {
            for(i in seq_along(x))
              x[[i]] <- .local(x[[i]], fun, ...)
       }
       fun(x, ...)
    }
    .local(x, fun, ...)
}

angular.mean <-
function(a, ...) {
    a <- c(a, ...)
    a <- a[!is.na(a)]
    atan2(sum(sin(a)), sum(cos(a)))
}

# modulo division compatible with JS operator '%'
# mod2(-3, 2) == -1
# -3 %% 2
mod2 <-
function(x, y) (abs(x) %% abs(y)) * sign(x)

toRadians <-
function(a) a / 180 * pi

# returns 1 rather than 0 for positive 'x'
sign2 <-
function(x) {
	s <- sign(x)
	s[s == 0L] <- 1L
	s
}

angleBetween <-
function(u, v) {
	s <- sign2((u[1L] * v[2L]) - (u[2L] * v[1L]))
	p <- sum(u * v)
	n <- sqrt(sum(u^2)) * sqrt(sum(v^2))
	s * acos(round(p / n, 13))
}

angleBetween2 <-
function(u, v) {
    if(!is.matrix(u)) u <- matrix(u, , 2L)
    if(!is.matrix(v)) v <- matrix(v, , 2L)
    if(any(dim(u) != dim(v)))
        stop("length of 'u' and 'v' is not equal.")
    s <- sign2((u[, 1L] * v[, 2L]) - (u[, 2L] * v[, 1L]))
    p <- rowSums(u * v)
    n <- sqrt(rowSums(u^2)) * sqrt(rowSums(v^2))
    s * acos(round(p / n, 13L))
}

splitattrs <-
function(attrs, proptypes = .proptypes, drop = TRUE)  {
    i <- match(names(proptypes), names(attrs), nomatch = 0L)
    split(attrs[i], proptypes[i != 0], drop = drop)
}

# elements of y override those of x
#combine2(c(a = 1, b = 2), c(a = 3, c = 2))
# --> c(b = 2, a = 3, c = 2)
combine2 <-
function(x, y) {
    c(x[!names(x) %in% names(y)], y)
}


# Same as !is.null(x[[a]]):
has <- function(x, a) any(a %in% names(x))
#has <- function(x, a) any(a %in% names(x)) && !is.null(x[[a]])
#has <- function(x, a) !is.null(x[[a]])
#has <- function(x, a) any(deparse(substitute(a)) %in% names(x))
#has <- function(x, a) {
#    tryCatch(!is.null(x[[a]]), error = function(e) {
#    print(x)
#    print(a)
#     stop(e)
#
#    })
#}

    


nareplace <-
function(x, napos = attr(x,"na.action"), na = NA)
    insert(x, napos, na)

insert <-
function(x, pos, values) {
    if(is.null(pos) || length(pos) == 0L) return(x)
    rval <- vector(mode(x), length(pos) + length(x))
    rval[pos] <- values <- rep_len(values, length(pos))
    rval[-pos] <- x
    nmv <- !is.null(names(values))
    nmx <- !is.null(names(x))
    if(nmx || nmv)
        names(rval) <- character(length(rval))
    if(nmx) names(rval)[-pos] <- names(x)
    if(nmv) names(rval)[pos] <- names(values)
    rval
}
