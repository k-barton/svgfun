
hsl2col <-
function(hsl)
rgb2col(hsl2rgb(hsl))

hsl2rgb <-
function (hsl, maxColorValue = 255) {
#input: h in degrees [0,360] and s,v in [0,1] - output: r,g,b in [0,maxColorValue]
    if(!is.matrix(hsl) || nrow(hsl) < 3L)
        stop("'hsl' must be a matrix with 3 or 4 rows")
    h <- hsl[1L, ]
    s <- hsl[2L, ]
    l <- hsl[3L, ]
    a <- s * pmin(l, 1 - l)
    f <- function(n , k = (n + h / 30) %% 12)
        (l - a * pmax(pmin(k - 3, 9 - k, 1), -1)) * maxColorValue
    rbind(r = f(0), g = f(8), b = f(4),
        alpha = if(nrow(hsl) == 4)
        pmin(1, pmax(0, hsl[4L, ])) * maxColorValue else
        NULL)
}

rgb2col <-
function (r, g = NULL, b = NULL, alpha = NULL, maxColorValue = 255) {
    rgb <- if (is.null(g) && is.null(b))
        as.matrix(r) else rbind(r, g, b, alpha)
    if (!is.numeric(rgb)) 
        stop("rgb matrix must be numeric")
    d <- dim(rgb)
    if (d[1L] != 3L && d[1L] != 4L) 
        stop("rgb matrix must have 3 or 4 rows")
    rgb <- (rgb / maxColorValue) * 255
    mode(rgb) <- "integer"
    rgb <- array(sprintf("%02x", rgb), dim = dim(rgb))
    paste0("#", apply(rgb, 2L, paste0, collapse = ""))
}

addopacity <-
function(col, alpha) {
    ok <- grepl("^#[0-9a-zA-Z]{2}{3,4}$", col)
    nc <- nchar(col)
    alpha0 <- rep.int(1, sum(ok))
    alpha <- rep_len(alpha[ok], sum(ok))
    if(any(hasalpha <- nc[ok] == 9L))
        alpha0[hasalpha] <-
            as.numeric(as.hexmode(substring(col[ok][hasalpha], 8L, 9L))) / 255
    col[ok] <- sprintf("%s%02x", substring(col[ok], 1L, 7L),
        as.integer(alpha0 * pmin(1, alpha) * 255))
    col
}
