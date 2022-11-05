css2col <-
function(x, ref = NULL) {
    
    x <- na.omit(x)
    nas <- attr(x,"na.action")
    #x <- tolower(x)
    
    done <- logical(length(x))
    
    # named colours
    if(!all(done)) {
        ss <- match(tolower(x[!done]), names(.css.colors), nomatch = 0L)
        if(any(ss != 0L)) {
            b <- logical(length(done))
            b[!done] <- ss != 0L
            x[b] <- .css.colors[ss]
            done <- done | b
        }
    }

    # hex rgb
    if(!all(done)) {
        x[!done] <- gsub("^#([0-9a-f])([0-9a-f])([0-9a-f])([0-9a-f])?$",
            "#\\1\\1\\2\\2\\3\\3\\4\\4", x[!done], ignore.case = TRUE)
        done[!done] <- grepl("^#[0-9a-f]{6}([0-9a-f]{2})?$", x[!done],
                perl = TRUE, ignore.case = TRUE)
    }
    done[!done] <- x[!done] %in% c("inherit", "currentcolor")
 
    # hsl / rgb ()
    if(!all(done)) {
        mfn <- regexec("^ *(hsl|rgb)a? *\\(([^\\)]+)\\) *", x[!done], perl = TRUE, ignore.case = TRUE)
        ss <- !vapply(mfn, function(x) length(x) == 1L && x == -1L, logical(1L))
        if(any(ss)) {
            m <- regmatches(x[!done][ss], mfn[ss])
            fun <- tolower(vapply(m, "[", "", 2L))
            arg <- vapply(m, "[", "", 3L)
            # XXX: exponential number format?
            arg <- regmatches(arg, gregexpr("[-+]?[0-9]*(?:\\.?[0-9]+)[a-zA-Z%]{0,4}", arg, perl = TRUE))
            arg <- lapply(arg, css2num)
            col <- character(n <- length(fun))
            # rgb: The integer value 255 corresponds to 100%
            for(j in seq_len(n)) {
                v <- arg[[j]]
                col[j] <- switch(fun[j], hsl = {
                    v[1L] <- v[1L] %% 360
                    v[-1L] <- pmax(0, pmin(v[-1L], 1))
                    hsl2col(matrix(v))
                }, rgb = {
                    percent <- attr(v, "unit") == "%" | seq_along(v) == 4L
                    v[!percent] <- v[!percent] / 255

                    rgb2col(matrix(pmax(0, pmin(1, v))), maxColorValue = 1)
                }, {
                    warning(sprintf("'%s' colour is not supported", fun[j]))
                    NA_character_
                })
            }
            x[!done][ss] <- col
            done[!done][ss] <- TRUE  
        }
    }

    # url(#id)
    if(!all(done) && length(ref) != 0L) {
        murl <- regexec("^\\s*url\\(\\s*([\"']*)#(.+)\\1\\s*\\)\\s*$", x[!done],
            perl = TRUE, ignore.case = TRUE)
        ss <- !vapply(murl, function(x) length(x) == 1L && x == -1L, logical(1L))
        a <- sapply(regmatches(x[!done][ss], murl[ss]), "[", 3L)
        x[!done][ss] <- ref[match(a[ss], names(ref), nomatch = NA)]
        done[!done][ss]  <- TRUE
    }


    x[!done] <- NA
    nareplace(x, nas)
}
