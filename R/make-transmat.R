
#make.transmat(c("translate(-10px, -20px) scale(2) rotate(45deg)", "rotate(45)"))

#TODO: The function does recognize units and % but it should take percentages
#      of the viewport:
#      translate(x,y) scale(x,y) skew(x,y) skewX(x) skewY(y) rotate(angle) 


# takes "transform" attribute value as character string and returns
# a transformation matrix
make.transmat <- 
function(s) {
    matches <- gregexpr("([a-zA-Z]+)\\s*\\(\\s*([^\\)]+)\\s*\\)\\s*", s, perl = TRUE)
    captures <- regcaptures(s, matches)
    captures <- do.call("rbind", captures)
    
    fnNames <- captures[, 1L]
    #names(arguments) <- fnNames
    ok <- fnNames %in% c("matrix", "rotate", "translate", "scale", "skew", "skewX", "skewY")
    if(!all(ok)) return(NULL)
    
    #fnNames <- fnNames[ok]
    arguments <- css2num(captures[, 2L])
    arguments <- split(arguments, attr(arguments,"element"))
    arguments <- arguments[ok] 
 
    narg <- list(matrix = 6L, rotate = c(1L, 3L), translate = 1:2, scale = 1:2,
        skew = 1:2, skewX = 1L, skewY = 1L)

    # if there's wrong argument number in any of the functions, reject all:
    if(!all(mapply(`%in%`, sapply(arguments, length), narg[fnNames])))
        return(NULL)
    
    T <- diag(3L)
    for(i in seq_along(arguments)) {
        a <- arguments[[i]]
        T1 <- switch(fnNames[i],
            matrix = transmat(matrix. = a),
            skew = transmat(skew = a),
            skewX = transmat(skew = c(a[1L], 0)),
            skewY = transmat(skew = c(0, a[1L])),
            rotate = transmat(rotate = a),
            translate = transmat(translate = a),
            scale = transmat(scale = a), {
                warning(sprintf("transform function '%s' ignored", fnNames[i]))
                diag(3L)
            })
        T <- T %*% T1
    }
    T
}
