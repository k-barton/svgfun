vgtransform <-
function(x, rotate = 0, translate = c(0, 0), scale = c(1, 1),
    skew = c(0, 0), origin = c(0, 0), matrix. = NULL)
UseMethod("vgtransform")


vgtransform.vgcontainer <-
function(x, rotate = 0, translate = c(0, 0), scale = c(1, 1),
         skew = c(0, 0), origin = c(0, 0), matrix. = NULL) {

    if(identical(origin, "center"))
        origin <- colMeans(range(x))

    rapply(x, classes = "vgshape", how = "replace",
       vgtransform, rotate = rotate, translate = translate, scale = scale,
       skew = skew, origin = origin, matrix. = matrix.)
}


vgtransform.vgshape <-
function(x, rotate = 0, translate = c(0, 0), scale = c(1, 1),
         skew = c(0, 0), origin = c(0, 0), matrix. = NULL) {
    
    if(!is.matrix(x) || ncol(x) != 2L)
        stop("'x' must be a two-column matrix")
        
    if(identical(origin, "center"))
        origin <- colMeans(.xylimits(x))
        
    tm <- transmat(rotate, translate, scale, skew, origin, matrix.)
    
    # XXX: check timing x[]<- vs. x<-
    #       keep attributes
    x[] <- t((tm %*% rbind(t(x), 1))[-3L, ])
    
    tma <- attr(x, "transform.applied")
    attr(x, "transform.applied") <-
        if(is.matrix(tma) && all(dim(tma) == 3L))
            tm %*% tma else tm
    x
}



transmat <-
function(rotate = 0, translate = c(0, 0), scale = c(1, 1),
         skew = c(0, 0), origin = c(0, 0), matrix. = NULL) {
    
    mode(skew) <- mode(rotate) <- mode(scale) <-
        mode(translate) <- mode(origin) <- "numeric"

    T0 <- if(!is.null(matrix.)) {
        if(!is.numeric(matrix.)) stop("'matrix.' must be numeric")
        if(length(matrix.) == 6L)
            rbind(matrix(matrix., 2L, 3L), c(0,0,1))
        else if(length(matrix.) == 9L) {
            matrix(matrix., 3L, 3L)
        } else stop("'matrix.' must be of length 6 or 9")
    } else diag(3L)
    
    if(length(rotate) == 3L) {
        if(any(origin[1L:2L] != 0))
            warning("'rotate[2:3]' is used instead of 'origin'")
        origin <- rotate[2L:3L]
    }
    
    backtrans <- -origin[c(1L, 2L)]
    translate <- rep_len(c(translate, 0), 2L) 
    translate <- translate - backtrans
    a <- toRadians(rotate[1L])
    scale <- rep_len(scale, 2L)
    skew <- toRadians(rep_len(skew, 2L))
        
    T1 <- matrix(c(1,0,0, 0,1,0, translate,1), ncol = 3L)
    T2 <- matrix(c(1,0,0, 0,1,0, backtrans,1), ncol = 3L)
    S <- matrix(c(scale[1L],tan(skew[2L]),0,
        tan(skew[1L]),scale[2L],0, 0,0,1), ncol = 3L)
    R <- matrix(c(cos(a),sin(a),0, -sin(a),cos(a),0, 0,0,1), ncol = 3L)
    T0 %*% T1 %*% S %*% R %*% T2
}

vgtransform.matrix <-
vgtransform.vgshape
