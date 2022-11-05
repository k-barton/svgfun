# `(x,y)` (or  `(dx,dy)`) specify where the line ends. 
vgpath <-
function(d, origin = c(0, 0), n.curve = 25L,
         verbose = FALSE, nodes = FALSE) {
    
    if(length(origin) != 2L && !is.numeric(origin))
        stop("'origin' must be numeric[2]")

    .msg <- .messageCtor(verbose)
    
    bcn <- n.curve + 1L
    bcseq <- seq(1 / bcn, 1, by = 1 / bcn)
    
    d <- paste0(d, collapse = " ")
    d <- unlist(regmatches(d, gregexpr(
        sprintf("([%s]|[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?)",
            paste0(.cmdnames, collapse = "")), d, perl = TRUE)))
    
    ditem <- match(d, .cmdnames, nomatch = 0L)
    dcmdpos <- which(ditem != 0L)
    cmds <- ditem[dcmdpos]
    .msg("path definition has %d items, including %d commands", length(ditem), length(cmds))

    if(all(cmds[1L] != 1L:2L))
        stop("path definition must start with \"moveTo\" command.")
    
    npar <- .pathcmdnpar[cmds]
    param <- as.numeric(d[-dcmdpos])
    
    if(anyNA(param))
        stop("path definition contains malformed numbers")
    kpar <- diff(c(dcmdpos, length(ditem) + 1L)) - 1L
    ncmd <- kpar / npar
    ncmd[!is.finite(ncmd)] <- 1L
    if(!all(ok <- kpar == npar | (kpar %% npar == 0L))) {
        bad <- which(!ok)[1L]
        bad <- sum(kpar[1L:bad]) + bad + 1L
        stop(sprintf("unexpected %s at position %d",
            if(bad > length(d)) "end" else sQuote(d[bad], FALSE), bad))
    }
    cmdslong <- rep.int(cmds, ncmd)
    
    # replace M..->MLL and m..->mll 
    cmdidx <- unlist(lapply(ncmd, seq_len), use.names = FALSE) 
    mi <- match(cmdslong, .cmdname2idx[c("m", "M")], nomatch = 0L)
    j <- mi != 0 & cmdidx != 1L
    cmdslong[j] <- .cmdname2idx[c("l", "L")][mi[j]]
    ###
    #parentcmd <- rep(seq_along(ncmd), ncmd)
    
    npar1 <- rep.int(npar, ncmd)
    pos2 <- cumsum(npar1)
    pos1 <- c(1L, pos2 + 1L)
    m <- length(cmdslong)

    addMove <- seq_len(m) != 1L & cmdslong %in%  .cmdname2idx[c("m", "M")]
    xylen <- rep.int(1L, m)
    xylen[.CMDNAMES[cmdslong] %in% c("C", "S", "Q", "T", "A")] <- bcn ### XXX!
    xylenmv <- xylen + addMove
    xypos <- cumsum(c(start = 1L, xylen[-m] + addMove[-1L]))
    nxy <- sum(xylenmv)
    xy <- array(NA_real_, dim = c(nxy, 2L),
        dimnames = list(rep.int(.cmdnames[cmdslong], xylenmv), c("x", "y")))

    if(nodes <- isTRUE(nodes)) {
        nodelist <- vector("list", m)
        names(nodelist) <- .cmdnames[cmdslong]
    }
    
    .msg("path has %d subpaths", sum(addMove) + 1L)
    #cbind(cmdnum = cmdslong, parentcmd, cmdidx, npar1,
          #s = pos1[1L:m], e = pos2, addMove, xylen, xypos) 
    
    # requires bzord, pos2, param
    .beziermirrorpoint <-
    function(j, pos = c(0, 0)) {
        if(.beziercurveorder[cmdslong[j]] != 0L && .beziercurveorder[cmdslong[j]] == .beziercurveorder[cmdslong[j + 1L]]) {
            .msg("smooth node %s after %s", .cmdnames[cmdslong[j + 1]], .cmdnames[cmdslong[j]])
            k <- seq.int(pos2[j] - 3L, pos2[j])
            #.msg("%d: smooth handle: %g x %g", j + 1, param[k[3L]] - param[k[1L]], param[k[4L]] - param[k[2L]])
            pos - param[k[1L:2L]] + param[k[3L:4L]]
        } else pos
    }
    
    pos <- start <- origin
    segmentlen <- integer(m)
    for(i in seq_len(m)) {
        k <- if(npar1[i] != 0L) pos1[i]:pos2[i] else NULL
        cmd <- .cmdnames[cmdslong[i]]
        CMD <- .cmdnames[cmdslong[i] + .isRelCmd[cmdslong[i]]]
        # Note: z is NOT relative
    
        pts <- switch(cmd,
            z =, Z = start,
            H = c(param[k], pos[2L]),
            h = c(param[k], 0),
            V = c(pos[1L], param[k]),
            v = c(0, param[k]),
            T =, S = c(.beziermirrorpoint(i - 1L, pos), param[k]),
            t =, s = c(.beziermirrorpoint(i - 1L), param[k]),
            param[k]) # switch cmd
        
        
        if(.isRelCmd[cmdslong[i]]) {
            if(cmd == "a") {
                pts[6L:7L] <- pos + pts[6L:7L]
            } else
                pts <- pos + pts
        }
        
        switch(CMD, z =, Z =, M = {
            pos[] <- xy1 <- pts
            start[] <- pos 
        }, V =, H =, L = {
            pos[] <- xy1 <- pts 
        }, T =, S =, Q =, C = {
            xy1 <- bezier(bcseq, matrix(c(pos, pts), byrow = TRUE, ncol = 2L))
            pos[] <- xy1[bcn, ]
        }, A = {
            # A rX,rY, rotation, arc, sweep, eX, eY
            # arc([sX, sY], [eX, eY], [rX,rY], rotation, arc, sweep)
            xy1 <- arc(pos, pts[6L:7L], pts[1L:2L], pts[3L],
                pts[4L] != 0, pts[5L] != 0, bcseq)

            # In that cases, shift the remaining coordinates up and
            # trim xy from the bottom.
            n1 <- NROW(xy1)
            if(n1 != bcn && n1 == 1L) {
                xylen[i] <- n1
                xypos[j] <- xypos[j <- (i + 1L):m] + n1 - bcn
            }
            pos[] <- xy1[n1, ]
          
            if(nodes) {
                pts[1L:2L] <- attr(xy1, "rxy")
                attr(pts, "center") <- attr(xy1, "center")
            }

        }, stop(sprintf("unknown path command '%s'", cmd))
        )
        .msg("% 3d: %s to %g %g", i, .cmdnames[cmdslong[i]], pos[1], pos[2])
        segmentlen[i] <- length(xy1)
        xy[seq.int(xypos[i], length.out = xylen[i]), ] <- xy1
        
        if(nodes) nodelist[[i]] <- pts
        
    } # j
    
    if(xypos[i] + xylen[i] - 1L < nxy)
        xy <- xy[seq.int(1L, xypos[i] + xylen[i] - 1L), , drop = FALSE]
    rownames(xy) <- NULL
    
    if(nodes) attr(xy, "nodes") <- nodelist
    class(xy) <- c("vgshape", "vgelement", class(xy))
    attr(xy, "svg:name") <- "path"
    attr(xy, "seglen") <- segmentlen %/% 2L
    
    xy
}
    
.pathcmdnpar <- 
local({
    s <- rep(c(m = 2L, l = 2L, h = 1L, v = 1L, c = 6L, s = 4L, q = 4L, t = 2L,
        a = 7L, z = 0L), each = 2L)
    storage.mode(s) <- "integer"
    i <- seq.int(2L, length(s), by = 2L)
    names(s)[i] <- toupper(names(s)[i])
    s
})
    
.beziercurveorder <-
local({
    x <- c(c = 2L, s = 2L, q = 3L, t = 3L, C = 2L, S = 2L, Q = 3L, T = 3L)
    storage.mode(x) <- "integer"
    x1 <- .pathcmdnpar
    x1[] <- 0L
    x1[names(x)] <- x
    x1
})
    
.cmdnames <- names(.pathcmdnpar)
.cmdname2idx <- structure(seq_along(.cmdnames), names = .cmdnames)
.CMDNAMES <- toupper(.cmdnames)
.isRelCmd <- .cmdnames != .CMDNAMES & .CMDNAMES != "Z" 
