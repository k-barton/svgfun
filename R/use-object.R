use.element <-
function(node, props = objectprops(node))  {
    count <- 1L
    ids <- character(0L) # collect ids to prevent endless loop in a potentially
                         # possible self-referencing group of elements:
                         # id1 -> id2 -> id1
    props1 <- list(transform = diag(3L))
    repeat {
        props <- objectprops(node)
        ids[count] <- if(is.null(props$id)) "" else props$id
        if(any(duplicated(ids))) {
            message("skipping self-referencing 'use' elements: ",
                paste0("#", sQuote(ids), collapse = ", "))
            return(NULL)
        }
           
        props1$transform <- props1$transform %*% gettransmat(props)
        props1$painting <- combine2(props1$painting, props$painting)
        ptypes <- names(props)
        for(b in ptypes[! ptypes %in% c("painting", "transform", "id")])
             props1[[b]] <- combine2(props1[[b]], props[[b]])
        if(props$tagname == "use" && has(props, "href")) {
            node <- getreferencenode(node, FALSE)
            if(!inherits(node, "xml_node")) break
            count <- count + 1L
        } else break
    }
    list(node = node, props = props1, n = count)
}


# used for *gradient, only
gradient.element <-
function(node)  {
    count <- 1L
    props1 <- list(transform = diag(3))
    repeat {
        props <- objectprops(node)
        props1$transform <- props1$transform %*% gettransmat(props)
        props1$painting <- combine2(props1$painting, props$painting)
        ptypes <- names(props)
        for(b in ptypes[! ptypes %in% c("painting", "transform", "id")])
             props1[[b]] <- combine2(props1[[b]], props[[b]])
        if(props$tagname == "use" && has(props, "href")) {
            node <- getreferencenode(node, FALSE)
            if(!inherits(node, "xml_node")) break
            count <- count + 1L
        } else break
    }
    list(node = node, props = props1, n = count)
}


#use.element <-
#function(node, props = objectprops(node))  {
#    count <- 0L
#    repeat {
#        print(node)
#        print(props$transform)
#
#        if(props$tagname == "use" && has(props, "href")) {
#            target <- getreferencenode(node, FALSE)
#            if(inherits(target, "xml_node")) {
#                targetprops <- objectprops(target)
#                targetprops$transform <-
#                    gettransmat(props) %*% gettransmat(targetprops)
#                targetprops$painting <-
#                    combine2(props$painting, targetprops$painting)
#                #targetprops$id <- props$id
#                ptypes <- union(names(targetprops), names(props))
#                for(b in ptypes[! ptypes %in% c("painting", "transform", "id")])
#                    targetprops[[b]] <- combine2(props[[b]], targetprops[[b]])
#                count <- count + 1L
#            } else break
#            node <- target
#            props <- targetprops
#        } else break
#    }
#    list(node = node, props = props, n = count)
#    #attr(props, "node") <- node
#    #props
#}

        
#{{ Most attributes on use do not override those already on the element
#   referenced by use. Only the attributes x, y, width, height and href on the
#   use element will override those set on the referenced element. However, any
#   other attributes not set on the referenced element will be applied to the
#   use element. Note: width, and height have no effect on use elements, unless
#   the element referenced has a viewbox - i.e. they only have an effect when
#   use refers to a svg or symbol element.
#}} 
