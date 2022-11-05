getreferencenode <- 
function(node, deep = TRUE) {
    href <- xml_attr(node, "href")
    if(is.na(href)) return(node)
    hrefid <- substr(wsstrip(href), 2L, 256L)
    target <- xml_find_first(xml_root(node), sprintf("//*[@id =\"%s\"]", hrefid))
    if(is.na(target) || inherits(target, "xml_missing"))
        return(NA)
    if(isTRUE(deep)) getreferencenode(target) else target
}





