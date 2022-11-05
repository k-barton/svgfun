
listofnumbers <-
function(x) 
    as.numeric(unlist(regmatches(x,
        gregexpr("[-+]?[0-9]*\\.?[0-9]+(?:[eE][-+]?[0-9]+)?", x, perl = TRUE))))

    