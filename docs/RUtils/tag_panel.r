#---~---
#   This function is based on egg::tag_facet, but with a minor change to keep the 
# strip text of facets.
#---~---
tag_panel <<- function ( p, open = "(", close = ")", tag_pool = letters, x = -Inf
                       , y = Inf, hjust = -0.10, vjust = 1.10, fontface = 1, fontsize = 1.5
                       , family = "Helvetica", ...){
    gb   = ggplot_build(p)
    lay  = gb$layout$layout
    tags = cbind(lay, label = paste0(open, tag_pool[lay$PANEL],close), x = x, y = y)
    ans  = p + geom_text( data        = tags
                        , mapping     = aes_string(x = "x", y = "y", label = "label")
                        , ...
                        , hjust       = hjust
                        , vjust       = vjust
                        , fontface    = fontface
                        , size        = unit(fontsize,"pt")
                        , family      = family
                        , inherit.aes = FALSE
                        )#end geom_text
    return(ans)
}#end function tag_panel
#---~---
