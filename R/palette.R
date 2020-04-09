##' @importFrom ggplot2 discrete_scale aes element_blank geom_tile ggplot ggtitle scale_fill_manual scale_x_continuous theme
##' @importFrom grDevices palette

## from https://github.com/mwaskom/seaborn/blob/master/seaborn/palettes.py
seaborn.names = c("color_palette", "hls_palette", "husl_palette", "mpl_palette",
           "dark_palette", "light_palette", "diverging_palette",
           "blend_palette", "xkcd_palette", "crayon_palette",
           "cubehelix_palette", "set_color_codes")


SEABORN_PALETTES = list(
    deep=c("#4C72B0", "#DD8452", "#55A868", "#C44E52", "#8172B3",
          "#937860", "#DA8BC3", "#8C8C8C", "#CCB974", "#64B5CD"),
    deep6=c("#4C72B0", "#55A868", "#C44E52",
           "#8172B3", "#CCB974", "#64B5CD"),
    muted=c("#4878D0", "#EE854A", "#6ACC64", "#D65F5F", "#956CB4",
           "#8C613C", "#DC7EC0", "#797979", "#D5BB67", "#82C6E2"),
    muted6=c("#4878D0", "#6ACC64", "#D65F5F",
            "#956CB4", "#D5BB67", "#82C6E2"),
    pastel=c("#A1C9F4", "#FFB482", "#8DE5A1", "#FF9F9B", "#D0BBFF",
            "#DEBB9B", "#FAB0E4", "#CFCFCF", "#FFFEA3", "#B9F2F0"),
    pastel6=c("#A1C9F4", "#8DE5A1", "#FF9F9B",
             "#D0BBFF", "#FFFEA3", "#B9F2F0"),
    bright=c("#023EFF", "#FF7C00", "#1AC938", "#E8000B", "#8B2BE2",
            "#9F4800", "#F14CC1", "#A3A3A3", "#FFC400", "#00D7FF"),
    bright6=c("#023EFF", "#1AC938", "#E8000B",
             "#8B2BE2", "#FFC400", "#00D7FF"),
    dark=c("#001C7F", "#B1400D", "#12711C", "#8C0800", "#591E71",
          "#592F0D", "#A23582", "#3C3C3C", "#B8850A", "#006374"),
    dark6=c("#001C7F", "#12711C", "#8C0800",
           "#591E71", "#B8850A", "#006374"),
    colorblind=c("#0173B2", "#DE8F05", "#029E73", "#D55E00", "#CC78BC",
                "#CA9161", "#FBAFE4", "#949494", "#ECE133", "#56B4E9"),
    colorblind6=c("#0173B2", "#029E73", "#D55E00",
                 "#CC78BC", "#ECE133", "#56B4E9")
)

seaborn_name <- function (palette) {
    if (is.character(palette)) {
        if (!palette %in% names(SEABORN_PALETTES)) {
            warning("Unknown palette ", palette)
            palette <- "muted"
        }
        return(palette)
    }
    names(SEABORN_PALETTES)[palette]
}

seaborn_pal <- function(palette=3,direction=1) {
    pal <- seaborn_name(palette)
    force(direction)
    function(n) {
      if (direction == 1) {
        SEABORN_PALETTES[[pal]][seq_len(n)]
      } else {
        rev(SEABORN_PALETTES[[pal]])[seq_len(n)]
      }
    }
}

##' seaborn scales for ggplot2
##'
##' designed to bring the seaborn scale 
##' @title seaborn scales for ggplot2
##' @param ... passed to \link[ggplot2]{discrete_scale}
##' @param palette optional, name of palette, default is "muted". Use
##'   \link{show_seaborn} to see all options
##' @param aesthetics optional, define which aesthetics (fill, colour)
##'   to set the scale for
##' @param direction optional, default 1. set direction=-1 to use the
##'   colours in the opposite order
##' @return palette for ggplot2 object
##' @export
##' @author Chris Wallace
##' @rdname seaborn
##' @examples
##' library(ggplot2)
##' ggplot(data.frame(x=1:5,y=1:5,fill=factor(1:5)), aes(x=x,y=y,fill=fill)) +
##' geom_tile()
##'
##' ggplot(data.frame(x=1:5,y=1:5,fill=factor(1:5)), aes(x=x,y=y,fill=fill)) +
##' geom_tile() +
##' scale_fill_seaborn()
##'
##' ggplot(data.frame(x=1:5,y=1:5,fill=factor(1:5)), aes(x=x,y=y,fill=fill)) +
##' geom_tile() +
##' scale_fill_seaborn(palette="dark6",direction=-1)
scale_fill_seaborn <- function(..., palette="muted", aesthetics="fill", direction=1) {
  discrete_scale(aesthetics, "seaborn", seaborn_pal(palette, direction), ...)
}

##' @rdname seaborn
##' @export
scale_colour_seaborn <- function(..., palette="muted", aesthetics="colour", direction=1) {
  discrete_scale(aesthetics, "seaborn", seaborn_pal(palette, direction), ...)
}

##' display seaborn palettes 
##'
##' generate a display of all seaborn palette options on current graphics device
##' @title show all seaborn palettes
##' @return ggplot2 object
##' @export
##' @author Chris Wallace
##' @examples
##' show_seaborn()
show_seaborn <- function() {
  df <- data.frame(colour=unlist(SEABORN_PALETTES),
                   x=unlist(lapply(SEABORN_PALETTES, seq_along)),
                   palette=rep(names(SEABORN_PALETTES), sapply(SEABORN_PALETTES, length)))
  breaks <- as.character(unique(df$colour))
  names(breaks) <- breaks

  x <- colour <- NULL # get around R CMD check
  ggplot(df, aes(x=x,y=palette, fill=colour)) +
    geom_tile(col="grey", height=0.8) +
    scale_fill_manual(values=breaks) +
    scale_x_continuous(expand=c(0,0)) +
    theme(legend.position="none",
          panel.background=element_blank(),
          axis.text.x=element_blank(),
          axis.title=element_blank(),
          axis.ticks=element_blank()) +
    ggtitle("seaborn colour palettes")
}
