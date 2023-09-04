shadowtext <- function(x, y=NULL, labels, col=gray(1), bg=gray(0), theta= seq(0, 2*pi, length.out=50), r=0.5, ... ) {
    xy <- xy.coords(x,y); xo <- r*strwidth('A'); yo <- r*strheight('A')
    for (i in theta) text( xy$x + cos(i)*xo, xy$y + sin(i)*yo, labels, col=bg, ... )
    text(xy$x, xy$y, labels, col=col, ... )}
