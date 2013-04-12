\name{plot.GIS}
\alias{plot.GIS}
\title{plot.GIS}
\usage{
  plot.GIS(gis_data, main = "", xlab = "", ylab = "",
    xlim = NULL, ylim = NULL, show.axis = TRUE,
    show.grid = TRUE, grid.div = 1, round.grid = FALSE,
    grid.col = "grey", fill.col = NA, line.col = NULL,
    bg.col = "white", box.col = NA, new.window = TRUE,
    no.margin = FALSE, max.dimen = 13, cex.main = 1.2,
    cex.lab = 1, cex.axis = 0.8, blank.plot = FALSE,
    plot.shape = TRUE, additions = FALSE,
    return.dimen = TRUE)
}
\arguments{
  \item{gis_data}{GIS object, or list of objects, to be
  plotted}

  \item{main}{Text variable controlling the main title
  placed on the plot - value needs to be a text string
  (default == '', title is blank)}

  \item{xlab}{Text variable controlling the label for the
  x-axis - value needs to be a text string (default = "",
  blank)}

  \item{ylab}{Text variable controlling the label for the
  y-axis - value needs to be a text string (default = "",
  blank)}

  \item{xlim}{Numberical variable setting the lower and
  upper limits for the x-axis. xlim can be rounded to
  nearest grid divison (controlled by grid.div and
  round.grid) that allows the specified coordinates to be
  plotted (default = \code{NULL}, fixs the limits to the
  nearest grid line (see grid.divs) that allows all GIS
  shapes to be plotted)}

  \item{ylim}{Numberical variable setting the lower and
  upper limits for the y-axis.  ylim can be rounded to the
  nearest grid divison (controlled by grid.div and
  round.grid) that allows the specified coordinates to be
  plotted (default = \code{NULL}, fixs the limits to the
  nearest grid line (see grid.divs) that allows all gis
  shapes to be plotted)}

  \item{show.axis}{Logical variable controlling whether
  axis are plotted}

  \item{show.grid}{Logical variable determining whether to
  overlay gridlines ontop on the plot}

  \item{grid.div}{Numerical variable determining the
  interval between axis ticks and also the gridlines. Note:
  when show.grid = \code{TRUE} this value still determines
  the axes tick intervals (default = 1)}

  \item{round.grid}{Logical, if TRUE the bounding box is
  enlarged so that that entire grid cells are shown
  (default = FALSE)}

  \item{grid.col}{Variable determining the colour of the
  lines used for the overlaid grid.  Value can be text
  string of a named colour, numerical value, etc, see help
  for function 'par' for more details (default =
  \code{"grey"}).}

  \item{fill.col}{Variable determining the colour used to
  fill the landmasses plotted.  Value can be text string of
  a named colour, numerical value, etc, see help for
  function 'par' for more details (default = \code{NA},
  meaning no fill)}

  \item{line.col}{Variable determining the colour of the
  line used to plot the landmasses.  Value can be text
  string of a named colour, numerical value, etc, see help
  for function 'par' for more details (default =
  \code{NULL}, which results in black lines)}

  \item{bg.col}{Variable determining the colour of the
  background of the plot area. Value can be text string of
  a named colour, numerical value, etc, see help for
  function 'par' for more details (default =
  \code{"white"})}

  \item{box.col}{Variable determining the colour of the
  border surrounding the plot area (default = \code{NA}, no
  border is added to plot)}

  \item{new.window}{Logical variable determining whether
  plot should create and plot in a new window or plot
  within an existing window/device. NOTE: when new.window =
  \code{TRUE} the window dimensions are determined based
  upon the xlim and ylim in order to keep the correct
  aspect ratio and preserve the map shape, if changing
  new.window to \code{FALSE} the sizing of the
  device/window is down the the user/previous plot (default
  \code{TRUE})}

  \item{no.margin}{Logical variable determining whether
  plot should include a margin NOTE - main title, x-labels
  and y-labels are written in margin so do not remove
  margin if titles/labels are required (default =
  \code{FALSE}, plot includes a margin).}

  \item{max.dimen}{Numberical variable determining the
  maximum window dimension. Variable determines height or
  width depend on whether xlim range is greater than ylim
  range (default = \code{13})}

  \item{cex.main}{Numberical variable determining the
  relative sizing of the main plot title (default =
  \code{1.2})}

  \item{cex.lab}{Numberical variable determining the
  relative sizing of the axis labels (default = \code{1})}

  \item{cex.axis}{Numberical variable determining the
  relative sizing of the axis values (default =
  \code{0.8})}

  \item{blank.plot}{Logical variable determining whether
  function should finish once blank plot window created or
  if it should continue further with the plotting (default
  = \code{FALSE}, plotting continues)}

  \item{plot.shape}{Logical variable determining whether to
  plot lines from shape file or whether to only setup the
  background of the plot (i.e grid lines, axis) to which
  outline can be added later, useful where outline is to
  cover plotting symbols/colours (default = \code{TRUE})}

  \item{additions}{Logical variable determining whether
  plot is to be created from scratch or whether components
  (i.e. axes, gridlines, outline) are to be added to an
  existing plot (default = \code{FALSE})}
}
\value{
  A data.frame giving the dimensions of the plot area.
}
\description{
  This function can be used to plot gis data that has been
  loaded from shape files using the \code{readShapePoly()}
  function contained in the 'maptools' R package.
}
\examples{
\dontrun{
# load example shapefile that was created using readShapePoly() from library 'maptools'
data(world)

# Example 1
# plot with defaults, main plot title set to 'World Map', grid line intervals at units of 10
plot.GIS(world, main = "World Map", grid.div = 10)

# Example 2
# define a region to plot using xlim and ylim, colour plot background, and fill landmass
plot.GIS(world, main = "World Map", grid.div = 10, xlim = c(-10,20), ylim = c(30,60),
         fill.col = "lightgreen", bg.col = "lightblue")

# Example 3
# plot with finer scale, black, grid lines, and labelled axes
plot.GIS(world, main = "World Map", grid.div = 5, grid.col = 'black', xlab = 'Longitude',
         ylab = 'Latitude', xlim = c(-10,20), ylim = c(30,60), fill.col = "lightgreen",
         bg.col = "lightblue")

# Example 4
# plot Africa without gridlines, axes labels or margins
plot.GIS(world, xlab="", ylab="", show.axis = FALSE, show.grid = FALSE, fill.col = "lightgreen",
no.margin = TRUE, xlim = c(-20,55), ylim = c(-40,40))

# Example 5
# Plot UK with fill colour and background colour but no grid, then add points and labels
# highlighting the locations of the capital cities
plot.GIS(world, main="UK Capital Cities", ylim = c(48, 60), xlim = c(-10,3), show.grid = FALSE,fill.col = "lightgreen", bg.col = "lightblue")
city.x = c(0.1062, -3.2200, -3.1771, -6.2661, -5.9167)
city.y = c(51.5171, 55.9500, 51.4780, 53.3428, 54.6000)
points(city.x,city.y,pch=16, col="red")
text(city.x[1:2], city.y[1:2], labels = c("London", "Edinburgh"), col="black", pos=1)
text(city.x[3], city.y[3], labels = "Cardiff", col="black", pos=3)
text(city.x[4:5], city.y[4:5], labels = c("Dublin", "Belfast"), col="black", pos=2)

# Example 6
# This plot highlights that when round.grid=TRUE the real xlim and ylim for the plot are determined
# by taking user specified values and rounding to the nearest grid division allowing given
# coordinates to be plotted
plot.GIS(world, main = "This is Sparta!", xlim=c(15,25), ylim = c(33,43), grid.div = 10,
fill.col = "lightgreen", bg.col = "lightblue", round.grid=TRUE)
rect(15,33,25,43, border ="red", lty=2)
points(x = 22.4303, y = 37.0765, col="red", pch=16, cex = 1.7)
text(x = 22.4303, y = 37.0765, labels = "Sparta", pos = 2, cex = 1.5)
}
}
\keyword{GIS,}
\keyword{mapping,}
\keyword{shapefile}

