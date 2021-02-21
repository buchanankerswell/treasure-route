cat('Loading packages\n')
suppressMessages({
  library(ggplot2)
  library(cowplot)
  library(dplyr)
  library(purrr)
  library(trackeR)
  library(sf)
  library(patchwork)
  library(leaflet)
  library(htmlwidgets)
  library(RColorBrewer)
  library(ggrepel)
})

cat('Defining course mapping functions\n')
# Remove outliers
replace_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - 2*H)] <- 0
  y[x > (qnt[2] + 2*H)] <- 0
  y
}
# Draw a widened box from a st_bbox object
bbox_widen <- function(bbox, crs, borders = c('left' = 0.5, 'right' = 0.5, 'top' = 0, 'bottom' = 0)) {
  b <- bbox # current bounding box
  xrange <- b$xmax - b$xmin # range of x values
  yrange <- b$ymax - b$ymin # range of y values
  b[1] <- b[1] - (borders['left'] * xrange) # xmin - left
  b[3] <- b[3] + (borders['right'] * xrange) # xmax - right
  b[2] <- b[2] - (borders['bottom'] * yrange) # ymin - bottom
  b[4] <- b[4] + (borders['top'] * yrange) # ymax - top
  box <- st_polygon(list(matrix(c(b$xmin, b$ymax, b$xmin, b$ymin, b$xmax, b$ymin, b$xmax, b$ymax, b$xmin, b$ymax), ncol = 2, byrow = TRUE))) %>%
    st_sfc(crs = crs)
  return(box)
}

# Split segments and buffers into equidistant subsegments
splt <- function(dst, cut.prop = 4) {
  # Calculate cut length
  cut.length <- cumsum(dst)[length(cumsum(dst))] / cut.prop
    # Find indices to split points into groups with equal distances
    cut.ind <- c(1, rep(NA, ceiling(cut.prop)))
    save.ind <- 2
    start.ind <- 1
    for(i in 1:length(dst)) {
      cmsm <- cumsum(dst[start.ind:i])
      tot <- cmsm[length(cmsm)]
      if(tot < cut.length) {
        i <- i + 1
      } else {
        cut.ind[save.ind] <- i
        start.ind <- i
        save.ind <- save.ind + 1
      }
    }
    # Last cut should be end of segment line
    cut.ind[length(cut.ind)] <- length(dst)
    # Return results
    return(cut.ind)
}
cat('Reading files\n')
# List gpx files
files <- list.files('.', '*.gpx')
# Filenames
fname <- map_chr(files, ~substr(.x, 1, nchar(.x)-4))
cat('Found', length(files), 'files\n')
cat('Courses:', fname, sep = '\n')
# Take Rscript arguments
args <- commandArgs(TRUE)
f <- args[1]
cat('Reading course:', f, '\n')
# Read courses
d <- readGPX(files[fname %in% f]) %>% as_tibble()
course <- st_as_sf(d, coords = c(3,2), crs = 4326) %>%
  mutate(
  grad = c(0, diff(altitude)/diff(distance))*100,
  'grad' = replace(grad, is.infinite(grad), 0),
  'grad' = replace(grad, is.na(grad), 0),
  'grad' = replace_outliers(grad),
  'grad' = c(rep(0, 100), kernapply(grad, kernel('daniell', 100)), rep(0, 100)),
  av.grad = round(rollmeanr(grad, 3, fill = 0), 1),
  .before = speed
)

# Course summary
course.smmry <- course %>%
  st_set_geometry(NULL) %>%
  summarise(tot.dist = sum(diff(distance)),
            ele.gain = sum(diff(altitude)[diff(altitude) > 0]),
            duration = round(sum(difftime(max(time),
                                          min(time),
                                          units = 'hours')),1))
# Places
places <- tibble(
  place = c('district coffee'),
  lat = c(43.618260),
  lon = c(-116.204595)
) %>%
  st_as_sf(coords = c(3,2), crs = 4326)

# Plotting
cat('Plotting course:', f, '\n')

# Plot course
course.box <- st_bbox(bbox_widen(st_bbox(course),
                                 crs = 4326,
                                 c('left' = 0.1,
                                   'right' = 0.1,
                                   'top' = 0.1,
                                   'bottom' = 0.1)))
p.course <- course %>%
  st_combine() %>%
  st_cast('LINESTRING') %>%
  ggplot() +
  geom_sf(data = bbox_widen(st_bbox(course), crs = 4326, c('left' = 0.1, 'right' = 0.1, 'top' = 0.1, 'bottom' = 0.1)), fill = 'cornflowerblue', alpha = 0.2, color = NA) +
  geom_sf(fill = NA, color = 'black', size = 1.25) +
  geom_sf(data = places, shape = 8, size = 4, color = 'deeppink') +
  geom_sf_text(data = places, aes(label = place), vjust = -1) +
  annotate('text',
           x = course.box$xmin + (1/5*(course.box$xmax - course.box$xmin)),
           y = course.box$ymin + (1/5*(course.box$ymax - course.box$ymin)),
           label = paste0('Distance: ', round(course.smmry$tot.dist/1609), ' mi\n',
                          'Elevation: ', round(course.smmry$ele.gain*3.28084), ' ft\n',
                          'Duration: ~', course.smmry$duration, ' hr'),
           size = 4) +
  labs(x = NULL, y = NULL) +
  scale_shape_manual(name = NULL, values = c('volcano' = 2)) +
  scale_color_viridis_c(option = 1) +
  scale_fill_viridis_c(name = bquote(mWm^-2), option = 1, na.value = 'transparent') +
  theme_map(font_size = 8) +
  theme(
    axis.text = element_text(),
    panel.border = element_blank(),
    panel.grid = element_line(size = 0.25, color = rgb(0.1, 0.1, 0.1, 0.5)),
    panel.background = element_blank(),
    panel.ontop = TRUE,
    plot.background = element_rect(fill = "transparent", color = NA)
  )
p.profile <- course %>%
  ggplot() +
  geom_ribbon(aes(x = distance/1609, ymin = min(altitude)*3.28084, ymax = altitude*3.28084), alpha = 0.1) +
  geom_path(aes(x = distance/1609, y = altitude*3.28084, color = av.grad), size = 2, linejoin = 'round', lineend = 'round') +
  labs(x = 'Miles', y = NULL, title = 'Course Profile', color = 'Grade') +
  scale_color_viridis_c(option = 'viridis') +
  scale_y_continuous(limits = c(min(course$altitude*3.28084), max(course$altitude*3.28084))) +
  theme_classic(base_size = 11, base_family = 'Helvetica') +
  theme(plot.background = element_rect(fill = 'transparent', color = NA),
        panel.background = element_rect(fill = 'transparent', color = NA),
        plot.title = element_text(hjust = 0.5, vjust = 0.5),
        axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = 'black'),
        axis.text.x = element_text(color = 'black'),
        legend.position = 'bottom',
        legend.background = element_rect(fill = 'transparent', color = NA))
p.comp <- p.course / p.profile + plot_layout(ncol = 1, heights = c(5,1)) &
  theme(panel.background = element_rect(fill = 'transparent', color = NA),
        plot.background = element_rect(fill = 'transparent', color = NA))
cat('Saving course maps to courses/,', f ,'*.png\n', sep = '')
suppressWarnings({
  ggsave(
    paste0('courses/', f, '-course.png'),
    plot = p.course,
    device = 'png',
    scale = 1,
    width = 7,
    height = 7,
    units = 'in',
    bg = 'transparent'
  )
  ggsave(
    paste0('courses/', f, '-profile.png'),
    plot = p.profile,
    device = 'png',
    scale = 1,
    width = 10,
    height = 3,
    units = 'in',
    bg = 'transparent'
  )
  ggsave(
    paste0('courses/', f, '-comp.png'),
    plot = p.comp,
    device = 'png',
    scale = 1,
    width = 7,
    height = 5,
    units = 'in',
    bg = 'transparent'
  )
  ggsave(
    paste0('courses/', f, '-comp-wide.png'),
    plot = p.comp,
    device = 'png',
    scale = 1,
    width = 10,
    height = 5.6,
    units = 'in',
    bg = 'transparent'
  )
})
cat('Saving leflet map to courses/', f ,'-leaflet.html\n', sep = '')
leaflet(course %>% st_combine() %>% st_cast('LINESTRING'),
        sizingPolicy = leafletSizingPolicy(browser.fill = F)) %>%
  addAwesomeMarkers(data = places, label = ~place,
             icon = awesomeIcons(icon = 'coffee-mug', markerColor = 'black')) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolylines(color = 'deeppink', opacity = 0.8) %>%
  saveWidget(file=paste0('courses/', f, '-leaflet.html'))
