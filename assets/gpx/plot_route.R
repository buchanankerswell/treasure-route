library(ggplot2)
library(cowplot)
library(dplyr)
library(purrr)
library(trackeR)
library(sf)
library(patchwork)
library(leaflet)
library(htmlwidgets)

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
f <- list.files('.', '*.gpx')
# Filenames
fname <- map_chr(f, ~substr(.x, 1, nchar(.x)-4))
cat('Found', length(f), 'files\n')
cat('Files:', fname, sep = '\n')
cat('Plotting course maps')
# Read courses
course <- map(f, ~{
  d <- readGPX(.x) %>% as_tibble()
  st_as_sf(d, coords = c(3,2), crs = 4326)
})
# Calculate grades
grade <- map(course, ~{
  cut.ind <- splt(.x$distance, 7)
  tibble(x = .x$distance[cut.ind],
         y = .x$altitude[cut.ind]-min(.x$altitude),
         grad = c(0, diff(y)/diff(x))*100,
         grad.label = c('Grade:', paste0(round(grad[-1], 1), '%')),
         gradx = x-(c(0, diff(x))/2),
         grady = y-(c(0, diff(y))/2))
})
# Plot course
pwalk(list(course, grade, fname),  ~ {
  course.box <- st_bbox(bbox_widen(st_bbox(..1),
                                   crs = 4326,
                                   c('left' = 0.1,
                                     'right' = 0.1,
                                     'top' = 0.1,
                                     'bottom' = 0.1)))
  course.smmry <- ..1 %>%
    st_set_geometry(NULL) %>%
    summarise(tot.dist = sum(diff(distance)),
              ele.gain = sum(diff(altitude)[diff(altitude) > 0]),
              duration = round(sum(difftime(max(time),
                                            min(time),
                                            units = 'hours')),
                               1))
  p.course <- ..1 %>%
    st_combine() %>%
    st_cast('LINESTRING') %>%
    ggplot() +
    geom_sf(data = bbox_widen(st_bbox(..1), crs = 4326, c('left' = 0.1, 'right' = 0.1, 'top' = 0.1, 'bottom' = 0.1)), fill = 'cornflowerblue', alpha = 0.2, color = NA) +
    geom_sf(fill = NA, color = 'black', size = 1.25) +
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
  p.profile <- ..1 %>%
    ggplot() +
    geom_area(aes(x = distance/1609,
                  y = (altitude-min(altitude))*3.28084),
              color = 'black',
              alpha = 0.1,
              fill = 'black',
              size = 1,
              position = "identity") +
    labs(x = 'Miles', y = NULL, title = 'Course Profile') +
    theme_classic(base_size = 11, base_family = 'Helvetica') +
    theme(plot.background = element_rect(fill = 'transparent', color = NA),
          panel.background = element_rect(fill = 'transparent', color = NA),
          plot.title = element_text(hjust = 0.5, vjust = 0.5),
          axis.line.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_line(color = 'black'),
          axis.text.x = element_text(color = 'black'))
  p.comp <- p.course / (p.profile +
                          geom_segment(data = ..2, aes(x = x/1609, xend = x/1609, y = 0, yend = y*3.28084)) +
                          annotate('text', x = ..2$gradx/1609, y = Inf, label = ..2$grad.label, size = 3, vjust = 1)) +
    plot_layout(ncol = 1, heights = c(5,1)) &
    theme(panel.background = element_rect(fill = 'transparent', color = NA),
          plot.background = element_rect(fill = 'transparent', color = NA))
  p.comp.wide <- p.course + (p.profile +
                               geom_segment(data = ..2, aes(x = x/1609, xend = x/1609, y = 0, yend = y*3.28084)) +
                               annotate('text', x = ..2$gradx/1609, y = Inf, label = ..2$grad.label, size = 4, vjust = 1)) +
    plot_layout(nrow = 1, widths = c(1,3)) &
    theme(panel.background = element_rect(fill = 'transparent', color = NA),
          plot.background = element_rect(fill = 'transparent', color = NA))
  ggsave(
    paste0('courses/', ..3, '-course.png'),
    plot = p.course,
    device = 'png',
    scale = 1,
    width = 7,
    height = 7,
    units = 'in',
    bg = 'transparent'
  )
  ggsave(
    paste0('courses/', ..3, '-profile.png'),
    plot = p.profile +
      geom_segment(data = ..2, aes(x = x/1609, xend = x/1609, y = 0, yend = y*3.28084)) +
      annotate('text', x = ..2$gradx/1609, y = Inf, label = ..2$grad.label, size = 5, vjust = 1),
    device = 'png',
    scale = 1,
    width = 10,
    height = 3,
    units = 'in',
    bg = 'transparent'
  )
  ggsave(
    paste0('courses/', ..3, '-comp.png'),
    plot = p.comp,
    device = 'png',
    scale = 1,
    width = 7,
    height = 5,
    units = 'in',
    bg = 'transparent'
  )
  ggsave(
    paste0('courses/', ..3, '-comp-wide.png'),
    plot = p.comp,
    device = 'png',
    scale = 1,
    width = 10,
    height = 5.6,
    units = 'in',
    bg = 'transparent'
  )
  leaflet(..1 %>% st_combine() %>% st_cast('LINESTRING'),
          sizingPolicy = leafletSizingPolicy(browser.fill = F)) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolylines(color = 'deeppink', opacity = 0.8) %>%
    saveWidget(file=paste0('courses/', ..3, '-leaflet.html'))
})
