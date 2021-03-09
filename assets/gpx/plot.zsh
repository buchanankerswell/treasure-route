#!/bin/zsh

for i in *.gpx; Rscript plot_route.R ${i%.*}