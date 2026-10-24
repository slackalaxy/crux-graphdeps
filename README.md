# GraphDeps - a graphical overview of CRUX ports dependencies.

[![Run on Posit](https://img.shields.io/badge/Posit-GraphDeps-447099?logo=posit&logoColor=white)](https://slackalaxy-graphdeps.share.connect.posit.cloud/)

GraphDeps is a small R Shiny app to graphically show the dependency relations between [CRUX](https://crux.nu/) ports. You can show all dependencies of a port recursively, or its first level dependents (what depends on the port). 

Clicking on a node shifts focus on it, nodes can be dragged around and the view can be zoomed by the mouse wheel. [You can use the app online at Posit](https://slackalaxy-graphdeps.share.connect.posit.cloud/).

## Features
- Repos: `core`, `opt`, `xorg`, `contrib`, `compat-32` and `kde-plasma-6`.
- Arrows point from a dependency to the port that needs it.
- Only the dependencies listed in `# Depends on:` are considered.

![](./www/screenshot.png)

## Local use
- Installing R dependencies:
```r
install.packages("visNetwork")
install.packages("shiny")
```
- Run manually `download.R`. This will generate the database as CSV (`all.csv`) and a timestamp (`timeUTC.txt`)
- Run GraphDeps from `app.R`

## AI usage
Developed with help from Claude (Anthropic) for code suggestions and debugging.
