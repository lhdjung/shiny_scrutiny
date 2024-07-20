# Scrutiny webapp

<!-- badges: start -->

<!-- badges: end -->

This repo contains the source code of the [**error detection webapp**](https://lukasjung.shinyapps.io/shiny_scrutiny/). It's a Shiny app that leverages the R package [scrutiny](https://lhdjung.github.io/scrutiny/) for error detection in science.

Run the app locally using this R code:

```
# install.packages("shiny")
shiny::runGitHub("shiny_scrutiny", "lhdjung")
```

The app provides an intuitive web interface that brings the powers of scrutiny to the broader scientific public:

-   Consistency testing with GRIM, GRIMMER, etc.

-   Duplicate detection
