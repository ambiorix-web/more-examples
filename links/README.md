# Deep Link Showcase

This example highlights how an [ambiorix](https://github.com/ambiorix-dev/ambiorix)
application can expose links that jump straight to a specific
page, panel, and tab combination.

## Requirements

- R (>= 4.1)
- Packages: `ambiorix`, `htmltools`, `base64enc`

Install them from CRAN if needed:

```r
install.packages(c("ambiorix", "htmltools", "base64enc"))
```

## Run the app

```bash
Rscript index.R
```

Visit the printed URL (defaults to <http://127.0.0.1:3000/pages/welcome/orientation/tour>).

Each section in the UI has its own route, so switching pages, panels,
or nested tabs updates the browser path immediately.
Copy the generated link in the "Share this exact view" card to demonstrate
deep-linking down to a specific section.

## Shareable URL structure

Links follow the format:

```
/pages/<page>/<panel>/<tab>
```

This makes it easy to reference precise sections—for example
`/pages/analytics/traffic/campaigns` opens the Analytics page,
Traffic panel, and Campaigns tab in one click.

## Sample data

The showcase seeds a small synthetic analytics and operations data set on
startup so every tab renders real tables and KPI cards—handy for folks who
expect data-driven pages while they explore the deep-linking pattern.

The `Simulations` page nods to the classic random distribution example:
pick a distribution and sample size, then deep-link colleagues directly
to the plot, summary, or table view thanks to routes like
`/pages/simulations/random/summary?dist=lnorm&n=750`.
