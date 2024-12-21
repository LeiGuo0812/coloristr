# Convinent color and theme settings for ggplot2
The color palettes are mainly from [tidyplots package](https://github.com/jbengler/tidyplots/). 

## Install
```r
devtools::install_github("LeiGuo0812/coloristr")
```
## Usage

- Check all palettes
```r
library(coloristr)
palette_info
```

- Get palette colors
```r
get_continuous_palette('viridis')
```

- Applying color palettes

```r
library(coloristr)
library(ggplot2)

mtcars |> 
  ggplot(aes(x = mpg, y = disp, color = factor(cyl))) +
  geom_point() +
  scale_color_discrete_cr()
```
