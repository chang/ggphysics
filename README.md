## ggphysics

!['composite'](https://github.com/ericchang00/ggplot2-animations/raw/master/img/composite.gif)

Using ggplot2 to make physics animations, just for fun. To render an animation,

```bash
Rscript fireworks.R
```

Each frame is rendered as a ggplot2 plot, and written to `/img/animation`. I also wrote a simple utility to stitch the frames together: [gifmake](https://github.com/ericchang00/gifmake). You can do
```bash
pip install gifmake
gifmake /img/animation
```
