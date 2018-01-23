## ggphysics

Using ggplot2 for things it wasn't meant for, just for fun.  

To make physics animations with ggplot2, each particle (a ball, rain drop, firework, etc.) is represented as an observation in a dataframe with a position, velocity, and acceleration (and possible additional fields for color and size). The frames are then plotted and updated using elementary physics equations in a loop.  

I use [gifmake](https://github.com/ericchang00/gifmake) to stitch the individual frames together into animations.  

!['fireworks'](https://github.com/ericchang00/ggplot2-animations/raw/master/img/fireworks.gif)

!['fireworks'](https://github.com/ericchang00/ggplot2-animations/raw/master/img/flubber.gif)

!['fireworks'](https://github.com/ericchang00/ggplot2-animations/raw/master/img/rain.gif)

!['bouncy_ball'](https://github.com/ericchang00/ggplot2-animations/raw/master/img/bouncy_ball.gif)
