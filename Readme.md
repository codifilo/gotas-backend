## Description

Gotas is REST API that given a GPS location returns a JSON list of past and future rainfall in mm/h.

I created this project to practice my recently acquired knowledge of Haskell (functional programming) and Docker (used to compile in my local machine and to deploy to Amazon AWS).

I also implemented in a different [repository]([I'm an inline-style link](https://www.google.com) an Android application written in Kotlin that consumes this service.

## How it works
First, it transforms the GPS location to a pixel location of that coordinate in the forecast provider image. Then it downloads in parallel forecast images of two hours before and two hours after and reads the selected pixel of each image. Finally, the pixel is converted to the HSL color space and the minimum euclidian distance is calculated to each color in the forecast image legend and converted to the matched mm/h amount.

Because forecast images are only updated every 15 minutes, they are hold in a concurrent cached using Haskell STM.
