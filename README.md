This repository contains code for simulating Zachery B Walter's [claimed Linear-time 3SAT algorithm](http://arxiv.org/abs/1510.00409) with Microsoft's LIQUID, as well as code for simulating the classical variant of the algorithm.

## Results

Based on the behavior of the simulation, and my analysis of the algorithm, [I say it's actually exponential time](http://algorithmicassertions.com/post/1617).

Notice the way the right-side bits seem to be pulled to 100%:

![Typical run](http://algorithmicassertions.com/assets/2016-06-19-quantum-vs-np-2b/simulation-trace.gif)

The effect gets exponentially stronger as the number of bits is increased.
