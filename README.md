# ScaHAL, a machine learning and AI toolkit for Scala
A Machine Learning and AI toolkit/library for Scala, [depends on Scalala](https://github.com/scalala/Scalala).
This is a work in progress, but will implement the following things at the least:

* K Nearest Neighbour classifier (IMPLEMENTED)
* Naive Bayes classifier (IMPLEMENTED)
* Logistic regression classifier (IMPLEMENTED)
* Support Vector Machines
* Maximum Entropy classifier
* k-means clustering
* Apriori analysis
* Boltzmann machines and other neural networks
* Auto-associative memory
* ..and much more..

## Usage
TODO, for now the best place to see usage is the unit test source code.

## Design goals
There are many implementations of many of the algorithms in Java, however most of the implementations suffer from many thread safety issues, inconsistent API's and scalability problems. For these reasons, ScaHAL aims to provide:

* An immutable implementation
* A consistent API
* A thread-safe  implementation that is safe to run in concurrent environments.

## Long term goals
The initial phase of ScaHAL is to implement many of the commonly used algorithms in machine learning and artificial intelligence. However I feel that there is much progress that can be done in the general field by using a more cross-scientific approach and borrow ideas from other fields such as neuroscience. Hopefully this project can serve as a launchpad for ultimately creating considerably more intelligent machines.