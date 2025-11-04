---
title: "Information Theoretic Chunking"
author: Nathaniel Bos
date: 2025-11-04
---

One of the more salient features of our cognition is the [organization
of related parts into
wholes](https://en.wikipedia.org/wiki/Chunking_(psychology)).

In the absence of clear statistical principles guiding this impulse to
*construct*, we attempt to reproduce a similar structure by growing a
prediction model greedily in the direction of maximal compression.

For this, we design a format to serialize text and its derived
prediction model using combinatorial objects of known lengths, which we
simplify into a loss function guiding efficient, deterministic
construction of dictionaries, which we evaluate in appearance and
compression efficiency.

## Note on Overfitting

When modeling data using a growing numbers of parameters, the likelihood
of the data can easily reach zero as the complexity of the data is
transfered into the model instead of getting distilled into its
underlying features.

In machine learning, this is called
[overfitting](https://en.wikipedia.org/wiki/Overfitting) and is best
pictured by [regression
lines](https://en.wikipedia.org/wiki/Polynomial_regression) (the
predictions) drifting away in wild and unlikely interpolations as more
parameters are added to the model.

![](res/chunk/overfit.svg)

This behavior is undesirable in machine learning as it undermines the
*generalizability* of the model which is usually an important aspect of
the exercise.

In the context of compression, where the prediction of unobserved data
is of little importance, the behavior is equally pathological: as the
information to encode the data approaches zero, the information required
to describe the model measurably increases in proportion.

While solutions to overfitting in machine learning almost always
implicate [carving off sections of the
data](https://en.wikipedia.org/wiki/Training,_validation,_and_test_data_sets)
to hide from the model only to be used for measuring generalizability,
the solution in the context of compression is simply to measure the
information (likelihood) of *both the data and the model* in the loss
function.

This is a basic requirement of parametric information measures to avoid
falling into traps where information seems to disappear during
compression.

## Serializing Combinatorial Objects

[As shown previously](count.html#combinatorial-view),
[counting](https://en.wikipedia.org/wiki/Combinatorics) the
[variety](https://en.wikipedia.org/wiki/Variety_(cybernetics)) of
parametrized systems can produce simple closed formulations of their
code length (information) using optimal encoders.

For example, the code length of a sequence of $N$ symbols from an
alphabet of size $m$ with known individual counts $n_0, n_1, ... n_m$
(i.e. a
[categorical](https://en.wikipedia.org/wiki/Categorical_distribution)
[MLE](https://en.wikipedia.org/wiki/Maximum_likelihood_estimation)) is
simply the $\log$ of the [multinomial
coefficient](https://en.wikipedia.org/wiki/Binomial_coefficient#Generalization_to_multinomials)

$$N \choose n_0,n_1,\ldots,n_m.$$

Further, given a total order
(e.g. [lexicographic](https://en.wikipedia.org/wiki/Lexicographic_order#Finite_subsets))
on the summoned combinatorial objects (here [multiset
permutations](https://en.wikipedia.org/wiki/Permutation#Permutations_of_multisets)),
one can
[derive](https://en.wikipedia.org/wiki/Combinatorial_number_system) so
called "ranking" and "unranking" algorithms which, converting the
resulting integers into their binary expansion, are equivalent to
entropy encoders and decoders for serializations of minimal length.

Thus, combinatorial descriptions give both information measure and
codecs (encoder/decoder) to verify those measures.

## Format Description

Using this approach, the order of a sequence of a known alphabet with
known counts can be encoded with code length:

$$\underbrace{\log {N \choose n_0,n_1,\ldots,n_m} \vphantom{\prod_{\displaystyle i}}}
	_{\displaystyle\mathrm{String} \vphantom{\prod}}$$

which depends on knowing the counts of each symbol, which has a variety
equal to the binomial coefficient according to the
[stars-and-bars method](https://en.wikipedia.org/wiki/Stars_and_bars_(combinatorics)):

$$N + m - 1 \choose m - 1$$

where the number of distribution of $N$ identical values across $m$ bins
by counting the number of orderings of the $N$ values (stars) and $m-1$
separators (bars).

We prepend this code so that the string's code can be interpreted
correctly:

$$\underbrace{\log {N + m - 1 \choose m - 1} \vphantom{\prod_{\displaystyle i}}}
	_{\displaystyle n_0,n_1,\ldots,n_m \vphantom{\prod}}
+ \underbrace{\log {N \choose n_0,n_1,\ldots,n_m} \vphantom{\prod_{\displaystyle i}}}
	_{\displaystyle\mathrm{String} \vphantom{\prod}}$$

This, in turn, depends on knowing parameters $N$ and $m$. The length of
the binary expansion of a number is on the order of $\log_2 n$, but
without the [prefix
property](https://en.wikipedia.org/wiki/Prefix_code), the length of such
an expansion is unknowable by a decoder. To address this, a [universal
integer
code](https://en.wikipedia.org/wiki/Universal_code_(data_compression))
(e.g. [Elias](https://en.wikipedia.org/wiki/Elias_delta_coding)) is used
taking at most $2\log_2 n$ bits:

$$\underbrace{2\log m\vphantom{\prod_{\displaystyle i}}}
	_{\displaystyle m \vphantom{\prod}}
+ \underbrace{2\log N\vphantom{\prod_{\displaystyle i}}}
	_{\displaystyle N \vphantom{\prod}}
+ \underbrace{\log {N + m - 1 \choose m - 1} \vphantom{\prod_{\displaystyle i}}}
	_{\displaystyle n_0,n_1,\ldots,n_m \vphantom{\prod}}
+ \underbrace{\log {N \choose n_0,n_1,\ldots,n_m} \vphantom{\prod_{\displaystyle i}}}
	_{\displaystyle\mathrm{String} \vphantom{\prod}}$$

The only part now missing is the actual set of constructions that make
up the $m$ symbols.

### Inductive Constructions

We grow our dictionary of constructions one-by-one, by appending
construction rules consisting of two previously defined symbols (atomic
or constructed) based resting on an assumption that a whole can only be
statistically significant if both of its parts also are.

The resulting model can then be interpreted as a sparse version of a
high-dimensional
[$n$-gram](https://en.wikipedia.org/wiki/Word_n-gram_language_model)
model which avoids the [combinatorial
explosion](https://en.wikipedia.org/wiki/Curse_of_dimensionality)
associated with assigning parameters to every combination of symbols.

Starting with an alphabet of the 256 bytes, the first construction is
one among

$$256 \times 256$$

possible pairs. With the second construction, we include the first:

$$256 \times 256 \times 257 \times 257...$$

In general, for $m$ symbols, we have 256 atoms and $m-256$
constructions. This has variety

$$V(\mathrm{Rules}) = \left(\frac{m!}{255!}\right)^2$$

which is information

$$I(\mathrm{Rules}) = 2\log\left(\frac{m!}{255!}\right).$$

We can also get away with encoding $m - 255$ since $m \geq 256$. The
complete formula for our code length (which is our loss function) is
therefore:

$$\underbrace{2\log (m-256)\vphantom{\prod_{\displaystyle i}}}
	_{\displaystyle m \vphantom{\prod}}
+ \underbrace{2\log \left(\frac{m!}{255!}\right)\vphantom{\prod_{\displaystyle i}}}
	_{\displaystyle\mathrm{Rules} \vphantom{\prod}}
+ \underbrace{2\log N\vphantom{\prod_{\displaystyle i}}}
	_{\displaystyle N \vphantom{\prod}}
+ \underbrace{\log {N + m - 1 \choose m - 1} \vphantom{\prod_{\displaystyle i}}}
	_{\displaystyle n_0,n_1,\ldots,n_m \vphantom{\prod}}
+ \underbrace{\log {N \choose n_0,n_1,\ldots,n_m} \vphantom{\prod_{\displaystyle i}}}
	_{\displaystyle\mathrm{String} \vphantom{\prod}}$$

which is only a function of number of symbols $m$ and counts $n_0, n_1,
..., n_m$.
