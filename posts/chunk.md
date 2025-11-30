---
title: "Information Theoretic Chunking"
author: Nathaniel Bos
date: 2025-11-13
---

One of the more salient features of our cognition is the [organization of
related parts into
wholes](https://en.wikipedia.org/wiki/Chunking_(psychology)).

In the absence of clear statistical principles guiding this drive to
*construct*, we attempt to reproduce similar structure by growing a
prediction model greedily in the direction of maximal compression.

For this, a serialization format is defined for text, based on
combinatorial objects
(e.g. [permutations](https://en.wikipedia.org/wiki/Permutation))
[equivalent](count.html) to such a model, which produces codes of known
lenghts (i.e. [information
content](https://en.wikipedia.org/wiki/Information_content)), from which
we derive a loss function to guide deterministic construction of
dictionaries, of which we note the appearance and performance in
compression.

## Note on Overfitting

When modeling data with an increasing number of parameters, the
[likelihood](https://en.wikipedia.org/wiki/Likelihood_function) of the
data can easily reach zero as the complexity of the data merely gets
transfered to the model instead of getting distilled into underlying
features.

In machine learning, this is called
[overfitting](https://en.wikipedia.org/wiki/Overfitting) and is best
pictured by [regression
lines](https://en.wikipedia.org/wiki/Polynomial_regression)
(predictions) drifting away in wild and unlikely interpolations as more
parameters are added to the model:

![](res/chunk/overfit.svg)

This behavior is undesirable in machine learning as it undermines the
*generalizability* of the model which is usually an important aspect of
the exercise.

In the context of compression, where the prediction of unobserved data
is not as central, the phenomenon is still pathological: as the
information of the data w.r.t. the model approaches zero, the
information required to describe the model measurably increases in
proportion.

While solutions to overfitting in machine learning almost always
implicate [carving off sections of the
data](https://en.wikipedia.org/wiki/Training,_validation,_and_test_data_sets)
to hide from the model only to be used later for measuring
generalizability, the context of compression allows a cleaner solution.

Instead of only minimizing the information (maximizing the
log-likelihood) of the data $\mathrm{\bf x}$ given the model $\theta$:
$$I(\mathrm{\bf x} \mid \theta),$$ we minimize the information of *both
the data and the model*:

$$I(\mathrm{\bf x},\theta) = I(\mathrm{\bf x} \mid \theta) + I(\theta).$$

By including the model in the calculation, we capture any information
that merely transfers from data (given the model) to the model and avoid
an increase in parameters that doesn't decrease *total* information.

This sort of inclusion of the model in the measurement is a basic
requirement for counting information to avoid falling for traps where
information seems to magically disappear from compression.

## Serializing Combinatorial Objects

As shown [previously](count.html#combinatorial-view),
[counting](https://en.wikipedia.org/wiki/Combinatorics) the
[variety](https://en.wikipedia.org/wiki/Variety_(cybernetics)) of
parametrized systems can produce simple closed formulas of their
information (code length) w.r.t. optimal encoders.

For example, the information of a sequence of $N$ symbols from an
alphabet of size $m$ with counts (a.k.a. multiplicities) $n_0, n_1,
... n_{m-1}$ using an optimal probabilistic model (i.e. an updating
[categorical](https://en.wikipedia.org/wiki/Categorical_distribution)
[MLE](https://en.wikipedia.org/wiki/Maximum_likelihood_estimation)) is
simply the $\log$ of the [multinomial
coefficient](https://en.wikipedia.org/wiki/Binomial_coefficient#Generalization_to_multinomials)
with those parameters:

$$\log {N \choose n_0,n_1,\ldots,n_{m-1}},$$

which is simply the number of ways to order a
[multiset](https://en.wikipedia.org/wiki/Permutation) of size $N$ with
multiplicities $n_0,n_1,\ldots,n_{m-1}$.

Further, given a total order
(e.g. [lexicographic](https://en.wikipedia.org/wiki/Lexicographic_order#Finite_subsets))
on the summoned combinatorial object (here [multiset
permutations](https://en.wikipedia.org/wiki/Permutation#Permutations_of_multisets)),
one can
[derive](https://en.wikipedia.org/wiki/Combinatorial_number_system)
so-called "ranking" and "unranking" algorithms to map to and from
natural numbers. Interpreting that numbers in binary results in a
serialization that is equivalent in compression efficiency to entropy
coding the sequence symbol-by-symbol using derived probabilities.

Combinatorial descriptions can therefore give both information content
formulae and the codecs (encoder/decoder) to verify them.

## Format Description

As just stated, a sequence of $N$ symbols from an alphabet of size $m$
with counts $n_0,n_1,\ldots,n_{m-1}$ can be encoded as a multiset
permutation with information:

$$\underbrace{\log {N \choose n_0,n_1,\ldots,n_{m-1}},
\vphantom{\prod_{\displaystyle i}}}
_{\displaystyle\mathrm{String} \vphantom{\prod}}$$

given that the counts are known.

The counts can themselves be encoded by counting the number of multisets
of total count $N$ and distinct symbols $m$, according to the [multiset
coefficient](https://en.wikipedia.org/wiki/Multiset#Counting_multisets):

$$\left(\!\!\!{m \choose N}\!\!\!\right) = {m+N-1 \choose N} = \frac{(m+N-1)!}{N!\,(m-1)!},$$

or equivalently, using
[stars-and-bars](https://en.wikipedia.org/wiki/Stars_and_bars_(combinatorics))
(a permutation of $N$ stars and $m-1$ bars, giving the [binomial
coefficient](https://en.wikipedia.org/wiki/Binomial_coefficient):

$${N + m - 1 \choose m - 1} = \frac{(N+m-1)!}{N!\,(m-1)!}.$$

We prepend this encoding of counts to the encoding of the sequence:

$$\underbrace{\log {N + m - 1 \choose m - 1} \vphantom{\prod_{\displaystyle i}}}
	_{\displaystyle n_0,n_1,\ldots,n_{m-1} \vphantom{\prod}}
+ \underbrace{\log {N \choose n_0,n_1,\ldots,n_{m-1}} \vphantom{\prod_{\displaystyle i}}}
	_{\displaystyle\mathrm{String} \vphantom{\prod}}$$

This, in turn, assumes knowledge of parameters $N$ and $m$.

The length of the binary expansion of a natural number is on the order
of $\log_2 n$, but without the [prefix
property](https://en.wikipedia.org/wiki/Prefix_code), the length of such
an expansion (i.e. the cut-off point between the number's code and the
rest of the code) is unknowable by a decoder. To address this, a
[universal integer
code](https://en.wikipedia.org/wiki/Universal_code_(data_compression))
(e.g. [Elias](https://en.wikipedia.org/wiki/Elias_delta_coding)) is used
taking at most $2\log_2 n$ bits:

$$\underbrace{2\log m\vphantom{\prod_{\displaystyle i}}}
	_{\displaystyle m \vphantom{\prod}}
+ \underbrace{2\log N\vphantom{\prod_{\displaystyle i}}}
	_{\displaystyle N \vphantom{\prod}}
+ \underbrace{\log {N + m - 1 \choose m - 1} \vphantom{\prod_{\displaystyle i}}}
	_{\displaystyle n_0,n_1,\ldots,n_{m-1} \vphantom{\prod}}
+ \underbrace{\log {N \choose n_0,n_1,\ldots,n_{m-1}} \vphantom{\prod_{\displaystyle i}}}
	_{\displaystyle\mathrm{String} \vphantom{\prod}}$$

which only leaves the dictionary of constructions to be encoded so that
constructed symbols may be properly interpreted.

### Inductive Constructions

We want the dictionary (alphabet) of constructions (symbols) to grow
inductively, by appending *rules* consisting of two previously defined
symbols (either atomic or constructed) based on an assumption that *a
whole can only be statistically significant if both of its parts also
are*.

By then substituting instances where the parts appear together in the
string with the new "joint" symbol after each introduction, the
resulting exploration into the space of
[$n$-grams](https://en.wikipedia.org/wiki/Word_n-gram_language_model)
will remain *sparse*, avoiding the [combinatorial
explosion](https://en.wikipedia.org/wiki/Curse_of_dimensionality)
associated with assigning parameters to each possible combination of
symbols and the requirement to decide on a predetermined model size.

We start with an alphabet of the 256 bytes. The first construction rule
is therefore one among

$$256 \times 256$$

possible pairs. For the second rule introduction, the set of possible
pairs is one larger:

$$256 \times 256 \times 257 \times 257 \times \ldots$$

In general, for $m$ symbols, this has variety

$$\left(\frac{(m-1)!}{255!}\right)^2$$

which is information

$$\begin{align}
\log\left(\left(\frac{(m-1)!}{255!}\right)^2\right)
= 2\log\left(\frac{(m-1)!}{255!}\right).
\end{align}$$

We can also get away with encoding $m - 256$ instead of $m$ since it
will always be the case that $m \geq 256$. The information content of
our entire encoding is therefore:

$$\begin{align}
I(m, \mathrm{\bf r}, N, \mathrm{\bf n}, \mathrm{\bf s})
=~&~ \underbrace{2\log (m-256)\vphantom{\prod_{\displaystyle i}}}
	_{\displaystyle m \vphantom{\prod}}
+ \underbrace{2\log \left(\frac{(m-1)!}{255!}\right)\vphantom{\prod_{\displaystyle i}}}
	_{\displaystyle\mathrm{{\bf r}: Rules} \vphantom{\prod}}
+ \underbrace{2\log N\vphantom{\prod_{\displaystyle i}}}
	_{\displaystyle N \vphantom{\prod}}\\[10pt]
&+ \underbrace{\log {N + m - 1 \choose m - 1} \vphantom{\prod_{\displaystyle i}}}
	_{\displaystyle \mathrm{\bf n}: n_0,n_1,\ldots,n_{m-1} \vphantom{\prod}}
+ \underbrace{\log {N \choose n_0,n_1,\ldots,n_{m-1}} \vphantom{\prod_{\displaystyle i}}}
	_{\displaystyle\mathrm{{\bf s}: String} \vphantom{\prod}}
\end{align}$$

which is only a function of dictionary size $m$ and counts vector
$\mathrm{\bf n}: n_0, n_1, ..., n_{m-1}$.

## Loss Function

Evaluating the above formula in whole for each possible addition to the
model to determine the optimal next step is logically what we want to
achieve but computationally excessive.

First, the terms for the encoding of $m$ (dictionary size) and
$\mathrm{\bf r}$ (dictionary) are constant in information regardless of
*which* symbols are chosen for the next construction. While they
contribute in determining when the program stops (when the total length
stops decreasing), they can be dropped when sorting candidate
joints. Same goes for the encoding of $N$ which is mostly constant
between introductions and at worse only a few bits in size at specific
steps between powers of two, which at most one will be crossed as the
maximum change $N$ can take is to get halved if every pair gets
constructed into a new symbol.

For the introduction of a joint symbol with count $n_{01}$, constructed
from symbols with counts $(n_0,n_1)$, the main terms that affect the
total information are the term for the counts:

$$\begin{align}
\Delta I(\mathrm{\bf n})
&= \log {N - n_{01} + m \choose m} - \log {N + m - 1 \choose m - 1}\\[5pt]
&= \log \left(\frac{(N - n_{01} + m)!}{m!\,(N - n_{01})!}\right)
	- \log \left(\frac{(N + m - 1)!}{(m - 1)!\,N!}\right) \\[5pt]
&= \log \left(\frac{(N - n_{01} + m)!\,(m - 1)!\,N!}
	{(N + m - 1)!\,m!\,(N - n_{01})!}\right) \\[5pt]
&= \log \left(\frac{(N - n_{01} + m)!\,N!}
	{(N + m - 1)!\,m\,(N - n_{01})!}\right) \\[5pt]
\end{align}$$

<!-- The equivalent expansion of the multinomial coefficient for the length -->
<!-- of the multiset permutation has on the order of $m$ terms but most -->
<!-- cancel out leaving only those implicated in the rule $(s_0, s_1) \to -->
<!-- s_{01}$: -->

and the term for the ordering of the string:

$$\begin{align}
\Delta I(\mathrm{\bf s})
&= \log {N - n_{01} \choose n_0 - n_{01}, n_1 - n_{01},\ldots,n_{m-1}, n_{01}}
	- \log {N \choose n_0,n_1,\ldots,n_{m-1}}\\[5pt]
&= \begin{cases}
	\log\left(\displaystyle \frac{(N - n_{01})!\,n_0!}
	{N!\,(n_0 - 2n_{01})!\,n_{01}!} \right)
	& \text{when } s_0 = s_1 \\[5pt]
	\log\left(\displaystyle \frac{(N - n_{01})!\,n_0!\,n_1!}
	{N!\,(n_0 - n_{01})!\,(n_1 - n_{01})!\,n_{01}!} \right)
	& \text{when } s_0 \neq s_1
	\end{cases} \\[5pt]
&= \log \left( \frac{(N - n_{01})!\,n_0!}{N!\,n_{01}!} \right) + \begin{cases}
	\log \left(\frac{\displaystyle 1}{\displaystyle (n_0 - 2n_{01})!} \right)
	& \text{when } s_0 = s_1 \\
	\log \left(\frac{\displaystyle n_1!}
	{\displaystyle (n_0 - n_{01})!\,(n_1 - n_{01})!} \right)
	& \text{when } s_0 \neq s_1
	\end{cases}
\end{align}$$

$$$$

Together, some additional terms/factors cancel out:

$$\begin{align}
\Delta I(\mathrm{\bf n},\mathrm{\bf s})
&= \log \left(\frac{(N - n_{01} + m)!\,N!}
	{(N + m - 1)!\,m\,(N - n_{01})!}\right)
	+ \log \left( \frac{(N - n_{01})!\,n_0!}{N!\,n_{01}!} \right) \\[5pt]
& ~~~~ + \begin{cases}
	\log \left(\frac{\displaystyle 1}{\displaystyle (n_0 - 2n_{01})!} \right)
	& \text{when } s_0 = s_1 \\
	\log \left(\frac{\displaystyle n_1!}
	{\displaystyle (n_0 - n_{01})!\,(n_1 - n_{01})!} \right)
	& \text{when } s_0 \neq s_1
	\end{cases} \\[5pt]
&= \log \left(\frac{(N - n_{01} + m)!\,n_0!}
	{(N + m - 1)!\,m\,n_{01}!} \right) + \begin{cases}
	\log \left(\frac{\displaystyle 1}{\displaystyle (n_0 - 2n_{01})!} \right)
	& \text{when } s_0 = s_1 \\
	\log \left(\frac{\displaystyle n_1!}
	{\displaystyle (n_0 - n_{01})!\,(n_1 - n_{01})!} \right)
	& \text{when } s_0 \neq s_1
	\end{cases}
\end{align}$$

which, by [logarithmic
identity](https://en.wikipedia.org/wiki/List_of_logarithmic_identities#Using_simpler_operations),
can be computed in an efficient and numerically stable way with an
implementation of the
[log-factorial](https://hackage.haskell.org/package/math-functions-0.3.4.4/docs/Numeric-SpecFunctions.html#v:logFactorial)
using the expansion

$$\log((N - n_{01} + m)!) + \log(n_0!) - \log((N + m - 1)!)~-~...$$

$$$$

Finding the pair of symbols with joint count $n_{01}$ and individual
counts $(n_0,n_1)$ which minimize this function at each step in the
evolution of the dictionary is sufficient to find the rule to introduce
which locally maximizes the compressibility of the whole system.

As long as this loss---and $N$'s term---offset the increase in code length
from terms $\{m,\mathrm{\bf r}\}$ incurred by an introduction, we grow
the dictionary/rule set.

## Implementation

We implement a greedy algorithm that, given a text file, repeatedly
combines the pair of symbols that brings the greatest reduction in the
overall information content until no pair produces an overall loss below
zero. We log the introduced construction rules and information content
at each step.

The program is written in Haskell:

- [Source (GitHub)](https://github.com/nbos/diagram_)

### Optimizations

A number of additional optimizations are required to run the program to
completion on inputs of significant length.

#### Counts Bookkeeping

As most of the symbol counts $n_0,n_1,\ldots,n_{m-1}$ stay constant
between rule introductions, the count vector ($O(m)$ space) is preserved
between iterations and only those affected by the introduced rule (the
counts of the parts and the count of the new symbol) get modified. This
can be achieved in $O(1)$ time with a dynamic array.

#### Joint Counts Bookkeeping

Similarly, the joint counts (number of times each pair of symbols appear
together) vary only slightly between iterations. The whole joint count
map ($O(m^2)$ space) is therefore preserved as each is a potential
candidate for the next rule.

When rewriting the string with the introduced rule $(s_0,s_1) \mapsto
s_{01}$, symbols occuring immediately before $s_0$ and immediately after
$s_1$ are subject to have their joint count with the respective part
decremented and the joint count with $s_{01}$ incremented.

<!-- At worst, this has the same complexity as re-counting all joint counts, -->
<!-- but in practice where  -->

#### Joint Positions Bookkeeping

At this point, the program runs to completion in a reasonable amount of
time on strings of thousands (KB) up to a million (1 MB) symbols. The
operation taking by far most of the run time on large strings is the
$O(N)$ pass over the input required to

1) re-write joint occurences of $(s_0,s_1)$ into $s_{01}$ and

2) update joint counts wherever they appear around sites where $s_{01}$
gets written

This is especially problematic after the first few symbols have been
introduced and joint counts in natural text fall sharply following a
[Pareto](https://en.wikipedia.org/wiki/Pareto_distribution)-like drop in
counts into a very long tail. The algorithm then spends most of its time
performing $O(N)$ scans of the working string looking for the few
locations where the joint it has already decided to introduce appears.

The obvious alternative to this is to index every possible construction
site by their joint and store the string in a structure so that given a
joint, each site can be read an rewritten in $O(1)$ time.

Because rewriting turns two symbols into one symbol, a simple vector
cannot support arbitrary amounts of random rewrites while guaranteeing
constant-time access to the left and right positions of random
construction sites. Introducing a layer of indirection on the positions
in the string (essentially implementing a doubly-linked-list) we can
ensure those complexity bounds with only a $O(N)$ increase in space
requirements.

This incurs significant overhead at the begining of the execution of the
program, but pays for itself many times over in the tail of the
execution.

## Results

The dataset of choice is [a 2006 snapshot of the English
Wikipedia](https://mattmahoney.net/dc/textdata.html) (XML format),
following the example of the [Large Text Compression
Benchmark](https://mattmahoney.net/dc/textdata.html) and
[Hutter](http://prize.hutter1.net/).

The first $10^9$ bytes of the snapshot are referred to as
`enwik9`. Truncations of smaller magnitudes are named accordingly:

![](res/chunk/datasets.svg)

$$$$

We show the total and relative share in code length (information) of the
different components (terms) of the encoding (formula). The
contributions of the encodings of parameters $m$ and $N$ are too small
to be noticeable. The size of the encoding at $m=256$ (starting state)
is indicated with a marker on the Y axis:

![](res/chunk/stacked/enwik4.svg)

![](res/chunk/stacked/enwik5.svg)

![](res/chunk/stacked/enwik6.svg)

![](res/chunk/stacked/enwik7.svg)

As one would expect the bulk of the gain in compressability occur with
the first few introduced symbols and tapers out as they produce fewer
modifications to the string.

We also notice that greater compressability is achieved with greater
inputs (and larger dictionaries).

We can measure the compressability, or "information density" resulting
from the encoding at each point in the model's evolution as a
compression *factor*

$$\text{compression factor} = \frac{\text{original size}}{\text{compressed size}}$$

A compression factor slightly above 1.5 is achieved across the board
with an empty dictionary simply by virtue of the encoding. For a given
number of symbols, greater factors are achieved by smaller sets,
probably due to the reduced variance of the data, but larger inputs
produce gerater factors in the long run. The X axis is displayed in
log-scale:

![](res/chunk/self.svg)

<!-- ![](res/chunk/enwik7.svg) -->

The exponentially increasing size of the input translates to
exponentially increasing achieved dictionary sizes (and running time)
and *linearly* increasing compression factors.
