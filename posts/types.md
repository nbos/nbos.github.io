---
title: "Chunking with Types"
author: Nathaniel Bos
date: 2026-02-17
---

In a [previous post](chunk.html), we showed that inductively growing a
simple
[categorical](https://en.wikipedia.org/wiki/Categorical_distribution)
text model using a greedy policy of maximal compression lead to a
meaningful-looking
[constructions](https://en.wikipedia.org/wiki/Chunking_(psychology))
from the training data.

To bring the model closer to being capable of generating some kind of
[grammar](https://en.wikipedia.org/wiki/Formal_grammar) by itself, [as
humans do](https://en.wikipedia.org/wiki/Language_acquisition), we
[complete the logic](https://en.wikipedia.org/wiki/Algebraic_data_type)
with the dual of chunking:
[categorization](https://en.wikipedia.org/wiki/Cognitive_categorization).

While chunking was formalized as *joint* symbols---a concatenation of
consecutive symbols---categories are formalized as [sets of possible
symbols](https://en.wikipedia.org/wiki/Enumerated_type) we refer to as
*unions*.

## Union Semantics

Unlike chunking, adding classes to a categorical model doesn't
immediately improve its predictive power.

### Information of Joint Introduction

*(this is a recap of the math supporting [the previous
post](chunk.html#format-description))*

Encoding a categorical model of counts
([multiplicities](https://en.wikipedia.org/wiki/Multiplicity_(mathematics)))
$n_0, n_1, ... n_{m-1}$ and instantiating it into a string of symbols
matching those counts takes information approximately[^1] equal to:

[^1]: Ignoring the encoding of hyperparameters: number of defined joints
	$m$ and length of string (sum of counts) $N$. Previous post has full
	formulas.

$$\underbrace{\log {N + m - 1 \choose m - 1} \vphantom{\prod_{\displaystyle i}}}
	_{\displaystyle n_0,n_1,\ldots,n_{m-1} \vphantom{\prod}}
+ \underbrace{\log {N \choose n_0,n_1,\ldots,n_{m-1}} \vphantom{\prod_{\displaystyle i}}}
	_{\displaystyle\mathrm{String} \vphantom{\prod}}
~~~\text{where } N = \sum_i n_i.$$

Introducing a new joint symbol means docking the joint count $n_{01}$
from individual symbol counts $n_0$ and $n_1$ and appending count
$n_{01}$ as a new "individual" count [resulting
in](chunk.html#loss-function) an increase in the description length of
the counts vector:

$$\begin{align}
\Delta I^*_{\mathrm{\bf n}}
&= \log {N + m - n_{01} \choose m} - \log {N + m - 1 \choose m - 1}\\[5pt]
&= \log \left(\frac{(N + m - n_{01})!\,N!}
	{m\,(N + m - 1)!\,(N - n_{01})!}\right) \\[5pt]
\end{align}$$

and a decrease in the length of the encoding of the string (as a ranked
[permutation](https://en.wikipedia.org/wiki/Permutation#Permutations_of_multisets)):

$$\begin{align}
\Delta I^*_\mathrm{\bf s}
&= \log {N - n_{01} \choose n_0 - n_{01}, n_1 - n_{01},\ldots,n_{m-1}, n_{01}}
	- \log {N \choose n_0,n_1,\ldots,n_{m-1}}\\[5pt]
&= \log \left( \frac{(N - n_{01})!\,n_0!}{N!\,n_{01}!} \right) + \begin{cases}
	\log \left(\frac{\displaystyle 1}{\displaystyle (n_0 - 2n_{01})!} \right)
	& \text{when } s_0 = s_1 \\
	\log \left(\frac{\displaystyle n_1!}
	{\displaystyle (n_0 - n_{01})!\,(n_1 - n_{01})!} \right)
	& \text{when } s_0 \neq s_1
	\end{cases}
\end{align}$$

which, together, are negative (i.e. reduce total information) only if
the joint count $n_{01}$ is sufficently larger than what could be
expected by independence.

### Information of Union Introduction

Because of the way permutations (and their
[coefficients](https://en.wikipedia.org/wiki/Multinomial_theorem#Multinomial_coefficients))
compose, changing a categorical distribution by re-classifying symbols,
then adding the appropriate codes to resolve the ambiguity inside those
unions always has an *insignificant* effect on the total code length.

Say, for whichever reason, we place $\ell$ symbols under a union
$S_{0\ell} = \{s_0,s_1,...,s_{\ell-1}\}$ with a cummulative count
$n_{0\ell}$ where

$$n_{0\ell} = \sum_{i=0}^{\ell-1}n_i~~,$$

moving their counts to a secondary counts vector produces a combined
description length:

$$\begin{align}
I^*_{\mathrm{\bf n}}
&= \log {N - n_{0\ell} + m - \ell - 1 \choose m - \ell - 1}
	+ \log {n_{0\ell} + \ell - 1 \choose \ell - 1}\\[5pt]
&= \log \left( {N - n_{0\ell} + m - \ell - 1 \choose m - \ell - 1}
	{n_{0\ell} + \ell - 1 \choose \ell - 1} \right).
\end{align}$$

Similarly, removing those counts from the encoding of the permutation
and adding them to a disambiguating permutation separates the
coefficient:

$$\begin{align}
I^*_{\mathrm{\bf s}}
&= \log {N - n_{0\ell}\choose n_\ell,n_{\ell+1},\ldots,n_{m-1}}
	+ \log {n_{0\ell}\choose n_0,n_1,\ldots,n_{\ell-1}} \\[5pt]
&= \log \left( {N - n_{0\ell}\choose n_\ell,n_{\ell+1},\ldots,n_{m-1}}
	{n_{0\ell}\choose n_0,n_1,\ldots,n_{\ell-1}} \right),
\end{align}$$

which, by [Vandermonde's
identity](https://en.wikipedia.org/wiki/Vandermonde%27s_identity)

$${m+n \choose r}=\sum _{k=0}^{r}{m \choose k}{n \choose r-k},$$

are approximately equal to the initial code length.

Respectively,

$$\begin{align}
I^*_{\mathrm{\bf n}}
&= \log \left( {N - n_{0\ell} + m - \ell - 1 \choose m - \ell - 1}
	{n_{0\ell} + \ell - 1 \choose \ell - 1} \right)\\[5pt]
&= \log {N+m \choose m},
\end{align}$$

and

$$\begin{align}
I^*_{\mathrm{\bf s}}
&= \log \left( {N - n_{0\ell}\choose n_\ell,n_{\ell+1},\ldots,n_{m-1}}
	{n_{0\ell}\choose n_0,n_1,\ldots,n_{\ell-1}} \right)\\[5pt]
&= \log {N \choose n_0,n_1,\ldots,n_{m-1}},
\end{align}$$

match the initial code lengths once an additional $O(\log m)$ is
accounted for which happens to correspond to the length a code would
take to specify which one of the $m$ symbols is a now a union.

The bottom line is that in combinatorics, permutations are constant
under hierarchical decompositions which, for our application, indicates
that categories by themselves don't offer value regarding the
compression of the data.

### Joints of Unions

The way to make unions do work for us is to put them into joints.
<!-- Categorization is only useful in the context of chunking. -->

Where the previously described structure of categorization produces
clusers of symbols in the alphabet:

![](res/types/figs/subset.svg)

we instead look for clusters in pairs---a "joint-of-unions"---such that
they have a relatively high number of joints between them.

![](res/types/figs/bipartite.svg)

As a [graph](https://en.wikipedia.org/wiki/Graph_theory), the set of
joint occurences of symbols in a string form edges in a [bipartite
graph](https://en.wikipedia.org/wiki/Bipartite_graph) connecting left
and right symbols. Then, we are looking for a subgraph with high
connectivity.

Logically, this is a [type](https://en.wikipedia.org/wiki/Type_theory)
of joint, specifically a [product
type](https://en.wikipedia.org/wiki/Product_type) containing the set of
joints corresponding to the [Cartesian
product](https://en.wikipedia.org/wiki/Cartesian_product) of the left
and right union types:

$$\frac{s_0 \in A ~~~~ s_1 \in B}{(s_0,s_1) \in A \times B}.$$

Loosely speaking, the unions still don't work themselves to reduce the
size of codes---it's only that through the introduction of the joint,
unions will allow joint types to be defined between groups of symbols
which, individually, would lack the numbers to justify the introduction
of a construction rule for all the individual joints that the type
covers.

<!-- This is another way (together with [the sparsity of a -->
<!-- dictionary](chunk.html#inductive-constructions)) that we overcome the -->
<!-- taditional limitations of the [$n$-gram -->
<!-- model](https://en.wikipedia.org/wiki/Word_n-gram_language_model) -->
<!-- regarding exponentially large datasets required to support words of -->
<!-- increasing size. -->

## Strategy

Unlike the enumeration of all joints which is a $O(n^2)$ operation, the
enumeration of [all pairs of subsets of two
sets](https://en.wikipedia.org/wiki/Power_set) is exponential at
$O(2^{2n})$ and computationally infeasible for all but very small
alphabets.

We therefore skip the description of an exact greedy algorithm and
instead call upon classical optimization techniques like [hill
climbing](https://en.wikipedia.org/wiki/Hill_climbing) or
[EM](https://en.wikipedia.org/wiki/Expectation%E2%80%93maximization_algorithm),
with a fixed number of random initializations to produce clusters in a
top-down fashion.

To do so, we need to (1) generate random types and (2) mutate those
types greedily until (local) maxima are reached.

### Random Initialization

#### The Problem

Although the
[inclusion/exclusion](https://en.wikipedia.org/wiki/Indicator_function)
of all available symbols for a right and left union form a simple
[Hamming](https://en.wikipedia.org/wiki/Hamming_space) basis in the
space of possible types, e.g.

```
+---------- A ----------+
|                       |
{s0, s1, s2, s3, s4, ...} × {s0, s1, s2, s3, s4, ...},
 0   0   0   0   1   ...     1   1   0   0   1   ...
                            |                       |
                            +---------- B ----------+
```

randomly sampling this space and then counting the affected joints by:

$$\frac{s_i \in A ~~~~ s_j \in B}{(s_i,s_j) \in A \times B}$$

won't always produce types that are optimal w.r.t. the joints they
cover---in the sense that some symbols included in one side's union type
could have no joint with any of the symbols of the opposite union.

On the bipartite graph, this would correspond to unconnected vertices
inside the subgraph of the joint type (but connected outside of
it). Informationally, the introduction could immediately be improved by
dropping them from the type, thereby reducing the length of the code
required to resolve the unions back into concrete symbols.

Fixing the randomly generated types by dropping unconnected symbols as a
normalization step will produce a biased generation pointing away from
the inclusion of any symbol with few associated joints. Similarly,
enforcing "tightness" by including random neighbors for every dangling
vertex, will be biased in the opposite direction.

For a truly uniform sampling of the space, we could re-sample every time
"tightness" is violated, but eperiments will show this is not
reliable. We [try this
method](https://github.com/nbos/diagram/tree/8424b2182aad989f4da9e9143c070d8c4327b7a4)
on our dataset of choice: [a 2006 snapshot of the English
Wikipedia](https://mattmahoney.net/dc/textdata.html) (XML format)
truncated at different magnitudes from the start:

```
 Filename   Size (N)   Size (bytes)

  enwik1      10^1        10 B
  enwik2      10^2       100 B
  enwik3      10^3         1 KB
  enwik4      10^4        10 KB
  enwik5      10^5       100 KB
   ...        ...          ...
```

For a given string, we collect all joint occurences of pairs of symbols,
form [top](https://en.wikipedia.org/wiki/Any_type) left and right
unions, randomly select symbols with a `0.5` probability in each union
and check whether the "tightness" property holds.

We repeat this at least $10^6$ times for each dataset.

The *unlikelihood* of generating "tightness" by accident is
immediately noticeable on a large enough dataset:

```
 Filename    P_tight (%)

   enwik1    1.56208 %
   enwik2    0.00157 %
   enwik3    0.06044 %
   enwik4    0.54991 %
   enwik5    0.00000 %
```

Intuitively: for a sufficiently large and diverse dataset, there are
enough symbols which form isolated joints (two symbols that only appear
in one joint together) that a random assignment either selects both or
neither (the only arrangements that don't break tightness) *in every
case* becomes exponentially close to 0.

#### Tightness in a Graph

We can call a bipartite graph "tight" if it contains no isolated vertex,
i.e. no vertex without an edge connecting it to an opposing vertex.

Since the left and right unions of all joints in a string contain only
symbols that have at least one joint with another symbol in the string,
the [top](https://en.wikipedia.org/wiki/Any_type) joint type is "tight".

The number of subgraphs in a "tight" graph that are also "tight" is the
number of ways, for each subset of one of the unions, to select at least
one vertex neighboring each vertex in that subset. In other words, given
a subset of either the left or right union, the number of possible
accompanying subsets on the opposite side is the number of ways to
select at least one vertex for each set of neighbors of vertices in the
given subset.

This reduces to the problem of counting [hitting
sets](https://en.wikipedia.org/wiki/Set_cover_problem#Hitting_set_formulation),
(which is equivalent to counting [set
covers](https://en.wikipedia.org/wiki/Set_cover_problem)) for each
subset of one of the unions, which is
[intractable](https://en.wikipedia.org/wiki/%E2%99%AFP-complete) in
general. This means that sampling types by
[unranking](https://en.wikipedia.org/wiki/Combinatorial_number_system)
uniformly sampled integers is equally intractable.

The best option we have at this point is to enforce the tightness
invariant progressively through a sufficeintly randomized selection,
hoping that no bias arises.

By randomly selecting a side, a symbol in that side and a selection for
that symbol (included/excluded), then propagating the choice by
eliminating possible symbols that would be made dangling if later
selected or introducing a random symbol among its neighbors if none are
so far selected, we achieve a uniform-looking, 100% "tight" random type
generation.

## Code Length Formula

To score different mutations of a randomly sampled joint type to
greedily select the best, we express the length of the whole encoding
before and after the introduction of a joint type and prior and after
each possible mutation of that joint type.

Starting from the format used in the joints-only logic (marked with a
${}^*$) as a sum of code lengths

$$\begin{align}
I^*(m,{\bf n})
\leq~&~ \underbrace{2\log (m-256)\vphantom{\prod_{\displaystyle i}}}
	_{\displaystyle m \vphantom{\prod}}
+ \underbrace{2\log \left(\frac{(m-1)!}{255!}\right)\vphantom{\prod_{\displaystyle i}}}
	_{\displaystyle\mathrm{Rules} \vphantom{\prod}}
+ \underbrace{2\log N\vphantom{\prod_{\displaystyle i}}}
	_{\displaystyle N \vphantom{\prod}}\\[10pt]
&+ \underbrace{\log {N + m - 1 \choose m - 1} \vphantom{\prod_{\displaystyle i}}}
	_{\displaystyle \mathrm{\bf n}: n_0,n_1,\ldots,n_{m-1} \vphantom{\prod}}
+ \underbrace{\log {N \choose n_0,n_1,\ldots,n_{m-1}} \vphantom{\prod_{\displaystyle i}}}
	_{\displaystyle\mathrm{{\bf s} : String} \vphantom{\prod}},
\end{align}$$

the $\mathrm{Rules}$ term is replaced with one defining the joint types
($\mathrm{Types}$) and a new term that counts the information required
to resolve each type back into joints is added ($\mathrm{Resolution}$).

### Types

For introducing individual joints in the previous logic (one left
symbol, one right symbol), two indexes with information $\log(m)$ were
sufficient to specify a construction rule, where $m$ is the number of
symbols before the introduction.

Cummulatively for a dictionary of $m$ total symbols, that came out to

$$\begin{align} I^*_{\bf r}(m)
&= \log\left(\prod_{i=256}^{m-1} i^2\right)\\
&= 2\log\left(\frac{(m-1)!}{255!}\right),
\end{align}$$

assuming we start with 256 atomic symbols (a stream of bytes).

To encode a joint type inductively, each symbol defined so far needs to
be potentially included or excluded, twice (once for each side), pushing
the information per introduction from $2\log(m)$ to $2m$. 

Still assuming 256 atomic symbols, we get a total

$$\begin{align}
I_{\bf t}(m)
&= 2 \cdot 256 + 2 \cdot 257 + \ldots + 2 \cdot (m - 1) \\[5pt]
&= 2 \left( \sum_{i=1}^{m-1}i - \sum_{i=1}^{255}i \right) \\[5pt]
&= 2 \left( \frac{(m-1)m}{2} - \frac{255 \cdot 256}{2} \right) \\[5pt]
&= (m-1)m - 255 \cdot 256\\[5pt]
&= \underbrace{m^2 - m - 65280 \vphantom{\prod}}
	_{\displaystyle\mathrm{Types} \vphantom{\prod}}
\end{align}$$

### Resolution

Given the final string of symbols, the definition of composite symbols
is no longer sufficient to expand them back into a string of atoms, as
symbols that are joint types now refer to sets of unspecified joints.

For each symbol $t_i$, its
[variety](https://en.wikipedia.org/wiki/Variety_(cybernetics))
is

$$v_i = V(t_i) = \begin{cases}
1 & \text{when }t_i\text{ atomic (fully determined)} \\
|A| \cdot |B| & \text{when }t_i\text{ joint type } A \times B
\end{cases}
$$

which, for joint types, is the number of joints under the type, i.e. the
product of the sizes of its unions.

For each symbol $t_i$, a code of length $$\log v_i$$ is required to
disambiguate each of the joints it is to instantiate into. The number of
instantiation is $n_i$ at the moment of introduction, but as subsequent
introductions create chunks out of this symbol, this count $n_i$ goes
down while the number of joints to disambiguates remain constant, as
disambiguations have to cascade through all parent definitions. We
therefore refer to the count at the time of introduction as $k_i$.

The sum of the lengths of all those codes is:

$$I_{\bf r}({\bf k}, {\bf v}) = \underbrace{\sum_i k_i \log v_i}
_{\displaystyle \mathrm{Resolution} \vphantom{\prod}}$$

and the vector of initial counts $\bf k$ can be reconstructed at
decode-time given the vector of counts $\bf n$ and the definition of
the types $\bf t$ by adding the counts of child symbols to their
parent symbols.

Together, we have the total length of the encoding:

$$\begin{align}
I(m,{\bf n},{\bf k},{\bf v})
\leq~&~ \underbrace{2\log (m-256)\vphantom{\prod_{\displaystyle i}}}
	_{\displaystyle m \vphantom{\prod}}
+ \underbrace{m^2 - m - 65280\vphantom{\prod_{\displaystyle i}}}
	_{\displaystyle\mathrm{{\bf t} : Types} \vphantom{\prod}}
+ \underbrace{2\log N\vphantom{\prod_{\displaystyle i}}}
	_{\displaystyle N \vphantom{\prod}}\\[10pt]
&+ \underbrace{\log {N + m - 1 \choose m - 1} \vphantom{\prod_{\displaystyle i}}}
	_{\displaystyle \mathrm{\bf n}: n_0,n_1,\ldots,n_{m-1} \vphantom{\prod}}
+ \underbrace{\log {N \choose n_0,n_1,\ldots,n_{m-1}} \vphantom{\prod_{\displaystyle i}}}
	_{\displaystyle\mathrm{{\bf s} : String} \vphantom{\prod}}\\[10pt]
&+ \underbrace{\sum_i^{m-1} k_i \log v_i \vphantom{\prod_{\displaystyle i}}}
	_{\displaystyle \mathrm{{\bf r} : Resolution} \vphantom{\prod}}
\end{align}$$

which by reducing in value through strategic type introduction will
result in compression.

### Difference Across Introductions

To score different joint type introductions against each other, we
formulate the difference in information produced by a single
introduction.

First, we ignore the length of the encodings of hyperparameter $m$ and
$\mathrm{Types}$ definitions which are constant regardless of which new
joint type is introduced. Changes in the length of $N$ are minimal and
occur at intervals of powers of two regardless of the chosen type
structure so we also ignore it for the scoring of types.

We are left with generalizing the difference in length of $\bf n$ and
$\bf s$ from the introduction of a joint $$(t_0,t_1) \mapsto t_m$$
[from](chunk.html#loss-function):

$$\begin{align}
\Delta I^*_{(\mathrm{\bf n},\mathrm{\bf s})}
&= \log \left(\frac
	{(N + m - n_m)!}
	{m\,(N + m - 1)!\,n_m!} \right)\\
&~~~~~ + \begin{cases}
	\log \left(\frac{\displaystyle n_0!}{\displaystyle (n_0 - 2n_m)!} \right)
	& \text{when } t_0 = t_1 \\
	\log \left(\frac{\displaystyle n_0!\,n_1!}
	{\displaystyle (n_0 - n_m)!\,(n_1 - n_m)!} \right)
	& \text{when } t_0 \neq t_1
\end{cases}
\end{align}$$

to the introduction

$$\forall (a,b)\!:\! A\!\times\! B. (a,b) \mapsto t_m$$

of a joint type $t_m$:

$$\Delta I_{(\mathrm{\bf n},\mathrm{\bf s})} =
\log \left(\frac
	{(N + m - n_m)!}
	{m\,(N + m - 1)!\,n_m!} \right)
+ \sum_i^{m-1} \log\left(\frac{n_i!}{n_i'!}\right)$$

where $n_i'$ is the updated entry in the counts vector $\bf n$ after the
type introduction, i.e. with however many times symbol $t_i$ appears as
either $a$ or $b$ (as $A$ and $B$ may intersect) subtracted from its
previous count $n_i$.

To this we add the difference in length of resolutions, which is only
the addition of resolutions for $t_m$:

$$\begin{align}\Delta I_{\bf r}
&= \sum_i^m k_i \log v_i - \sum_i^{m-1} k_i \log v_i\\
&= k_m \log v_m
\end{align}$$

which, at the time of introduction, is the same as

$$n_m \log v_m$$

<div id="loss-formula">
All together, we get:

$$\begin{align}
\Delta I_{(\mathrm{\bf n},\mathrm{\bf s},\mathrm{\bf r})} &=
\log \left(\frac
	{(N + m - n_m)!}
	{m\,(N + m - 1)!\,n_m!} \right)
+ \sum_i^{m-1} \log\left(\frac{n_i!}{n_i'!}\right)
+ n_m \log v_m.
\end{align}$$
</div>

### Difference Across Mutations

For a given type, we enumerate the ways to mutate it into a valid
neighboring type and evaluate the difference in code length incurred by
each mutation individually.

For this, we compute the difference in the information delta (a
difference of difference) using the [loss function from
above](#loss-formula):

$$\begin{align}
\Delta\Delta I_{(\mathrm{\bf n},\mathrm{\bf s},\mathrm{\bf r})} 
&= \Delta I_{(\mathrm{\bf n},\mathrm{\bf s},\mathrm{\bf r})}' 
	- \Delta I_{(\mathrm{\bf n},\mathrm{\bf s},\mathrm{\bf r})}\\[10pt]
&= \log \left(\frac{(N + m - n_m')!}{n_m'!} \right)
+ \sum_i^{m-1} \log\left(\frac{n_i!}{n_i''!}\right)
+ n_m' \log v_m' \\
&~~~~~ - \log \left(\frac{(N + m - n_m)!}{n_m!} \right)
- \sum_i^{m-1} \log\left(\frac{n_i!}{n_i'!}\right)
- n_m \log v_m\\[10pt]
&= \log \left(\frac{(N + m - n_m')!\,n_m!}{(N + m - n_m)!\,n_m'!} \right)
+ \sum_i^{m-1} \log\left(\frac{n_i'!}{n_i''!}\right)
+ n_m' \log v_m' - n_m \log v_m
\end{align}$$

where $n_i'$ and $n_i''$ are the updated symbol counts of a symbol $s_i$
by the introduction before and after the mutation,
respectively. Original counts $n_i$ (prior to the type introduction) of
affected symbols cancel out and so do every count for symbols that don't
have their counts affected ($\log\!\frac{n!}{n!} = 0$).

With this compact formula, mutations can be incrementally added in a
greedy fashion until a minimum/maximum is reached (i.e. [hill
climbing](https://en.wikipedia.org/wiki/Hill_climbing)) or, to
accelerate the process, all mutations with a negative loss can be added
at once (if we make sure to enumerate a set of entirely independent
mutations) and advance the state of the type to be introduced in
alternating phases of evaluation and selection
(i.e. [expectation-maximization](https://en.wikipedia.org/wiki/Expectation%E2%80%93maximization_algorithm))
until the minimum is reached.
