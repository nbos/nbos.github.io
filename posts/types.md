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
the joint count $n_{01}$ is sufficiently larger than what could be
expected by independence.

### Information of Union Introduction

Because of the way permutations (and their
[coefficients](https://en.wikipedia.org/wiki/Multinomial_theorem#Multinomial_coefficients))
compose, changing a categorical distribution by re-classifying symbols,
then adding the appropriate codes to resolve the ambiguity inside those
unions always has an *insignificant* effect on the total code length.

Say, for whichever reason, we place $\ell$ symbols under a union
$S_{0\ell} = \{s_0,s_1,...,s_{\ell-1}\}$ with a cumulative count
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
clusters of symbols in the alphabet:

![](res/types/figs/subset.svg)

we instead look for clusters in pairs---a "joint-of-unions"---such that
they have a relatively high number of joints between them.

![](res/types/figs/bipartite.svg)

As a [graph](https://en.wikipedia.org/wiki/Graph_theory), the set of
joint occurrences of symbols in a string form edges in a [bipartite
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
<!-- traditional limitations of the [$n$-gram -->
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
"tightness" is violated, but experiments will show this is not
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

For a given string, we collect all joint occurrences of pairs of symbols,
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
invariant progressively through a sufficiently randomized selection,
hoping that no bias arises.

By randomly selecting a side, a symbol in that side and a selection for
that symbol (included/excluded), then propagating the choice by
eliminating possible symbols that would be made dangling if later
selected or introducing a random symbol among its neighbors if none are
so far selected, we achieve a uniform-looking, 100% "tight" random type
generation.

## Code Length Formulation

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

Cumulatively for a dictionary of $m$ total symbols, that came out to

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

To score different type introductions against each other, we formulate
the difference in information produced by a single introduction.

First, we ignore the length of the encodings of hyperparameter $m$ and
$\mathrm{Types}$ definitions which are constant regardless of which new
joint type is introduced. Similarly, changes in the length of $N$ are
minimal and occur at intervals of powers of two regardless of the chosen
type structure so we also ignore it for the scoring of types.

We are left with generalizing the difference in length of $\bf n$ and
$\bf s$ from the introduction of a joint

$$(t_0,t_1) \mapsto t_m$$

from [loss](chunk.html#loss-function):

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

to the introduction of a joint type

$$\forall (a,b)\!:\! A\!\times\! B. (a,b) \mapsto t_m$$

with loss:

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

As stated before, instead of evaluating the loss on all possible types,
we apply mutations to a randomly sampled type, again in a greedy
manner. For each type, there is a $O(m)$ sized set of valid mutations:
either adding or removing each symbol in the alphabet to each side's
union, unless they would produce isolated vertices, in which case pairs
are added or removed together.

We evaluate the difference in code length incurred by each mutation
individually, computing the difference in the difference in code length
on the whole serialization.

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

### Complexity Issues

Given the [heavy bookkeeping required to make the previous algorithm
usable](chunk.html#optimizations), we approach the implementation with
the intention of eliminating all operations equal or greater than $O(N)$
in complexity, if not for each introduction, then at least for each
mutation, and certainly when evaluating each mutation candidate.

Unfortunately, it looks like the expressiveness of joint types
introduces too many contextual dependencies to make this possible.

#### Counting Joints in a String

For a pair of symbols

$$(s_0,s_1)$$

and a string of length $N$, the loss associated with its introduction
according the the previous logic is a function of three values:
individual symbol counts $n_0$ and $n_1$, and the joint count
$n_{01}$. All of those parameters can be gathered in one $O(N)$ pass
over the string and only require $O(n_{01})$ updates per introduction.

When $s_0 \neq s_1$, all encountered joints count towards the joint
count:

![](res/types/figs/joints-neq.svg)

but when $s_0 = s_1$, there is the subtlety that consecutive occurrences
of the joint overlap:

![](res/types/figs/joints-eq.svg)

and every other pair must be skipped and not contribute towards the
joint count $n_{01}$. This is easily managed by keeping track of parity
when encountering spans of identical symbols while scanning the string
and discarding joints that are not "constructible".

#### Counting Joints of a Type in a String

For a joint type

$$A \times B$$

and a string of length $N$, the loss associated with its introduction is
a function of the total joint count $n_m$ and as many individual symbols
counts as there are symbols in both unions:

$$\Delta{\bf n} = \{~n_i ~|~ s_i \in A \cup B~\}.$$

Since every new type will produce distinct overlap events along the
string, a single pass over the string is no longer sufficient to gather
a record of those values.

Still, assuming we do a $O(N)$ pass over the string for each newly
sampled type, we must find a way to efficiently compute or bookkeep the
differences in $\Delta{\bf n}$ produced by each candidate mutation after
each mutation application.

This becomes problematic when chains or arbitrary length of overlapping
joints

![](res/types/figs/joints-eq.svg)

can have their parity flipped by the introduction of a new symbol in the
left union

![](res/types/figs/joints-mut.svg)

which either increments or decrements the individual count of the last
symbol in the chain. This, by itself is still manageable if we keep
track of all joint sites, constructive or not, for all joints in binary
trees that merge and resolve collisions in less than $O(N)$, keeping
pointers between head and tail of each such segment, etc. But deletions
of joints in the middle of a segment now requires each joint to also
point to either the head or the tail, which would require on the order
of $O(n_m)$ updates at each mutation application, etc.

The simple fact that our permutation format has to deal with the
introduction of non-overlapping chunks, while candidates for chunking
*are* overlapping, creates a tension that keeps this simple extension
from progressing further.

We therefore swap the underlying combinatorial model for something more
appropriate.

## Reformulation

While a multiset permutation model can be simplified by reducing the
size of the string through the introduction of ever larger
(non-overlapping) chunks:

![](res/types/figs/joints-neq.svg)

we now aim for a structure of known variety that can be restricted by
accumulating links between states, or conditional transitions:

![](res/types/figs/joints-eq.svg)

From a vector of counts:

![](res/chunk/figs/counts-fig.svg)

that we resolve into a string though the encoding of a permutation:

![](res/chunk/figs/permutation.svg)

we instead start from a directed multigraph:

![](res/types/figs/graph-cat.svg)

where paths from and to $\varepsilon$
([epsilon](https://en.wikipedia.org/wiki/Empty_string)) represent
unchunked or unconditional symbols. We can resolve the graph into a
string though the ranking/unranking of a [Eulerian
path](https://en.wikipedia.org/wiki/Eulerian_path):

![](res/types/figs/graph-path.svg)

![](res/chunk/figs/string-fig.svg)

It so happens that counting Eulerian paths in a directed graph is only
as complex as computing a
[determinant](https://en.wikipedia.org/wiki/Determinant) on its
[Laplacian matrix](https://en.wikipedia.org/wiki/Laplacian_matrix)
(specifically, a
[cofactor](https://en.wikipedia.org/wiki/Minor_(linear_algebra))) which
has complexity $O(n^3)$ for a graph of $n$ vertices or, for our case,
$O(m^3)$ for an alphabet of $m$ symbols.

The Laplacian matrix records the number of connections between vertices
in a graph (in negative) with the degree of vertices on the diagonal.

In our example, we would have:

<!-- $$L = \begin{pmatrix} -->
<!--  7 & -1 & -3 & -2 & -1 \\ -->
<!-- -1 &  1 &  0 &  0 &  0 \\ -->
<!-- -3 &  0 &  3 &  0 &  0 \\ -->
<!-- -2 &  0 &  0 &  2 &  0 \\ -->
<!-- -1 &  0 &  0 &  0 &  1 -->
<!-- \end{pmatrix}$$ -->

<!-- $$\begin{array}{cccccc} -->
<!--       & \varepsilon & a  & b  & c  & d  \\ -->
<!-- \varepsilon &  7 & -1 & -3 & -2 & -1 \\ -->
<!-- a     & -1 &  1 &  0 &  0 &  0 \\ -->
<!-- b     & -3 &  0 &  3 &  0 &  0 \\ -->
<!-- c     & -2 &  0 &  0 &  2 &  0 \\ -->
<!-- d     & -1 &  0 &  0 &  0 &  1 -->
<!-- \end{array}$$ -->

$$
\begin{array}{c@{\hspace{4pt}}c}
 & \begin{array}{ccccc}
	\,~\varepsilon\,~ & \,~a\,~ & \,~b\,~ & \,~c\,~ & \,~d\,~
\end{array} \\[2pt]
\begin{array}{c}
	\varepsilon \\ a \\ b \\ c \\ d
\end{array}
&
\left(\begin{array}{rrrrr}
 7 & -1 & -3 & -2 & -1 \\
-1 &  1 &  0 &  0 &  0 \\
-3 &  0 &  3 &  0 &  0 \\
-2 &  0 &  0 &  2 &  0 \\
-1 &  0 &  0 &  0 &  1
\end{array}\right)
\end{array}
$$

or for any such graph of a multiset:

$$L_{\bf n} ~=~~~
\begin{array}{c@{\hspace{4pt}}c}
 & \begin{array}{ccccc}
	~~\varepsilon~~~~~ & ~~s_0~~ & ~~s_1~~ & \cdots & ~~s_{m-1}
\end{array} \\[2pt]
\begin{array}{c}
	\varepsilon \\ s_0 \\ s_1 \\ \vdots \\ s_{m-1}
\end{array}
&
\left(\begin{array}{ccccc}
 N & -n_0 & -n_1 & \cdots & -n_{m-1} \\
-n_0 &  n_0 &  0 & \cdots &  0 \\
-n_1 &  0 &  n_1 & \cdots &  0 \\
\vdots & \vdots & \vdots & \ddots &  0 \\
-n_{m-1} &  0 &  0 &  0 &  n_{m-1}
\end{array}\right)
\end{array}
$$

where

$$N = \sum_{i=0}^{m-1} n_i.$$

The number of
[arborescences](https://en.wikipedia.org/wiki/Arborescence_(graph_theory)),
a.k.a. directed rooted trees on a graph is equal to the determinant of
*any* cofactor of the Laplacian, which is equal to the determinant of
any submatrix resulting from the deletion any one row and any one
column. Arbitrarily, we delete the firsts:

$$t_{\bf n} = \mathrm{det}\begin{pmatrix}
n_0 &  0 & \cdots &  0 \\
0 &  n_1 & \cdots &  0 \\
\vdots & \vdots & \ddots &  0 \\
0 &  0 &  0 &  n_{m-1}
\end{pmatrix} = \prod_{i=0}^{m-1} n_i.
$$

Then, the number of (unrooted) Eulerian circuits (a.k.a. cycle, tour,
etc.) is given by the [BEST
theorem](https://en.wikipedia.org/wiki/BEST_theorem) as the product of
the number of arborescences and the factorial of each vertex's degree
minus one:

$$ec = t \cdot \prod_{v\in V}\left(\mathrm{deg}(v)-1\right)!$$

For the graph of a multiset we get

$$\begin{align}ec_{\bf n}
&= t_{\bf n} \cdot \prod_{i=0}^m\left(L_{ii}-1\right)!\\
&= \prod_{i=0}^{m-1} n_i \cdot (N-1)! \cdot \prod_{i=0}^{m-1}\left(n_i-1\right)!\\
&= (N-1)! \cdot \prod_{i=0}^{m-1}n_i!
\end{align}$$

which does not correspond to the number of strings we can produce from
the vector of counts of a multiset, which is given by the multinomial
coefficient:

$${N \choose n_0,n_1,\ldots,n_{m-1}}
~=~ \frac{N!}{\prod_{i=0}^{m-1}n_i!}
~\neq~ (N-1)! \cdot \prod_{i=0}^{m-1}n_i!.$$

Inpecting the structure of a Eulerian circuit:

![](res/types/figs/graph-path.svg)

it is defined as a circular sequence of edges, while a string is defined
as a sequence of characters, which here correspond to vertices. For each
pair of vertices $u$ and $v$ all edges $u \to v$ and all edges $v \to u$
form equivalence classes that are overcounted in the computation of the
BEST theorem. Because there are $n_i$ incoming and outgoing edges for
each symbol, we get

$$\begin{align}\frac{N!}{\prod_{i=0}^{m-1}n_i!}
&\neq \frac{(N-1)! \cdot \prod_{i=0}^{m-1}n_i!}{\prod_{i=0}^{m-1}(n_i!)^2}\\[10pt]
&\neq \frac{(N-1)!}{\prod_{i=0}^{m-1}n_i!}.
\end{align}$$

Finally, the BEST theorem counts *unrooted* circuits, while our string
has a definite begining and end. This means it undercounts the number of
strings by a factor of $N$, or the number of places a circuit can be
"cut" into a string.

$$\begin{align}\frac{N!}{\prod_{i=0}^{m-1}n_i!}
&= \frac{(N-1)!}{\prod_{i=0}^{m-1}n_i!} \cdot N \\[5pt]
&= \frac{N!}{\prod_{i=0}^{m-1}n_i!}.
\end{align}$$

With this, we can produce a formula for the number of distinct strings
produceable from such Eulerian graphs.

In general, the Laplacian sub-matrix is not diagonal

<!-- ![](res/types/figs/graph-joints.svg) -->
