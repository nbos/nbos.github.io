---
title: "Chunking with Types"
author: Nathaniel Bos
date: 2026-02-06
---

In a [previous post](chunk.html), we showed how greedily appending to a
simple
[categorical](https://en.wikipedia.org/wiki/Categorical_distribution)
text model in the direction of maximal compression lead to meaningful
[chunking](https://en.wikipedia.org/wiki/Chunking_(psychology)) of the
data.

To bring the model closer to one capable of learning a
[grammar](https://en.wikipedia.org/wiki/Formal_grammar), we [complete
the logic](https://en.wikipedia.org/wiki/Algebraic_data_type) by
incorporating the dual of chunking:
[categorization](https://en.wikipedia.org/wiki/Cognitive_categorization).

While chunking was formalized as "joint" symbols---consecutive symbols
concatenated together---categories are formalized as sets of possible
symbols called "unions".

## Union Semantics

Unlike chunking, introducing classes of symbols under a categorical
model doesn't immediately produce savings in code length.

### Information of Joint Intro. (Recap)

Encoding a categorical model with counts $n_0, n_1, ... n_{m-1}$ and
instantiating it into a string of symbols [takes information
approximately equal to](chunk.html#format-description):

$$\underbrace{\log {N + m - 1 \choose m - 1} \vphantom{\prod_{\displaystyle i}}}
	_{\displaystyle n_0,n_1,\ldots,n_{m-1} \vphantom{\prod}}
+ \underbrace{\log {N \choose n_0,n_1,\ldots,n_{m-1}} \vphantom{\prod_{\displaystyle i}}}
	_{\displaystyle\mathrm{String} \vphantom{\prod}}
~~~\text{where } N = \sum_i n_i.$$

Introducing a new joint symbol means docking a joint count $n_{01}$ from
symbol counts $n_0$ and $n_1$ and appending count $n_{01}$ [resulting
in](chunk.html#loss-function) an increase in the description length of
the counts vector:

$$\begin{align}
\Delta I^*_{\mathrm{\bf n}}
&= \log {N + m - n_{01} \choose m} - \log {N + m - 1 \choose m - 1}\\[5pt]
&= \log \left(\frac{(N + m - n_{01})!\,N!}
	{(N + m - 1)!\,m\,(N - n_{01})!}\right) \\[5pt]
\end{align}$$

and a decrease in the length of string permutation:

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

which, together, is negative (i.e. reduces total information) only if
the joint count $n_{01}$ is sufficently large compared to what would be
expected by independence.

### Information of Union Intro. (Naive)

Because of the way permutations (and their coefficients) compose,
changing a categorical distribution by re-classifying symbols, then
adding appropriate codes to disambiguate the produced union-symbols has
an insignificant effect on the total code length.

Say we place $\ell$ symbols under a union $S_{0\ell} =
\{s_0,s_1,...,s_{\ell-1}\}$ with a cummulative count $n_{0\ell}$ where

$$n_{0\ell} = \sum_{i=0}^{\ell-1}n_i~~,$$

moving their counts to a secondary counts vector (as required to
interpret the code of their permutation) produces a combined description
length:

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

are approximately equal to the initial code length, modulo an additional
$O(\log m)$ term which matches the length a code would take to specify
which of the $m-\ell$ symbols is a union.

The bottom line is that the counting of permutations remains constant
under hierarchical decompositions of the set of symbols, indicating that
categories get their meaning from something beyond themselves.

### Joints of Unions

The way to make unions do work for us is to put them into joints.

The naive (ineffective) approach being of looking for a cluser of
symbols according to some arbitraty attribute:

![](res/types/figs/subset.svg)

we instead look for clusters in pairs---a "joint-of-unions"---such that
they have a relatively high number of joints between them.

As a [graph](https://en.wikipedia.org/wiki/Graph_theory), the set of
joint occurences of symbols in a string form edges in a [bipartite
graph](https://en.wikipedia.org/wiki/Bipartite_graph) connecting left
and right symbols. Then, we are looking for a subgraph with high
connectivity. Visually:

![](res/types/figs/bipartite.svg)

Logically, this is a [type](https://en.wikipedia.org/wiki/Type_theory)
of joint, specifically a [product
type](https://en.wikipedia.org/wiki/Product_type) containing the set of
joints corresponding to the [Cartesian
product](https://en.wikipedia.org/wiki/Cartesian_product) of the left
and right union type:

$$\frac{s_0 \in U ~~~~ s_1 \in V}{(s_0,s_1) \in U \times V}.$$

Properly speaking, the unions still don't do work themselves to reduce
the size of codes---that is only done through the introduction of the
joint, but unions will allow joint types to be defined between groups of
symbols which, individually, would lack the numbers to justify the
introduction of construction rule for all the joints that the type
covers. This is another way (together with [the sparsity of our chunks
dictionary](chunk.html#inductive-constructions)) that we overcome the
taditional limitations of the [$n$-gram
model](https://en.wikipedia.org/wiki/Word_n-gram_language_model)
regarding exponentially large datasets required to support words of
increasing size.


## Format Description

Starting from the format used in the joints-only logic as a sum of code
lengths

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
	_{\displaystyle\mathrm{{\bf s}: String} \vphantom{\prod}},
\end{align}$$

the $\mathrm{Rules}$ term is replaced with one defining the joint
$\mathrm{Types}$ and for each, a code that resolves $n_i$ joints from
that type.

### Types

For introducing individual joints (one left symbol, one right symbol),
two indexes with information $\log(m)$ were sufficient to specify a
construction rule, where $m$ is the number of symbols before the
introduction.

Cummulatively for a total of $m$ symbols, that came out to

$$I^*_{\bf r}(m) = 2\log\left(\frac{(m-1)!}{255!}\right)$$

assuming we start with 256 atomic symbols (stream of bytes).

To encode a joint type, each symbol needs to be potentially included or
excluded, twice (once for each side), pushing the information per
introduction from $2\log(m)$ to $2m$. Still assuming 256 atomic symbols,
we get a total

$$\begin{align}
I_T(m)
&= 2 \cdot 256 + 2 \cdot 257 + \ldots + 2 \cdot (m - 1) \\[5pt]
&= 2 \left( \sum_{i=1}^{m-1}i - \sum_{i=1}^{255}i \right) \\[5pt]
&= 2 \left( \frac{(m-1)m}{2} - \frac{255 \cdot 256}{2} \right) \\[5pt]
&= (m-1)m - 255 \cdot 256\\[5pt]
&= m^2 - m - 65280
\end{align}$$

### Resolution

Given the final string of symbols, the definition of composite symbols
is no longer sufficient to expand them back into a string of atoms.

For each symbol $s_i$, its
[variety](https://en.wikipedia.org/wiki/Variety_(cybernetics)) $\ell_i$
is

$$\ell_i = \begin{cases}
1 & \text{when atomic} \\
|U_i| \cdot |V_i| & \text{when joint type } U_i \times V_i
\end{cases}
$$

which, for joint types, is the number of joints under the type, i.e. the
product of the sizes of its unions.

Then, for each composite symbol $s_i$, a code of length $\log \ell_i$ is
required to disambiguate each of the $n_i$ joints it is to expand
into. The sum of the lengths of all those codes makes:

$$I_{\bf r}({\bf n}, {\bf l}) = \sum_i n_i \log \ell_i$$

Together, we have the total length of the encoding:

$$\begin{align}
I(m,{\bf n},{\bf l})
\leq~&~ \underbrace{2\log (m-256)\vphantom{\prod_{\displaystyle i}}}
	_{\displaystyle m \vphantom{\prod}}
+ \underbrace{m^2 - m - 65280\vphantom{\prod_{\displaystyle i}}}
	_{\displaystyle\mathrm{Types} \vphantom{\prod}}
+ \underbrace{2\log N\vphantom{\prod_{\displaystyle i}}}
	_{\displaystyle N \vphantom{\prod}}\\[10pt]
&+ \underbrace{\log {N + m - 1 \choose m - 1} \vphantom{\prod_{\displaystyle i}}}
	_{\displaystyle \mathrm{\bf n}: n_0,n_1,\ldots,n_{m-1} \vphantom{\prod}}
+ \underbrace{\log {N \choose n_0,n_1,\ldots,n_{m-1}} \vphantom{\prod_{\displaystyle i}}}
	_{\displaystyle\mathrm{{\bf s}: String} \vphantom{\prod}}\\[10pt]
&+ \underbrace{\sum_i n_i \log \ell_i \vphantom{\prod_{\displaystyle i}}}
	_{\displaystyle \mathrm{{\bf r} : Resolution} \vphantom{\prod}}
\end{align}$$

which by reducing in value through type introduction will result in
compression.
