---
title: "Chunking with Unions"
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
symbols called "unions"[^1].

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

Introducing a joint symbol means docking a joint count $n_{01}$ from
symbol counts $n_0$ and $n_1$ and appending count $n_{01}$ [resulting
in](chunk.html#loss-function) an increase in the description length of
the counts vector:

$$\begin{align}
\Delta I_{\mathrm{\bf n}}
&= \log {N + m - n_{01} \choose m} - \log {N + m - 1 \choose m - 1}\\[5pt]
&= \log \left(\frac{(N + m - n_{01})!\,N!}
	{(N + m - 1)!\,m\,(N - n_{01})!}\right) \\[5pt]
\end{align}$$

and a decrease in the length of string permutation:

$$\begin{align}
\Delta I_\mathrm{\bf s}
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

which, together, is negative only if the joint count $n_{01}$ is
sufficently large compared to what would be expected by independence.

### Information of Union Intro. (Naive)

Because of the way permutations (and their coefficients) compose,
changing a categorical distribution by re-classifying symbols, then
adding appropriate codes to disambiguate the produced union-symbols has
little to no effect on the total code length.

Say we place $\ell$ symbols under a union $S_{0\ell} =
\{s_0,s_1,...,s_{\ell-1}\}$ with a cummulative count $$n_{0\ell} =
\sum_{i=0}^{\ell-1}n_i~~,$$

moving their counts to a secondary counts vector (as required to
interpret the code of their permutation) produces a combined description
length:

$$\begin{align}
I_{\mathrm{\bf n}}
&= \log {N - n_{0\ell} + m - \ell - 1 \choose m - \ell - 1}
	+ \log {n_{0\ell} + \ell - 1 \choose \ell - 1}\\[5pt]
&= \log \left( {N - n_{0\ell} + m - \ell - 1 \choose m - \ell - 1}
	{n_{0\ell} + \ell - 1 \choose \ell - 1} \right).
\end{align}$$

Similarly, removing those counts from the encoding of the permutation
and adding them to a disambiguating permutation separates the
coefficient:

$$\begin{align}
I_{\mathrm{\bf s}}
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

### Joints-of-Unions

The way to make unions do work for us is to put them into joints.

The naive approach being of looking for a cluser of symbols according to
an arbitraty attribute:

![](res/union/figs/subset.svg)

we instead look for clusters in pairs---a "joint-of-unions"---such that
they have a relatively high number of joints between them.

As a [graph](https://en.wikipedia.org/wiki/Graph_theory), the set of
joint occurences of symbols in a string form edges in a [bipartite
graph](https://en.wikipedia.org/wiki/Bipartite_graph) connecting left
and right symbols. Then, we are looking for a subgraph with high
connectivity. Visually:

![](res/union/figs/bipartite.svg)

Properly speaking, the unions still don't do work themselves to reduce
the size of codes---that is still done by the introduction of the joint,
but unions will allow joints to be defined between groups of symbols
which, individually, would lack the numbers to justify the introduction
of construction rule for each combination of joint that our
joint-of-unions covers. This is another way (together with [the sparsity
of our chunks dictionary](chunk.html#inductive-constructions)) that we
overcome the taditional limitations of the [$n$-gram
model](https://en.wikipedia.org/wiki/Word_n-gram_language_model)
regarding exponentially large datasets required to support words of
increasing size.

[^1]: Whereas product operators
([structs](https://en.wikipedia.org/wiki/Record_(computer_science)),
[intersections](https://en.wikipedia.org/wiki/Intersection_(set_theory)),
[conjunction](https://en.wikipedia.org/wiki/Logical_conjunction)) all
seem to carry connotations that make them hard to apply to our case,
leading us to opt instead for the term "joints" [from probability
theory](https://en.wikipedia.org/wiki/Joint_probability_distribution)
which shares aspects of co-occurence and independence, sum operators
([enum](https://en.wikipedia.org/wiki/Enumerated_type),
[union](https://en.wikipedia.org/wiki/Union_(set_theory)),
[disjunction](https://en.wikipedia.org/wiki/Logical_disjunction)) all
pretty much have the same semantics of choice, so we go for the term
"union" for its ubiquity.
