---
title: "Information Theoretic Chunking"
author: Nathaniel Bos
date: 2025-12-01
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
I_{(m, \mathrm{\bf r}, N, \mathrm{\bf n}, \mathrm{\bf s})}
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
\Delta I_{\mathrm{\bf n}}
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
\Delta I_\mathrm{\bf s}
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

<div id="loss-formula">
$$\begin{align}
\Delta I_{(\mathrm{\bf n},\mathrm{\bf s})}
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
</div>

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

A number of additional optimizations were required to run the program to
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

<!-- At worst, this has the same complexity as re-counting all joint
counts, but in practice where -->

#### Joint Positions Bookkeeping

With the above optimizations, the program runs to completion in a
reasonable amount of time on strings of thousands (KB) up to a million
(1 MB) symbols.

At this point, the operation taking by far most of the run time on large
strings is the $O(N)$ pass over the input required to

1) rewrite joint occurences of $(s_0,s_1)$ into $s_{01}$ and

2) update joint counts at sites where $s_{01}$ gets written

This is especially problematic after the first few symbols have been
introduced and joint counts---for a sample of natural text---fall
sharply following a
[Pareto](https://en.wikipedia.org/wiki/Pareto_distribution)-like drop
into a very long tail. The algorithm then spends most of its time
performing uneventful $O(N)$ scans of the working string, looking for
the few locations where the joint it has already decided to introduce
appears.

The alternative to this is to store the set construction sites (indexes)
for each candidate joint and use a string representation that allows
efficient random access at those locations when a rule is introduced.

Because the act of rewriting turns two symbols into one symbol, a vector
of symbols traditionally indexed either accumulates gaps of arbitrary
length over time, hindering access, or requires on the order of $O(N)$
rewrites per introduction to close them.

The solution is to use a doubly linked list together and the permanent
memory addresses of nodes as indexes allowing both random access closing
gaps in constant time without affecting indexes down the line.

In practice, this incurs significant overhead at the begining of the
execution of the program, but pays for itself many times over in the
long tail of the execution.

#### Joint Loss Bookkeeping

The next operations which appropriate the bulk of the run-time on large
inputs is the evaluation and sorting of all joints according to the
[loss function](#loss-formula).

We can see this becomes severe when the size of the dictionary ($m$) is
large as the number of possible joints grows on the order of
$$O(\min(m^2,N)).$$

Examining our loss formula:

$$\Delta I_{(\mathrm{\bf n},\mathrm{\bf s})} =
\log \left( \frac
	{(N - n_{01} + m)!\,n_0!}
	{(N + m - 1)!\,m\,n_{01}!}
\right)
+ \begin{cases} \log  \left( \frac
	{\displaystyle 1}
	{\displaystyle (n_0 - 2n_{01})!}
\right) & \text{when } s_0 = s_1 \\
\log \left( \frac
	{\displaystyle n_1!}
	{\displaystyle (n_0 - n_{01})!\,(n_1 - n_{01})!}
\right) & \text{when } s_0 \neq s_1,
\end{cases}$$

splitting factors according to which term they came from

$$\Delta I_{(\mathrm{\bf n},\mathrm{\bf s})} =
\Delta I_\mathrm{\bf n}' + \Delta I_\mathrm{\bf s}'$$

we have a loss on the encoding of the counts:

$$\Delta I_\mathrm{\bf n}' =
	\log \left( \frac
		{(N - n_{01} + m)!}
		{(N + m - 1)!\,m}
	\right),
$$
which is

1) always negative, given $N,m,n_{01} > 1$
2) only depends on $N$, $m$, and $n_{01}$, and
3) for a given $N$ and $m$, minimal when $n_{01}$ is maximal,

$$$$

and one on the encoding of the ordering of the string:

$$\Delta I_\mathrm{\bf s}' =
\begin{cases}
	\log \left( \frac
		{ \displaystyle n_0! }
		{ \displaystyle n_{01}! \, (n_0 - 2n_{01})! }
	\right)  & \text{when } s_0 = s_1 \\
	\log \left( \frac
		{ \displaystyle n_0! \, n_1! }
		{ \displaystyle n_{01}! \, (n_0 - n_{01})! \, (n_1 - n_{01})! }
	\right) & \text{when } s_0 \neq s_1,
\end{cases}
$$
which is

1) always positive since it can be rewritten as

$$\Delta I_\mathrm{\bf s}' =
\begin{cases}
	\log \left( \displaystyle
		{ n_0 \choose 2n_{01}} \cdot (2n_{01})!
	\right)  & \text{when } s_0 = s_1 \\
	\log \left( \displaystyle
	    { n_0 \choose n_{01}} \cdot { n_1 \choose n_{01}} \cdot n_{01}!
	\right) & \text{when } s_0 \neq s_1,
\end{cases}
$$

2) only depends on $n_0$, $n_1$, and $n_{01}$, and
3) is minimal given $n_{01}$ when $n_0$ and $n_1$ are also minimal,
which is
	$$\mathrm{argmin}~\Delta I_\mathrm{\bf s}'(n_0,n_1 \mid n_{01}) =
		\begin{cases}
			n_0 := 2n_{01}  & \text{when } s_0 = s_1 \\
			(n_0,n_1) := (n_{01},n_{01}) & \text{when } s_0 \neq s_1,
		\end{cases}
	$$

$$$$

Given these bounds, we can restrict our search for the minimal value of
$\Delta I_{(\mathrm{\bf n},\mathrm{\bf s})}$ on a certain subset of the
highest values of $n_01$.

The chosen strategy was to sort joints first according to the joint
count $n_{01}$ and---for joints with the same value of
$n_{01}$---according to the value of $\Delta I_\mathrm{\bf s}'$. These
indexes remain constant between rule introductions where $N$ and $m$
necessarily change value, but most counts and joint counts remain
constant.

Then, at each iteration, given the values of $N$ and $m$, we traverse
the joints along the spine of $n_{01}$'s, from high to low, computing
the lowest values available for $\Delta I_\mathrm{\bf n}'(N,m,n_{01})$
in order, and for each, add it to the minimal value of $\Delta
I_\mathrm{\bf s}'(n_0,n_1 \mid n_{01})$ (as indexed) and interrupt the
traversal when

$$\begin{align}
\text{best so far} <~& \Delta I_\mathrm{\bf n}'(N,m,n_{01}) \\
&\!+~ \mathrm{argmin}~\Delta I_\mathrm{\bf s}'(n_0,n_1 \mid n_{01}).
\end{align}$$

This way, retrieving the joint with the minimal loss is almost instant
at the cost of having to maintain the indexes of joints for which any of
$n_0$, $n_1$, or $n_{01}$ gets changed after each introduction.

## Results

The dataset of choice is a [2006 snapshot of the English
Wikipedia](https://mattmahoney.net/dc/textdata.html) (XML format),
following the example of the [Large Text Compression
Benchmark](https://mattmahoney.net/dc/textdata.html) and
[Hutter prize](http://prize.hutter1.net/).

The first $10^9$ bytes of the snapshot are referred to as
`enwik9`. Truncations of smaller magnitudes are named accordingly:

![](res/chunk/datasets.svg)

$$$$

For different sized inputs, we show the evolution of the total code
length (information) divided among the different components (terms) of
the encoding (formula) as the dictionary grows. The contributions of the
encodings of parameters $m$ and $N$ are too small to be noticeable and
left out. The size of the encoding at $m=256$ (empty dictionary) is
indicated with a marker on the Y axis:

![](res/chunk/stacked/enwik4.svg)

![](res/chunk/stacked/enwik5.svg)

![](res/chunk/stacked/enwik6.svg)

![](res/chunk/stacked/enwik7.svg)

As one would expect the bulk of the gain in compressability occur with
the first few introduced symbols and tapers out as they produce fewer
modifications to the string.

We also notice that greater compressability is achieved with greater
inputs (and larger dictionaries).

We can compare the compressability, or "information density" between
scales by computing a compression *factor* at each point in the model's
evolution:

$$\text{compression factor} = \frac{\text{original size}}
{\text{compressed size}}.$$

A compression factor slightly above 1.5 is achieved across the board
with an empty dictionary simply by virtue of the encoding. Final
compression factors are marked on the Y axis. The X axis is displayed in
log-scale:

![](res/chunk/self.svg)

For a given number of symbols, greater factors are achieved by smaller
sets, probably due to the reduced variance of a smaller dataset, but
larger inputs are amenable to greater factors in the long run, as the
large string encoding term ($\mathrm{\bf s}$ decreasing) can support
rule introductions for longer ($\mathrm{\bf r}$ and $\mathrm{\bf n}$
increasing) before they begin to outweigh the reduction on $\mathrm{\bf
s}$.

Ultimately, exponentially increasing input sizes translate to roughly
exponentially increasing dictionary sizes (and running time), and
*linearly* increasing compression factors.

Full output: [`enwik4`](res/chunk/self/enwik4.csv),
[`enwik5`](res/chunk/self/enwik5.csv),
[`enwik6`](res/chunk/self/enwik6.csv),
[`enwik7`](res/chunk/self/enwik7.csv).

### Appearance

Chunks produced for `enwikX` datasets are a mix of English morphemes,
words and phrases as well as markup strings specific to Wikipedia's XML
schema:

```
256:  "]"     +  "]"    ==>  "]]"
257:  "["     +  "["    ==>  "[["
258:  "t"     +  "h"    ==>  "th"
259:  "th"    +  "e"    ==>  "the"
260:  ","     +  " "    ==>  ", "
261:  "'"     +  "'"    ==>  "''"
262:  " "     +  "the"  ==>  " the"
263:  " the"  +  " "    ==>  " the "
264:  "\n"    +  "*"    ==>  "\n*"
265:  "q"     +  "u"    ==>  "qu"
266:  "&"     +  "qu"   ==>  "&qu"
267:  "i"     +  "n"    ==>  "in"
268:  "a"     +  "n"    ==>  "an"
269:  "o"     +  "n"    ==>  "on"
270:  "an"    +  "d"    ==>  "and"
271:  "o"     +  "f"    ==>  "of"
272:  " "     +  "of"   ==>  " of"
273:  ">"     +  "\n"   ==>  ">\n"
274:  "in"    +  "g"    ==>  "ing"
275:  "t"     +  ";"    ==>  "t;"
276:  "e"     +  "n"    ==>  "en"
277:  "<"     +  "/"    ==>  "</"
278:  "1"     +  "9"    ==>  "19"
279:  "&qu"   +  "o"    ==>  "&quo"
280:  "&quo"  +  "t;"   ==>  "&quot;"
...:  ...     +  ...    ==>  ...
```
<!-- 1000: "&amp;nbsp"  +  ";"             ==>  "&amp;nbsp;" -->
<!-- 1001: "19"         +  "4"             ==>  "194" -->
<!-- 1002: "Ch"         +  "r"             ==>  "Chr" -->
<!-- 1003: "Chr"        +  "ist"           ==>  "Christ" -->
<!-- 1004: "Christ"     +  "ian"           ==>  "Christian" -->
<!-- 1005: "t"          +  "ain"           ==>  "tain" -->
<!-- 1006: "pro"        +  "v"             ==>  "prov" -->
<!-- 1007: "most"       +  " "             ==>  "most " -->
<!-- 1008: ". "         +  "It"            ==>  ". It" -->
<!-- 1009: "w"          +  "ell"           ==>  "well" -->
<!-- 1010: ".\n\n=="    +  "="             ==>  ".\n\n===" -->
<!-- 1011: "u"          +  "ally "         ==>  "ually " -->
<!-- 1012: "such"       +  " "             ==>  "such " -->
<!-- 1013: " the"       +  "m"             ==>  " them" -->
<!-- 1014: "a"          +  "th"            ==>  "ath" -->
<!-- 1015: "de"         +  "ath"           ==>  "death" -->
<!-- 1016: "\n*"        +  "[http://www."  ==>  "\n*[http://www." -->
<!-- 1017: "it"         +  "ies"           ==>  "ities" -->
<!-- 1018: "th"         +  "um"            ==>  "thum" -->
<!-- 1019: "|"          +  "thum"          ==>  "|thum" -->
<!-- ...   ...             ...                  ... --
<!-- 70000: "rop"         +  "e "           ==>  "rope " -->
<!-- 70001: "Clock"       +  "wise "        ==>  "Clockwise " -->
<!-- 70002: "Governor "   +  "       = [["  ==>  "Governor        = [[" -->
<!-- 70003: "dynamic "    +  "soar"         ==>  "dynamic soar" -->
<!-- 70004: "soft "       +  "mud"          ==>  "soft mud" -->
<!-- 70005: "button"      +  "s "           ==>  "buttons " -->
<!-- 70006: "aband"       +  "on "          ==>  "abandon " -->
<!-- 70007: "phot"        +  "on "          ==>  "photon " -->
<!-- 70008: "os"          +  "is"           ==>  "osis" -->
<!-- 70009: "Azerbaijan"  +  "is"           ==>  "Azerbaijanis" -->
<!-- 70010: "is"          +  "lav"          ==>  "islav" -->
<!-- 70011: "galax"       +  "ies"          ==>  "galaxies" -->
<!-- 70012: "galax"       +  "y"            ==>  "galaxy" -->
<!-- 70013: "economy "    +  "is "          ==>  "economy is " -->
<!-- 70014: "Arthrit"     +  "is "          ==>  "Arthritis " -->

Full output: [`enwik7`](res/chunk/self/enwik7.csv).

Compare these chunks to those that result from a naive combination
strategy where the most frequent joint is combined into a new symbol
(e.g. [Re-Pair](https://en.wikipedia.org/wiki/Re-Pair)). The produced
chunks are shorter and marginally less meaningful, at least at the
start:

```
256: "e"   +  " "   ==>  "e "
257: "t"   +  "h"   ==>  "th"
258: "s"   +  " "   ==>  "s "
259: "e"   +  "r"   ==>  "er"
260: "i"   +  "n"   ==>  "in"
261: "a"   +  "n"   ==>  "an"
262: "]"   +  "]"   ==>  "]]"
263: "["   +  "["   ==>  "[["
264: "d"   +  " "   ==>  "d "
265: "o"   +  "n"   ==>  "on"
266: ","   +  "  "  ==>  ", "
267: "t"   +  " "   ==>  "t "
268: "o"   +  "r"   ==>  "or"
269: "th"  +  "e "  ==>  "the "
270: "e"   +  "n"   ==>  "en"
271: "t"   +  "i"   ==>  "ti"
272: "a"   +  "r"   ==>  "ar"
273: "a"   +  "l"   ==>  "al"
274: " "   +  " "   ==>  "  "
275: "o"   +  "f"   ==>  "of"
276: "y"   +  " "   ==>  "y "
277: "of"  +  " "   ==>  "of "
278: "r"   +  "e"   ==>  "re"
279: "s"   +  "t"   ==>  "st"
280: "e"   +  "d "  ==>  "ed "
...: ...   +  ...   ==>  ...
```
<!-- 1000: "w"     +  "ould "  ==>  "would " -->
<!-- 1001: "op"    +  "h"      ==>  "oph" -->
<!-- 1002: "2"     +  "5"      ==>  "25" -->
<!-- 1003: "p"     +  "or"     ==>  "por" -->
<!-- 1004: "i"     +  " "      ==>  "i " -->
<!-- 1005: "ic"    +  "t"      ==>  "ict" -->
<!-- 1006: "pro"   +  "duc"    ==>  "produc" -->
<!-- 1007: "M"     +  "ar"     ==>  "Mar" -->
<!-- 1008: "y"     +  "p"      ==>  "yp" -->
<!-- 1009: "\n\n"  +  "=="     ==>  "\n\n==" -->
<!-- 1010: "er"    +  "n "     ==>  "ern " -->
<!-- 1011: "l"     +  "arg"    ==>  "larg" -->
<!-- 1012: "gu"    +  "st"     ==>  "gust" -->
<!-- 1013: "er"    +  "\"      ==>  ", er\, " -->
<!-- 1014: "S"     +  "e"      ==>  "Se" -->
<!-- 1015: "c"     +  "ap"     ==>  "cap" -->
<!-- 1016: "[["    +  "P"      ==>  "[[P" -->
<!-- 1017: "er "   +  "of "    ==>  "er of " -->
<!-- 1018: "oug"   +  "h"      ==>  "ough" -->
<!-- 1019: "in"    +  "to "    ==>  "into " -->
<!-- ...   ...        ...           ... -->
<!-- 70000: "Build"        +  "ings and "  ==>  "Buildings and " -->
<!-- 70001: "fronti"       +  "er"         ==>  "frontier" -->
<!-- 70002: "Cypr"         +  "us]] "      ==>  "Cyprus]] " -->
<!-- 70003: "description "  +  "| url=http://www."  ==>  "description | url=http://www." -->
<!-- 70004: "ρι"           +  "στο�"      ==>  "ριστο�" -->
<!-- 70005: "Characters in Atlas_Shrugged" + "|" ==> "Characters in Atlas_Shrugged|" -->
<!-- 70006: "homosexual "  +  "sex is "    ==>  "homosexual sex is " -->
<!-- 70007: "Benjamin "    +  "Tuck"       ==>  "Benjamin Tuck" -->
<!-- 70008: "in [[leap year]]s"  +  ")\"  ==>  "with , in [[leap year]]s)\, with " -->
<!-- 70009: "Ba"           +  "ad"         ==>  "Baad" -->
<!-- 70010: "Ele"          +  "azar"       ==>  "Eleazar" -->
<!-- 70011: "Fres"         +  "nel,"       ==>  "Fresnel," -->
<!-- 70012: "UR"           +  "PS "        ==>  "URPS " -->
<!-- 70013: "dop"          +  "e.com/"     ==>  "dope.com/" -->
<!-- 70014: "eight"        +  "-"          ==>  "eight-" -->
<!-- 70015: "gosp"         +  "el"         ==>  "gospel" -->
<!-- 70016: "march"        +  "ed to "     ==>  "marched to " -->
<!-- 70017: "tum"          +  "or"         ==>  "tumor" -->
<!-- 70018: "tum"          +  "ul"         ==>  "tumul" -->
<!-- 70019: "imprison"     +  "ment"       ==>  "imprisonment" -->
<!-- 70020: "imprison"     +  "ment "      ==>  "imprisonment " -->
<!-- 70021: "resembl"      +  "ing "       ==>  "resembling " -->
<!-- 70022: "his father "  +  "had "       ==>  "his father had " -->
<!-- 70023: "persec"       +  "ution of "  ==>  "persecution of " -->
<!-- 70024: "trag"         +  "edy "       ==>  "tragedy " -->

Full output: [`enwik7-naive`](res/chunk/enwik7-naive.csv).

Naive chunks visibly have a bias towards combining symbols with high
occurences even if the combination doesn't hold much more meaning than
the sum of its parts. For example compared to the more meaningful

```
">\n", "</", "ing", "&quot;",
```

the following chunks have more occurences and are therefor selected
earlier by the naive policy:

```
"e ", "s ", "d ", "t ".
```

In terms of probability:

$$p(s_0,s_1) \sim p(s_0)p(s_1).$$

Instead, if we are interested in how much a joint occurs *relative* to a
null hypothesis of independence, we get a ratio:

$$\frac{p(s_0,s_1)}{p(s_0)p(s_1)}$$

which, in information terms, is known as the [pointwise mutual
information
(PMI)](https://en.wikipedia.org/wiki/Pointwise_mutual_information):

$$\begin{align} \mathrm{pmi}(s_0;s_1)
&= \log\left(\frac{p(s_0,s_1)}{p(s_0)p(s_1)}\right) \\[5pt]
&= \log\left(\frac{n_{01} \cdot N \cdot N}{N \cdot n_0 \cdot n_1}\right) \\[5pt]
&= \log\left(\frac{n_{01} \, N}{n_0 \, n_1}\right) \\
\end{align}$$

Using this to score joint candidates, produces the somewhat degenerate
dictionary containing rare byte pairs that occur almost exclusively
together:

```
256: "�"   +  "�"  ==>  "ی"
257: "�"   +  "�"  ==>  "Ә"
258: "�"   +  "�"  ==>  "ے"
259: "@"    +  "@"   ==>  "@@"
260: "@@"   +  "@@"  ==>  "@@@@"
261: "@@@@" +  "@@"  ==>  "@@@@@@"
262: "@@@@" +  "@"   ==>  "@@@@@"
263: "@@"   +  "@"   ==>  "@@@"
264: "�"   +  "�"  ==>  "ґ"
265: "�"   +  "�"  ==>  "��"
...: ...    +  ...   ==>  ...
```

We scale this information by the joint count to obtain a function more
representative of total change in information:

$$\begin{align} \mathrm{spmi}(s_0;s_1) 
&~=~ n_{01} \cdot \, \mathrm{pmi}(s_0;s_1) \\[5pt]
&~=~ n_{01} \cdot \, \log\left(\frac{n_{01} \, N}{n_0 \, n_1}\right),
\end{align}$$

which gives a dictionary starting with:

```
256:  "]"     +  "]"    ==>  "]]"
257:  "["     +  "["    ==>  "[["
258:  "t"     +  "h"    ==>  "th"
259:  "th"    +  "e"    ==>  "the"
260:  "i"     +  "n"    ==>  "in"
261:  "a"     +  "n"    ==>  "an"
262:  "o"     +  "n"    ==>  "on"
263:  ","     +  " "    ==>  ", "
264:  " "     +  "the"  ==>  " the"
265:  "'"     +  "'"    ==>  "''"
266:  " the"  +  " "    ==>  " the "
267:  "o"     +  "f"    ==>  "of"
268:  "an"    +  "d"    ==>  "and"
269:  "e"     +  "r"    ==>  "er"
270:  "e"     +  "n"    ==>  "en"
271:  "o"     +  "r"    ==>  "or"
272:  " "     +  "of"   ==>  " of"
273:  "\n"    +  "*"    ==>  "\n*"
274:  "a"     +  "r"    ==>  "ar"
275:  "a"     +  "l"    ==>  "al"
276:  "e"     +  "d"    ==>  "ed"
277:  "in"    +  "g"    ==>  "ing"
278:  "a"     +  "t"    ==>  "at"
279:  "t"     +  ";"    ==>  "t;"
280:  "&"     +  "q"    ==>  "&q"
...:  ...     +  ...    ==>  ...
```

which is much more similar to the dictionary obtained from our loss
function.

<!-- In fact, the topic of restoring the bias of PMI for less frequent
pair has been addressed at length in the field of NLP, -->
