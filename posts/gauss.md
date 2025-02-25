---
title: "Encoding Numbers with Gaussians"
author: Nathaniel Bos
date: 2025-02-08
---

Arithmetic codes are pretty useful for compression. You can find a
number of implementations of the algorithm online, some work with static
models, some adaptive. Often, an abstract interface is defined so users
can call the algorithm with their own model implementation. It makes
sense for the algorithm to interface with a user-defined model through a
vector of probabilities of the next-symbols, but one can find this
limiting if trying to model **very large domains** like those of
numbers.

In arithmetic codes, you usually think of the 1D space addressable by
codes as divided between next-symbols based on their relative
probabilities:

![](images/alphabet-wide.png)

But nothing in the spirit of the technique prevents you from
partitioning the space in **infinitely** many bins to make use of
continuous probability distributions, as long as each bin can be
assigned a **finite** code/interval and vice versa. 

For instance, a
[Gaussian](https://en.wikipedia.org/wiki/Normal_distribution):

![](images/gauss-cat-wide.png)

An implementation of arithmetic coding that works with *any* model would
have to be more abstract than what you typically find online. The
usually included logic for resolving the categorical CDF (i.e. the
[quantile function](https://en.wikipedia.org/wiki/Quantile_function)) is
replaced with an abstract interface for any *distribution* that can
repeatedly *truncate* at given *cumulative probabilities* until a
specific *bin* is resolved. Here is my implementation in Rust:

- [Source (GitHub)](https://github.com/nbos/cont-arith-code)
- [Documentation](doc/cont_arith_code/index.html)

We use this algorithm to verify the ability of Gaussian distributions to
encode values compactly.

## Why (not) Gaussians

Why this kind of distribution in the first place? The short answer is
that (A) there aren't that many other good options and (B) it turns out
they have a bunch of neat properties that make them special. A short
list would include the [central limit
theorem](https://en.wikipedia.org/wiki/Central_limit_theorem), the fact
that they have [maximum
entropy](https://en.wikipedia.org/wiki/Maximum_entropy_probability_distribution#Other_examples)
given mean and variance, that they are closed under
[intersection](http://www.lucamartino.altervista.org/2003-003.pdf),
[conditioning](https://en.wikipedia.org/wiki/Multivariate_normal_distribution#Conditional_distributions),
all while generalizing to [multiple
dimensions](https://en.wikipedia.org/wiki/Multivariate_normal_distribution).

So where might things go wrong? For our present purposes it would be
sufficient to demonstrate that a Gaussian [Maximum Likelihood Estimation
(MLE)](https://en.wikipedia.org/wiki/Maximum_likelihood_estimation) of a
dataset can exhibit, for the bins corresponding to each number within
the set:

1. sufficiently high likelihood (so that the codes are reasonably
   small), and

2. a computable interval derived through successive truncations of the
   probability distribution.

Of course you can break both of these conditions if you select a small
enough bin size, so we assume only a reasonable limit to the amount of
precision per number (e.g. on the order of e.g. 32 or 64 bits). 

We first address theoretical considerations and follow with the
implementation considerations.

## Worst Likelihood Analysis

Which distribution of data produces the lowest likelihoods in its
Gaussian MLE? Presumably, those that are the most dissimilar from the
bell shape of the Gaussian PDF, in particular where you end up with data
far off in the tails. Since the mapping from datasets to the Gaussian
MLE has scale, location and count invariance, there are only a few
corner cases to examine:

![](images/gauss-mle-corner-cases.png)

The **unimodal case** is not problematic for our uses as long as you
implement the logic to handle it. It is a "degenerate" Gaussian,
(a.k.a. [Dirac
delta](https://en.wikipedia.org/wiki/Dirac_delta_function)) with zero
variance, infinite probability density at the mode and zero probability
everywhere else, but that just means the code length for each value is
zero and only the message length has to be encoded.

The **bimodal case** is hardly an issue either and has significant
probability density at the two modes no matter how "separated" you make
them.

The **outlier case** is the only one where the likelihood of a data point
can fall really low. If all but one point have non-significant variance
around a point, the probability density at that outlier point can be
prohibitively low. So how bad can it get?

### Outlier Likelihood

The Gaussian MLE of a set of $n$ values $\{x_0, x_1, x_2, ...\}$ has the
parameters:

$$\mu = \frac{\sum x}{n} ~~~~~~~~~~~~ \sigma^2 = \frac{\sum (x - \mu)^2}{n}$$

Another formulation of variance (which also happens to be the more
numerically stable) is:

$$\sigma^2 = \frac{\sum x^2}{n} - \frac{(\sum x)^2}{n^2}$$

or even:

$$\mu = \frac{s_1}{s_0} ~~~~~~~~~~~~ \sigma^2 = \frac{s_2}{s_0} -
\frac{(s_1)^2}{(s_0)^2}$$

where the only parameters are sums of the values raised to a power:

$$s_i = \sum{x^i}$$

With this formulation we can model the probability density at the
outlier by setting the majority at $0$ and the outlier at $1$ (by
invariance, this is representative of any outlier case modulo a constant
factor):

$$\begin{array}{|l|c|c|c|}
\hline
\text{Population} & s_0 & s_1 & s_2 \\
\hline
\{0,1\} & 2 & 1 & 1 \\
\hline
\{0,0,1\} & 3 & 1 & 1 \\
\hline
\{0,0,0,1\} & 4 & 1 & 1 \\
\hline
\{0,0,...,0,1\} & n & 1 & 1 \\
\hline
\end{array}$$

Then we have $s_1 = s_2 = 1$. We substitute $\mu$ and $\sigma^2$ in the
PDF to get a function of $n$:

$$\begin{align}\mathrm{pdf}(x) &= \frac {1}{\sqrt {2\pi \sigma ^{2}}}e^{-{\frac {(x-\mu )^{2}}{2\sigma ^{2}}}}\\
\mathrm{pdf}(1) &= \frac{1}{\sqrt {\frac{2\pi}{n} - \frac{2\pi}{n^2}}}e^{-\frac{(n-1)^2}{2n-2}} \end{align}$$

Which looks like

![](images/pdf1.png)

which drops pretty quickly (exponentially quickly), but code length only grows
in one over the logarithm of the probability so:

![](images/neglnpdf1.png)

Which is a pretty unambiguous $0.5n$ towards infinity. 

So it looks like the code length of the outlier of a Gaussian only grows
in $O(n)$ of the size of the data set, which is only as bad as the
performance of the uniform distribution of fixed-length codes. 

Now, a full account of the performance of the distribution also has to
take into account the code lengths of non-outliers, although we only
expect their likelihood to grow with increasing $n$ compared to the
outlier. For completeness, computing the sum of code lengths using
proper probability intervals (not density) on the CDF, with bin size
$\pm 0.5$ around integers, in base 2, we show the total code length also
achieves linear size w.r.t. $n$ in this outlier "worst case":

![](images/outliercasecodelength.png)

## Numerical Stability

The quantile function for Gaussians is continuous, one-to-one, monotone
and has finite value everywhere except at $0 \mapsto -\infty$ and $1
\mapsto \infty$. 

![](images/cdfquantile.png)

For our application, what's important is that *repeatedly splitting* in
half an interval of the probability mass down to any symbol to encode
happens with *constant progress* (i.e. the middle of any interval cannot
be equal to either boundaries) and no overflow to any infinity. This
reflects on the CDF and quantile by requiring the following: any two
bounds $a,b$ such that $0<a<b<1$ and where both bounds are not already
within the bounds of a symbol, then the middle point `quantile(cdf(a) +
0.5*(cdf(b) - cdf(a)))` must be strictly greater than `a` and strictly
less than `b`. If it is not, encoding becomes impossible.

An easy measure to ensure progress might be to fall back to *linear*
interpolation whenever the call to the quantile function runs out of
precision, assuming local linearity. While this is a reasonable
approximation in the central bulk of the distribution, it fails in the
tails.

To see why, consider the PDF and its derivative:

![](images/pdfdiff.png)

While both flatten out at the tails, for any given interval in the
tails, the relative difference becomes greater the further away you move
from the center. To see this, normalize the (absolute) derivative to the
value of the function:

![](images/pdfdiffnorm.png)

That is, the tails may be *absolutely* flat, but they become
*relatively* steeper the further away you go. Another way to demonstrate
this is by blowing up the PDF at different scales (here, successive
factors of 10):

![](images/pdfscales.png)

which makes it clear we cannot rely on any "flatness" in the tails. We
are forced to find an analytic or at least numeric solution.

### The Proper Way

Like is usually the case in probability, the solution to numerical
instability is found in the
[log-domain](https://en.wikipedia.org/wiki/Log_probability). This gives
us two analogous functions for the cumulative probability with more
manageable shapes:

![](images/logcdfquantileexp.png)

Furthermore, we can model all right tail calculations by using the
left's and avoid all asymptotes by exploiting the symmetry of the
Gaussian PDF. This leaves us with two almost linear curves.

Fortunately, we are not the first to reach this point of the
journey. SciPy has well documented and precise polynomial approximations
of the log-CDF
[`log_ndtr`](https://docs.scipy.org/doc/scipy/reference/generated/scipy.special.log_ndtr.html)
([source](https://github.com/scipy/scipy/blob/ab84560b96cf5816be0015b0ee3a41cef708f675/scipy/special/xsf/stats.h#L84))
and quantile-exp
[`ndtri_exp`](https://docs.scipy.org/doc/scipy/reference/generated/scipy.special.ndtri_exp.html)
([source](https://github.com/scipy/scipy/blob/ab84560b96cf5816be0015b0ee3a41cef708f675/scipy/special/_ndtri_exp.pxd#L163)). This
affords us the precise interpolations on the probability mass of the
Gaussian we require, at least for now.

## Examples

For the examples below, *information content* is calculated as the sum
of the log$_2$-probabilities of each integer in the distribution. The
*expected code length* is that value rounded up. *Code length* is the
empirical result. All codes decode back to the encoded values.

### Degenerate Cases

Integer sets with a single value produce empty codes:
```
Set: [0]
Model: Gaussian { μ: 0, σ: 0 } (1, 0, 0)
Information Content: 0 bits
Expected code length: 0 bits
Code: ''
Code length: 0 bits
Analysis: +0 bits (+0.0%) compared to expected
Decoding successful

Set: [1]
Model: Gaussian { μ: 1, σ: 0 } (1, 1, 1)
Information Content: 0 bits
Expected code length: 0 bits
Code: ''
Code length: 0 bits
Analysis: +0 bits (+0.0%) compared to expected
Decoding successful

Set: [8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8]
Model: Gaussian { μ: 8, σ: 0 } (12, 96, 768)
Information Content: 0 bits
Expected code length: 0 bits
Code: ''
Code length: 0 bits
Analysis: +0 bits (+0.0%) compared to expected
Decoding successful
```

### Small Sets

Small symmetric sets produce consistently optimal codes:
```
Set: [-1, 1]
Model: Gaussian { μ: 0, σ: 1 } (2, 0, 2)
Information Content: 4.0970591008090445 bits
Expected code length: 5 bits
Information Contributions (bits): [2.05, 2.05]
Code: '01010'
Code length: 5 bits
Analysis: +0 bits (+0.0%) compared to expected
Decoding successful

Set: [-1234, 1234]
Model: Gaussian { μ: 0, σ: 1234 } (2, 0, 3045512)
Information Content: 24.632444528661186 bits
Expected code length: 25 bits
Information Contributions (bits): [12.32, 12.32]
Code: '0010100010100110000100000'
Code length: 25 bits
Analysis: +0 bits (+0.0%) compared to expected
Decoding successful

Set: [1, 0, -1]
Model: Gaussian { μ: 0, σ: 0.816496580927726 } (3, 0, 2)
Information Content: 5.274689097597744 bits
Expected code length: 6 bits
Information Contributions (bits): [2.08, 1.12, 2.08]
Code: '111000'
Code length: 6 bits
Analysis: +0 bits (+0.0%) compared to expected
Decoding successful
```

### Outlier Cases

Outlier cases like this one ($n = 10$):
```
Set: [0, 0, 0, 0, 0, 0, 0, 0, 0, 1]
Model: Gaussian { μ: 0.1, σ: 0.3 } (10, 1, 1)
Information Content: 5.0256952907839185 bits
Expected code length: 6 bits
Information Contributions (bits): [0.17, 0.17, 0.17, 0.17, 0.17, 0.17, 0.17, 0.17, 0.17, 3.45]
Code: '1001110'
Code length: 7 bits
Analysis: +1 bits (+16.7%) compared to expected
Decoding successful

```

are generated between $n = 1$ and $n = 100$, reproducing the plot from
an earlier section:

![](images/outlierresults.png)

which is not optimal everywhere, but good enough.

### Random Data

The error becomes less noticeable as we move to sets containing more
information. Here we sample $n$ elements from a
[**uniform**](https://en.wikipedia.org/wiki/Continuous_uniform_distribution)
distribution between -5 and 5, once for each $n$:

![](images/randomuniform.png)

Seemingly identical performance is obtained when sampling from a
[**normal**](https://en.wikipedia.org/wiki/Normal_distribution)
distribution with the same variance $(\sigma^2 = \frac{10^2}{12} =
8.\overline{3})$:

![](images/randomnormal.png)

Sampling from any wider distribution produces code lengths closer to the
information content than is visually distinguishable.
