---
title: "Encoding Numbers with Gaussians"
author: Nathaniel Bos
date: 2025-10-19
---

[Arithmetic codes](arith.html) are pretty useful for compression.

There are a number of implementations available online, typically
employing the equivalent of a [categorical
distribution](https://en.wikipedia.org/wiki/Categorical_distribution)
over a finite domain. For example, using a probability table over the
alphabet comensurate with common English text, each letter gets a
section of the unit interval proportional to its frequency:

![](res/gauss/frequency-bars.svg)

![](res/gauss/alphabet.svg)

But any probability distribution with a well defined [quantile
function](https://en.wikipedia.org/wiki/Quantile_function) could be used
for arithmetic coding, even on infinite domains. For instance, a
[Gaussian](https://en.wikipedia.org/wiki/Normal_distribution) prior over
integers could be used to compress whole numbers:

![](res/gauss/gauss-pdf.svg)

![](res/gauss/gauss-cdf.svg)

Each integer $i \in \mathbb{Z}$ is assigned the probability mass
contained within the interval $i \pm 0.5$ on the Gaussian of choice.


## Viability

### PDF as a Estimator for Probability
The code length achievable by an arithmetic encoder is within two bits
of the information content of the encoded message, which is the sum of
the individual information content of the constituent symbols. The
information content of a symbol is inversely related to its probability:

$$I(x) = -\log P(x)$$

The probability we assign each integer is the probability mass within
the bounds $x \pm 0.5$ of the $PDF$ (the [probability density
funtcion](https://en.wikipedia.org/wiki/Probability_density_function),
pictured above). This is computed as

$$\begin{align}P(x) &= CDF(x)|_{x-0.5}^{x+0.5}\\[6pt]
	&= CDF(x+0.5) - CDF(x-0.5)\end{align}$$

in terms of the Gaussian $CDF$ (the [cummulative distribution
function](https://en.wikipedia.org/wiki/Cumulative_distribution_function)),
which, for a normal $\mathcal{N}(\mu,\sigma^2)$ is equal to:

$$CDF(x) = \frac {1}{2}\left[1+\operatorname {erf} \left({\frac {x-\mu
}{\sigma {\sqrt {2}}}}\right)\right]$$

which is expressed in terms of the non-elementary, sigmoid, [error
function](https://en.wikipedia.org/wiki/Error_function) "erf" which
doesn't factor or reduce well enough to be useful going further.

Conveniently, however, the $PDF$ already approximates the
$CDF(x)|_{x-0.5}^{x+0.5}$ on grounds that the $CDF$ is the integral of
the $PDF$:

$$PDF(x) = \lim_{h\to 0} \frac{CDF(x)|_{x-h/2}^{x+h/2}}{h}$$

which is just a statement of the [fundamental theorem of
calculus](https://en.wikipedia.org/wiki/Fundamental_theorem_of_calculus). Practically
speaking, this means the $PDF$ approximates the interval-$CDF$,
especially when the interval is small enough compared to the variance
(here $\mu = 0, \sigma = 1$):

![](res/gauss/ftc0.svg)

and as the variance approaches 0, our probability function flattens
relative to the $PDF$ (here $\sigma \in \{ 0.8^i$ | $i \in \{0,1,2,...\}
\}$, and both axes are scaled to keep the $PDF$ at the same place):

![](res/gauss/ftc1-0.svg)

with sigmoid-shaped steps, where the $\pm 0.5$ interval crosses into and
out of the actual $PDF$:

![](res/gauss/ftc1-1.svg)

meaning that either $PDF$ is a good estimator for
$CDF(x)|_{x-0.5}^{x+0.5}$ when $\sigma \geq 1$ or it underestimates the
probability in the tails (even beyond the sigmoid steps) when $\sigma <
1$. In either case I argue it works as a good "worst-case".

This is convenient because the $PDF$ doesn't contain special functions:

$$PDF(x) = \frac{1}{\sqrt{2\pi \sigma^2}}
	\;\exp\!\left(-\frac{(x-\mu)^2}{2\sigma^2}\right)$$

We use the $PDF$ to explore the information content of different data
sets w.r.t. their MLE ([maximum likelihood
estimator](https://en.wikipedia.org/wiki/Maximum_likelihood_estimation)),
i.e. the Gaussian with mean and variance equal to the mean and variance
of the data.

### Re-parametrization of Gaussian MLE's
The Gaussian MLE of a set of $n$ values $\{x_0, x_1, x_2, ...\}$ has
parameters:

$$\mu = \frac{\sum x}{n} ~~~~~~~~~~~~
\sigma^2 = \frac{\sum (x - \mu)^2}{n}$$

Another formulation of variance (which also happens to be the more
numerically stable) is:

$$\sigma^2 = \frac{\sum x^2}{n} - \frac{(\sum x)^2}{n^2}$$

or simply:

$$\mu = \frac{s_1}{s_0} ~~~~~~~~~~~~
\sigma^2 = \frac{s_2}{s_0} - \frac{(s_1)^2}{(s_0)^2}$$

where the only parameters are sums of the values raised to a power:

$$s_i = \sum{x^i} ~~~~~~~~~~~~ i \in \{0,1,2\}$$

### Modeling the Information of Data Sets
#### Multimodal

For the multimodal case:

$$\renewcommand{\arraystretch}{1.5}
\begin{array}{l|c|c|c}
\text{Data} & s_0 & s_1 & s_2 \\
\hline
\{0,1\} & 2 & 1 & 1 \\
\{0,1,2\} & 3 & 3 & 5 \\
\{0,1,2,3\} & 4 & 6 & 14 \\
\{0,1,2,\ldots,n\} & n+1 & \dfrac{n(n+1)}{2} & \dfrac{n(n+1)(2n+1)}{6}
\end{array}$$

we have parameters:

$$\begin{align}
	\mu &= \frac{s_1}{s_0} = \frac{n(n+1)}{2(n+1)} = \frac{n}{2}
\end{align}$$

and

$$\begin{align}
	\sigma^2 &~=~ \frac{s_2}{s_0} - \frac{(s_1)^2}{(s_0)^2}\\
		&~=~ \frac{n(n+1)(2n+1)}{6(n+1)} - \frac{n^2(n+1)^2}{4(n+1)^2}\\
		&~=~ \frac{n(2n+1)}{6} - \frac{n^2}{4}\\
		&~=~ \frac{2n(2n+1) - 3n^2}{12}\\
		&~=~ \frac{4n^2 + 2n - 3n^2}{12}\\
		&~=~ \frac{n^2 + 2n}{12}
\end{align}$$

which flattens out with increasing $n$ e.g. $n \in \{1,2,...,12\}$:

![](res/gauss/multimodals.svg)

In general, we get the class of $PDF$s:
$$
\begin{aligned}
PDF(n,x)_{multimodal}
&= \frac{1}{\sqrt{2\pi \cdot \frac{n(n+2)}{12}}}
   \;\exp\!\left(-\frac{\left(x-\frac{n}{2}\right)^2}{2 \cdot \frac{n(n+2)}{12}}\right) \\
&= \frac{1}{\sqrt{\pi \cdot \frac{n(n+2)}{6}}}
   \;\exp\!\left(-\frac{\left(x-\frac{n}{2}\right)^2}{\frac{n(n+2)}{6}}\right) \\
&= \sqrt{\frac{6}{\pi\,n(n+2)}}
   \;\exp\!\left(-\frac{6\left(x-\frac{n}{2}\right)^2}{n(n+2)}\right) \\
\end{aligned}
$$

As $n$ increases, the probability densities at each of the data points
$\{0,..,n\}$ converge meaning, at worst, we get an information content
of:

$$
\begin{aligned}
I(n)_{multimodal}
&= -n\log(PDF(n,0)_{multimodal}) \\
&= -n\log\left(\sqrt{\frac{6}{\pi\,n(n+2)}} 
	\; \exp\!\left(-\frac{6\left(0-\frac{n}{2}\right)^2}{n(n+2)}\right)\right) \\
&= -n\log\left(\sqrt{\frac{6}{\pi\,n(n+2)}} 
	\; \exp\!\left(-\frac{3n}{2(n+2)}\right)\right) \\
&= -\frac{n}{2}\log\left(\frac{6}{\pi\,n(n+2)}\right) + \frac{3n^2}{2(n+2)} \\
&= \frac{n}{2}\log\left(\frac{\pi\,n(n+2)}{6}\right) + \frac{3n^2}{2(n+2)} \\
\end{aligned}
$$

which is $O(n)$ and exactly what is expected from the information of a
data set of size $n$.

#### Outlier case

For a more degenerate case, consider the probability density at an
outlier by setting the majority at $0$ and the outlier at $1$ (by
invariance, this is representative of any outlier case, modulo a
constant factor):

$$\begin{array}{l|c|c|c}
\text{Data} & s_0 & s_1 & s_2 \\
\hline
\{0,1\} & 2 & 1 & 1 \\
\{0,0,1\} & 3 & 1 & 1 \\
\{0,0,0,1\} & 4 & 1 & 1 \\
\{0,0,...,0,1\} & n & 1 & 1 \\
\end{array}$$

In general, we get parameters:

$$\begin{align}
	\mu &= \frac{s_1}{s_0} = \frac{1}{n}
\end{align}$$

and

$$\begin{align}
	\sigma^2 &~=~ \frac{s_2}{s_0} - \frac{(s_1)^2}{(s_0)^2} ~=~ \frac{1}{n} - \frac{1}{n^2}
	~=~ \frac{n-1}{n^2}
\end{align}$$

The $PDF$ becomes

$$\begin{align}
PDF(n,x)_{outlier}
&= \frac{1}{\sqrt{2\pi \cdot\frac{n-1}{n^2}}}
	\exp\!\left(-\frac{\left(x-\frac{1}{n}\right)^2}{2\cdot \frac{n-1}{n^2}}\right)\\
&= \frac{n}{\sqrt{2\pi(n-1)}}\exp\!\left(-\frac{n^2\left(x-\frac{1}{n}\right)^2}{2(n-1)}\right)
\end{align}$$

For the informational content of a data set of size $n$, we have $(n-1)$
times $P(0)$ and $P(1)$ once. As $n$ approaches infinity, the density at
$x=0$ goes beyond $1$, also approaching infinity, which would produce
negative information. Bounding the probability of non-outliers at $1$,
they contribute an information content of $0$. The whole of the
information content thefore comes from the shrinking probability of the
outlier:

$$\begin{align}
I(n)_{outlier} &= -\log(PDF(n,1)_{outlier}) \\
&= -\log\left(\frac{n}{\sqrt{2\pi(n-1)}} \exp\left(-\frac{n-1}{2}\right)\right) \\
&= -\log(n) + \log(\sqrt{2\pi(n-1)}) - \log\left(\exp\left(-\frac{n-1}{2}\right)\right) \\
&= -\log(n) + \frac{1}{2}\log(2\pi(n-1)) + \frac{n-1}{2} \\
&= \frac{n-1}{2} + \frac{1}{2}\log(2\pi(n-1)) - \log(n)
\end{align}$$

which is $O(n)$, again:

![](res/gauss/neglnpdf1.svg)

We can confirm this trend by computing the information using the real
assigned probabilities with $CDF(x)|_{x-0.5}^{x+0.5}$ for the first few
cases:

![](res/gauss/outliercasecodelength.svg)

with a slope of at little under $2$ bits per number once the line
settles around $n \sim 20$. This is subjectively tolerable behavior
under so-called "worst" conditions.

## An Abstract Arithmetic Coding Interface

An implementation of arithmetic coding that works with *any* model is
more abstract than what you typically find online. Here is my
implementation in Rust:

- [Source (GitHub)](https://github.com/nbos/cont-arith-code)
- [Documentation](res/doc/cont_arith_code/index.html)

Traits are defined for a "model" that emits "distributions" which are
repeatedly truncated at cumulative probabilites $\in (0,1)$ until it
resolves to a specific symbol $s$`:i64` which is fed back to the
"model", updating it, before requesting the next "distribution".

Through defining a limited interface:

```rust
type Index = i64;
pub trait Model<T> {
    fn next_distr(&mut self) -> Box<dyn UnivariateDistribution>;
    fn push(&mut self, s: Index) -> Option<T>;
}
```

```rust
pub trait UnivariateDistribution {
    fn truncated(&self) -> Box<dyn TruncatedDistribution>;
}
```

```rust
pub trait TruncatedDistribution {
    fn quantile(&self, cp: f64) -> (Index, f64); // returns (s, s_rem)
    fn truncate(&mut self, cp: f64, s: Index, s_rem: f64, bit: bool);
    fn lo(&self) -> Index; // symbol the lower-bound is in
    fn hi(&self) -> Index; // symbol the upper-bound is in
    fn is_resolved(&self) -> bool { self.lo() == self.hi() }
}
```

the algorithm handles the composition (i.e. nesting) of distributions in
the code-space, with interfaces for serialization and de-serialization
of values.

An
[implementation](res/doc/cont_arith_code/distribution/categorical/struct.Categorical.html)
for categorical distributions is defined for a typical use-case of
arithmetic coding.

## Gaussian Implementation
### Tackling Numerical Instability

The quantile function for Gaussians is continuous, one-to-one, monotone
and has finite value everywhere except at $0 \mapsto -\infty$ and $1
\mapsto \infty$.

![](res/gauss/cdfquantile.svg)

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

![](res/gauss/pdfdiff.svg)

While both flatten out at the tails, for any given interval in the
tails, the relative difference becomes greater the further away you move
from the center. To see this, normalize the (absolute) derivative to the
value of the function:

![](res/gauss/pdfdiffnorm.svg)

That is, the tails may be flat in absolute terms, but they become
steeper relative to themselves the further away you go. Another way to
demonstrate this is by blowing up the PDF at different scales (here,
successive factors of 10):

![](res/gauss/pdfscales.svg)

which makes it more clear why we cannot rely on linear interpolations in
the tails. We are forced to find an analytic or at least numeric
solution that is more faithful to the distribution.

### Tackling Numerical Instability (for real)

Like is usually the case in probability, the solution to numerical
instability is found in the
[log-domain](https://en.wikipedia.org/wiki/Log_probability). This gives
us two analogous functions for the cumulative probability with more
manageable shapes:

![](res/gauss/logcdfquantileexp.svg)

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
of the $\log_2$-probabilities of each integer in the distribution. The
*expected code length* is that value rounded up. *Code length* is the
empirical result. All codes decode back to the encoded values.

### Degenerate Case

Integer sets with a single value produce empty codes:
```python
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
```python
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

### Outlier Case

Outlier cases like this one ($n = 10$):
```python
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

![](res/gauss/outlierresults.svg)

which is not optimal everywhere, but good enough.

### Random Samples

The error becomes less noticeable as we move to sets containing more
information. Here we sample $n$ elements from a
[**uniform**](https://en.wikipedia.org/wiki/Continuous_uniform_distribution)
distribution between -5 and 5, once for each $n$:

![](res/gauss/randomuniform.svg)

Seemingly identical performance is obtained when sampling from a
[**normal**](https://en.wikipedia.org/wiki/Normal_distribution)
distribution with the same variance $(\sigma^2 = \frac{10^2}{12} =
8.\overline{3})$:

![](res/gauss/randomnormal.svg)

Sampling from any wider distribution produces code lengths closer to the
information content than is visually distinguishable.
