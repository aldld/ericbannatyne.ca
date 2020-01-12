---
title: Polynomial Multiplication via Fast Fourier Transforms
date: 2017-02-04
---

*This post is based on an advanced tutorial for CSC373: Algorithm Design,
Analysis, and Complexity, which I presented on January 30, 2017. For more
information, check out the course textbook [@clrs2009], and [@dpv2008].*
$\newcommand{\DFT}{\operatorname{DFT}}$

If you've ever done anything involving signal processing or data analysis,
there's a good chance that you've heard of the fast Fourier transform.
Originally explored to solve some specific engineering problems, and first
published in its general form---as we know it today---by James Cooley and John
Tukey in 1965 [@cooley1965algorithm], the fast Fourier transform is an
incredibly famous algorithm that has tons of applications in areas like digital
signal processing, speech recognition, data compression, communications, and
computer vision, to name a few.

The goal of this post is to introduce the fast Fourier transform, by looking
specifically at how it can be applied to obtain efficient algorithms for
polynomial multiplication.


Polynomial Multiplication
-------------------------

Suppose that we are given two polynomials of degree $n - 1$:[^1]

\begin{align*}
A(x) &= a_0 + a_1x + a_2x^2 + \dots + a_{n-1}x^{n-1} \\
B(x) &= b_0 + b_1x + b_2x^2 + \dots + b_{n-1}x^{n-1}.
\end{align*}

[^1]: For simplicity, we'll assume that $n$ is a power of 2.

We wish to find the product of $A$ and $B$, a polynomial $C$ such that $C(x) =
A(x)B(x)$. Suppose that we represent $A$ as a vector of its $n$ coefficients $a
= (a_0,\dots,a_{n-1})$, and likewise $B$ with $b = (b_0,\dots,b_{n-1})$. Then
there is a straightforward algorithm for computing the coefficients of $C$, by
simply expanding out the algebraic expression $A(x)B(x)$ and collecting like
terms---essentially what you would have done in your high school algebra class.
This procedure takes $\Theta(n^2)$ operations, since we're essentially
multiplying all pairs of coefficients from $A$ and $B$. In the rest of this
post, we'll derive a faster method of computing the product $C$.

As an aside, the algorithm for polynomial multiplication that we're about to
discover also forms the basis for what were, until recently, some of the
best-known algorithms for *integer multiplication*. If you think about it a bit,
this should make some intuitive sense: You can think of the coefficients
$(a_0,\dots,a_{n-1})$ as representing the digits of an integer in a given base
(typically base 2), when this base is substituted for the variable $x$. Then
integer multiplication is kind of like polynomial multiplication, except that we
also have to take into account carry digits. Combining the fast Fourier
transform with several clever number theory tricks gives us the
[Schönhage–Strassen algorithm](https://en.wikipedia.org/wiki/Sch%C3%B6nhage%E2%80%93Strassen_algorithm),
which lets us multiply two $n$-digit integers in time $O(n\log{n}\log\log{n})$.

### Representing Polynomials

So far, we've assumed that a polynomial is represented by its list of
coefficients. However, the following fact will suggest an alternative way of
representing polynomials.

*Fact.* A set of $n$ point-value pairs (say in $\mathbb{R}^2$ or $\mathbb{C}^2$)
uniquely determines a polynomial of degree $n - 1$.

This is essentially a generalization of the statement "given two points in the
plane, there is a unique line that passes through them." Proving this is an
exercise in linear algebra; given $n$ point-value pairs, we can compute the
coefficients of a polynomial passing through those points by solving a system of
linear equations.[^2]

[^2]: This system of equations is defined using a Vandermonde matrix, which
we'll see in more detail later.

This leads us to the *point-value representation* of a polynomial: Given
points $x_0,\dots,x_{n-1} \in \mathbb{C}$, a degree-$n-1$ polynomial $A(x)$ can
be represented by the set


$$\{(x_0, A(x_0)), (x_1, A(x_1)), \dots, (x_{n-1}, A(x_{n-1}))\}.$$

Using this representation, it's now much easier to multiply two polynomials $A$
and $B$, by simply multiplying their values on the sample points
$x_0,\dots,x_{n-1}$ pointwise:

$$
\label{eqn:bad}
\{(x_0, A(x_0)B(x_0)), (x_1, A(x_1)B(x_1)), \dots, (x_{n-1}, A(x_{n-1})B(x_{n-1}))\}. \tag{$\star$}
$$

Therefore, if we use the point-value representation for polynomials, then we can
multiply two polynomials of degree $n - 1$ using only $\Theta(n)$ arithmetic
operations, instead of the original $\Theta(n^2)$.

However, there's still a slight problem: If $A(x)$ and $B(x)$ are both
polynomials of degree $n - 1$, then their product will be a polynomial $C(x) =
A(x)B(x)$ of degree $n - 1 + n - 1 = 2n - 2$. But the result computed by
$\eqref{eqn:bad}$ only contains $n$ sample points; not the $2n - 1$ points
needed to represent a polynomial of degree $2n - 2$.

To fix this, we can represent $A$ and $B$ using an "extended" point-value
representation, by using $2n$ point-value pairs, instead of just[^3] $n$.
Asymptotically, this doesn't affect our overall running time, since it only
increases our input size by a constant factor of 2.

[^3]: We use $2n$ point-value pairs instead of just $2n-1$, since if $n$ is a
power of 2, then so is $2n$. This will be useful later.

### Plan of Attack

It's great that we can multiply polynomials in linear time, using their point-
value representation, but we'd *really* like to do this using their coefficient
representation. This gives us the outline of our algorithm for multiplying
polynomials: Given two polynomials $A$ and $B$ in coefficient form, convert them
to point-value form by *evaluating* them at $2n$ points, then multiply them
pointwise in linear time, and finally convert the result back to coefficient
form, via a process called interpolation.

<div class="figure">
<img src="../../images/polynomial-fast-fourier-transform/fft-poly.png" alt="Outline of the approach to efficient polynomial multiplication using the fast Fourier transform."
    style="width:100%">
<p class="caption">Outline of the approach to efficient polynomial
multiplication using the fast Fourier transform.</p>
</div>

In general, evaluating a polynomial $A$ at a single point $x$ takes $\Theta(n)$
operations, and since we have to evaluate $2n$ points, the evaluation step takes
time $\Theta(n^2)$. So it seems like we still aren't doing any better than the
ordinary multiplication algorithm.

The key point is that the evaluation step will take $\Theta(n^2)$ operations if
we just choose any old points $x_0,\dots,x_{2n-1}$. But for a particular choice
of points, namely, the complex roots of unity, we'll see how we can evaluate $A$
on that set of points using only $\Theta(n\log{n})$ operations, using the fast
Fourier transform. It'll also be possible to use the FFT to do the interpolation
step, once we have the product of $A$ and $B$ in point-value form.


Complex Roots of Unity
----------------------

A number $z \in \mathbb{C}$ is an *$n$th root of unity* if $z^n = 1$. The
*principal* $n$th root of unity is $\omega_n = e^{\frac{2\pi i}{n}}$. For $n
\geq 1$, the $n$ $n$th roots of unity are
$\omega_n^0,\omega_n^1,\dots,\omega_n^{n-1}$. For a bit of geometric intuition,
the $n$th roots of unity form the vertices of a regular $n$-gon in the complex
plane. We'll use a few basic properties of complex roots of unity.

**Lemma.** (Cancellation lemma)
For integers $n \geq 0$, $k \geq 0$, $d > 0$, we have $\omega_{dn}^{dk} = \omega_n^k$.

*Proof.*
$\omega_{dn}^{dk} = \left(e^{\frac{2\pi i}{dn}}\right)^{dk} = e^{\frac{2\pi i k}{n}} = \omega_n^k.$

**Corollary.** (Halving lemma)
If $n > 0$ is even, and $k > 0$, we have $(\omega_n^k)^2 = \omega_{n/2}^k$.

The proof essentially amounts to applying the cancellation lemma with $d = 2$.
What halving lemma means though, is that if $n$ is even, then squaring each of
the $n$ complex $n$th roots of unity gives us exactly the $\frac{n}{2}$ complex
$\frac{n}{2}$th roots of unity.

Note that squaring all of the $n$th roots of unity gives us each $\frac{n}{2}$th
root of unity twice, since $(\omega_n^{k+n/2})^2 = \omega_n^{2k+n} =
\omega_n^{2k}\omega_n^n = (\omega_n^k)^2$.

**Lemma.** (Summation lemma)
If $n \geq 1$ and $k$ is not divisible by $n$, then

$$
\sum_{j=0}^{n-1} (\omega_n^k)^j = 0.
$$

*Proof.*
Using the formula for the sum of a geometric series,

$$
\sum_{j=0}^{n-1} (\omega_n^k)^j = \frac{(\omega_n^k)^n - 1}{\omega_n^k - 1}
= \frac{(\omega_n^n)^k - 1}{\omega_n^k - 1} = \frac{1^k - 1}{\omega_n^k - 1} = 0.
$$

Since $k$ is not divisible by $n$, we know that $\omega_n^k \neq 1$, so that the
denominator is not zero.

The Discrete Fourier Transform for Polynomial Evaluation
--------------------------------------------------------

Now we are ready to define the discrete Fourier transform, and see how it can be applied to the
problem of evaluating a polynomial on the complex roots of unity.

**Definition.** Let $a = (a_0,\dots,a_{n-1}) \in \mathbb{C}^n$. The discrete
Fourier transform of $a$ is the vector $\DFT_n(a) =
(\hat{a}_0,\dots,\hat{a}_{n-1})$, where[^4]

$$
\hat{a}_k = \sum_{j=0}^{n-1}
a_je^{\frac{2\pi ikj}{n}} = \sum_{j=1}^{n-1} a_j\omega_n^{kj} \qquad 0 \leq k
\leq n - 1.
$$

[^4]: If you've seen Fourier transforms before in the context of signal
processing or other areas, you've probably seen it defined in terms of
$\omega_n^{-1}$ rather than $\omega_n$. The form used here simplifies matters a
bit for our specific application, and the math is almost the same either way.

If you look closely, you can see that this definition is in fact exactly what we
want. Let $A(x) = a_0 + a_1x + a_2x^2 + \dots + a_{n-1}x^{n-1}$ be the
polynomial whose coefficients are given by $a$. Then for $0 \leq k \leq n - 1$,
the polynomial $A$ evaluated at the root of unity $\omega_n^k$ is exactly

$$
A(\omega_n^k) = \sum_{j=0}^{n-1} a_j(\omega_n^k)^j = \hat{a}_k.
$$

Here, the $n$ is really the $2n$ from previous sections, however we will simply
use $n$ to denote the length of the vector $a$ when describing the DFT to
simplify matters.

So far though, we still haven't gained much, since applying this definition and
directly computing the sum for each $k$ takes $\Theta(n^2)$ operations. This is
where the fast Fourier transform comes in: this will allow us to compute
$\DFT_n(a)$ in time $\Theta(n\log{n})$.

Fast Fourier Transform
----------------------

The fast Fourier transform is an algorithm for computing the discrete Fourier
transform of a sequence by using a divide-and-conquer approach.

As always, assume that $n$ is a power of 2. Given $a = (a_0,\dots,a_{n-1}) \in
\mathbb{C}^n$, we have the polynomial $A(x)$, defined as above, which has $n$
terms. We can also define two other polynomials, each with $\frac{n}{2}$ terms,
by taking the even and odd coefficients of $A$, respectively:

\begin{align*}
A_e(x) &= a_0 + a_2x + a_4x^2 + \dots + a_{n-2}x^{\frac{n}{2} - 1}, \\ A_o(x) &=
a_1 + a_3x + a_5x^2 + \dots + a_{n-1}x^{\frac{n}{2} - 1}.
\end{align*}

Then, for any $x \in \mathbb{C}$, we can evaluate $A$ at $x$ using the formula

$$
A(x) = A_e(x^2) + xA_o(x^2). \tag{$*$} \label{eqn:comb}
$$

So the problem of evaluating $A$ at $\omega_n^0, \omega_n^1, \dots,
\omega_n^{n-1}$ reduces to performing the following steps:

1. Evaluate the two polynomials $A_e$ and $A_o$ of degree $\frac{n}{2} - 1$ at
the points $(\omega_n^0)^2, (\omega_n^1)^2, \dots, (\omega_n^{n-1})^2$.
2. Use the formula $\eqref{eqn:comb}$ to combine the results from step 1.

By the halving lemma, we only actually need to evaluate $A_e$ and $A_o$ at
$\frac{n}{2}$ points, rather than at all $n$ points. Thus, at each step, we
divide the initial problem of size $n$ into two subproblems of size
$\frac{n}{2}$.

Now that we have the general idea behind how the FFT works, we can look at some
pseudocode.[^5]

[^5]: Thanks to [jQuery Pseudocode](http://davidstutz.github.io/jquery-pseudocode/)
by [David Stutz](http://davidstutz.de/), for excellent pseudocode formatting
script.

<pre class="pseudocode">
function Fast-Fourier-Transform(\(a\))

    \(n = a.\mathit{length}\)
    if \(n = 1\)
        return \(a\)

    \(a_e = (a_0, a_2, \dots, a_{n-2})\)
    \(a_o = (a_1, a_3, \dots, a_{n-1})\)

    \(y_e =\) Fast-Fourier-Transform(\(a_e\))
    \(y_o =\) Fast-Fourier-Transform(\(a_o\))

    \(\omega_n = e^{\frac{2\pi i}{n}}\)
    \(\omega = 1\)

    for \(k = 0\) to \(\frac{n}{2} - 1\)
        \(y_k = (y_e)_k + \omega(y_0)_k\)
        \(y_{k + \frac{n}{2}} = (y_e)_k - \omega(y_o)_k\)
        \(\omega = \omega\omega_n.\) // \(\omega = \omega_n^{k+1}\)

    \(y = (y_0,\dots, y_{n-1})\)
    return \(y\)
</pre>

The pseudocode more or less follows the algorithmic structure outlined above.
The main thing that we need to check is that the for loop actually combines the
results using the correct formula. For $k = 0,1,\dots, \frac{n}{2}-1$, in the
recursive calls we compute, by the cancellation lemma,

\begin{align*}
(y_e)_k =
A_e(\omega_{n/2}^k) = A_e(\omega_n^{2k}), \\ (y_o)_k = A_o(\omega_{n/2}^k) =
A_o(\omega_n^{2k}).
\end{align*}

In the for loop, for each $k = 0,\dots, \frac{n}{2} - 1$, we compute

$$
y_k = A_e(\omega_n^{2k}) + \omega_n^kA_o(\omega_n^{2k}) = A(\omega_n^k).
$$

We also compute

\begin{align*}
y_{k+\frac{n}{2}} &= A_e(\omega_{n/2}^k) - \omega_n^kA_o(\omega_{n/2}^k) \\
&= A_e(\omega_n^{2k}) + \omega_n^{k+\frac{n}{2}}A_o(\omega_n^{2k}) \\
&= A_e(\omega_n^{2k+n}) + \omega_n^{k+\frac{n}{2}}A_o(\omega_n^{2k+n}) \\
&= A(\omega_n^{k+\frac{n}{2}}).
\end{align*}

Therefore $y_k = A(\omega_n^k) = \hat{a}_k$ for all $0 \leq k \leq n - 1$, and
therefore this algorithm returns the correct result. Moreover, let $T(n)$ denote
the running time of `Fast-Fourier-Transform(a)` when $a$ has length $n$. Since the
algorithm makes two recursive calls on subproblems of size $\frac{n}{2}$, and
uses $\Theta(n)$ steps to combine the results, the overall running time of this
algorithm is

$$
T(n) = 2T\left(\frac{n}{2}\right) + \Theta(n) = \Theta(n\log{n}).
$$

Inversion Formula and Interpolation via FFT
-------------------------------------------

Now that, given the coefficients of a polynomial, we can evaluate it on the
roots of unity by using the FFT, we want to perform the inverse operation,
interpolating a polynomial from its values on the $n$th roots of unity, so that
we can convert a polynomial from its point-value representation, back to its
coefficient representation.

The first idea here will be to observe that $\DFT_n:\mathbb{C}^n \to
\mathbb{C}^n$ is a linear map. This means that it can be written in terms of a
matrix multiplication:

$$
\begin{bmatrix}
1      & 1        & 1          & \cdots & 1              \\
1      & \omega_n & \omega_n^2 & \cdots & \omega_n^{n-1} \\
\vdots & \vdots   & \vdots     & \ddots & \vdots         \\
1      & \omega_n^{n-1} & \omega_n^{2(n-1)} & \cdots & \omega_n^{(n-1)^2}
\end{bmatrix}\begin{bmatrix}
a_0 \\ a_1 \\ \vdots \\ a_{n-1}
\end{bmatrix}
=
\begin{bmatrix}
\hat{a}_0 \\ \hat{a}_1 \\ \vdots \\ \hat{a}_{n-1}
\end{bmatrix}.
$$

We can denote the matrix on the left-hand side of this equation by
$M_n(\omega_n)$. This matrix has a special name in linear algebra, and is called
a Vandermonde matrix. To solve the problem of performing the reverse of the
discrete Fourier transform, and convert from the point-value representation to
the coefficient representation of a polynomial, we simply need to find the
inverse of this matrix. It turns out that the structure of this particular
matrix makes it particularly easy to invert.

**Theorem.** (Inversion formula)
For $n \geq 1$, the matrix $M_n(\omega_n)$ is invertible, and

$$
M_n(\omega_n)^{-1} = \frac{1}{n}M_n(\omega_n^{-1}).
$$

*Proof.* The $(j, j')$ entry of $M_n(\omega_n)$ is equal to $\omega_n^{jj'}$.
Similarly, the $(j, j')$ entry of $\frac{1}{n}M_n(\omega_n^{-1})$ is
$\frac{1}{n}\omega_n^{-jj'}$. Therefore, the $(j, j')$ entry of the product
$\frac{1}{n}M_n(\omega_n^{-1})M_n(\omega_n)$ is equal to

$$
\frac{1}{n}\sum_{k=0}^{n-1} \omega_n^{-kj}\omega_n^{kj'} = \frac{1}{n}\sum_{k=0}^{n-1} \omega_n^{k(j'-j)}.
$$

If $j' = j$, then this sum equals $n$, and so the entire expression evaluates to
1. On the other hand, if $j' \neq j$, then this sum equals 0, by the summation
lemma. Therefore $\frac{1}{n}M_n(\omega_n^{-1})M_n(\omega_n) = I_n$, the $n
\times n$ identity matrix, and so $M_n(\omega_n)^{-1} =
\frac{1}{n}M_n(\omega_n^{-1})$.

This key takeaway of this inversion formula is that it enables us to invert the
discrete Fourier transform by using the exact same algorithm as the fast Fourier
transform, but with $\omega_n$ replaced with $\omega_n^{-1}$, and then dividing
the entire result by $n$. Therefore it is also possible to perform polynomial
interpolation (from the complex roots of unity) in time $\Theta(n\log{n})$.

Putting it all together, this demonstrates how it is possible to multiply two
polynomials, by computing their values on the $2n$th roots of unity, doing
pointwise multiplication, and interpolating the result to obtain the
coefficients of the product. All of this takes time $\Theta(n\log{n})$, which is
a significant improvement over the naive $\Theta(n^2)$ algorithm.

References
----------
