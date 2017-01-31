---
title: Strassen's Fast Matrix Multiplication Algorithm
date: 2017-01-31
---

This semester I'm TAing the course CSC373: Algorithm Design, Analysis, and
Complexity at UofT. Before I give a tutorial, I'll usually write up a set of
tutorial notes. Whenever I can, I'll post a (slightly) cleaned-up version of my
notes here.

Today's topic is algorithms for fast matrix multiplication. Given two
$n \times n$ matrices $A$ and $B$ with entries $a_{ij}, b_{ij} \in \mathbb{R}$,
the matrix $AB = C$ has entries $c_{ij}$ defined by

$$c_{ij} = \sum_{k=1}^n a_{ik}b_{kj}.$$

Given two $n \times n$ matrices $A$ and $B$, it is straightforward to compute
their product $AB$, namely, by simply using this formula iteratively to compute
each entry in the product individually.

How quickly does this simple algorithm run? We'll measure complexity in terms of
the number of arithmetic operations performed. To compute a single entry in $C$,
we perform $n$ multiplications, and $n$ addition operations, so $2n$ arithmetic
operations. Since $C$ is an $n \times n$ matrix, there are $n^2$ entries we need
to compute, so the total running time is $2n \cdot n^2 = 2n^3 = \Theta(n^3)$.

Simple Divide and Conquer Algorithm
-----------------------------------

Based on the definition of matrix multiplication (from the formula), it seems
like $\Theta(n^3)$ is the best running time we can hope for, for any algorithm
for multiplying two square matrices. It turns out that we can, in fact, do
better, by applying the divide and conquer strategy.

For simplicity, we'll assume that the inputs $A$ and $B$ are both $n \times n$
matrices, and that $n$ is always a power of 2. This will make things easier,
since it ensures that we can always divide an $n \times n$ matrix neatly into
four equally-sized $\frac{n}{2} \times \frac{n}{2}$ matrices. It's okay to
assume this, since we can always pad our matrices with zeros until $n$ is a
power of 2, while increasing the size of each matrix by a factor of at most 4.

The main idea is that we view matrix multiplication in terms of block matrices:

$$
\begin{pmatrix}A_{11} & A_{12} \\ A_{21} & A_{22}\end{pmatrix}
\begin{pmatrix}B_{11} & B_{12} \\ B_{21} & B_{22}\end{pmatrix}
= \begin{pmatrix}C_{11} & C_{12} \\ C_{21} & C_{22}\end{pmatrix},
$$

where each submatrix $A_{ij}$ is an $\frac{n}{2} \times \frac{n}{2}$ matrix. It
turns out that we can compute a product of block matrices by applying the usual
formula, but using matrix multiplication (of the smaller matrices) rather than
by multiplying and adding numbers, so that
$C_{11} = A_{11}B_{11} + A_{12}B_{12}$ and so on. This leads to a natural
recursive algorithm.

For the base case, the product of two $1 \times 1$ matrices is just
$\begin{pmatrix}a\end{pmatrix}\begin{pmatrix}b\end{pmatrix} = \begin{pmatrix}ab\end{pmatrix}$.

Otherwise, given two $n \times n$ matrices, partition each of them into four
$\frac{n}{2} \times \frac{n}{2}$ matrices as above. Perform 8 matrix
multiplications recursively, and combine the results:

$$
\begin{pmatrix}A_{11} & A_{12} \\ A_{21} & A_{22}\end{pmatrix}
\begin{pmatrix}B_{11} & B_{12} \\ B_{21} & B_{22}\end{pmatrix}
= \begin{pmatrix}
    A_{11}B_{11} + A_{12}B_{12} & A_{11}B_{12} + A_{12}B_{22} \\
    A_{21}B_{11} + A_{22}B_{21} & A_{21}B_{12} + A_{22}B_{22}
\end{pmatrix}.
$$

### Running Time

Let $T(n)$ be the number of steps that this algorithm takes to multiply two
$n \times n$ matrices. The base case is simple; it's a single arithmetic
operation, so $T(1) = O(1)$.

For the recursive case, we perform 8 recursive calls on
$\frac{n}{2} \times \frac{n}{2}$ matrices. We also do $\Theta(n^2)$ extra work
to combine (via matrix addition) the results of these subproblems. This gives us
the recurrence relation

$$
T(n) = \begin{cases}
\Theta(1) & \text{ if } n = 1, \\
8T(n/2) + \Theta(n^2) & \text{ if } n > 1.
\end{cases}
$$

However, if we solve this recurrence using the master theorem, we get
$T(n) = \Theta(n^{\log_2{8}}) = \Theta(n^3)$, which is the same running time as
the simple iterative algorithm. So why even bother with any of this divide and
conquer business?

Strassen's Algorithm (CLRS 4.2)
-------------------------------

Key idea behind Strassen's algorithm: Using a bit (a lot) of matrix algebra, we
can reduce the number of recursive calls from 8 down to 7. This will reduce our
running time to $\Theta(n^{\log_2{7}}) \approx \Theta(n^{2.8074})$.

Strassen's algorithm involves four high-level steps, given $n \times n$ matrices
$A$ and $B$:

1. Partition $A$ and $B$ into $\frac{n}{2} \times \frac{n}{2}$ submatrices. This
takes $\Theta(1)$ time using indices (or $\Theta(n^2)$ by copying entries).
2. Create 10 $\frac{n}{2} \times \frac{n}{2}$ matrices $S_1,\dots,S_{10}$ by
adding and subtracting the submatrices from step 1. This requires $\Theta(n^2)$
time.
3. Using the submatrices in step 1 and the 10 matrices from step 2, recursively
compute 7 matrix products $P_1,\dots,P_7$.
4. Compute $C_{11}, C_{12}, C_{21}, C_{22}$ by adding and subtracting
combinations of the $P_i$ matrices. $\Theta(n^2)$ time.

Now for the details, here are the matrices that we compute in step 2:
\begin{align*}
S_1 = B_{12} - B_{22} \qquad& S_2 = A_{11} + A_{12} \\
S_3 = A_{21} + A_{22} \qquad& S_4 = B_{21} - B_{11} \\
S_5 = A_{11} + A_{22} \qquad& S_6 = B_{11} + B_{22} \\
S_7 = A_{12} - A_{22} \qquad& S_8 = B_{21} + B_{22} \\
S_9 = A_{11} - A_{21} \qquad& S_{10} = B_{11} + B_{12}.
\end{align*}

Then in step 3, we compute the following matrices:
\begin{align*}
P_1 = A_{11}S_1 \qquad& P_2 = S_2B_{22} \\
P_3 = S_3B_{11} \qquad& P_4 = A_{22}S_4 \\
P_5 = S_5S_6    \qquad& P_6 = S_7S_8 \\
P_7 = S_9S_{10}. \qquad&
\end{align*}

In step 4, we combine the results of previous computations in the following way:
\begin{align*}
C_{11} = P_5 + P_4 - P_2 + P_6 \qquad& C_{12} = P_1 + P_2 \\
C_{21} = P_3 + P_4 \qquad& C_{22} = P_5 + P_1 - P_3 - P_7.
\end{align*}

Checking that this works out$\require{cancel}$: \begin{align*}
C_{21} &= P_3 + P_4 \\
&= S_3B_{11} + A_{22}S_4 \\
&= (A_{21} + A_{22})B_{11} + A_{22}(B_{21} - B_{11}) \\
&= A_{21}B_{11} + \cancel{A_{22}B_{11}} + A_{22}B_{21} - \cancel{A_{22}B_{11}} \\
&= A_{21}B_{11} + A_{22}B_{21}.
\end{align*}

The rest of the entries of $C$ an be checked similarly.

### Improvements

Strassen's algorithm gives us an asymptotic improvement over the $\Theta(n^3)$
iterative matrix multiplication algorithm. The bound $\Theta(n^{2.8074})$ seems
a bit unnatural, so one might ask "can we do better?" What's the best we could
possibly hope to do? Since the output is an $n \times n$ matrix, it'll take
$n^2$ steps to write down the output, so any algorithm for multiplying two $n
\times n$ matrices has a running time of at least $\Omega(n^2)$.

Over the years, since Strassen's algorithm was first published, computer
scientists have developed faster and faster matrix multiplication algorithms,
gradually bringing down the exponent over time. The following table gives us an
idea of how upper bounds on the complexity of matrix multiplication have evolved
over the years.

Author(s)                      Year  Upper bound
-----------------------------  ----  ------------------
Binet (?)                      1812  $O(n^3)$
Strassen                       1969  $O(n^{2.8074})$
Coppersmith--Winograd          1990  $O(n^{2.375})$
Andrew Stothers                2010  $O(n^{2.374})$
Virginia Vassilevska Williams  2013  $O(n^{2.3728642})$
Fran&ccedil;ois Le Gall        2014  $O(n^{2.3728639})$

Table: Increasingly better upper bounds on the complexity of matrix
multiplication.

As you can see, although we have seen improved bounds, progress has been
gradual, and we are still far from the original trivial lower bound of
$\Theta(n^2)$. In fact, this leads us to one of the major open problems in
theoretical computer science: Can we prove an upper bound that matches this
lower bound (i.e. by finding an algorithm that multiplies matrices in $O(n^2)$
time)? If not, can we prove an optimal lower bound?

In practice though (as far as I can tell; I'm not an expert on scientific
computing), we do not typically use algorithms that are faster than Strassen's
algorithm to actually multiply matrices. This is because the constants hidden in
the big O notation are so large that they only provide a noticeable speedup for
extremely huge matrices. There are other factors that need to be considered when
choosing a practical matrix multiplication algorithm as well, such as numerical
stability (due to the finite precision of floating point numbers), and cache
performance.

There also exist other ways of obtaining faster matrix multiplication algorithms,
such as exploiting shared-memory parallel processing and distributed computation
to multiply matrices, however, that's a topic for another day.
