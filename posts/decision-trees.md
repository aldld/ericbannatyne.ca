---
title: A Fun Counting Argument For Decision Trees
date: 2016-05-28
---

I'm currently in the process of writing a longer post about the Rivest-Vuillemin
theorem, which gives a lower bound for the number of queries required to
recognize certain types of graph properties, but I felt like it would be nice to
share a bit about a fun counting argument that I came across while reading up on
the result.

String Properties
-----------------

Although the Rivest-Vuillemin phrased in terms of properties of graphs, for now
we'll consider string properties instead. Recall that $\{0,1\}^n$ is the set of
all strings of length $n$ over the alphabet $\{0,1\}$. A *property* is a
function $f:\{0,1\}^n \to \{0, 1\}$ mapping strings to truth values.

### Decision Trees and Evasive Properties

Decision trees are one of the simplest concrete models of computation, enabling
us to study the complexity of recognizing string properties in terms of the
number of queries used. If you haven't seen decision trees before, they're
exactly what you might expect: Each internal node queries a particular bit $x_i$
of the input $x = (x_1, \dots, x_n) \in \{0,1\}^n$, and has two children
corresponding to the results $x_i = 0$ and $x_i = 1$. The computation on some
input $x$ is the path obtained by starting at the root, and continuing down the
tree by answering each query, until we reach a leaf, labeled either $0$ or $1$,
which tells us the output of the decision tree.

Just like with Turing machines and other models of computation, there are many
different variants of decision trees, such as nondeterministic and randomized
decision trees, each with their own interesting properties. For now though we
will only focus on regular (deterministic) decision trees.

The depth of a decision tree is the longest path from the root to a leaf. This
is the number of queries that a decision tree must make on some input in the
worst case. The decision tree complexity $D(f)$ of a function
$f:\{0,1\}^n \to \{0,1\}$ is the depth of the shallowest (i.e. most efficient)
decision tree computing $f$.

It's not hard to see that $D(f) \leq n$ for any function $f$, since a decision
tree that queries all $n$ bits on every input can simply hardcode the correct
answer for every possible input of length $n$. We say that a property $f$ is
*evasive* if $D(f) = n$. That is, recognizing $f$ requires inspecting every bit
of the input in the worst case.

A simple example of an evasive string property is parity; for any
$x \in \{0,1\}^n$, $PAR_n(x) = 1$ if and only if the number of 1 bits in $x$ is
odd. It's easy to see that if a decision tree only queries $n - 1$ bits of $x$
or fewer, then the parity of $x$ is determined by the remaining bits, which the
decision tree has not queried, and so it can't determine the correct value.
Therefore $D(PAR_n) = n$, so parity is an evasive string property.

A Nice Counting Argument
------------------------

While reading [Jeff Erickson's lecture notes](http://jeffe.cs.illinois.edu/teaching/497/05-evasive.pdf),
I came across a nice, short result regarding evasive string properties:

> **Lemma.** Let $f:\{0,1\}^n \to \{0,1\}$ be a string property. If there is an
> odd number of strings $x$ such that $f(x) = 0$, then $f$ is evasive.

*Proof:* Fix some decision tree recognizing such a string property $f$, and let
$v$ be some node in the decision tree whose depth is at most $n - 1$; namely,
not all bits of the input have been queried on that path. Then there must be an
even number of strings that reach $v$, since there are an even number of
possible configurations of the remaining, unexamined bits, all of which must
reach $v$. Since each input string reaches exactly one leaf, if there is an odd
number of strings that reach a leaf labeled 0, then some leaf must only be
reached by one input. This path must therefore query all $n$ bits of the input.

