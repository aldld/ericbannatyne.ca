---
title: The Rivest-Vuillemin Theorem, Part 1: Graph Properties and Scorpions
date: 2016-08-28
---

Lower bounds are the bread and butter of complexity theory. One in particular
that I remember hearing about, when I took a course on data structures during my
second year at UofT, was the Rivest-Vuillemin theorem, concerning the number of
queries needed to recognize a certain class of graph properties, when the graph
is represented by its adjacency matrix. However, we didn't have time to cover
the proof in class. So I've decided to read through
[Rivest and Vuillemin's original proof](https://people.csail.mit.edu/rivest/RivestVuillemin-OnRecognizingGraphPropertiesFromAdjacencyMatrices.pdf)
and try to summarize and understand it.

In a [previous post](http://ericbannatyne.ca/posts/decision-trees/), I discussed
a bit about the general problem of recognizing string properties, and the
concrete decision tree model of computation. We proved a very simple lower bound
that applies to any string property $f:\{0,1\}^n \to \{0,1\}$ that have an odd
number of strings $x$ such that $f(x) = 0$. That post gives some basic
definitions regarding string properties and decision trees, so if you haven't
already I suggest you give that a quick read over.

Let $P:\{0,1\}^n \to \{0,1\}$ be a string property. Recall that $D(P)$ (n.b. in
the original paper they use the notation $C(P)$) is the height of the shortest
decision tree recognizing $P$. The Rivest-Vuillemin theorem aims to resolve the
following conjecture known as the Aanderaa-Rosenberg conjecture (as stated in
the original paper):

> In the worst case, $\Omega(v^2)$ operations are required to determine from the
> adjacency matrix of a graph $G$ whether it has a property $P$ which is (i)
> nontrivial, (ii) monotone, (iii) independent of the labellings of the
> vertices, and (iv) independent of the existence of self-loops.

The condition (i) that $P$ be nontrivial simply means that $P$ is not always
false and always true, like the property answering the question "Is $G$ a
graph?" Condition (ii), monotonicity, will be defined more formally later, but
for now you can think of it as the condition that adding an edge to $G$ does not
change $P$ from being true to being false. Connectivity is an example of a
monotone graph property: If you start with a connected graph and add a new edge,
the resulting graph is still connected.

Condition (iii) is pretty self-explanatory: The kinds of graph properties we
care about are those that tell us something about the structure of the graph
itself, rather than how we (arbitrarily) choose to name the vertices. In formal
terms, this says that $P$ should be *invariant under graph isomorphisms*, for
which we will give a precise definition later.

Condition (iv) is mainly just a technical point that we don't really need to
worry about. In their paper, Rivest and Vuillemin focus mainly on undirected
graphs, which, under the standard definition, do not permit self-loops.

The rest of this post focuses on the first couple sections of Rivest and
Vuillemin's paper, which gives the basic definitions needed to approach the
problem, as well as some motivation for the theorem and why it can only hold
for monotone graph properties.

Some Basic Definitions
----------------------

To begin, I'm assuming that you have some basic idea of what a
[group](https://en.wikipedia.org/wiki/Group_(mathematics)#Definition_and_illustration)
is. The actual proof doesn't assume much knowledge of advanced group theory, but
you should at least be aware of the definition of a group. Some of the notation
and definitions can be stated more generally, however I will focus on the more
specialized definitions used in Rivest and Vuillemin's paper. Since these are
simply basic definitions, much of it will simply be verbatim from their paper.

In their paper, the symbol $\Sigma_d$ denotes the symmetric group of degree $d$,
consisting of all permutations (bijective functions from a set to itself) of the
set $\{1, 2, \dots, d\}$. For a group $\Gamma$, let $|\Gamma|$ denote the number
of elements in $\Gamma$, and $\Gamma_1 \leq \Gamma_2$ is a shorthand for
"$\Gamma_1$ is a subgroup of $\Gamma_2$".

A permutation group $\Gamma$ acting on the set $\{1,\dots,d\}$ is transitive if
for any pair of integers $i$ and $j$ in $\{1,\dots,d\}$, we can find some
permutation $\sigma \in \Gamma$ that maps $i$ to $j$. (Symbolically,
$\sigma(i) = j$.)

I'm going to define a bit of shorthand to make some equations a bit cleaner. Let
$x$ be a string in $\{0,1\}^d$, and let $\sigma \in \Sigma_d$ be a permutation
mapping the set $\{1,\dots,d\}$ to itself. If $x = x_1x_2\dots x_d$, we can
apply the permutation $\sigma$ to $x$ by defining

$$\sigma(x) = \sigma(x_1x_2\dots x_d) = x_{\sigma(1)}x_{\sigma(2)}\dots x_{\sigma(d)}.$$

That is, $\sigma(x)$ is obtained from $x$ by permuting the $d$ symbols in $x$
according to the permutation $\sigma$.

If $P:\{0,1\}^d \to \{0,1\}$ is some string property, then $\Gamma(P)$ denotes
the stabilizer of $P$, which is defined as the subgroup of all permutations
$\sigma \in \Sigma_d$ such that $P(\sigma(x)) = P(x)$ for all strings
$x \in \{0,1\}$.

If $x \in \{0,1\}^d$ and $\Gamma$ is a subgroup of $\Sigma_d$, the orbit,
denoted by $x\Gamma$ (using Rivest and Vuillemin's notation) under the action of
$\Gamma$ on $\{0,1\}^d$ is the collection of all strings $y \in \{0,1\}^d$ such
that $x = \sigma(y)$ for some permutation $\sigma \in \Gamma$.

The authors add a note that if $y \in x\Gamma(P)$ then $P(x) = P(y)$, however
the converse is not true in general. They also say that a string property
$P:\{0,1\}^d \to \{0,1\}$ is transitive whenever the stabilizer $\Gamma(P)$ is
transitive.

Given two binary strings $x = x_1x_2\dots x_d$ and $y = y_1y_2\dots y_d$ both
in $\{0,1\}^d$, we write $x_i \leq y_i$ for all $i \in \{1,\dots,d\}$. That is,
$x \leq y$ means that $y$ can be obtained from $x$ by flipping some $0$ bits in
$x$ to be $1$ (but not flipping any $1$'s to $0$'s).

A string property $P:\{0,1\}^d \to \{0,1\}$ is *monotone* if for all
$x,y \in \{0,1\}^d$, if $x \leq y$ then $P(x) \leq P(y)$. The *weight* $w(x)$ of
a boolean string $x$ is the number of ones in $x$.

### Graph Properties

We've talked before about string properties: functions that map boolean strings
in $\{0,1\}^d$ for some fixed $d$ to truth values in $\{0,1\}$; now we'd like to
define the more specialized notion of graph properties.

Let $G = (V, E)$ be an undirected graph. The adjacency matrix $x$ for $G$ is a
boolean string of length $\binom{v}{2}$, with one position for every possible
edge between two distinct vertices $i,j \in V$ that has value $1$ iff
$\{i,j\} \in E$. For simplicity, we may sometimes abuse notation by using a
graph $G$ and its adjacency matrix interchangeably.

Let $V^{(2)}$ denote the set of all 2-element subsets of $V$, so that
$E \subseteq V^{(2)}$.
We want to define the concept of graph isomorphisms within the framework
outlined by Rivest and Vuillemin so far: Recall that $\Sigma_v$ is the group of
all permutations acting on a set $V$ where $|V| = v$. Every element $\sigma$ of
$\Sigma_v$ is a permutation of the vertex set $V$, which can be extended to a
permutation of the edge set $E$ (or rather of V^{(2)}, since it's simpler to
define it in this way) by defining $\sigma({i,j}) = \{\sigma(i), \sigma(j)\}$.
Let $\Sigma_v^{(2)}$ be the group of all permutations of $V^{(2)}$ defined in
this way.

Two graphs $G = (V, E)$ and $G' = (V, E')$ defined on the same vertex set are
isomorphic if there exists a permutation $\sigma \in \Sigma_v^{(2)}$ such that
${i,j} \in E$ if and only if $\sigma({i,j}) \in E'$. The permutation $\sigma$ is
called a *graph isomorphism*.

Let $d = \binom{v}{2}$. A boolean function $P:\{0,1\}^d \to \{0,1\}$ is a graph
property if $P(G) = P(G')$ whenever $G$ and $G'$ are isomorphic graphs. A more
succinct way of writing this is that $P$ is a graph property exactly when
$\Sigma_v^{(2)} \leq \Gamma(P)$. Intuitively, this means that the property $P$
depends only on the actual structure of the graph, and not on how we choose to
label the vertices. At the end of this section, Rivest and Vuillemin note that
graph property is transitive.

### Scorpions: Why Monotonicity Matters

As an aside, now that we have some basic definitions, let's step back and look
at the Aanderaa-Rosenberg conjecture: that recognizing any nontrivial monotone
graph property requires $\Omega(v^2)$ edge queries in the worst case, when the
graph is represented using an adjacency matrix.

Before moving on to the proof of the Rivest-Vuillemin theorem, it is helpful to
look at the conditions that are necessary for the result to hold. Clearly a
graph property must be nontrivial in order for the $\Omega(v^2)$ lower bound to
hold, since a trivial property like "Is $G$ a graph?" can be answered without
querying any edges.

What about monotonicity? Rosenberg's original conjecture in 1973 did not include
the requirement that the graph property $P$ be monotone, but simply that $P$ is
a nontrivial graph property. However, in the case of directed graphs (which we
hadn't really considered so far), the property of having a sink, which is
nontrivial and nonmonotone, can be tested in $O(v)$ queries. By a sink in a
directed graph, we mean a vertex $v$ such that every other vertex has an edge
going into $v$, and there are no edges going out of $v$.

The case of undirected graphs is a bit more complicated, however there is also
a standard example showing that Rosenberg's original conjecture does not hold
for nonmonotone properties of undirected graphs.

Given a graph $G = (V, E)$, we say that $G$ is a scorpion graph if it contains
vertices $u, v, w \in V$ such that $u$'s only neighbour is $v$, $v$'s only
neighbours are $u$ and $w$, and $w$ is adjacent to every other vertex in $V$
except for $u$.

![A scorpion graph. The vertices $u$, $v$, and $w$ are drawn at the top, forming the tail and stinger of the scorpion. Source: [Wikimedia Commons](https://commons.wikimedia.org/wiki/File:Scorpion_graph.svg)](/images/rivest-vuillemin/Scorpion_graph.svg)

It's not hard to see that the property of being a scorpion is nontrivial and
nonmonotone. What's less obvious is that there is an algorithm that decides
whether a graph is a scorpion or not, using only $O(v)$ edge queries of the
graph's adjacency matrix.

Finding the actual algorithm is left as an exercise for the reader, partly
because it is highly nontrivial, and partly because this was given to my class
as a homework problem when I took UofT's data structures course, and I wouldn't
want to spoil the answer for future students. As a warmup, you might want to try
and find an $O(v)$-time algorithm that determines whether a graph $G$ is a
scorpion when $G$ is represented as an adjacency list, rather than an adjacency
matrix.

We now have some basic definitions as well as motivation for the conditions of
the Rivest-Vuillemin theorem. Next time, we'll dive deeper into their paper,
focusing on the next section in which the authors develop techniques for
studying the decision tree complexity (or as they call it, the argument
complexity) of arbitrary string properties.














