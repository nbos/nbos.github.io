---
title: "Counting Trees"
author: Nathaniel Bos
date: 2026-11-24
---

One may encounter, in the field of [formal
languages](https://en.wikipedia.org/wiki/Formal_language), the language
of balanced strings of brackets

$$\epsilon,\,\texttt{()},\,\texttt{(())},\,\texttt{()()},
\,\texttt{((()))},\,\texttt{(()())},\,...$$

named the [Dyck language](https://en.wikipedia.org/wiki/Dyck_language).

Strings from this language, called **Dyck words**, having the same
length $2n$ always have a matching set of $n$ left-brackets and $n$
right-brackets.

The number of Dyck words of a given length $n$ is given by the
$n$<sup>th</sup> [Catalan
number](https://en.wikipedia.org/wiki/Catalan_number), defined as:

$$\begin{align}
C_n &= \frac{1}{n+1}{2n \choose n}\\[5pt]
	&= \frac{1}{n+1}\frac{(2n)!}{n!\,n!}\\[5pt]
	&= \frac{(2n)!}{(n+1)!\,n!}
\end{align}$$

a function on integers $n \ge 0$, making the rapidly growing
[series](https://oeis.org/A000108):

```
+-----+---+---+---+---+----+----+-----+-----+-----
| n   | 0   1   2   3    4   5     6     7    ...
+-----+---+---+---+---+----+----+-----+-----+-----
| C_n | 1   1   2   5   14   42   132   429   ...
+-----+---+---+---+---+----+----+-----+-----+-----
```

We are interested in these numbers since Dyck words have a one-to-one
mapping on different classes of trees. Being able to count them allows
us to rank and unrank---and therefore efficiently serialize
(i.e. compress)---tree data structures.

## Rooted, Ordered Trees

Rooted (as usually expected of trees) and ordered (meaning node order is
significant) trees map the most naturally onto Dyck words, by
interpreting the nesting of parentheses as parent and children in the
tree, with empty sets $\texttt{()}$ corresponding to leaves.

| ![](res/trees/general/t1.svg)                   |
|:------------------------------------------------|
| $$\mathcal{D_3}\texttt{[0]} = \texttt{((()))}$$ |

The order of the concatenation of sub-Dyck expression is preserved as
order of sibling sub-trees. For $\mathcal{D_3}\texttt{[1,2,3]}$:

| ![](res/trees/general/t2.svg) | ![](res/trees/general/t3.svg) | ![](res/trees/general/t4.svg) |
|:-----------------------------:|:-----------------------------:|:-----------------------------:|
| $$\texttt{(()())}$$           | $$\texttt{(())()}$$           | $$\texttt{()(())}$$           |
|                               |                               |                               |

Although every pair of parentheses can be thought of as a node, the root
node does not appear as such in the word, but can be thought of as
implicit.

| ![](res/trees/general/t5.svg)                   |
|:------------------------------------------------|
| $$\mathcal{D_3}\texttt{[4]} = \texttt{()()()}$$ |

Which gives, for the class of ordered trees of $n+1$ nodes, a
correspondance to Dyck words of length $2n$, with cardinality given by
the Catalan number:

$$|\mathcal{D_n}| = C_n.$$

## Binary Trees

Binary trees, in the simplest sense, have for each node either a left
child, a right child, or both, unless the node is a leaf. This means
that we differentiate between a left- or right-child even if that node
has no siblings.

This class of trees maps onto non-binary trees (from the previous
section) through a so called ["left-child right-sibling
representation"](https://en.wikipedia.org/wiki/Left-child_right-sibling_binary_tree).

Every first child connection is introduced as a node to the left (or
root) of its parent and each successive sibling is introduced as a node
to the right (child) of its previous sibling.

| ![](res/trees/binary/t1.svg)                    | ![](res/trees/binary/t2.svg)                    |
|:-----------------------------------------------:|:-----------------------------------------------:|
| $$\mathcal{D_3}\texttt{[0]} = \texttt{((()))}$$ | $$\mathcal{D_3}\texttt{[1]} = \texttt{(()())}$$ |

In other words, to get a binary tree from a non-binary tree:

- discard the root node and its edges
- discard all edges besides those going from the first sibling to its
  parent (left-child)
- connect with an edge every sibling after the first to its previous
  sibling (right-sibling)
- rotate tree 45° clockwise

| ![](res/trees/binary/t3.svg)                     |
|:------------------------------------------------:|
| $$\mathcal{D_3}\texttt{[2]} = \texttt{(())()}$$  |

| ![](res/trees/binary/t4.svg)                    | ![](res/trees/binary/t5.svg)                    |
|:-----------------------------------------------:|:-----------------------------------------------:|
| $$\mathcal{D_3}\texttt{[3]} = \texttt{()(())}$$ | $$\mathcal{D_3}\texttt{[4]} = \texttt{()()()}$$ |

Notice that, since edges in the non-binary tree equivalent corresponds
to nodes in this binary representation, we have trees of $n$ nodes for
Dyck words of length $2n$, with cardinality $C_n$.

## Full Binary Trees

Restricting the space to binary trees where every non-leaf node has
exactly two children---like expressions built fromt binray
operators---we get for a Dyck word of length $2n$, trees with $n+1$
leaves, or $2n-1$ total nodes.

| ![](res/trees/binary-full/t1.svg)               | ![](res/trees/binary-full/t2.svg)               |
|:-----------------------------------------------:|:-----------------------------------------------:|
| $$\mathcal{D_3}\texttt{[0]} = \texttt{((()))}$$ | $$\mathcal{D_3}\texttt{[1]} = \texttt{(()())}$$ |

The mapping matches the "left-child right-sibling" representation of the
non-full binary trees, except that every node becomes an internal one,
leaves getting two children and unsaturated internal nodes becoming
saturated.

| ![](res/trees/binary-full/t3.svg)               |
|:-----------------------------------------------:|
| $$\mathcal{D_3}\texttt{[2]} = \texttt{(())()}$$ |

| ![](res/trees/binary-full/t4.svg)               | ![](res/trees/binary-full/t5.svg)               |
|:-----------------------------------------------:|:-----------------------------------------------:|
| $$\mathcal{D_3}\texttt{[3]} = \texttt{()(())}$$ | $$\mathcal{D_3}\texttt{[4]} = \texttt{()()()}$$ |

## Code Length

Code length (or
[information](https://en.wikipedia.org/wiki/Information_content) being
the logarithm of the
[variety](https://en.wikipedia.org/wiki/Variety_(cybernetics)), we have
for a Dyck word of length $2n$,

$$\begin{align}
I(\mathcal{D_n}) &= \log V(\mathcal{D_n})\\[5pt]
	&= \log C_n\\[5pt]
	&= \log \left(\frac{(2n)!}{(n+1)!\,n!}\right)\\[5pt]
	&= \log ((2n!)) - \log ((n+1)!) - \log (n!)
\end{align}$$

which grows on the order of $O(n)$.

![](res/trees/plots/on.svg)
