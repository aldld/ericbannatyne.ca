---
title: "hasp: Writing a Lisp Interpreter in Haskell"
date: 2016-08-25
---

For a while, I had been meaning to write a nontrivial piece of code in Haskell,
in order to gain a greater degree of familiarity with the language, and with
functional programming in general. A few weeks ago I finished up an internship
at Facebook (where they do have teams that code in Haskell, though I didn't work
on that), and there were still several weeks left before school starts up again,
so I decided to use that time to work on a personal side project. I ended up
writing a Lisp interpreter in Haskell, based on a friend's suggestion.

This turned out to be an interesting suggestion, for a couple of reasons. I had
often read that writing an interpreter for a programming language, usually some
subset of Scheme, is something that everyone must do to be considered a "real
programmer". One of the classic *[SICP](https://mitpress.mit.edu/sicp/)*-style
programming projects involves writing a Scheme interpreter in Scheme. I hadn't
written a full interpreter from scratch before, so this seemed like a good way
to learn more about how interpreters actually work.

I had also heard that Haskell is supposed to be a good language for writing
programming language interpreters and compilers. From what I can tell, Haskell
seems to be pretty popular among programming languages people in general (or
maybe they've all moved on to Idris or something else I don't know). Haskell
also seems popular for defining domain-specific languages, such as
[Forest](http://www.forestproj.org/),
[diagrams](http://projects.haskell.org/diagrams/), and
[BASIC](http://augustss.blogspot.ca/2009/02/regression-they-say-that-as-you-get.html).

There seems to be a lot of Haskell tutorials online that teach the language by
demonstrating how to write a Lisp interpreter. However, I wanted to figure out
as much as possible on my own, and I resolved to check online tutorials only
when I got stuck on some technical details. Because of this, I'm almost certain
that there are places where I could have written more idiomatic Haskell code;
I've already done some refactoring, and if I have time I'll probably do a bit
more. The intention of this post is not to be yet another Lisp-in-Haskell
tutorial, but rather to give a high level overview of what I've worked on so
far. You can see the code that I've written in the
[hasp GitHub repo](https://github.com/aldld/hasp). You can also check out the
[GitHub issues page](https://github.com/aldld/hasp/issues) for hasp to see what
I plan to work on next.

The hasp language is inspired by the syntax and semantics of Lisp, mainly taking
inspiration from Scheme, a language known for its elegance and simplicity. I
wouldn't call hasp a subset of Scheme, though, because I don't have a formal
specification for the language (I'm currently making it up as I go along), so
I'm almost certain that it deviates, or will deviate, from Scheme in at least
some respects. I'm hesitant even to call it a dialect of Lisp, since I'm not
even sure what criteria a language must satisfy to be considered a Lisp.
Nevertheless, it's similar enough that the name hasp seemed appropriate for such
a language.


Writing the Parser
------------------

Generally, the first step in a compiler or interpreter is to parse input source
code, a sequence of characters, and produce an
[abstract syntax tree](https://en.wikipedia.org/wiki/Abstract_syntax_tree), a
data structure that encodes the syntactic structure of the input program. Most
Haskell tutorials that involve writing an interpreter use libraries like
[Parsec](https://wiki.haskell.org/Parsec) and the machinery of parser
combinators, which do most of the heavy lifting. Parsec is a very powerful
library for writing parsers, and I'd probably appreciate its generality if I
were to write a parser for a more syntactically-complex language. Instead, I
decided to write the parser from scratch, as I figured I'd learn more that way.
For a language like Lisp, this doesn't end up being too complicated, since its
syntax is very simple; the source code is basically the abstract syntax tree.

Even before we can run the parser though, we first need to execute a single pass
through the source code with the tokenizer, or lexer. This simply involves
splitting the source code, presented as a single string, into a sequence of
"chunks" or tokens, with a specific meaning to the interpreter. For instance, it
should turn this hasp source code

```scheme
(+ 1 (length (list 2.3 "hello world")) 4)
```

into this list of tokens

```haskell
["(", "+", "1", "(", "length", "(", "list", "2.3", "\"hello world\"", ")", ")", "4", ")"]
```

Essentially, the tokenizer is just a fancy string splitter. I originally used
Haskell's built-in string splitting functions to handle tokenization, however I
quickly found that it would become too complicated (at least as far as I could
tell) to specify all of the rules for determining which delimiters to keep,
which to drop, dealing with string literals, and all that. So instead, I wrote
my own simple tokenizer.

Its behaviour can be modeled as a simple finite state machine that passes
through the input from left to right, switching "states" depending on the
current character, with special rules, like what to do if we are currently in
the middle of a string. I implemented the state machine as a series of Haskell
functions that simply pass around a couple of accumulators to each other, and
decide what to do based on pattern matching with the current character. In the
code in
[Tokenizer.hs](https://github.com/aldld/hasp/blob/master/src/Tokenizer.hs), you
can probably guess how the state diagram for the tokenizer would look.

Once we have obtained a sequence of tokens from the input source code, the
parser's job is to convert the sequence of tokens into an abstract syntax tree.
We can define a Haskell data structure that encodes the AST for a given hasp
expression.

```haskell
type Identifier = String
data Atomic = StringLiteral String
            | IntLiteral Integer
            | FloatLiteral Float
            | BoolLiteral Bool
            | Id Identifier
            deriving (Show)

data Expr = Atom Atomic
          | List [Expr]
          deriving (Show)
```

Here, an `Expr` represents a hasp expression, which can either be an `Atom` (a
single piece of data, like a string literal or a variable name), or a `List` of
expressions. In the future one could also imagine adding quoted expressions to
the list, but I haven't implemented that yet. If you're not familiar with
Haskell, the `deriving (Show)` simply makes it so that expressions and atomic
values can be represented as strings for output.

Now we want to take a list of tokens and produce a list of expressions (since
the input source code might contain several expressions), that is, we want the
type signature of the main parser function to be something like

```haskell
parseExprs :: [Token] -> ThrowsError [Expr]
```

Here, `Token` is a type synonym for `String`, indicating that the data in the
string specifically represents a token.
The output type of `parseExprs` is actually `ThrowsError [Expr]` instead of
simply `[Expr]`, since it's possible that it may throw an error, say when the
input source code contains an unclosed parenthesis. In such a case, we want to
use the `ThrowsError` monad for handling errors, which I will talk about more
later.

Due to the simplicity of Lisp's (and therefore hasp's) syntax, the algorithm to
parse hasp source code is almost as simple. Rather than put all the code for the
parser, which you can find in
[Parser.hs](https://github.com/aldld/hasp/blob/master/src/Parser.hs), in this
post, I figured it would be easier to describe the algorithm in English.

The algorithm makes a single pass from left to right over the sequence of
tokens, maintaining a stack whose elements are lists of `Expr`s, as well as an
accumulator of expressions that have been fully processed so far. The idea
behind the stack is that it helps us keep track of our current level of nesting
within the current expression being parsed.

At each token, the algorithm does the following:

1. If the current token is an *opening parenthesis* `(`, push a new empty list
on top of the stack, indicating that we have reached a new level of nesting.

2. If the current token is a *closing parenthesis* `)`, squash the top two
elements of the stack together, by taking the top list and appending it to the
list immediately below it.

3. Otherwise, the current token is an atomic value, so we parse the token as an
atom (using regular expressions to distinguish between identifiers, string
literals, numbers, etc.) and append it to the list on top of the stack.

Once the stack is down to a single element and the algorithm encounters a
closing parenthesis, we pop that value off of the stack and append it to the end
of the accumulator. Once we have gone through all of the tokens, the algorithm
outputs the contents of the accumulator. This description of the algorithm
glosses over some details such as handling invalid syntax, but it should give
you the general idea of how the parser works.

Evaluating hasp Expressions
---------------------------

Once we have the AST generated from some hasp source code, we want to actually
evaluate it and produce a result. That is, given the *syntax* of a hasp
expression, we want to interpret its *semantics*.

### hasp Data Types

In a previous section, we defined the `Expr` data type, which represents the
syntactic expressions that form a hasp source file. On the other hand, we can
also define a data type (which I've called `HData`) that represent the actual
results of hasp computations.

One of Haskell's main features is its powerful static type system, which enables
type checking to be done by the compiler. On the other hand, most Lisp dialects
tend to be dynamically typed, that is, all type checking happens at runtime.
One of the challenges I faced was trying to reconcile these two different type
systems in a somewhat elegant way. This is one situation where static typing
makes things a bit more difficult, since the nature of an interpreter is that
the output depends on the results of computations whose type cannot be known at
compile time (unless the interpreter only works for *one* specific program).

After several iterations, I settled on the following definition for `HData`.

```haskell
data HNum = HInt Integer
          | HFloat Float
          deriving (Eq)

data HData = HN HNum
           | HBool Bool
           | HString String
           | HList [HData]
           | HQuote Expr
           | HFunc Env (Env -> [HData] -> ThrowsError HData)
```

Another alternative that I had considered was to have `HInt`, `HFloat`, `HBool`,
etc. all be their own types that are instances of a typeclass called `HType`,
and then to define `HData` using
[existentially quantified types](https://en.wikibooks.org/wiki/Haskell/Existentially_quantified_types).

```haskell
data HData = forall a. HType a => HData a
```

However, this approach ended up being difficult when it came to checking types
later on, when I found out that you can't pattern match on existentially
quantified types in Haskell.

There's more to the definition of `HData` in
[DataTypes.hs](https://github.com/aldld/hasp/blob/master/src/DataTypes.hs), such
as making `HNum` an instance of `Num` (so that they support standard arithmetic
operations like addition and multiplication), and making `HData` an instance of
`Eq` and `Show`.

### Evaluation

Actually evaluating an expression in hasp follows the similar rules as in most
Lisp dialects. When we are given an expression like

```scheme
(a b c d)
```

First, the interpreter recursively evaluates the expressions `a`, `b`, `c` and
`d` from left to right. (Actually, since hasp is a pure functional language with
no side effects, they can be evaluated in any order, or even in parallel.) Then
it treats the result of evaluating `a` as a function, and it evaluates it on the
arguments obtained by evaluating `b`, `c`, and `d`.

In the definition of `HData`, you may have noticed the type constructor `HFunc`,
which represents a hasp function, and includes a parameter of type `Env`. The
`Env` data type is short for "environment", and is essentially a mapping of
variable names, or identifiers, to `HData` values. This is necessary so that
hasp expressions can reference predefined name bindings and use their values in
their own computations. The second argument of `HFunc` is the actual function
itself: it takes an environment and a list of arguments, and outputs a value (or
possibly an error).

The hasp interpreter includes a default global environment, which defines a
basic set of built-in functions that are available to all hasp programs.

```haskell
globalEnv :: Env
globalEnv = Env $ Map.fromList
    [ ("+", numericFold (+) 0)
    , ("*", numericFold (*) 1)
    , ("-", minusHNum)
    , ("/", numericBinOp "/" divideHNum)
    , ("quotient", numericBinOp "quotient" divHNum)
    , ("modulo", numericBinOp "modulo" modHNum)
    , ("abs", numericUnaryOp "abs" abs)
    , ("sgn", numericUnaryOp "sgn" signum)
    , ("eq?", numericBinPred "eq?" (==))
    , ("=", numericBinPred "=" (==))
    , ("<", numericBinPred "<" (<))
    , ("<=", numericBinPred "<=" (<=))
    , (">", numericBinPred ">" (>))
    , (">=", numericBinPred ">=" (>=))
    , ("!=", numericBinPred "!=" (/=))
    , ("list", list)
    , ("cons", cons)
    , ("car", car)
    , ("cdr", cdr)
    , ("empty?", testEmptyList) ]
```

I've only included the bare minimum so far. The reason for this is that these
functions, including basic arithmetic operations, comparison operators, and
list operations, can only really be defined in Haskell and made available to
hasp code. However, it can be a bit of a pain to define hasp functions in the
interpreter itself because type checking needs to be done manually. For example,
the function `numericBinOp`, which takes a Haskell numeric binary operation and
converts it to the equivalent operation in hasp, has the following (rather
inelegant) definition:

```haskell
numericBinOp :: String -> (HNum -> HNum -> ThrowsError HNum) -> HData
numericBinOp opName op =
    HFunc emptyEnv $ \_ args ->
        case args of
            [HN x, HN y] -> do
                result <- x `op` y
                return $ HN result
            [x, HN _] -> throw . errNotNum $ show x
            [HN _, y] -> throw . errNotNum $ show y
            [x, _] -> throw . errNotNum $ show x
            _ -> throw $ errNumArgs opName 2 (length args)
```

Because of this, only a few built-in functions are defined directly in the
interpreter's Haskell code. However, once we have these basic functions (and
assuming we have constructs like `define`, `lambda` and `if`), it's possible to
define standard library functions like `map`, `filter` and `fold`. For example,

```scheme
(define map (lambda (f lst)
    (if (empty? lst)
        ()
        (cons (f (car lst)) (map f (cdr lst))))))
```

Currently hasp doesn't do any sort of tail call elimination, so I haven't
bothered to define `map` in a tail recursive way. However if I do add that
feature in the future, it would be simple to convert this definition into one
that is tail recursive.

Of course, language constructs like `define`, `lambda` and `if` can't be defined
as hasp functions in the usual sense. In the case of `define`, that is because
its job is to mutate the current environment to define new name bindings.
Similarly, `lambda` works with environments in a way that ordinary functions
cannot, creating closures that capture the values of free identifiers from the
context in which the `lambda` expression is evaluated.

On the other hand, `if` is not a regular hasp function because it is short
circuiting: in any computation, only one of the two branches of an `if`
statement is ever evaluated. This violates the usual rules of function
evaluation, which means that `if` needs to be implemented as a special syntactic
keyword.

Handling Errors
---------------

In an earlier iteration of hasp, all of the functions that returned the type
`ThrowsError a` originally returned `Either Error a`, and contained a lot of
boilerplate code like

```haskell
define (Env envMap) [Atom (Id name), expr] =
    case evalExpr (Env envMap) expr of
        Left err -> Left err
        Right (result, _) ->
            Right (HList [], Env $ Map.insert name result envMap)
```

Here, the call to `evalExpr` is one that may return an error. If an error is
returned, we propagate it forward, and if not, we make its result available to
the rest of the computation. This pattern appears throughout the hasp
interpreter, as well as any Haskell code that has to deal with errors.

In Haskell, one of the main benefits of monads is the ability to hide
boilerplate code like in the above snippet. I defined a `ThrowsError a` data
type to be a type synonym for `Either Error a`, and made it an instance of the
`Monad` typeclass. The above code snippet can then be rewritten as

```haskell
define (Env envMap) [Atom (Id name), expr] = do
    (result, _) <- evalExpr (Env envMap) expr
    return (HList [], Env $ Map.insert name result envMap)
```

Most of the code that I used for error handling is from Bartosz Milewski's
[post on error handling](https://www.schoolofhaskell.com/school/starting-with-haskell/basics-of-haskell/10_Error_Handling)
at the School of Haskell, which explains the basics of how to use monads for
error handling.

Conclusion and Next Steps
-------------------------

There are some details of hasp that I haven't had time to cover in this post,
and overall the project is not quite finished yet. There are several additional
features that I plan to add, such as support for macros, struct data types, and
a larger standard library. I also plan to do a lot of refactoring to make my
Haskell code more idiomatic (which will probably involve some use of monad
transformers), add unit tests and do some bug squashing.

The source code for the hasp interpreter is freely available on the
[hasp GitHub repo](https://github.com/aldld/hasp). If you are interested in
contributing to hasp, feel free to fork the repo and submit a pull request. If
you have any comments, questions, or suggestions, or if you give hasp a bug and
you find a bug, feel free to email me, or
[create an issue](https://github.com/aldld/hasp/issues) on the hasp GitHub page.
