
# Parser Combinators

The parser combinator file that we will be inspecting can be found
[here](https://github.com/heliaxdev/juvix/blob/develop/library/Frontend/src/Juvix/Frontend/Parser.hs).


```{note}
In order to be able to understand this code, we suggest reading about
how parser combinators work. [Here](https://hasura.io/blog/parser-combinators-walkthrough/)'s a wonderful introduction.
```
 
## The infix operator

We'll briefly mention some code related to the infix handler:

```haskell
  expressionGen :: Parser Types.Expression -> Parser Types.Expression
  expressionGen p =
    Expr.makeExprParser (spaceLiner (expressionGen' p)) tableExp

  -- For Expressions
  tableExp :: [[Expr.Operator Parser Types.Expression]]
  tableExp =
    [ [refine],
      [infixOp],
      [arrowExp]
    ]

  infixOp :: Expr.Operator Parser Types.Expression
  infixOp =
    Expr.InfixR
      ( P.try $ do
          inf <- spaceLiner infixSymbolDot
          pure
            (\l r -> Types.Infix (Types.Inf l inf r))
      )
```

Here we outline three key functions:

- The `expressionGen` uses the `tableExp` as the precedence table of infix parsers. So `refine` has a higher precedence than `infixOp`. At the end of `expressionGen` we get back a new parser for any expression parser we may hand it.

- The `infixOp` structure explicitly states what kind of infix it is, in our case we make it nest on the right, so `a -> b -> c` gets turned into `a -> (b -> c)`. The important thing to note is that we only parse for the inifix symbol itself, we let the parser we hand to `expressionGen` handle the other side.

Every other tool you'll see is an abstraction on top of these base
tools. Even the infix handler is built upon the first two primitives
we've outlined.

## Juvix's custom parser combinators

We use parser combinators mostly in the standard way, however you'll often see the forms `skipLiner`, `spaceLiner`, and the convention `*SN` which are not typical in a parser combinator system.

- `spaceLiner` just eats all empty symbols, these are spaces and
newlines after the current parsed expression.

- `skipLiner` is the same as `spaceLiner`, but it is for any character given to it.

- Finally `*SN` just calls `spaceLiner` on whatever parser. E.g.
```haskell
sumSN :: Parser Types.Sum
sumSN = spaceLiner sum
```

```{note}
These concepts exist namely due to the wish of eventually making the
parser indent sensitive.
```

---------

```{warning}
The remaining document needs to be written better and eventually remove all those backticks (e.g. do''', app'') from the code
```

The main confusing bit of our layout is the many variants of
expression:

```haskell
  do''' :: Parser Types.Expression
  do''' = Types.Do <$> do'

  app'' :: Parser Types.Expression
  app'' = Types.Application <$> P.try application

  all'' :: Parser Types.Expression
  all'' = P.try do''' <|> app''

  -- used to remove do from parsing
  expression' :: Parser Types.Expression
  expression' = expressionGen app''

  -- used to remove both from parsing
  expression''' :: Parser Types.Expression
  expression''' = expressionGen (fail "")

  expression :: Parser Types.Expression
  expression = expressionGen all''
```

We have three main variants, ones with application, ones with do
syntax, and ones with both! These exists because certain
transformations will go into an infinite loop if you're not
careful. This is mainly due to how some forms like `do` and infix
generators behave together. In other cases like in adt declarations,
we want to disable application `type List a = Cons a (List a)`. It
would be a shame if the `a` was applied to `List a`!