# Examples

This page contains a few code examples that should give you an idea of how to solve problems with MindScript.

## Hello, world!

We start with the classical "Hello, world!" example, but with a twist. We will implement it in two ways to illustrate the difference between formal and informal computation.

### Formal computation

We define a function that returns the string "Hello, world!"
```
# Say "Hello, world!"
let greet1 = fun() -> Str do
    return("Hello, world!")
end
```
Invoking `greet1()` yields:
```
"Hello, world!"
```
as expected.

### Unstructured computation

Now we define an oracle who will use the input-output signature
and the comment annotation in order to derive the output:

```
# Say "Hello, world!"
let greet2 = oracle() -> Str
```

If we evaluate `greet2()`, we should get

```
"Hello, world!"
```
but since this computation is informal and performed by the LLM, it might also return another string or fail (i.e. return `null`).

## Sentiment analysis

We want to specify a computation that takes a text (a string) 
as an input and returns either "positive" or "negative", depending on the
sentiment of the input sentence. Definining an algorithm is hard, hence
we write an oracle.

First, let's define the type of the output as a string `Enum`
with two possible values `"positive"` or `"negative"`:
```
let Sentiment = type Enum ["positive", "negative"]
```

Now we define an oracle that takes a text string as an input and
then determines its sentiment.
```
# Given a text, determine its sentiment.
let evaluateSentiment = oracle(sentence: Str) -> Sentiment
```

The oracle will now evaluate texts:
```
> evaluateSentiment("The cat isn't happy.")

"negative"

> evaluateSentiment("The markets are pretty bullish!")

"positive"
```

## Querying an oracle for information

We can leverage the knowledge of an oracle. Let's assume we
want to figure out the name of a famous researcher given
a subject field.

```
# Given a subject, determine the name of a famous researcher.
let researcher = oracle(subject: Str) -> {name: Str}
```

This oracle can be queried:

```
> researcher("physics")

{"name": "Albert Einstein"}

> researcher("chemistry")

{"name": "Marie Curie"}

> researcher("economics")

{"name": "John Maynard Keynes"}
```


## Extract prices from a web page

We can use oracles to process the content of a page in a
semantic manner. For instance, the next script extracts
the price of the cryptocurrency ETH from the front page
of CoinMarketCap:

```
# Given the HTML of a web page, extract the price of a given cryptocurrency.
let extractPrice = oracle(html: Str, currency: Str) -> Num

# Get the front page of CoinMarketCap.
let page = www("https://coinmarketcap.com")

# The price of ETH.
let ethPrice = extractPrice(page, "ETH")
```

At the time of writing this, `ethPrice` was:
```
> ethPrice

The price of ETH
2625.87
```

## Using the induction capabilities of oracles

By providing an oracle with examples, we can leverage their
ability to induce new outputs from previous ones.

### Predicting the Fibonacci sequence

For instance, let's compute the Fibonacci sequence
0, 1, 1, 2, 3, 5, ... Starting from the first two,
0 and 1, the next one is obtained by adding the previous
two.

```
# Compute the n-th Fibonacci number.
let fib = fun(n: Int) do
  if n == 0 then
    return(0)
  elif n == 1 then
    return(1)
  end
  return( fib(n-1) + fib(n-2) )
end

# The first ten Fibonacci numbers.
let numbers = []
for n in range(0, 10) do
  push(numbers, fib(n))
end
```

We can verify that these are indeed correct:
```
> numbers

[0, 1, 1, 2, 3, 5, 8, 13, 21, 34]
```

Now let's build an oracle `nextNumber` that, given a sequence of
numbers, generates the next one.

```
# Given a sequence of numbers, generate the next one.
let nextNumber = oracle(numbers: [Int]) -> Int
```

Let's test it. Starting with the first five Fibonacci
numbers, we let the oracle guess the next one in an iterative
way until we have a total of ten numbers.
```
let examples = slice(numbers, 0, 5)
for n in range(5, 10) do
  let p = nextNumber(examples)
  push(examples, p)
end
```
This outputs
```
> examples

[0, 1, 1, 2, 3, 5, 8, 13, 21, 34]
```
which are the correct first ten Fibonacci numbers.

!!! warning

    Obviously you should never fully rely on approaches like the above
    in critical applications since the correctness of the outputs such
    as the previous one relies heavily on the capabilities of the underlying LLM.

### Show, don't tell

Sometimes it's easier to show demonstrations rather than explaining what
needs to be done. We can instruct an oracle to follow our guide through
examples.

Consider the following text encoding rule, best shown in an example.
Take the input, say `HELLO`, and rewrite this in a zigzag pattern of three rows
```
H   O
 E L
  L
```
Then read the result of combining the letters line by line. The result is `HOELL`.
Next we build an oracle with a bunch of such examples:

```
let codeExamples =[
  ["HELLO", "HOELL"],
  ["ABCDEFGHIJK", "AEIBDFHJCGK"],
  ["ZIGZAGPATTERN", "ZATNIZGATRGPE"],
  ["1234567890", "1592468037"],
  ["PYTHONISFUN", "POFYHNSUTIN"],
  ["EXAMPLES", "EPXMLSAE"]
]

let encode = oracle(text: Str) -> {code: Str} from codeExamples
```

We can now test it on some inputs:
```
> encode("HELLO")

"HOELL"

> encode("SANDWICH")

"SHADICNW"

> encode("TOBEORNOTTOBE")

"TEOOBRTONBOTE"
```

As we can see, the first output is correct. The second and third
outputs are incorrect, as they should have been "SWADIHNC" and
"TOBEORNOTTOBE" respectively. 
