# addresses

## The challenge:

Street addresses are difficult. I specifically deal with Australian postal addresses, and one would expect most come in some form similar to `16 Fictional Street, Suburban Town`, right? But no. There is no law forcing people to conform to norm. Go open Google maps and type `The Horizon, Epping, Victoria`. Yes, this is not a street. This is *The Horizon*!

Now to further complicate matters, let's say we have to find an address on *the horizon* that is embedded in free text. Now we have a problem, right?

## The solution:

This is my attempt at one. I'm building a library based on [Haskell](https://haskell-lang.org) parser combinators to

#1 Parse the common cases of Australian postal addresses out of streams of unrelated text.
#1 The output should be Haskell Types, but in a format that can be easily transformed for validation against the comprehensive list of Australian Addresses ([G-NAF](https://www.psma.com.au/products/g-naf)).
#1 Correctly parse abbreviations (`12 St Paul St`).
#1 Will do the right thing when presented with ranges of numbers (`12-30 Fictional Street`), multiple addresses within one chunk of text, or even multiple addresses within a street (`12, 15, 20-30 Fictional Street`).

## The current state:

I'm not there yet. This is my initial attempt. Be aware that I'm a beginner Haskeller, so this implementation is a reflection of my current skills.
