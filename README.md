# dultik

this is a (*still very bad*) xextan parser. it returns all valid parses.

"why is it very bad" i hear you ask.
- it is not uptodate with current xextan (though neither is the PEG its based on)
- also it doesnt keep track of the parse structure yet which is kind of silly
- the code is probably incredibly unreadable

good news is now there is a *tokenizer* which makes it not ridiculously slow. yay

## what to do

1. clone
2. `cd dultik/src`
3. either
   - `ghci ../app/Main.hs` and do something like `test "xoi len sin jo tak xextan"`
   - edit `app/Main.hs` to parse whatever you want and then compile+run with `ghc -O2 ../app/Main.hs && ../app/Main`
