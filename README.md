# dultik

this is a (*very bad*) xextan parser. it returns all valid parses.

"why is it very bad" i hear you ask.
- it is **incredibly** slow. wanna parse "xoi len sin"? have fun waiting for 27 minutes
- it is not uptodate with current xextan (though neither is the PEG its based on)
- also it doesnt keep track of the parse structure yet which is kind of silly
## what to do

1. clone
2. `cd dultik/src`
3. either
   - `ghci` and do something like `parse (text <* eof) "xoi len"`
   - edit `app/Main.hs` to parse whatever you want and then compile+run with `ghc -O2 ../app/Main.hs && ../app/Main`
