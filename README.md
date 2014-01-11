recursiveSmileys
================

Recursive Smileys Art/Game

TO RUN:
	- compile with "ghc *.hs". Probably need to first do something like "cabal install gtk cairo", but I haven't bothered yet to get a complete list of dependencies.
	- ./Main <width in pixels> <height in pixels> <number of distinct faces> <recursion depth for each face>
	- Looks good to me, but runtime several seconds: ./Main 1900 1000 2000 5

TO DO:
	- layout algorithm is poor. Currently just (1) iteratively adds faces randomly, trying repeatedly to place a new face until not inside an existing face, and then (2) grows the face concentrically by fixing the center and setting the radius to be the minimum distance to another face. So to add a single new face, (1) is O(1/remaining space) ~ O(1/sqrt(num faces)) and (2) is O(num faces). This could be improved, for instance by growing faces non-concentrically in (2) to be tangent with two other faces/edges. Therefore, we could maintain a binary tree of empty spaces, as each new face would bisect an existing space into two new spaces.
	- Eyes can sometimes overlap each other or extrude beyond the circle of the face. Haven't decided if this is a bug or a feature. It's kind of cute.
	- Should have more interactive GUI things. That's why I chose Haskell, for the ease of interactive GUI things. Maybe when you click a face, it comes up with a dialog showing the face's parameters, and you can edit them, then re-render. There should be some way to zoom & pan.
	- Currently faces are populated to a fixed recursion depth. Should rather be to a fixed feature size (such as 1 pixel)
	- More algebraic face types, such as fixed image (of, say, a coin)
	- Background gradients/patterns
	- Animation? Spinning/blinking?
	- Teeth. Definitely needs teeth.
