qs-ws
=====
Runs quicksort, partitioning intervals on a smallest-available-interval-first basis. Inspired by <a href="http://bertrandmeyer.com/2014/12/07/lampsort/">this article</a>.

The frontend uses D3.js to visualizes quicksort, using code adapted from this [quicksort visualization](http://bl.ocks.org/mbostock/1582075) by Mike Bostock.

The Haskell backend runs quicksort on an array provided by the frontend via websocket then pushes swap and partition events back to the frontend via websocket for rendering. Yes, this could all be done on the client side in Javascript, but this was way more fun.
