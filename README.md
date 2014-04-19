#TinyMap

A time and space efficient strict hashmap. 
All values of TinyMap are serialized and gzip compressed.
Phantom Types are used to promote type safety
Keys retain Ord instancs to provide amortized O(min(n,W)) lookup and
insertion time
