#TinyMap

TinyMap is a highly space-efficient purely functional data structure.
All values of TinyMap are serialized and gzip compressed.
Phantom Types are used to promote type safety.
Keys retain Ord instancs to provide amortized O(log(n)) lookup and
insertion time.
