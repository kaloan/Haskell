# Huffman Encoding

Simple Huffman encoding in Haskell. I do save the whole huffman tree, although in reality the "lookup" list is enough and should be more judicious in terms of filesize.

A possible enhancement will be using `Data.Map` instead, and making use of `ByteString`.
