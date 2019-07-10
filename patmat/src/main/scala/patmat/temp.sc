import patmat.Huffman

val list: List[Char] = List('a', 'c', 'b', 'a', 'c', 'c')

val times = Huffman.times(list)

val ordered = Huffman.makeOrderedLeafList(times)

Huffman.decodedSecret

Huffman.encode(Huffman.frenchCode)("huffmanestcool".toList)
Huffman.quickEncode(Huffman.frenchCode)("huffmanestcool".toList)


