package forcomp

object tests extends App{

  val wordOccurrences = Anagrams.wordOccurrences("Ababagalamaga")
  println(wordOccurrences)

//  Anagrams.dictionaryByOccurrences.foreach(println)

  println(Anagrams.wordAnagrams("merits"))

  val testOc = List(('a', 2), ('b', 2))

  Anagrams.combinations(testOc).foreach(println)

  val larger = List(('a',7), ('b',2), ('g',2), ('l',1), ('m',1))
  val smaller = List(('a',7), ('m',1))

  println(larger.span(elem => elem._1 == 'b'))

//  println(Anagrams.subtract(larger,smaller))

}
