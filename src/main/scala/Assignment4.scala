import fpinscala.parsing.Parsers

object Assignment4 {

  /* Grammar for tasks 1 and 2
   *
   * Verse                 -> NounPhrase VerbPhrase
   * NounPhrase            -> ComplexNoun | ComplexNoun PrepositionalPhrase
   * VerbPhrase            -> ComplexVerb | ComplexVerb PrepositionalPhrase
   * PrepositionalPhrase   -> Preposition ComplexNoun
   * ComplexNoun           -> Article Noun
   * ComplexVerb           -> Verb | Verb NounPhrase
   * Article               -> a | the
   * Noun                  -> boy | girl | flower
   * Verb                  -> touches | likes | sees | moves
   * Preposition           -> with | for | as
   */

  // For task 2
  trait VerseItem {
    def <+>(that: VerseItem): VerseItem = {

      //helper function for stanza + Verse
      def sandv(s: Option[Stanza], ver:Verse): Stanza = {
        s match {
          case None => Stanza(ver,None)
          case Some(s_rec) => Stanza(s_rec.verse, Some(sandv(s_rec.next, ver)))
        }
      }
      //helper function for stanza + stanza
      def sands(s1: Stanza, s2: Option[Stanza]): Stanza = {
        s1 match {
          case Stanza(v, None) => Stanza(v, s2)
          case Stanza(v, Some(next)) => Stanza(v, Some(sands(next, s2)))
        }
      }
      //Matching different orders of datatypes that can be given to the operator
      //Returns Stanza or Fragment
      (this, that) match{
        case (v: Verse, s: Stanza) => Stanza(v, Some(s))
        case (v: Verse, v2: Verse) => Stanza(v, Some(Stanza(v2,None)))
        case (s: Stanza, v: Verse) => sandv(Some(s),v)
        case (s: Stanza, s2: Stanza) => sands(s, Some(s2))
        case (a:VerseItem, b:VerseItem) => Fragment(List(a,b))
      }
    }
  }

  // Note that Stanza and Fragment are intended for completion of task 2.
  // Strings are not parsed into either of these classes
  case class Stanza(verse: Verse, next: Option[Stanza]) extends VerseItem
  case class Fragment(pieces: List[VerseItem]) extends VerseItem

  // These are the classes and objects into which strings are parsed.
  // Their structure mirrors that of the grammar given above.
  case class Verse(noun: VerseItem, verb: VerseItem) extends VerseItem
  case class NounPhrase(noun: VerseItem, prepPhrase: Option[VerseItem]) extends VerseItem
  case class VerbPhrase(verb: VerseItem, prepPhrase: Option[VerseItem]) extends VerseItem
  case class PrepPhrase(preposition: VerseItem, noun: VerseItem) extends VerseItem
  case class ComplexNoun(article: VerseItem, noun: VerseItem) extends VerseItem
  case class ComplexVerb(part1: VerseItem, part2: Option[VerseItem]) extends VerseItem
  case object ArticleA extends VerseItem
  case object ArticleThe extends VerseItem
  case object NounBoy extends VerseItem
  case object NounGirl extends VerseItem
  case object NounFlower extends VerseItem
  case object VerbTouches extends VerseItem
  case object VerbLikes extends VerseItem
  case object VerbSees extends VerseItem
  case object VerbMoves extends VerseItem
  case object PrepWith extends VerseItem
  case object PrepFor extends VerseItem
  
  object VerseInstance {

    def verseParser[Parser[+_]](P: Parsers[Parser]): Parser[VerseItem] = {
      import P.{string => _,_}
      implicit def tok(s: String) = token(P.string(s))

      // For task 1
      //Verse-> NounPhrase VerbPhrase
      def verse: Parser[VerseItem] = (nounPhrase ** verbPhrase).map(v => Verse(v._1, v._2)) scope("Verse")

      //NounPhrase-> ComplexNoun | ComplexNoun PrepositionalPhrase
      //VerbPhrase -> ComplexVerb | ComplexVerb PrepositionalPhrase
      //use of attempt on ** combinators first, so that characters can be uncommited if the grammar doesnt match
      def nounPhrase: Parser[VerseItem] = attempt(complexNoun**prepPhrase).map(v => NounPhrase(v._1, Some(v._2))) | (complexNoun).map(v => NounPhrase(v, None))  scope("Noun Phrase")
      def verbPhrase: Parser[VerseItem] = attempt(complexVerb**prepPhrase).map(v => VerbPhrase(v._1, Some(v._2))) | (complexVerb).map(v => VerbPhrase(v, None))  scope("Verb Phrase")
      //PrepositionalPhrase -> Preposition ComplexNoun
      def prepPhrase: Parser[VerseItem] = (preposition ** complexNoun).map(v=> PrepPhrase(v._1, v._2)) scope ("prep Phrase")

      //ComplexNoun-> Article Noun
      def complexNoun: Parser[VerseItem] = (article**noun).map(v => ComplexNoun(v._1, v._2)) scope("Complex Noun")
      // ComplexVerb -> Verb | Verb NounPhrase
      def complexVerb: Parser[VerseItem] = attempt(verb ** (nounPhrase)).map(v => ComplexVerb(v._1,Some(v._2))) | verb.map(v=> ComplexVerb(v, None)) scope("Complex Verb")
      // This yields either ArticleA or ArticleThe when it 
      // encounters those respective strings
      def article: Parser[VerseItem] = scope("articles"){
        "a".as(ArticleA) |
          "the".as(ArticleThe)
      }
      // This yields either NounGirl, NounBoy, or NounFlower 
      // when it encounters those respective strings
      def noun: Parser[VerseItem] = scope("nouns"){
        "boy".as(NounBoy) |
          "girl".as(NounGirl) |
          ("flower".as(NounFlower))
      }
      // This yields either VerbTouches, VerbLikes, VerbSees, or VerbMoves 
      // when it encounters those respective strings
      def verb: Parser[VerseItem] = scope("verbs"){
        ("touches").as(VerbTouches) |
          ("likes").as(VerbLikes) |
          ("sees").as(VerbSees) |
          ("moves".as(VerbMoves))
      }
      // This yields either PrepWith or PrepFor when it encounters
      // those respective strings
      def preposition: Parser[VerseItem] = scope("preps"){
        ("with").as(PrepWith) |
          ("for".as(PrepFor))

      }
      //def value: Parser[VerseItem] = preposition | verb | noun
      // This acts like the starting symbol in the grammar
      root(verse) 
    }
  }

  // For task 3
  // The ae stands for arithmetic expression
  val ae: Map[Char,Seq[String]] = Map(
    'E' -> List("T", "E+T"),
    'T' -> List("F", "E*T"),
    'F' -> List("I", "(E)"),
    'I' -> List("a", "b"))

  // For a given sentential form, s, apply the right hand side (rhs) productions
  // at position k only
  def applyProductionAt(s: String, k: Int, rhs: Seq[String]): Seq[String] = {
    val res = rhs.map(rh => s.substring(0,k) + rh + s.substring(k+1))
    println(s"applying ${rhs} to ${s} at position ${k} yields ${res}")
    // As you debug your code you may notice that certain portions of the graph
    // never get visited. That's because the above grammar, ae, is recursive.
    // The following condition is meant to stop the traversal once it reaches 
    // the sentential form '(E)', so that your traversal yields more interesting
    // results.
    if (s.startsWith("(") && s.endsWith(")")) List() else res
  }

  // For a given string and for a given grammar (given as a Map[Char,Seq[String]]) 
  // yield a lazy list of all sentential forms arising from the string by applying
  // productions from the given grammar.
  val allSententialForms: (String,Map[Char,Seq[String]]) => Stream[String] = (a: String, mp: Map[Char,Seq[String]]) => {

    //Head of Stream is current Seq
    //Tail of stream will call traverse to Seq and provide all children node Sentential forms of the current Seq
    def f(s:Seq[String]): Stream[String] = {
      val next = s.flatMap(str => traverse(str))
      s.toStream #::: f(next)
    }

    //function to return Seq of resulting Sentential forms of a given string
    def traverse(s: String):Seq[String] = {
      //filter out odd num positions, since those positions are typically held by operator
      //these positions are used to provide k for aPA function for strings
      //check cases where there is no mapping for a single variable in the map ie. (E), a, b
      (0 to s.length-1).filter(x => x % 2 ==0)
        .flatMap(num =>
          if (s(num) == '(') applyProductionAt(s,num,List("(E)"))
          else if(s(num) == ')') applyProductionAt(s,num,List())
          else if(s(num) == 'a') applyProductionAt(s,num,List("a"))
          else if(s(num) == 'b') applyProductionAt(s,num,List("b"))
            else applyProductionAt(s, num , mp(s(num))))
    }
    //instantiate stream
    f(List("E"))
  }

  /* Please leave this function unaltered, so that as I evaluate everyone's 
   * submissions, I see the code you've written invoked under the same conditions.
   * If you'd like to test your code in other ways, it's best to make a separate 
   * function for that.
   */
  def go = {
    // For task 1
    println("Task 1 - Parsing a string using a context-free grammar")
    val P = fpinscala.parsing.Reference
    import fpinscala.parsing.ReferenceTypes.Parser
    val verses: Parser[VerseItem] = VerseInstance.verseParser(P)
    val s1 = "a girl moves with a flower"
    println(s"Parsing ${s1}")
    val parseTree1 = P.run(verses)(s1)
    println(s"Parse tree for string '${s1}': ${parseTree1}")
    val s2 = "the boy with a flower likes the girl with the girl with a flower"
    println(s"Parsing ${s2}")
    val parseTree2 = P.run(verses)(s2)
    println(s"Parse tree for string '${s2}': ${parseTree2}")
    val s3 = "a girl with a flower moves with the boy"
    println(s"Parsing ${s3}")
    val parseTree3 = P.run(verses)(s3)
    println(s"Parse tree for string '${s3}': ${parseTree3}")
    val s4 = "the flower for the girl touches the boy with a flower for the girl"
    println(s"Parsing ${s4}")
    val parseTree4 = P.run(verses)(s4)
    println(s"Parse tree for string '${s4}': ${parseTree4}")

    // For task 2
    println("Task 2 - Operator overloading to merge parse trees")
    val poem = parseTree1
      .flatMap(pt1 => parseTree2
      .flatMap(pt2 => parseTree3
      .flatMap(pt3 => parseTree4
      .map(pt4 => pt1 <+> pt2 <+> pt3 <+> pt4))))
    println(s"Combining all four using an overloaded operator yields: ${poem}")

    // For task 3
    println("Task 3 - Generating sentential forms from a context-free grammar")
    val n = 20
    val selectedSententialForms = allSententialForms("E",ae).take(n).toList
    println(s"First ${n} sentential forms: ${selectedSententialForms}")

  }
}