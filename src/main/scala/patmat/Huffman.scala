package patmat

import common._

/**
 * Assignment 4: Huffman coding
 *
 */
object Huffman {

  /**
   * A huffman code is represented by a binary tree.
   *
   * Every `Leaf` node of the tree represents one character of the alphabet that the tree can encode.
   * The weight of a `Leaf` is the frequency of appearance of the character.
   *
   * The branches of the huffman tree, the `Fork` nodes, represent a set containing all the characters
   * present in the leaves below it. The weight of a `Fork` node is the sum of the weights of these
   * leaves.
   */
    abstract class CodeTree
  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
  case class Leaf(char: Char, weight: Int) extends CodeTree
  

  // Part 1: Basics
    def weight(tree: CodeTree): Int = tree match {
      case Fork(lTree, rTree,  charList, weightInt) => weightInt;
      case Leaf(char, weightInt) => weightInt;
    }
  
  
    def chars(tree: CodeTree): List[Char] = tree match {
      case Fork(lTree, rTree,  charList, weightInt) => charList;
      case Leaf(char, weightInt) => char::Nil;
    }
  
  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))



  // Part 2: Generating Huffman trees

  /**
   * In this assignment, we are working with lists of characters. This function allows
   * you to easily create a character list from a given string.
   */
  def string2Chars(str: String): List[Char] = str.toList

  /**
   * This function computes for each unique character in the list `chars` the number of
   * times it occurs. For example, the invocation
   *
   *   times(List('a', 'b', 'a'))
   *
   * should    the following (the order of the resulting list is not important):
   *
   *   List(('a', 2), ('b', 1))
   *
   * The type `List[(Char, Int)]` denotes a list of pairs, where each pair consists of a
   * character and an integer. Pairs can be constructed easily using parentheses:
   *
   *   val pair: (Char, Int) = ('c', 1)
   *
   * In order to access the two elements of a pair, you can use the accessors `_1` and `_2`:
   *
   *   val theChar = pair._1
   *   val theInt  = pair._2
   *
   * Another way to deconstruct a pair is using pattern matching:
   *
   *   pair match {
   *     case (theChar, theInt) =>
   *       println("character is: "+ theChar)
   *       println("integer is  : "+ theInt)
   *   }
   */
    def times(chars: List[Char]): List[(Char, Int)] = {
      def createPair(x: Char): (Char, Int) = (x, chars.count { y => y==x    } )
      def doesNotcontainPair(pairList: List[(Char, Int)], char: Char): Boolean = 
      { pairList.forall(p => p match {case (p1, p2)=> char != p1}) }
      def addToList(pairList: List[(Char, Int)],  char: Char): List[(Char, Int)] = 
      { 
        if (!doesNotcontainPair(pairList, char)){ pairList;}//if
        else { createPair(char)::pairList;}//else
      }//addtoList
    
      def recTimesHelper(chars: List[Char], pairList:  List[(Char, Int)]): List[(Char, Int)] = {
        if (chars.isEmpty){  pairList ;}
        else {       recTimesHelper(chars.tail, addToList(pairList,  chars.head));    }
        }//rectimes
  
       recTimesHelper(chars, Nil);
  }
  
  /**
   *   s a list of `Leaf` nodes for a given frequency table `freqs`.
   *
   * The   ed list should be ordered by ascending weights (i.e. the
   * head of the list should have the smallest weight), where the weight
   * of a leaf is the frequency of the character.
   */
     def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = {
      def insertLeaf(leafList: List[Leaf], char: Char, integ: Int): List[Leaf] = {
        if (leafList.isEmpty){    List(Leaf(char, integ)); }
        else if(integ < leafList.head.weight  ) {    Leaf(char, integ)::leafList;}
        else {    leafList.head::insertLeaf(leafList.tail, char, integ);}
      }
      def recurMakeHelper(freq1: List[(Char, Int)], leafList1: List[Leaf]) : List[Leaf] =
      {
        if (freq1.isEmpty){   leafList1;}
        else {
             recurMakeHelper(freq1.tail, insertLeaf(leafList1, freq1.head._1, freq1.head._2))
        }
      }
         recurMakeHelper(freqs, Nil);
    }
  
  /**
   * Checks whether the list `trees` contains only one single code tree.
   */
     def singleton(trees: List[CodeTree]): Boolean = { if (trees.isEmpty){   false;}
      
         trees.tail.isEmpty;
    }
  
  /**
   * The parameter `trees` of this function is a list of code trees ordered
   * by ascending weights.
   *
   * This function takes the first two elements of the list `trees` and combines
   * them into a single `Fork` node. This node is then added back into the
   * remaining elements of `trees` at a position such that the ordering by weights
   * is preserved.
   *
   * If `trees` is a list of less than two elements, that list should be   ed
   * unchanged.
   */
     def combine(trees: List[CodeTree]): List[CodeTree] = {
      if (trees.isEmpty || singleton(trees)){    trees;}
      
       def insertTree(treeList: List[CodeTree], cTree: CodeTree): List[CodeTree] = {
        if (treeList.isEmpty){    List(cTree); }
        else if(weight(cTree) < weight(treeList.head)) {    cTree::treeList;}
        else {    treeList.head::insertTree(treeList.tail, cTree);}
      }
      
       def TreeHelper(trees1: List[CodeTree]): List[CodeTree] = 
       {
         if (trees1.isEmpty || singleton(trees1) ){    trees1;}
         else {
           val char2 = chars(trees1.head).union(chars(trees.tail.head));
           val int2 = weight(trees1.head)+ weight(trees1.tail.head);
           val newF = new Fork(trees1.head, trees.tail.head, char2, int2);
              insertTree(trees1.tail.tail, newF);
         }
       }
         TreeHelper(trees);
    }
  
  /**
   * This function will be called in the following way:
   *
   *   until(singleton, combine)(trees)
   *
   * where `trees` is of type `List[CodeTree]`, `singleton` and `combine` refer to
   * the two functions defined above.
   *
   * In such an invocation, `until` should call the two functions until the list of
   * code trees contains only one single tree, and then    that singleton list.
   *
   * Hint: before writing the implementation,
   *  - start by defining the parameter types such that the above example invocation
   *    is valid. The parameter types of `until` should match the argument types of
   *    the example invocation. Also define the    type of the `until` function.
   *  - try to find sensible parameter names for `xxx`, `yyy` and `zzz`.
   */
    def until(sing: List[CodeTree]=>Boolean, comb: List[CodeTree]=>List[CodeTree])(trees: List[CodeTree]): List[CodeTree] = {
    if (sing(trees)){    trees; }
    else {
         until(sing, comb)(comb(trees));
    }
    
    }
  
  /**
   * This function creates a code tree which is optimal to encode the text `chars`.
   *
   * The parameter `chars` is an arbitrary text. This function extracts the character
   * frequencies from that text and creates a code tree based on them.
   */
     def createCodeTree(chars: List[Char]): CodeTree = {
      
      val timeList = times(chars);
      val leafList = makeOrderedLeafList(timeList)
         until(singleton, combine)(leafList).head;
      
      
    }
  

  // Part 3: Decoding

  type Bit = Int

  /**
   * This function decodes the bit sequence `bits` using the code tree `tree` and   s
   * the resulting list of characters.
   */
      def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
    
      def recDecodeHelper(tree: CodeTree, bits: List[Bit], accumChar : List[Char], curTree : CodeTree)  : List[Char] = {
       
          curTree match {
            case Leaf(char, weightInt) => {
              if (!bits.isEmpty) {    recDecodeHelper(tree, bits, accumChar:+char, tree);}
              else {
                    accumChar:+char;
              }
            }
            case Fork(l, r, cList, weightInt) => 
              {
                if (bits.head==0){    recDecodeHelper(tree, bits.tail, accumChar, l); }
                else {    recDecodeHelper(tree, bits.tail, accumChar, r); }
                    
              }
          }
        
      }
      print( recDecodeHelper(tree, bits, Nil, tree) );
       recDecodeHelper(tree, bits, Nil, tree);
  }
  
  /**
   * A Huffman coding tree for the French language.
   * Generated from the data given at
   *   http://fr.wikipedia.org/wiki/Fr%C3%A9quence_d%27apparition_des_lettres_en_fran%C3%A7ais
   */
  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s',121895),Fork(Leaf('d',56269),Fork(Fork(Fork(Leaf('x',5928),Leaf('j',8351),List('x','j'),14279),Leaf('f',16351),List('x','j','f'),30630),Fork(Fork(Fork(Fork(Leaf('z',2093),Fork(Leaf('k',745),Leaf('w',1747),List('k','w'),2492),List('z','k','w'),4585),Leaf('y',4725),List('z','k','w','y'),9310),Leaf('h',11298),List('z','k','w','y','h'),20608),Leaf('q',20889),List('z','k','w','y','h','q'),41497),List('x','j','f','z','k','w','y','h','q'),72127),List('d','x','j','f','z','k','w','y','h','q'),128396),List('s','d','x','j','f','z','k','w','y','h','q'),250291),Fork(Fork(Leaf('o',82762),Leaf('l',83668),List('o','l'),166430),Fork(Fork(Leaf('m',45521),Leaf('p',46335),List('m','p'),91856),Leaf('u',96785),List('m','p','u'),188641),List('o','l','m','p','u'),355071),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u'),605362),Fork(Fork(Fork(Leaf('r',100500),Fork(Leaf('c',50003),Fork(Leaf('v',24975),Fork(Leaf('g',13288),Leaf('b',13822),List('g','b'),27110),List('v','g','b'),52085),List('c','v','g','b'),102088),List('r','c','v','g','b'),202588),Fork(Leaf('n',108812),Leaf('t',111103),List('n','t'),219915),List('r','c','v','g','b','n','t'),422503),Fork(Leaf('e',225947),Fork(Leaf('i',115465),Leaf('a',117110),List('i','a'),232575),List('e','i','a'),458522),List('r','c','v','g','b','n','t','e','i','a'),881025),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u','r','c','v','g','b','n','t','e','i','a'),1486387)

  /**
   * What does the secret message say? Can you decode it?
   * For the decoding use the `frenchCode' Huffman tree defined above.
   */
  val secret: List[Bit] = List(0,0,1,1,1,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1)

  /**
   * Write a function that   s the decoded secret
   */
     def decodedSecret: List[Char] = {
    
         decode(frenchCode, secret);
  }
  

  // Part 4a: Encoding using Huffman tree

  /**
   * This function encodes `text` using the code tree `tree`
   * into a sequence of bits.
   */
    def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {
                def recEncodeHelper(tree: CodeTree, textRemaining: List[Char], curTree: CodeTree, aggBits: List[Bit]): List[Bit] = 
                {
                  if (textRemaining.isEmpty) {    aggBits;}
                  else {
                        curTree match{
                          case Leaf(char, weightInt) =>    recEncodeHelper(tree, textRemaining.tail, tree, aggBits);
                          case Fork(l, r, cList, weightInt) => {
                            if (chars(l).contains(textRemaining.head)){    recEncodeHelper(tree, textRemaining, l, aggBits:+0);}
                            else {    recEncodeHelper(tree, textRemaining, r, aggBits:+1);}
                          }
                        }
                    }
                 
                }
      //println( recEncodeHelper(tree, text, tree, Nil) );
         recEncodeHelper(tree, text, tree, Nil);
    }
  
  // Part 4b: Encoding using code table

  type CodeTable = List[(Char, List[Bit])]

  /**
   * This function   s the bit sequence that represents the character `char` in
   * the code table `table`.
   */
  
   
   def sortByChar(x:(Char, List[Bit]), y: (Char, List[Bit])) = {
    x match { case (a, b) => y match { case (c, d) =>  a > c; }}
    
    }
    
    def sortTableAlph(ct: CodeTable) : CodeTable = {
        return ct.sortWith(sortByChar)
    }
    def codeBits(table: CodeTable)(char: Char): List[Bit] = 
    {
      def recCBHelper(t1: CodeTable) : List[Bit] = {
        if (t1.head._1 == char) {    t1.head._2;}
        else {    recCBHelper(t1.tail); }
      }
        recCBHelper(table);
    }
  
  /**
   * Given a code tree, create a code table which contains, for every character in the
   * code tree, the sequence of bits representing that character.
   *
   * Hint: think of a recursive solution: every sub-tree of the code tree `tree` is itself
   * a valid code tree that can be represented as a code table. Using the code tables of the
   * sub-trees, think of how to build the code table for the entire tree.
   */
    def convert(tree: CodeTree): CodeTable = {
      
        def recConvert(curTree: CodeTree): CodeTable = {
         curTree match {
           case Leaf(ch, intVal) =>    List((ch, Nil));
           case Fork(l, r, cList, weightInt) =>    mergeCodeTables(recConvert(l), recConvert(r));
         }
        }
           recConvert(tree);
      
    }
  
  /**
   * This function takes two code tables and merges them into one. Depending on how you
   * use it in the `convert` method above, this merge method might also do some transformations
   * on the two parameter code tables.
   */
    def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable ={
      
      //val newRight = Nil;
      
      def prependToAll(b1: CodeTable, newVal: CodeTable, intVal : Int ): CodeTable = {
        
        if (b1.isEmpty){   newVal;}
        else {
          b1.head match { case (ch, bitList) =>    prependToAll(b1.tail, (ch, intVal::bitList)::newVal, intVal);}
        }
      }
         prependToAll(a, Nil, 0).union( prependToAll(b, Nil, 1) );
    }
  
  /**
   * This function encodes `text` according to the code tree `tree`.
   *
   * To speed up the encoding process, it first converts the code tree to a code table
   * and then uses it to perform the actual encoding.
   */
    
   def bitString(bits: List[Bit]): String = {
     def recString(bits: List[Bit], curString : String): String= {
       if (bits.isEmpty){ return curString;}
       else { recString(bits.tail, curString + bits.head) }
     }
     return recString(bits, "");
   }
    
   def conCatStringTable(ct : CodeTable, curString: String) : String = {
       if (ct.tail.isEmpty) { return curString + ct.head._1 + ": " + bitString(ct.head._2) + ";";}
       val str = curString + ct.head._1 + ": " + bitString(ct.head._2) + "; ";
       return conCatStringTable(ct.tail, str);
   }
    
    def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = {
      val table = convert(tree);
     // println("here");
     // println(table);
      def recEncode(remainingText:List[Char], aggBits: List[Bit]) : List[Bit] = {
        //println("here2");
        //println(aggBits);
        if (remainingText.isEmpty){   aggBits;}
        else {
             recEncode(remainingText.tail, aggBits:::codeBits(table)(remainingText.head));
        }
      }
           recEncode(text, Nil);
    }
  }
