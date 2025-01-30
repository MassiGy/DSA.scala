/*
	@author: Massiles GHERNAOUT
	@login: gm213204
	@url: https://www-apps.univ-lehavre.fr/forge/gm213204/scala-trees


	@description:


	This project aims at implementing a basic version of a trie¹ using a functionnal
	programming paradigm expressed in Scala.

	We will make sure that all of our functions are pur and totaly deterministic. Thus,
	we will avoid side effects and we will leverage the Optinal/Maybe monads in case of
	a function that is not able to respect its signature 100% of the time.



 	¹: a tree used as a key dictionnary.


	@improvements:
	
	- Try to use helper functions in order to make most our methods be a tail recursion 
	(for compiler optimisation)
	
	- For the remove function, add a logic that removes the left behind branches that do not lead to a key.
	(
   for the remove function: 
   
   * I've tried it, without success - see the git commits as a reference.
   
   Encountred problems: 
   * The main problem is that at this point the functionnal programming paradigm is not helping at all!
   It is much more easier to consider this problem with an imperative approach, since it translates to a 
   set of cases to react to.

   In every attempt of my own, I either end up with a bunch of nested if statements or match cases, and for
   me that a "no go" if you want to stay in the functionnal programming paradigm 'style' .
  )

  
  @scala.version: Scala code runner version 3.3.3 -- Copyright 2002-2024, LAMP/EPFL
  
  @note: done without any AI assistance, except for the show() method.
	
  @note: Some warnnings maybe thrown depending on the scala.version used for the execution. 
  
  @note: If you get a warnning as: "the type test for Node[A] cannot be checked at runtime because its type arguments can't be determined from Tree[A]",
  these ones can be ignored, it is just because in our on-root match cases we do not match the nodes as Node(value, c, next, left, right) but
  as node:Node[A], that is it!

  We can wrap the second match case by another case Node(value, c, next, left, right), but I choosed not to since it will just make the code 
  less readable and add indentation.
 */

sealed trait Tree[+A] {

  import Tree._

  // insert the key to the tree, and attach to it the given value
  def insert[B >: A](key: String, value: B): Tree[B] =
    Tree.insert(this, key, value, 0)
  
  // fetch a key if any (maybe of value)
  def get(key: String): Option[A] = Tree.get(this, key, 0)

  // predicate on the existance of a key
  def contains(key: String): Boolean = Tree.contains(this, key)

  // count of inserted keys
  def size(): Int = Tree.size(this)
  
  /*
  // get the list of inserted keys (list[String]) 
  def toListOfKeys(): List[String] =
    Tree.toListOfKeys(this, "", Nil: List[String]).toSet.toList // .toSet.toList to remove the duplicates

  // get the list of values (List[A])
  def toListOfValues(): List[A] = Tree.toListOfValues(this, Nil: List[A]) // we can not add .toSet.toList since the values can be equal!
  */
  
  // a zip between the inserted keys and their respective values
  def toKeyValueList(): List[(String, A)] =
    Tree.toKeyValueList(this, "", Nil: List[(String, A)]).toSet.toList // the .toSet.toList removes duplicates but does not garentee the same order !

  
  // these are better, since they intelligently remove all the duplicates
  def toListOfValues(): List[A] = Tree.toKeyValueList(this, "", Nil: List[(String, A)]).toSet.toList.map((k,v) => v)
  def toListOfKeys(): List[String] = Tree.toKeyValueList(this, "", Nil: List[(String, A)]).toSet.toList.map((k,v) => k)

  
  
  
  // @note: be careful about the type signature, (used the same black magic as in the insert above)
  def remove[B >: A](key: String): (Option[Node[B]], Tree[B]) =
    Tree.remove(this, key, 0)

  // @note: this is a utility function, just to visualize the tests
  def show(): String = Tree.show(this, 0)
}
case object Leaf extends Tree[Nothing]
case class Node[A](
    value: Option[A],
    char: Char,
    left: Tree[A],
    next: Tree[A],
    right: Tree[A]
) extends Tree[A]

case object Tree {

  // for the constructor (to get the parentheses operator)
  def apply[A](): Tree[A] = Leaf

  // @note: this function causes a warnning to be thrown at compile time
  def insert[A](root: Tree[A], key: String, value: A, n: Int): Tree[A] =
    root match {
      case Leaf =>
        insert(Node(None, key.charAt(n), Leaf, Leaf, Leaf), key, value, n)
      case node: Node[A] if (node.char > key.charAt(n)) =>
        node.copy(left = insert(node.left, key, value, n))
      case node: Node[A] if (node.char < key.charAt(n)) =>
        node.copy(right = insert(node.right, key, value, n))
      case node: Node[A] if (n < key.length - 1) =>
        node.copy(next = insert(node.next, key, value, n + 1))
      case node: Node[A] => node.copy(value = Some(value))
    }

  // a projection of insert if A is Boolean.
  def insert(root: Tree[Boolean], key: String): Tree[Boolean] =
    insert(root, key, true, 0)

  // returns the count of inserted ( reachable ) keys  
  // only increment the count if a value is reached !
  def size[A](root: Tree[A]): Int = root match {
    case Leaf => 0

    case Node(value, _, left, next, right) =>
      value match {
        case None    => size(next) + size(left) + size(right)
        case Some(_) => 1 + size(next) + size(left) + size(right)
      }
  }

  // first variante of the toList: list of all values:A
  def toListOfValues[A](root: Tree[A], acc: List[A]): List[A] = root match {
    case Leaf => acc

    case node: Node[A] =>
      node match {

        case Node(value, c, left, next, right) =>
          value match {

            // if value == None, the accumulator stays the same
            case None =>
              toListOfValues(
                next,
                acc
              ) ++ // explore the next sub tree
                toListOfValues(
                  left,
                  acc
                ) ++ // explore the left adjacent tree
                toListOfValues(
                  right,
                  acc
                ) // explore the right adjacent tree

            // we only push to the accumulator if we arrive to a true value,
            case Some(v) =>
              toListOfValues(
                next,
                acc ++ List(v)
              ) ++ // found a value, ack it, and then explore the next sub tree
                toListOfValues(
                  left,
                  acc
                ) ++ // explore the left adjacent tree, ( do not ack the value again )
                toListOfValues(
                  right,
                  acc
                ) // explore the right adjacent tree ( do not ack the value again )
          }
      }
  }
  
  


  // second variante of the toList: list of all keys:String
  def toListOfKeys[A](
      root: Tree[A],
      visitedKeySegments: String,
      acc: List[String]
  ): List[String] = root match {
    case Leaf => acc

    case node: Node[A] =>
      node match {

        case Node(value, c, left, next, right) =>
          value match {

            // if value == None, the accumulator stays the same
            case None =>
              toListOfKeys(
                next,
                visitedKeySegments + c,
                acc
              ) ++ // explore the next sub tree
                toListOfKeys(
                  left,
                  visitedKeySegments,
                  acc
                ) ++ // explore the left adjacent tree
                toListOfKeys(
                  right,
                  visitedKeySegments,
                  acc
                ) // explore the right adjacent tree

            // we only push to the accumulator if we arrive to a true value,
            case Some(v) =>
              toListOfKeys(
                next,
                visitedKeySegments + c,
                acc ++ List(visitedKeySegments + c)
              ) ++ // found a value, ack the character, and then explore the next sub tree
                toListOfKeys(
                  left,
                  visitedKeySegments,
                  acc
                ) ++ // pivot to the left without acking the current char
                toListOfKeys(
                  right,
                  visitedKeySegments,
                  acc
                ) // pivot to the right without acking the current char
          }
      }
  }

  def get[A](root: Tree[A], key: String, n: Int): Option[A] = root match {

    case Leaf => None // if we hit a leaf, then return None

    case node: Node[A] if node.char > key.charAt(n) =>
      get(
        node.left,
        key,
        n
      ) // explore left adjacent tree

    case node: Node[A] if node.char < key.charAt(n) =>
      get(
        node.right,
        key,
        n
      ) // explore right adjacent tree

    case node: Node[A] if n < key.length - 1 =>
      get(
        node.next,
        key,
        n + 1
      ) // explore next sub tree

    case node: Node[A] if n == key.length - 1 && node.char == key.charAt(n) =>
      node.value // found the last segement of the key, return the host node value

    // @note: if the insert function and the trie are proprely designed, we could
    // remove the if condition on the last case

    // i.e: we can remove: if n == key.lenght - 1 && node.char == key.charAt(n)

    case _ => None // this should be never reached
  }

  def contains[A](root: Tree[A], key: String): Boolean = {
    // leverage get()
    get(root, key, 0) match {
      case None => false
      case _    => true
    }
  }
  
  
  

  def toKeyValueList[A](
      root: Tree[A],
      visitedKeySegments: String,
      acc: List[(String, A)]
  ): List[(String, A)] = root match {

    case Leaf => acc // end of recursion, return our accumulator

    case node: Node[A] =>
      node match {
        // no need for a Leaf match case,

        // to avoid pushing to the accumulator all the visitedKeySegments (partial segments), we are going to
        // match on the state of the value attribute.
        case Node(value, c, left, next, right) =>
          value match {

            // if value == None, the accumulator stays the same
            case None =>
              toKeyValueList(
                next,
                visitedKeySegments + node.char,
                acc
              ) ++ // move on by acking the current node char
                toKeyValueList(
                  left,
                  visitedKeySegments,
                  acc
                ) ++ // pivot to the left without acking the current char
                toKeyValueList(
                  right,
                  visitedKeySegments,
                  acc
                ) // pivot to the right without acking the current char

            // we only push to the accumulator if we arrive to a true value,
            case Some(v) =>
              toKeyValueList(
                next,
                visitedKeySegments + node.char,
                acc ++ List((visitedKeySegments + node.char, v))
              ) ++ // move on by acking the current node and adding it to the accumulator since we have a true value v
                toKeyValueList(
                  left,
                  visitedKeySegments,
                  acc
                ) ++ // pivot to the left without acking the current char
                toKeyValueList(
                  right,
                  visitedKeySegments,
                  acc
                ) // pivot to the right without acking the current char
          }
      }
  }
  
/*
 // this version should also remove the left behind branches that do not lead to a key, but it does not 
 // work, it seems like I have a reference error or something like that. (left it here to showcase the idea)

  def remove[A](
      root: Tree[A],
      key: String,
      n: Int
  ): (Option[Node[A]], Tree[A]) = root match {
    case Leaf => (None, Leaf) // If the tree is empty, nothing to remove

    case node: Node[A] if node.char > key.charAt(n) =>
      // If the current node's char is greater than the current key character, go left
      val (removedNode, newLeftTreeRef) = remove(node.left, key, n)
      val newTreeRef = node.copy(left = newLeftTreeRef)
      
      
      if (newTreeRef.value == None && newTreeRef.next == Leaf && newTreeRef.left == Leaf && newTreeRef.right == Leaf) {
           (removedNode, Leaf)

      } else if (newTreeRef.value == None && newTreeRef.next == Leaf && newTreeRef.left != Leaf && newTreeRef.right == Leaf) {
           (removedNode, newTreeRef.left)

      } else if(newTreeRef.value == None && newTreeRef.next == Leaf && newTreeRef.left == Leaf && newTreeRef.right != Leaf){
           (removedNode, newTreeRef.right)

      }else {
           (removedNode, newTreeRef)
      }


    case node: Node[A] if node.char < key.charAt(n) =>
      // If the current node's char is less than the current key character, go right
      
      val (removedNode, newRightTreeRef) = remove(node.right, key, n)
      val newTreeRef = node.copy(right = newRightTreeRef)
      
      
      if (newTreeRef.value == None && newTreeRef.next == Leaf && newTreeRef.left == Leaf && newTreeRef.right == Leaf) {
           (removedNode, Leaf)

      } else if (newTreeRef.value == None && newTreeRef.next == Leaf && newTreeRef.left != Leaf && newTreeRef.right == Leaf) {
           (removedNode, newTreeRef.left)

      } else if(newTreeRef.value == None && newTreeRef.next == Leaf && newTreeRef.left == Leaf && newTreeRef.right != Leaf){
           (removedNode, newTreeRef.right)
           
      }else {
           (removedNode, newTreeRef)
      }



    case node: Node[A] if n < key.length - 1 =>
      // If we haven't reached the end of the key yet, go down the next node
      val (removedNode, newNextTreeRef) = remove(node.next, key, n)
      val newTreeRef = node.copy(next = newNextTreeRef)
      
      
      if (newTreeRef.value == None && newTreeRef.next == Leaf && newTreeRef.left == Leaf && newTreeRef.right == Leaf) {
           (removedNode, Leaf)

      } else if (newTreeRef.value == None && newTreeRef.next == Leaf && newTreeRef.left != Leaf && newTreeRef.right == Leaf) {
           (removedNode, newTreeRef.left)

      } else if(newTreeRef.value == None && newTreeRef.next == Leaf && newTreeRef.left == Leaf && newTreeRef.right != Leaf){
           (removedNode, newTreeRef.right)
           
      }else {
           (removedNode, newTreeRef)
      }


    case node: Node[A] if n == key.length - 1 =>
      // We're at the end of the key, remove the value from this node
      if (node.value.isEmpty) {
        // If the value is None, then the key does not exist
        (None, root)
      } else {
  
        // keep a copy of the node that we will remove to return it afterwards, but    
        // override next,left and right since it is possible that they are needed
         
        
        
        // If the value is Some(v), clear it (i.e., remove the key-value pair)
        val newNode = node.copy(value = None)

        // Now we need to check if this node can be removed completely
        if (
          newNode.left == Leaf && newNode.right == Leaf && newNode.next == Leaf
        ) {
          (Some(node), Leaf) // If this node has no children, remove it
        } else {
          (Some(node), newNode) // Otherwise, keep the node (without its value)
        }
      }

    // this code should not be reached
    case _ =>
      (None, root)
  }
*/
  
  def remove[A](
      root: Tree[A],
      key: String,
      n: Int
  ): (Option[Node[A]], Tree[A]) = root match {
    case Leaf => (None, Leaf) // If the tree is empty, nothing to remove

    case node: Node[A] if node.char > key.charAt(n) =>
      // If the current node's char is greater than the current key character, go left
      val (removedNode, newTreeRef) = remove(node.left, key, n)
      (removedNode, node.copy(left = newTreeRef))

    case node: Node[A] if node.char < key.charAt(n) =>
      // If the current node's char is less than the current key character, go right
      val (removedNode, newTreeRef) = remove(node.right, key, n)
      (removedNode, node.copy(right = newTreeRef))

    case node: Node[A] if n < key.length - 1 =>
      // If we haven't reached the end of the key yet, go down the next node
      val (removedNode, newTreeRef) = remove(node.next, key, n + 1)
      (removedNode, node.copy(next = newTreeRef))

    case node: Node[A] if n == key.length - 1 =>
      // We're at the end of the key, remove the value from this node
      if (node.value.isEmpty) {
        // If the value is None, then the key does not exist
        (None, root)
      } else {
  
        // keep a copy of the node that we will remove to return it afterwards, but    
        // override next,left and right since it is possible that they are needed
        val _node = node.copy(next=Leaf, left=Leaf, right=Leaf) 
        
        
        // If the value is Some(v), clear it (i.e., remove the key-value pair)
        val newNode = node.copy(value = None)

        // Now we need to check if this node can be removed completely
        if (
          newNode.left == Leaf && newNode.right == Leaf && newNode.next == Leaf
        ) {
          (Some(_node), Leaf) // If this node has no children, remove it
        } else {
          (Some(_node), newNode) // Otherwise, keep the node (without its value)
        }
      }

    // this code should not be reached
    case _ =>
      (None, root)
  }

  
  
  def show[A](tree: Tree[A], indent: Int): String = tree match {
    case Leaf => " " * indent + "Leaf\n"
    case Node(value, char, left, next, right) =>
      val valueStr = value match {
        case Some(v) => s" -> ${v}"
        case None    => ""
      }
      val current = " " * indent + s"Node(${char}$valueStr)\n"

      // do not indent left and right since they are not sub trees
      // but adjacent trees ! 
      current  + show(left, indent) + show(next, indent+2) + show(
        right,
        indent
      )
  }
}

object TestTree {
  def main(args: Array[String]): Unit = {

    println("\n\n")
    println("----------------------------------------")
    val tree = Tree[Boolean]()
      .insert("chat", true)
      .insert("ch", true)
      .insert("c", true)
      .insert("coq", true)
      .insert("pie", true)
      .insert("chien", true)
      .insert("moto", true)
      
    println(tree.show())

    println("\n----------------------------------------")
    println(
      s"Taille de l'arbre (nombre de paires clé/valeur) : ${tree.size()}"
    )

    println("\n----------------------------------------")
    println(tree.toListOfKeys())
    println(tree.toListOfValues())

    println("\n----------------------------------------")
    println(tree.get("chat"))
    println(tree.get("chien"))
    println(tree.get("coq"))
    println(tree.get("pie"))
    println(tree.get("voiture"))

    println("\n----------------------------------------")
    println(tree.contains("chat"))
    println(tree.contains("chien"))
    println(tree.contains("coq"))
    println(tree.contains("pie"))
    println(tree.contains("voiture"))

    println("\n----------------------------------------")
    println(tree.toKeyValueList())

    println("\n----------------------------------------")
    val (n, tree2) = tree.remove("moto")
    println("Key to remove: moto")
    println(tree2.show())
    println(s"Removed node: ${n}")

    println("\n----------------------------------------")
    println(tree2.toKeyValueList())


    println("\n----------------------------------------")
    val (n1, tree3) = tree2.remove("chat")
    println("key to remove: chat")
    println(tree3.show())
    println(s"Removed node: ${n1}")

    println("\n----------------------------------------")
    println(tree3.toKeyValueList())
    
    println("\n----------------------------------------")
    val (n2, tree4) = tree3.remove("ch")
    println("key to remove: ch")
    println(tree4.show())
    println(s"Removed node: ${n2}")

    println("\n----------------------------------------")
    println(tree4.toKeyValueList())


    println("\n----------------------------------------")
    val (n3, tree5) = tree4.remove("c")
    println("key to remove: c")
    println(tree5.show())
    println(s"Removed node: ${n3}")

    println("\n----------------------------------------")
    println(tree5.toKeyValueList())
  }
}

