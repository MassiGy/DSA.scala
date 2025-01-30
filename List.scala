sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail*))

  def tail[A](l: List[A]): List[A] =
    l match
      // case Nil => sys.error("list is nil")    // this gets us out of our type space but it is much safer to do so, fail fast fail hard
      case Nil         => Nil
      case Cons(x, xs) => xs

  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else drop(tail(l), n - 1)

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] =
    l match
      case Nil                            => l
      case Cons(head, tail) if (!f(head)) => l
      case Cons(head, tail)               => dropWhile(tail)(f)

  def takeWhile[A](l: List[A])(f: A => Boolean): List[A] =
    l match
      case Nil                            => l
      case Cons(head, tail) if (!f(head)) => Nil
      case Cons(head, tail)               => Cons(head,takeWhile(tail)(f))
  /*
    def init[A](l : List[A]): List[A] = {
        l match
            case Nil => l
            case Cons(x, xs) =>  {
                xs match
                    case Nil => Nil     // this should be never reached
                    case Cons(h, Nil) => l
                    case Cons(h, t) =>Cons(h, init(t))
            }

    }
   */

  def init[A](l: List[A]): List[A] = {
    l match
      case Nil          => l
      case Cons(x, Nil) => Nil
      case Cons(x, xs)  => Cons(x, init(xs))
  }

  /*
    def head[A](l : List[A]): A = l match
        case Cons(head, tail) => head

    def concat[A](fst: List[A], snd: List[A]): List[A] = Cons(head(fst), concat(tail(fst), snd))

    def reverse[A](l: List[A]) : List[A] =
        l match
            case Nil => l
            case Cons(head, tail) => concat(reverse(tail), List(head))

   */

  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
    l match
      case Nil         => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match
      case Nil         => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }
  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((b, a) => Cons(a, b))

  def foldLeft2[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(reverse(l), z)((a, b) => f(b, a))

  def foldRight2[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(l), z)((a, b) => f(b, a))
  }

  def myAppend[A](l: List[A], t: List[A]): List[A] = {
    foldRight(l, t)((a, b) => Cons(a, b))
  }

  def myAppend2[A](l: List[A], t: List[A]): List[A] = {
    reverse(
      foldLeft(t, reverse(l))((b, a) => Cons(a, b))
    )
  }
  def myAppend3[A](l: List[A], t: List[A]): List[A] = {
    foldLeft(reverse(l), t)((b, a) => Cons(a, b))
  }

  def flatten[A](l: List[List[A]]): List[A] = 
    foldRight(l, List[A]())(myAppend)


  def myMap[A, B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[B])((a,b) => Cons(f(a), b))

  def myFilter[A](l: List[A])(f: A => Boolean): List[A] = 
    foldRight(l, Nil:List[A])((a,b) => if f(a) then Cons(a,b) else  b)
  
  def myFlatMap[A,B](l: List[A])(f: A => List[B]): List[B] = 
    flatten(myMap(l)(f))

  def myFilterWithFlatMap[A](l: List[A])(f: A => Boolean): List[A] = {
    myFlatMap(l)(a => if f(a) then List(a) else Nil)
  }
  // this function can be generalized to an A type that has a Numeric 
  // type constraint/type class (this will require from it an (+) operator)
  def addLists(fst: List[Int], snd: List[Int]): List[Int] = {
     (fst, snd) match {
       case (Nil, Nil) => Nil
       case (xs, Nil) => xs
       case (Nil, ys) => ys
       case (Cons(x,xs), Cons(y,ys)) => Cons(x+y, addLists(xs,ys))
     }
  }


  def zip[A,B](fst: List[A], snd: List[B]): List[(A,B)] = {
    (fst, snd) match {
      case (Cons(x,xs), Cons(y,ys)) => Cons((x,y), zip(xs,ys))
      case (_, _) => Nil    // if at least one is Nil then return Nil
    }
  }


  def hasSubsequence[A](l:List[A], sub:List[A]):Boolean = {
    (l, sub) match {
      case (_, Nil) => true
      case (Nil, _) => false
      case (Cons(x, xs), Cons(y, ys)) if x == y => hasSubsequence(xs, ys)
      case (Cons(_, xs), sub) => hasSubsequence(xs, sub)
    }
  }

  def hasSubsequence2[A](l:List[A], sub:List[A]):Boolean = {
    (l, sub) match {
      case (_, Nil) => true
      case (Nil, _) => false
      case (Cons(x, xs), Cons(y, ys))  =>if x == y then hasSubsequence(xs, ys) else hasSubsequence(xs, Cons(y,ys))
    }
  }

  def stringify[A](l: List[A]): String = {
    l match {
      case Nil => "Nil"
      case Cons(x,xs)=> ""+x+"->"+stringify(xs)
    }
  }
  def head[A](l: List[A]):A= {
    l match {
      case Nil => sys.error("can not call head on empty list")
      case Cons(x, _)=> x
    }
  }
  def length[A](l: List[A]): Int = {
    l match {
      case Nil => 0
      case Cons(_,xs)=> 1+length(xs)
    }
  }
  def equals[A](fst: List[A], snd: List[A]): Boolean = {
    (fst,snd) match {
      case (Nil, Nil)=> true
      case (Cons(x,xs), Cons(y,ys)) => if x == y then equals(xs,ys) else false
      case (_, _) => false
    }
  }


  def main(args: Array[String]) = {
    val l = List(1, 2, 3, 4)
      
    // println(dropWhile(l){el => el < 3})

    // val fn = dropWhile(l)
    // println(fn{el => el < 3})

    // println(foldRight(l, 0)((a, b) => a + b))
    // println(foldRight(l, 0)((_, acc) => acc + 1))

    // println()

    // println(foldLeft(l, 0)((b, a) => a + b))
    // println(foldLeft(l, 1)((b, a) => a * b))
    // println(foldLeft(l, 0)((acc, _) => acc + 1))

    // println()
    // println(foldRight(l, Nil: List[Int])((a, b) => Cons(a, b)))
    // println(foldLeft(l, Nil: List[Int])((b, a) => Cons(a, b)))

    // println()
    // println(foldRight2(l, Nil: List[Int])((a, b) => Cons(a, b)))
    // println(foldLeft2(l, Nil: List[Int])((b, a) => Cons(a, b)))

    // println()
    // println(myAppend(l, List(4)))
    // println(myAppend2(l, List(4)))
    // println(myAppend3(l, List(4)))

    // println(flatten(List(l))) // this should be equal to l

    // this adds 1 to every elem of l
    // println(foldRight(l, Nil:List[Int])((a,b) => Cons(a+1,b))) 

    // this will transfom every elem of l to a string (dollard price)
    // println(foldRight(l, Nil:List[String])((a,b) => Cons("$"+a,b))) 

    // this is a re-implementation of the map function
    // println(myMap(l)((a) => "$"+a))

    // println(myFilter(l)(a => a > 1))

    // println() 
    // println(myMap(l)(a => List(a, a %2 ==0)))
    // println(myFlatMap(l)(a => List(a, a %2 ==0)))
    println(myFilterWithFlatMap(l)(n => n %2== 0))
    println(myFilterWithFlatMap(l)(n => n >= 2))

    println()
    println(stringify(zip(List(1,2,3), List("A", "B", "C"))))
    println(
    stringify(
         myMap( 
           zip(List(1,2,3), List(10, 20, 30))
         )((a,b)=> a+b)
       )
     )

    println(stringify(takeWhile(l)(el => el < 4)))
    println(hasSubsequence(List(2,1,4,3), List(4,3)))
    println(hasSubsequence2(List(2,1,4,3), List(4,3)))


    println(stringify(addLists(List(1,2,3), List(10, 20, 30))))


  }

}

