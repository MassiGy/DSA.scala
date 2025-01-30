sealed trait Stream[+A] {
    import Stream._    // this will import the static methods of the object below

    def headOption:Option[A] = this match {
        case Empty => None
        case Cons(h, t) => Some(h())
    } 

    def toList:List[A] = this match {
        case Empty => Nil
        case Cons(h,t) => h()::t().toList
    }


    def take(count: Int): Stream[A] = this match {
        case Empty => Empty
        case _ if count == 0 => Empty
        case Cons(h, t) => cons(h(), t().take(count - 1))
    }


    def takeWhile(p: A=>Boolean): Stream[A] = this match {
        case Empty => Empty
        case Cons(h, t) => if p(h()) then cons(h(), t().takeWhile(p)) else Empty
    }

    final def drop(n:Int): Stream[A] = this match {
       case Empty => Empty 
       case Cons(h,t) => if n > 0 then t().drop(n-1) else this
    }

    def dropWhile(p: A=>Boolean):Stream[A] = this match {
        case Empty => Empty 
        case Cons(h, t)=> if p(h()) then t().dropWhile(p) else this
    }


    /* correction du prof */
    // Si `p(h())` vaut vrai, on n'évalue jamais `t()`, donc la
    // récursivité sur `t.exists(p)` n'est jamais lancée.
    def exists(p:A => Boolean):Boolean = this match {
         case Cons(h, t) => p(h()) || t().exists(p)
         case _ => false
    }



    /* correction du prof */
    def foldRight[B](z: => B)(f:(A, => B) => B): B = this match {
        case Cons(h,t) => f(h(), t().foldRight(z)(f))
        case _ => z
    }

    def forAll(p: A=>Boolean):Boolean = !(exists(e => !p(e)))

    def forAll2(p: A=>Boolean):Boolean = this match {
         case Cons(h, t) => p(h()) && t().forAll(p)
         case _ => false
    }




}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t:()=>Stream[A]) extends Stream[A]

object Stream {
    def cons[A](hd: => A, tl: =>Stream[A]): Stream[A] = {
        lazy val h = hd
        lazy val t = tl 
        Cons(()=>h, ()=>t)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
                  if (as.isEmpty) empty
                  else cons(as.head, apply(as.tail*))

    def foldr[A,B](s: Stream[A], z: B)(f: ( a: A, b:B) => B):B = s match {
        case Empty => z
        case Cons(h, t) => f(h(), foldr(t(), z)(f))
    }

    def foldr2[A,B](s: Stream[A], z: B, stopif:A=>Boolean)(f: (A,B) => B):B = s match {
        case Empty => z
        case Cons(h, t) => if !stopif(h()) then f(h(), foldr(t(), z)(f)) else z
    }


}


object TestStream {
    import Stream._

    def main(args: Array[String]):Unit = {
        val stream = Stream(1,11,3,5)
        println(stream.toList)

        println(stream.drop(2).toList)
        println(stream.take(2).toList)
        println(stream.takeWhile(a => a <= 3).toList)
        println(stream.dropWhile(a => a <= 3).toList)
        println(foldr(stream, 0)((a,b)=> a + b))
        println(foldr2(stream, 1, e => e == 0)((a,b)=> a * b))
        println(stream.forAll2(a=>a%2!=0))
    }
}
