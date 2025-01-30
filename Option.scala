sealed trait Option[+A] {

    def getOrElse[B >: A](default: => B):B = {
        this match {
            case None => default
            case Some(a) => a
        }
    }

    def orElse[B >: A](default: => Option[B]): Option[B] = {
        this match {
            case None => default
            case Some(a) => Some(a) // this will be casted
        }
    }

    def filter(f: A => Boolean): Option[A] = {
        this match {
            // case Some(a) => if f(a) then this else None
            case Some(a) if f(a) => this
            case _ => None
        }
    }
    def map[B](f: A => B): Option[B] = {
        this match {
            case None => None
            case Some(a)  => Some(f(a))
        }
    }
    def flatMap[B](f: A => Option[B]): Option[B] = {
        this match {
            case None => None
            case Some(a) => f(a)
        }
    }
}

case object None extends Option[Nothing]    // case object to get a singleton (it is like static)
case class Some[+A](get: A) extends Option[A]

object Option {
    def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C):Option[C] = {
        (a,b) match {
            case (Some(v), Some(w)) => Some(f(v,w))
            case _ => None
        }
    }
    def map3[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C):Option[C] = 
        a.flatMap(x => b.map(y => f(x,y)))
    
    def sequence[A](a:List[Option[A]]): Option[List[A]] = {
        a match {
            case Nil => Some(Nil:List[A])
            case None :: xs => None
            case Some(v) :: xs => sequence(xs).map(w => v :: w)

        }
    }
}


object TestOption {
    import Option._

    def main(args: Array[String]): Unit = {
        val a = None
        println(a.orElse(Some("empty")))

        val b = Some("hello world")
        println(a.orElse(Some("empty")).map(el => el.length))
        println(b.orElse(Some("empty")).map(el => el.length))
        println()


        val list = List(Some(1), Some(2))
        println(sequence(list))
    

}


