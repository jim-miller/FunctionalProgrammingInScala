import com.jimmiller.chapter03._
import com.jimmiller.chapter03.List._

// Exercise 3.8
//
// foldRight(), like apply(), uses recursion to construct
// the list from head to tail (i.e. left to right)

foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))

// Traced out:
foldRight(Cons(1, Cons(2, Cons(3, Nil))), Nil:List[Int])(Cons(_,_))
Cons(1, foldRight(Cons(2, Cons(3, Nil)), Nil: List[Int])(Cons(_,_)))
Cons(1, Cons(2, foldRight(Cons(3, Nil), Nil: List[Int])(Cons(_,_))))
Cons(1, Cons(2, Cons(3, Nil)))