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


// Exercise 3.9
// foldRight matches case Cons(x, xs) ⇒ f(x, foldRight(xs,z)(f))
def len = foldRight(_,0)((_,t) ⇒ t + 1)

len(List("a","b"))
foldRight(Cons("a", Cons("b", Nil)), 0)((_,t) ⇒ t + 1)
foldRight(Cons("b", Nil), 0)((_,t) ⇒ t + 1) + 1
foldRight(Nil,0)((_,t) ⇒ t + 1) + 1 + 1
0 + 1 + 1

