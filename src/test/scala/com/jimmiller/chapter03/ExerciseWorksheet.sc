import com.jimmiller.chapter03._
import com.jimmiller.chapter03.List._

// Exercise 3.8
//
// foldRight(), like apply(), uses recursion to construct
// the list from head to tail (i.e. left to right)

val curr = foldRight(List(1,2,3), Nil:List[Int])(_)

foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))
curr(Cons(_,_))

// Traced out:
foldRight(Cons(1, Cons(2, Cons(3, Nil))), Nil:List[Int])(Cons(_,_))
Cons(1, foldRight(Cons(2, Cons(3, Nil)), Nil: List[Int])(Cons(_,_)))
Cons(1, Cons(2, foldRight(Cons(3, Nil), Nil: List[Int])(Cons(_,_))))
Cons(1, Cons(2, Cons(3, Nil)))


// Exercise 3.9
// foldRight matches case Cons(x, xs) ⇒ f(x, foldRight(xs,z)(f))
def len = foldRight(_:List[Any],0)((_,t) ⇒ t + 1)

len(List("a","b"))
foldRight(Cons("a", Cons("b", Nil)), 0)((_,t) ⇒ t + 1)
foldRight(Cons("b", Nil), 0)((_,t) ⇒ t + 1) + 1
foldRight(Nil,0)((_,t) ⇒ t + 1) + 1 + 1
0 + 1 + 1

// Exercise 3.16 - playing with folds
val ints = List(1,2,3)

def addOne(as: List[Int]): List[Int] = {
  foldRight(as: List[Int], Nil:List[Int])((x,xs) ⇒ Cons(x+1, addOne(xs)))
}

addOne(ints)
foldRight(List(1,2,3), Nil:List[Int])((x,xs) ⇒ Cons(x+1, xs))


// Exercise 3.20 - playing with flatMap
val as = List(1,2,3)
def f[A](i: A) = { List(i,i) }
flatMap(as)(f)

val mapped = map(as)(f)
concat(mapped)

zipIntLists(Nil, Nil)