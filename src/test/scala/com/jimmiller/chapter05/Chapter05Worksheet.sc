def something() = {
  println("side effect")
  1 // return value
}

def callByValue(x: Int) = {
  x + x
}

def callByName(x: => Int) = {
  x + x
}

callByValue(something)

callByName(something)


import com.jimmiller.chapter05._

val s1 = Stream({1}, 2, 3)

s1.toList

def one = { println("side effect 1"); 1 }
val two = { println("side effect 2"); 2 }

val s2 = Stream(one, two, 3)

val l1 = s2.toList

s2.toList.head

