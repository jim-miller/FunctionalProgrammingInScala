package com.jimmiller.chapter04

import scala.{Option ⇒ _, Some ⇒ _, Either ⇒ _, Left ⇒ _, Right ⇒ _, _}

sealed trait Either[+E, +A] {

  def flatMap[EE >: E, B](f: A ⇒ Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }

  def map[B](f: A ⇒ B): Either[E, B] = this match {
    case Right(a) ⇒ Right(f(a))
    case Left(e) ⇒ Left(e)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) ⇒ C): Either[EE, C] = {
    for {a ← this; b1 ← b} yield {
      f(a, b1)
    }
  }

  def orElse[EE >: E, B >: A](b: ⇒ Either[EE, B]): Either[EE, B] = this match {
    case Left(_) ⇒ b
    case Right(a) ⇒ Right(a)
  }

}

case class Left[+E](get: E) extends Either[E, Nothing]

case class Right[+A](get: A) extends Either[Nothing, A]

object Either {

  private def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty) {
      Left("ERROR: cannot find mean on on empty list")
    } else {
      Right(xs.sum / xs.length)
    }

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = {
    try {
      Right(x / y)
    } catch {
      case e: Exception ⇒ Left(e)
    }
  }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    traverse(es)(x ⇒ x)
  }

  def traverse[E, A, B](es: List[A])(f: A ⇒ Either[E, B]): Either[E, List[B]] = es match {
    case Nil ⇒ Right(Nil)
    case h :: t ⇒ (f(h) map2 traverse(t)(f))(_ :: _)
  }

  def Try[A](a: ⇒ A): Either[Exception, A] =
    try Right(a) catch {case e: Exception ⇒ Left(e)}

}

sealed trait Option[+A] {

  def filter(f: A ⇒ Boolean): Option[A] = {
    flatMap(a ⇒ if (f(a)) {
      Some(a)
    } else {
      None
    })
  }

  def flatMap[B](f: A ⇒ Option[B]): Option[B] = {
    map(f).getOrElse(None)
  }

  def getOrElse[B >: A](default: ⇒ B): B = this match {
    case Some(a) ⇒ a
    case None ⇒ default
  }

  def map[B](f: A ⇒ B): Option[B] = this match {
    case Some(a) ⇒ Some(f(a))
    case None ⇒ None
  }

  def orElse[B >: A](ob: ⇒ Option[B]): Option[B] = {
    map(Some(_)).getOrElse(ob)
  }

  private def mean(xs: Seq[Double]): Double = {
    xs.sum / xs.size
  }

}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) ⇒ C): Option[C] = {
    a.flatMap(flatA ⇒ b.map(flatB ⇒ f(flatA, flatB)))
  }

  private def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) {
      None
    } else {
      Some(xs.sum / xs.length)
    }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil ⇒ Some(Nil)
    case h :: t ⇒ h.flatMap(flatH ⇒ sequence(t).map(flatH :: _))
  }

  def traverse[A, B](a: List[A])(f: A ⇒ Option[B]): Option[List[B]] = a match {
    case Nil ⇒ Some(Nil)
    case h :: t ⇒ map2(f(h), traverse(t)(f))(_ :: _)
  }

  /**
   * @see  http://www.mathsisfun.com/data/standard-deviation.html
   *
   * @param xs The set of numbers tested
   * @return an Optional result
   */
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap(m ⇒ mean(xs.map(x ⇒ math.pow(x - m, 2))))
  }
}