package com.jimmiller.chapter04

import scala.{Option ⇒ _, Some ⇒ _, Either ⇒ _, _}

sealed trait Option[+A] {

  def map[B](f: A ⇒ B): Option[B] = sys.error("todo")

  def getOrElse[B >: A](default: ⇒ B): B = sys.error("todo")

  def flatMap[B](f: A ⇒ Option[B]): Option[B] = sys.error("todo")

  def orElse[B >: A](ob: ⇒ Option[B]): Option[B] = sys.error("todo")

  def filter(f: A ⇒ Boolean): Option[A] = sys.error("todo")

}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]
