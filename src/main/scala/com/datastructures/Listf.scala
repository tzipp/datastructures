package com.datastructures


sealed trait cList[+A]
case object cNil extends cList[Nothing]
case class Cons[A](head: A, tail: cList[A]) extends cList[A]