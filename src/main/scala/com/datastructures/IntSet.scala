package com.datastructures


abstract class IntSet {
  def insert(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
  def traverse(): String
}

object Empty extends IntSet {
  def insert(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
  def contains(x: Int): Boolean = false
  def union(other: IntSet): IntSet = other
  def traverse(): String = ""
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {

  def insert(x: Int): IntSet =
    if (x < elem) new NonEmpty(elem, left insert x, right)
    else if (x > elem) new NonEmpty(elem, left, right insert x)
    else this

  def contains(x: Int): Boolean =
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true

  def union(other: IntSet): IntSet =
    ((left union right) union other) insert elem

  def traverse(): String =
    elem.toString + left.traverse() + right.traverse()
}

object IntSetTest {
  def run():Unit = {
    val set1 = new NonEmpty(5, Empty, Empty)
    val set2 = set1.insert(2).insert(3).insert(7)
    println(set2.traverse())

    val set3 = new NonEmpty(1, Empty, Empty)
    println((set2 union set3).traverse())
  }
}