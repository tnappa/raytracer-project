package Raytracer

// class to add current method to Iterator
class MyIterator[T](it: Iterable[T]) extends Iterable[T] {
  private val iter = iterator
  private var curr: Option[T] = None
  def hasNext = iter.hasNext
  def next(): T = {
    curr = Some(iter.next())
    curr.get
  }
  def current: T =
    if (curr.isDefined) curr.get
    else throw new NoSuchElementException

  def iterator: Iterator[T] = it.iterator
}

object MyIterator {
  def apply[T](it: Iterable[T]): MyIterator[T] = new MyIterator(it)
  def apply[T]() = new MyIterator[T](Iterable.empty)
}
