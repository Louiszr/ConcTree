object ConcTree {
  sealed trait Conc[+T] {
    def size: Int
    def level: Int
    def left: Conc[T]
    def right: Conc[T]
  }
  
  case object Empty extends Conc[Nothing] {
    def size = 0
    def level = 0
    val left = Empty
    val right = Empty
  }

  final case class Single[T](value: T) extends Conc[T] {
    def size = 1
    def level = 0
    val left = Empty
    val right = Empty
  }

  final class <>[T](val left: Conc[T], val right: Conc[T]) extends Conc[T] {
    assert(left != Empty && right != Empty)
    assert(math.abs(left.level - right.level) <= 1)

    val size = left.size + right.size
    val level = 1 + math.max(left.level, right.level)

    override def toString: String = {
//      s"<>\n|\\\n${left.toString}${right.toString}"
      s"<${left.toString}-${right.toString}>"
    }
  }

  def <>[T](left: Conc[T], right: Conc[T]): Conc[T] = {
    if (left == Empty) right
    else if (right == Empty) left
    else if (math.abs(left.level - right.level) <= 1) new <>(left, right)
    else concat(left, right)
  }

  def concat[T](xs: Conc[T], ys: Conc[T]): Conc[T] = {
    if (xs.level >= ys.level) {
      val isXsLeftLeaning = xs.left.size >= xs.right.size
      if (isXsLeftLeaning) {
        val newRight = <>(xs.right, ys)
        new <>(xs.left, newRight)
      }
      else {
        val newRightRight = <>(xs.right.right, ys)
        if (newRightRight.level == xs.level - 1) {
          // Then xs.left and xs.right.left needs to be first link together
          val newLeft = new <>(xs.left, xs.right.left)
          new <>(newLeft, newRightRight)
        }
        else {
          // Then relink three trees in the same way as they split
          val newRight = new <>(xs.right.left, newRightRight)
          new <>(xs.left, newRight)
        }
      }
    }
    else {
      val isYsRightLeaning = ys.left.size <= ys.right.size
      if (isYsRightLeaning) {
        val newLeft = <>(xs, ys.left)
        new <>(newLeft, ys.right)
      }
      else {
        val newLeftLeft = <>(xs, ys.left.left)
        if (newLeftLeft.level == ys.level - 1) {
          // Then ys.left.right and ys.right needs to be first link together
          val newRight = new <>(ys.left.right, ys.right)
          new <>(newLeftLeft, newRight)
        }
        else {
          // Then relink three trees in the same way as they split
          val newLeft = new <>(newLeftLeft, ys.left.right)
          new <>(newLeft, ys.right)
        }
      }
    }
  }
}
