package edu.colorado.fitzgero.sotestbed.model.numeric

class NonNegativeNumber (val value: Int) extends AnyVal {
  def + (that: NonNegativeNumber): NonNegativeNumber = new NonNegativeNumber(this.value + that.value)
  def < (that: NonNegativeNumber): Boolean = this.value < that.value
  override def toString: String = this.value.toString
}

object NonNegativeNumber {
  def apply(value: Int): Either[Exception, NonNegativeNumber] =
    if (value < 0) Left{ new IllegalArgumentException(s"cannot be negative (saw $value)") }
    else Right{ new NonNegativeNumber(value) }
  val Zero: NonNegativeNumber = new NonNegativeNumber(0)
  val One: NonNegativeNumber = new NonNegativeNumber(1)
}
