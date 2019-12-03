package edu.colorado.fitzgero.sotestbed.model.numeric

class NaturalNumber (val value: Int) extends AnyVal {
  def + (that: NaturalNumber): NaturalNumber = new NaturalNumber(this.value + that.value)
  def < (that: NaturalNumber): Boolean = this.value < that.value
}

object NaturalNumber {
  def apply(value: Int): Either[Exception, NaturalNumber] =
    if (value <= 0) Left{ new IllegalArgumentException(s"cannot be negative (saw $value)") }
    else Right{ new NaturalNumber(value) }
  val Zero: NaturalNumber = new NaturalNumber(0)
  val One: NaturalNumber = new NaturalNumber(1)
}
