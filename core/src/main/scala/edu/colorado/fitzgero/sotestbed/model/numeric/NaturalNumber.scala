package edu.colorado.fitzgero.sotestbed.model.numeric

class NaturalNumber (val value: Int) extends AnyVal

object NaturalNumber {
  def apply(value: Int): Either[Exception, NaturalNumber] =
    if (value <= 0) Left{ new IllegalArgumentException(s"cannot be negative (saw $value)") }
    else Right{ new NaturalNumber(value) }
  val Zero: NaturalNumber = new NaturalNumber(0)
}
