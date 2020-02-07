package edu.colorado.fitzgero.sotestbed.util

import scala.xml.Elem
import scala.xml.factory.XMLLoader
import javax.xml.parsers.SAXParser


/**
  * @see https://stackoverflow.com/questions/11315439/ignore-dtd-specification-in-scala
  */
object XMLParserIgnoresDTD extends XMLLoader[Elem] {
  override def parser: SAXParser = {
    val f = javax.xml.parsers.SAXParserFactory.newInstance()
    f.setFeature("http://apache.org/xml/features/nonvalidating/load-dtd-grammar", false);
    f.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false);
    f.newSAXParser()
  }
}


