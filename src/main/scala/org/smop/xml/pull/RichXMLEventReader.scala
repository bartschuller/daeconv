package org.smop.xml.pull

import xml.pull.{EvElemEnd, EvElemStart, XMLEventReader}
import io.Source

/**
 * Created by IntelliJ IDEA.
 * User: schuller
 * Date: Jan 4, 2010
 * Time: 9:05:36 PM
 * To change this template use File | Settings | File Templates.
 */

class RichXMLEventReader(source: Source) extends XMLEventReader(source) {
  def skipElement(start: EvElemStart): Boolean = {
    find(ev =>
      ev match {
        case nested @ EvElemStart(_, _, _, _) => {
          skipElement(nested)
          false
        }
        case EvElemEnd(start.pre, start.label) => true
        case _ => false
    })
    true
  }
}
