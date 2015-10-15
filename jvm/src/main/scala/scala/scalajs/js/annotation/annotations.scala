package scala.scalajs.js.annotation

import scala.annotation.Annotation

class JSExport extends scala.annotation.Annotation {
  def this(name: String) = this()
}

class JSExportAll extends scala.annotation.Annotation
