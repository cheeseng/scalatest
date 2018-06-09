import java.io.File
import java.io.BufferedWriter
import java.io.FileWriter
import scala.io.Source

object GenCompat {

  val generatorSource = new File("GenCompat.scala")

  def generateFile(targetDir: File, targetFileName: String, content: String): File = {
    targetDir.mkdirs()
    val targetFile = new File(targetDir, targetFileName + ".scala")
    if (!targetFile.exists || generatorSource.lastModified > targetFile.lastModified) {
      val writer = new BufferedWriter(new FileWriter(targetFile))
      try {
        writer.write(content)
        targetFile
      }
      finally {
        writer.flush()
        writer.close()
      }
    }
    targetFile
  }

  def generateEveryCompat(targetDir: File, scalaVersion: String): File = {
    val content =
      if (scalaVersion startsWith "2.13")
        """package org.scalactic
          |trait EveryCompat[+T] {
          |
          |  protected[scalactic] val underlying: Vector[T]
          |
          |  /**
          |    * Converts this <code>Every</code> into a collection of type <code>Col</code> by copying all elements.
          |    *
          |    * @tparam C1 the target collection type to convert to.
          |    * @param factory the collection factory to convert this collection to C1.
          |    * @return a new collection containing all elements of this <code>Every</code>.
          |    */
          |  final def to[C1](factory: scala.collection.Factory[T, C1]): C1 = underlying.to(factory)
          |
          |  final def union[U >: T](that: Seq[U]): Every[U] = {
          |    val vec = underlying.union(that)
          |    Every(vec.head, vec.tail: _*)
          |  }
          |}
        """.stripMargin
      else
        """package org.scalactic
          |
          |trait EveryCompat[+T] {
          |
          |  protected[scalactic] val underlying: Vector[T]
          |
          |  import scala.annotation.unchecked.{ uncheckedVariance => uV }
          |  import scala.language.higherKinds
          |
          |  /**
          |    * Converts this <code>Every</code> into a collection of type <code>Col</code> by copying all elements.
          |    *
          |    * @tparam Col the collection type to build.
          |    * @return a new collection containing all elements of this <code>Every</code>.
          |    */
          |  final def to[Col[_]](implicit cbf: scala.collection.generic.CanBuildFrom[Nothing, T, Col[T @uV]]): Col[T @uV] = underlying.to[Col](cbf)
          |
          |  /**
          |    * Converts this <code>Every</code> to an unspecified Traversable.
          |    *
          |    * @return a <code>Traversable</code> containing all elements of this <code>Every</code>.
          |    */
          |  final def toTraversable: Traversable[T] = underlying.toTraversable
          |
          |  final def union[U >: T](that: scala.collection.GenSeq[U])(implicit cbf: scala.collection.generic.CanBuildFrom[Vector[T], U, Vector[U]]): Every[U] = {
          |    val vec = underlying.union(that)(cbf)
          |    Every(vec.head, vec.tail: _*)
          |  }
          |}
        """.stripMargin

    generateFile(targetDir, "EveryCompat.scala", content)
  }

  def genMain(targetDir: File, version: String, scalaVersion: String): Seq[File] =
    Seq(
      generateEveryCompat(new File(targetDir, "org/scalactic"), scalaVersion)
    )

}