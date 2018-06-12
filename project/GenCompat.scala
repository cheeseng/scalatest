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
          |
          |  /**
          |   * Converts this <code>Every</code> to an unspecified Traversable.
          |   *
          |   * @return a <code>Traversable</code> containing all elements of this <code>Every</code>.
          |   */
          |  final def toTraversable: Traversable[T] = underlying.toTraversable
          |
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
          |   * Produces a new <code>Every</code> that contains all elements of this <code>Every</code> and also all elements of a given <code>GenSeq</code>.
          |   *
          |   * <p>
          |   * <code>everyX</code> <code>union</code> <code>ys</code> is equivalent to <code>everyX</code> <code>++</code> <code>ys</code>.
          |   * </p>
          |   *
          |   * <p>
          |   * Another way to express this is that <code>everyX</code> <code>union</code> <code>ys</code> computes the order-presevring multi-set union
          |   * of <code>everyX</code> and <code>ys</code>. This <code>union</code> method is hence a counter-part of <code>diff</code> and <code>intersect</code> that
          |   * also work on multi-sets.
          |   * </p>
          |   *
          |   * @param that the <code>GenSeq</code> to add.
          |   * @return a new <code>Every</code> that contains all elements of this <code>Every</code> followed by all elements of <code>that</code> <code>GenSeq</code>.
          |   */
          |  final def union[U >: T](that: Seq[U])(implicit cbf: scala.collection.generic.CanBuildFrom[Vector[T], U, Vector[U]]): Every[U] = {
          |    val vec = underlying.union(that)(cbf)
          |    Every(vec.head, vec.tail: _*)
          |  }
          |}
        """.stripMargin

    generateFile(targetDir, "EveryCompat.scala", content)
  }

  def generateChainCompat(targetDir: File, scalaVersion: String): File = {
    val content =
      if (scalaVersion startsWith "2.13")
        """package org.scalactic
          |trait ChainCompat[+T] {
          |
          |  val toList: List[T]
          |
          |  /**
          |   * Converts this <code>Chain</code> into a collection of type <code>Col</code> by copying all elements.
          |   *
          |   * @tparam Col the collection type to build.
          |   * @return a new collection containing all elements of this <code>Chain</code>.
          |   */
          |  final def to[C1](factory: scala.collection.Factory[T, C1]): C1 = toList.to(factory)
          |
          |  /**
          |   * Produces a new <code>Chain</code> that contains all elements of this <code>Chain</code> and also all elements of a given <code>GenSeq</code>.
          |   *
          |   * <p>
          |   * <code>chainX</code> <code>union</code> <code>ys</code> is equivalent to <code>chainX</code> <code>++</code> <code>ys</code>.
          |   * </p>
          |   *
          |   * <p>
          |   * Another way to express this is that <code>chainX</code> <code>union</code> <code>ys</code> computes the order-presevring multi-set union
          |   * of <code>chainX</code> and <code>ys</code>. This <code>union</code> method is hence a counter-part of <code>diff</code> and <code>intersect</code> that
          |   * also work on multi-sets.
          |   * </p>
          |   *
          |   * @param that the <code>GenSeq</code> to add.
          |   * @return a new <code>Chain</code> that contains all elements of this <code>Chain</code> followed by all elements of <code>that</code> <code>GenSeq</code>.
          |   */
          |  final def union[U >: T](that: Seq[U]): Chain[U] = {
          |    val l = toList.union(that)
          |    Chain(l.head, l.tail: _ *)
          |  }
          |}
        """.stripMargin
      else
        """package org.scalactic
          |
          |trait ChainCompat[+T] {
          |
          |  val toList: List[T]
          |
          |  import scala.annotation.unchecked.{ uncheckedVariance => uV }
          |
          |  /**
          |   * Converts this <code>Chain</code> into a collection of type <code>Col</code> by copying all elements.
          |   *
          |   * @tparam Col the collection type to build.
          |   * @return a new collection containing all elements of this <code>Chain</code>.
          |   */
          |  final def to[Col[_]](implicit cbf: scala.collection.generic.CanBuildFrom[Nothing, T, Col[T @uV]]): Col[T @uV] = toList.to[Col](cbf)
          |
          |  /**
          |   * Converts this <code>Chain</code> to an unspecified Traversable.
          |   *
          |   * @return a <code>Traversable</code> containing all elements of this <code>Chain</code>.
          |   */
          |  final def toTraversable: Traversable[T] = toList.toTraversable
          |
          |  /**
          |   * Produces a new <code>Chain</code> that contains all elements of this <code>Chain</code> and also all elements of a given <code>GenSeq</code>.
          |   *
          |   * <p>
          |   * <code>chainX</code> <code>union</code> <code>ys</code> is equivalent to <code>chainX</code> <code>++</code> <code>ys</code>.
          |   * </p>
          |   *
          |   * <p>
          |   * Another way to express this is that <code>chainX</code> <code>union</code> <code>ys</code> computes the order-presevring multi-set union
          |   * of <code>chainX</code> and <code>ys</code>. This <code>union</code> method is hence a counter-part of <code>diff</code> and <code>intersect</code> that
          |   * also work on multi-sets.
          |   * </p>
          |   *
          |   * @param that the <code>GenSeq</code> to add.
          |   * @return a new <code>Chain</code> that contains all elements of this <code>Chain</code> followed by all elements of <code>that</code> <code>GenSeq</code>.
          |   */
          |  final def union[U >: T](that: scala.collection.GenSeq[U])(implicit cbf: scala.collection.generic.CanBuildFrom[List[T], U, List[U]]): Chain[U] = {
          |    val l = toList.union(that)(cbf)
          |    Chain(l.head, l.tail: _*)
          |  }
          |}
        """.stripMargin

    generateFile(targetDir, "ChainCompat.scala", content)
  }

  def genMain(targetDir: File, version: String, scalaVersion: String): Seq[File] =
    Seq(
      generateEveryCompat(new File(targetDir, "org/scalactic"), scalaVersion),
      generateChainCompat(new File(targetDir, "org/scalactic"), scalaVersion)
    )

}