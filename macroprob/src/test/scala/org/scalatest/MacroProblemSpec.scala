package org.scalatest

class MacroProblemSpec extends FunSpec {

  def woof(f: => Unit) = "woof"
  def meow(x: Int = 0, y: Int = 3) = "meow"

  it("this should blow up during compilation") {
    assert(woof { meow(y = 5) } == "woof")
  }
}
