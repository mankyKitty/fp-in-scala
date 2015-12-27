import org.scalacheck.Properties
import org.scalacheck.Prop.{forAll,BooleanOperators}

object StreamProperties extends Properties("fpinscala Stream") {

  property("scanRight unit test") = {
    val s = fpinscala.stream.Stream(1,2,3).scanRight(0)(_+_).toList
    s == List(6,5,3,0)
  }

  property("forAll_same") = {
    val s = fpinscala.stream.Stream.apply(1,1,1,1)
    s.forAll( a => a == 1) == true
  }
}
