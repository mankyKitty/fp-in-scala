import org.scalacheck.Properties
import org.scalacheck.Prop.{forAll,BooleanOperators}

object MyModuleProperties extends Properties("MyModule") {

  property("reverse") = forAll{ (xs: List[Int]) =>
    xs == (MyModule.reverse(MyModule.reverse(xs)))
  }
  property("append_against_++") = forAll{ (xs: List[Int], ys: List[Int]) =>
    (MyModule.append(xs,ys)) == xs ++ ys
  }
  property("append_predefined") = {
    val xs = List(1,2)
    val ys = List(3,4)
    MyModule.append(xs,ys) == List(1,2,3,4)
  }
  property("concat") = {
    val xs = List(1,2)
    val ys = List(3,4)
    MyModule.concat(List(xs,ys)) == MyModule.append(xs,ys)
  }
  property("concat_length") = forAll{ (xxs: List[List[Int]]) =>
    (MyModule.foldLeft(xxs,0)((b,a) => b + MyModule.length(a))) ==
      (MyModule.length(MyModule.concat(xxs)))
  }
  property("oneToAll") = {
    val xs = List(1,2,3)
    (MyModule.oneToAll(xs)) == List(2,3,4)
  }
  property("stringlyDs") = {
    val xs = List[Double](1.0,2.0,3.0)
    (MyModule.stringlyDs(xs)) == List[String]("1.0","2.0","3.0")
  }
  property("StringlyD_Map") = forAll{ (xs: List[Double]) =>
    (MyModule.stringlyDs(xs)) == (MyModule.map(xs)(_.toString))
  }
  property("oneToAll_Map") = forAll{ (xs: List[Int]) =>
    (MyModule.oneToAll(xs)) == (MyModule.map(xs)(_ + 1))
  }
  property("filter_Simple") = {
    val xs = List(1,2,3,4,5,6)
    (MyModule.filter(xs)((a) => (a % 2) == 0)) == List(2,4,6)
  }
  property("flatMap_simple") = {
    val xs = List[Int](1,2,3)
    (MyModule.flatMap(xs)((i) => List(i,i))) == List(1,1,2,2,3,3)
  }
  property("filter_flatMap") = {
    val xs = List(1,2,3,4,5,6)
    (MyModule.filterFM(xs)((a) => (a % 2) == 0)) == List(2,4,6)
  }
  property("addLists_length") = forAll{ (xs:List[Int],ys:List[Int]) =>
    (MyModule.length(xs) > 0 && MyModule.length(ys) > 0) ==>
    ((MyModule.length(MyModule.addLists(xs,ys))) ==
      (math.min(MyModule.length(xs),MyModule.length(ys))))
  }
  property("addLists_Nil_Any") = forAll{ (xs:List[Int]) =>
    (MyModule.addLists(List(),xs)) == List()
  }
  property("addLists_Any_Nil") = forAll{ (xs:List[Int]) =>
    (MyModule.addLists(xs,List())) == List()
  }
  property("addLists_simple") = {
    val xs = List(1,2,3)
    val ys = List(4,5,6)
    (MyModule.addLists(xs,ys)) == List(5,7,9)
  }
  property("zipWith_simple") = {
    val xs = List(1,2,3)
    val ys = List(4,5,6)
    (MyModule.zipWith(xs,ys)(_+_)) == List(5,7,9)
  }
  property("hasSubsequence_simple") = {
    val xs = List(1,2,3,4,5,6)
    val ys = List(1,2)
    val zs = List(2,3,4)
    val ws = List(5,6)
    (
      (MyModule.hasSubsequence(xs, ys)) &&
      (MyModule.hasSubsequence(xs, zs)) &&
      (MyModule.hasSubsequence(xs, ws))
    )
  }
}

