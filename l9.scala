object scalasortfuncstyle
{
	def sort(a: List[Int]): List[Int] = {
		if(a.length < 2) a
		else {
			val pivot = a(a.length / 2)
			sort(a.filter(_< pivot)) :::
			a.filter(_ == pivot) :::
			sort(a.filter(_ > pivot))
		}
	}
	
	def main(args: Array[String])
	{
		val xs = List(6,2,8,5,1)
		println(xs)
		println(sort(xs))
	}
}


object ImperativeQuickSort {
 3
 4  /** Nested methods can use and even update everything 
 5   *  visible in their scope (including local variables or 
 6   *  arguments of enclosing methods). 
 7   */
 8  def sort(a: Array[Int]) {
 9
10    def swap(i: Int, j: Int) {
11      val t = a(i); a(i) = a(j); a(j) = t
12    }
13
14    def sort1(l: Int, r: Int) {
15      val pivot = a((l + r) / 2)
16      var i = l
17      var j = r
18      while (i <= j) {
19        while (a(i) < pivot) i += 1
20        while (a(j) > pivot) j -= 1
21        if (i <= j) {
22          swap(i, j)
23          i += 1
24          j -= 1
25        }
26      }
27      if (l < j) sort1(l, j)
28      if (j < r) sort1(i, r)
29    }
30
31    if (a.length > 0)
32      sort1(0, a.length - 1)
33  }
34
35  def println(ar: Array[Int]) {
36    def print1 = {
37      def iter(i: Int): String =
38        ar(i) + (if (i < ar.length-1) "," + iter(i+1) else "")
39      if (ar.length == 0) "" else iter(0)
40    }
41    Console.println("[" + print1 + "]")
42  }
43
44  def main(args: Array[String]) {
45    val ar = Array(6, 2, 8, 5, 1)
46    println(ar)
47    sort(ar)
48    println(ar)
49  }
50
51}