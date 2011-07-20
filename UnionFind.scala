class EquivalenceClass[T](var repr : Option[EquivalenceClass[T]], var rank : Int, val t : T) extends Iterable[T] {
	var next : EquivalenceClass[T] = this; // it's a doubly-linked list, lol
	var prev : EquivalenceClass[T] = this;

	def getRepr : EquivalenceClass[T] = {
		repr match {
			case None    => this
			case Some(r) => {
				val answer = r.getRepr
				repr = Some(answer)
				answer
			}
		}
	}

	def getFirstRepr = {
		repr match {
			case None    => t
			case Some(r) => r.t
		}
	}

	class It extends Iterator[T] {
		val start   = EquivalenceClass.this
		var current = None : Option[EquivalenceClass[T]]

		def hasNext = current match {
			case None => true
			case Some(c) => c.next != start
		}

		def next = {
			val answer = current match {
				case None => start
				case Some(c) => c.next
			}
			current = Some(answer)
			answer.t
		}
	}

	def iterator = new It
	def elements = new It
}

object UnionFind {
	def singleton[T](t : T) = new EquivalenceClass(None, 1, t)

	/* when we've already decided which equivalence class should be the new representative */
	def merge[T] (r1 : EquivalenceClass[T], r2 : EquivalenceClass[T]) {
		if(r1.rank == r2.rank) r1.rank = r2.rank + 1
		r1.repr  = Some(r2)
		val r1p  = r1.prev
		val r2n  = r2.next
		r1p.next = r2n
		r2n.prev = r1p
		r1.prev  = r2
		r2.next  = r1
	}

	/* decide which of the two should be the new representative */
	def union[T] (c1 : EquivalenceClass[T], c2 : EquivalenceClass[T]) {
		val r1 = c1.getRepr
		val r2 = c2.getRepr
		if (r1.rank > r2.rank)
			merge(r1, r2)
		else
			merge(r2, r1)
	}

	def main(args : Array[String]) {
		val a = singleton(3)
		val b = singleton(4)
		val c = singleton(5)
		val d = singleton(6)
		union(a, b)
		union(c, d)
		union(a, c)
		println(a.getFirstRepr)
		println(b.getFirstRepr)
		println(c.getFirstRepr)
		println(d.getFirstRepr)
		println("a")
		for(t <- a) println(t)
		println("b")
		for(t <- b) println(t)
		println("c")
		for(t <- c) println(t)
		println("d")
		for(t <- d) println(t)
	}
}
