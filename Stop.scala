class Stop(offsetArg : Double, widthArg : Option[Double]) {
	private var _offset : Double = offsetArg
	private var _width  : Option[Double] = widthArg
	val eqClass    : EquivalenceClass[Stop] = UnionFind.singleton(this)
	val dependency : Node[Stop] = new Node[Stop](this)

	def offsetl = _offset
	def width   = _width
	def offsetr = _width.map(_offset+_)

	def offset_=(newOffset : Double) {
		var grew   = false
		var shrunk = !_width.isEmpty
		for(w1 <- _width)
			for(other <- eqClass)
				for(w2 <- other._width) {
					grew   ||= (w1 + newOffset > w2 + other._offset)
					shrunk &&= (w1 + newOffset < w2 + other._offset)
				}
		_offset = newOffset
		if(grew || shrunk)
			for(w <- _width)
				for(other <- eqClass)
					for(child <- other.dependency.child)
						child.t.offset_=(_offset + w)
	}
}
