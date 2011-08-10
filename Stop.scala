import scala.math.max

class Stop(offsetArg : Double, widthArg : Double) {
	private var _offset : Double = offsetArg
	private var _width  : Double = widthArg
	private var _active : Boolean = true
	val eqClass    : EquivalenceClass[Stop] = UnionFind.singleton(this)
	val dependency : Node[Stop] = new Node[Stop](this)

	def offset = _offset
	def width  = _width
	def active = _active

	private def pingDependents(newSize : Double) {
		if(!active) return
		var grew   = false
		var shrunk = true
		var size   = newSize
		for(other <- eqClass) if(other.active) {
			grew   ||= (newSize > other.width + other.offset)
			shrunk &&= (newSize < other.width + other.offset)
			if(!eq(other))
				size = max(size, other.width + other.offset)
		}
		if(grew || shrunk)
			for(other <- eqClass) if(other.active)
				for(child <- other.dependency.child)
					child.t.offset = size
	}

	def offset_=(newOffset : Double) {
		pingDependents(width + newOffset)
		_offset = newOffset
	}

	def width_=(newWidth : Double) {
		pingDependents(newWidth + offset)
		_width = newWidth
	}

	def deactivate {
		if(!active) return
		for(child <- dependency.child) if(width + offset == child.offset) {
			/* TODO: ping eqClass */
			/* TODO: set child's offset (to 0 if parent is None, or based on parent's eqClass otherwise) */
		}
		dependency.remove
		active = false
	}
}
