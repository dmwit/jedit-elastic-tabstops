import scala.math.max

class Stop(offsetArg : Double, widthArg : Double) {
	private var _offset : Double = offsetArg
	private var _width  : Double = widthArg
	private var _active : Boolean = true
	private var offsetrcache : Double = offsetArg + widthArg
	private val eqClass : EquivalenceClass[Stop] = UnionFind.singleton(this)
	val dependency : Node[Stop] = new Node[Stop](this)

	def offset = _offset
	def width  = _width
	def active = _active
	def end    = offsetrcache

	private def pingDependents(newSize : Double) {
		if(!active) return
		val grew   = newSize > offsetrcache
		var shrunk = (width + offset == offsetrcache) && newSize < offsetrcache
		var size   = newSize
		val iter   = eqClass.iterator
		while(iter.hasNext && shrunk) {
			val other = iter.next
			shrunk &&= (newSize < other.width + other.offset)
			if(!eq(other))
				size = max(size, other.width + other.offset)
		}
		if(grew || shrunk) {
			for(other <- eqClass) if(other.active) {
				for(child <- other.dependency.child)
					child.t.offset = size
				other.offsetrcache = size
			}
		}
	}

	def offset_=(newOffset : Double) {
		pingDependents(width + newOffset)
		_offset = newOffset
	}

	def width_=(newWidth : Double) {
		pingDependents(newWidth + offset)
		_width = newWidth
	}

	def align(other : Stop) {
		if(other.offsetrcache < offsetrcache) { other.align(this); return }
		if(other.offsetrcache > offsetrcache)
			for(stop <- eqClass) {
				stop.offsetrcache = other.offsetrcache
				for(child <- stop.dependency.child)
					child.t.offset = other.offsetrcache
			}
		UnionFind.union(other.eqClass, eqClass)
	}

	def deactivate {
		if(!active) return
		for(child <- dependency.child) if(width + offset == child.t.offset) {
			// TODO: ping eqClass
			// TODO: set child's offset (to 0 if parent is None, or based on parent's eqClass otherwise)
		}
		dependency.remove
		_active = false
	}
}
