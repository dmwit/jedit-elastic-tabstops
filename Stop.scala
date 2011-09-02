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

	def deactivate : Boolean = {
		// avoid work whenever possible
		if(!active) return false
		if(dependency.parent.isEmpty && !dependency.child.isEmpty) return false

		// store dependencies, then deactivate
		val maybeChild  = dependency.child
		val maybeParent = dependency.parent
		_active = false
		dependency.remove

		// fix up offsets and caches
		// only need to fix up offsets when we are (one of) the stop(s)
		// forcing the current offset to be as large as it is
		if(offset + width == offsetrcache) {
			// compute the new offset
			var newSize = None : Option[Double]
			for(stop <- eqClass) if(stop.active) {
				newSize = newSize match {
					case None => Some(stop.width + stop.offset)
					case Some(size) => Some(max(size, stop.width + stop.offset))
				}
			}

			// inform everybody of the new offset
			for(size <- newSize)
				for(stop <- eqClass) if(stop.active) {
					stop.offsetrcache = size
					for(child <- stop.dependency.child)
						child.t.offset = size
				}
		}
		for(parent <- maybeParent)
			for(child <- maybeChild)
				child.t.offset = parent.t.offsetrcache

		return true
	}
}
