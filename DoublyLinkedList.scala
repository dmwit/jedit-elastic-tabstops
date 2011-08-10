class Node[T](val t : T) {
	private var _parent : Option[Node[T]] = None
	private var _child  : Option[Node[T]] = None

	private def pc = _parent match {
		case None => None
		case Some(parent) => parent._child
	}

	private def cp = _child match {
		case None => None
		case Some(child) => child._parent
	}

	private def pc_=(c : Option[Node[T]]) {
		_parent match {
			case None => ()
			case Some(parent) => parent._child = c
		}
	}

	private def cp_=(p : Option[Node[T]]) {
		_child match {
			case None => ()
			case Some(child) => child._parent = p
		}
	}

	def remove = {
		pc = _child
		cp = _parent
	}

	def insertAfter(parent : Node[T]) = {
		remove
		_child  = parent._child
		_parent = Some(parent)
		parent._child = Some(this)
		cp = Some(this)
	}

	def insertBefore(child : Node[T]) = {
		remove
		_parent = child._parent
		_child  = Some(child)
		child._parent = Some(this)
		pc = Some(this)
	}

	def parent = _parent
	def child  = _child
}
