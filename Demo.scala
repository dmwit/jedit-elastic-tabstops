// type Block = (Stop, String)
// type BlockName = String
// type Line = [BlockName]
// type Contents = Map BlockName Block
// type Document = (Contents, [Line])

object Demo {
	// UI building blocks {{{
	def pad(s : String, len : Int) = Stream.continually(" ").take(len - s.length).mkString + s
	def bracket(s : String) = "[" + s + "]"
	// TODO: formatting is a bit off for these two
	// prompt("What now?", Stream.continually("quit").take(15).toList,  6)
	def prompt(question : String, choices : List[String], default : Int) : Int = {
		if(choices.length == 0) throw new IllegalArgumentException("user was prompted for an answer, but there were no acceptable answers!")
		if(choices.length == 1) return 0
		val short   = (choices forall { _.length == 1 })
		val builder = new StringBuilder()
		if(short) {
			builder.append(question)
			builder.append(" (")
			for(s <- choices take default) builder.append(s)
			for(s <- choices drop default take 1) if(default >= 0) builder.append(bracket(s))
			for(s <- choices drop (default + 1)) builder.append(s)
			builder.append(")")
		} else {
			val valid   = (default >= 0 && default < choices.length)
			val padding = choices.length.toString.length + (if(valid) 3 else 1)
			for(i <- 0 to (choices.length - 1)) {
				var s = i.toString + "."
				if(i == default) s = bracket(s)
				else             s = s + (if(valid) " " else "")
				builder.append(pad(s, padding))
				builder.append(" ")
				builder.append(choices.apply(i))
				builder.append("\n")
			}
			builder.append(question)
		}
		val realQuestion = builder.toString
		var choice  = -1
		while(choice < 0 || choice >= choices.length) {
			choice = if(short) promptLine(realQuestion, "") match {
				case "" => default
				case s  => choices indexOf s
			} else promptInt(realQuestion, default)
		}
		choice
	}

	def promptLine(question : String, default : String) = {
		print(question + " ")
		readLine match {
			case "" => default
			case s  => s
		}
	}

	def promptInt(question : String, default : Int) : Int = {
		var answer = default
		var tryAgain = true
		while(tryAgain) {
			print(question + " ")
			readLine match {
				case "" => tryAgain = false
				case s  => try { answer = s.toInt; tryAgain = false } catch { case e : NumberFormatException => () }
			}
		}
		answer
	}
	// }}}

	def pprint(document : (Map[String,(Stop,String)], List[List[String]])) {
		for(line <- document._2) {
			var column = 0
			def printCount(s : String) { print(s); column += s.length }
			for(name <- line) {
				for(block <- document._1.get(name)) {
					// defensive programming: go to offset (even though we're supposed to already be there)
					for(i <- column to (block._1.offset.toInt - 1)) printCount(" ")
					printCount(block._2)
					for(i <- column to (block._1.end.toInt - name.length - 3))
						printCount(" ")
					printCount("{" + name + "}")
				}
			}
			println("")
		}
	}

	def promptStop(question : String, contents : Map[String,(Stop,String)]) = {
		val name = promptLine(question, "stop")
		(name +: Stream.from(2).map(name + _.toString)).filter(!contents.contains(_)).head
	}

	def main(args : Array[String]) {
		var document : (Map[String,(Stop,String)], List[List[String]]) = (Map(), List())
		while(true) {
			pprint(document)
			val operations =
				List("Add a line") ++
				(if(document._2.isEmpty) List() else List("Add a block", "Delete a block", "Edit text in a block")) ++
				(if(document._1.count(_ => true) >= 2) List("Align two stops") else List())
			prompt("What now?", operations, 0) match {
				case 0 =>
					val name = promptStop("Name the stop on the new line:", document._1)
					val stop = new Stop(0, name.length + 2)
					document = (document._1 + Tuple2(name, (stop, "")), document._2 ++ List(List(name)))
				case 1 =>
					val line = prompt("Which line?", (0 to (document._2.length - 1)).toList.map(_.toString), -1)
					val name = promptStop("Name the new stop:", document._1)
					val oldLine = document._2.apply(line)
					val oldStop = document._1.apply(oldLine.last)._1
					val stop = new Stop(oldStop.offset + oldStop.width, name.length + 2)
					stop.dependency.insertAfter(oldStop.dependency)
					document = (document._1 + Tuple2(name, (stop, "")),
						document._2.take(line) ++ List(oldLine ++ List(name)) ++ document._2.drop(line+1))
				case 2 =>
					val lineIx = prompt("Which line?", (0 to (document._2.length - 1)).toList.map(_.toString), -1)
					val line   = document._2.apply(lineIx)
					val nameIx = prompt("Which stop?", line, -1)
					val name   = line.apply(nameIx)
					if(document._1.apply(name)._1.deactivate) {
						val newLine  = List(line.take(nameIx) ++ line.drop(nameIx + 1)).filterNot(_.isEmpty)
						val newLines = document._2.take(lineIx) ++ newLine ++ document._2.drop(lineIx + 1)
						document = (document._1 - name, newLines)
					} else println("NOPE.avi")
				case 3 =>
					val allNames = document._1.keys.toList
					val name = allNames.apply(prompt("Which stop?", allNames, -1))
					val text = promptLine("Enter the new text:", "")
					val stop = document._1.apply(name)._1
					stop.width = name.length + text.length + 2
					document = (document._1 + Tuple2(name, (stop, text)), document._2)
				case 4 =>
					val allNames  = document._1.keys.toList
					val nameIx    = prompt("First stop to align:", allNames, -1)
					val mostNames = (allNames take nameIx) ++ (allNames drop (nameIx + 1))
					val nameIx2   = prompt("Second stop to align:", mostNames, -1)
					val name      = allNames.apply(nameIx)
					val name2     = mostNames.apply(nameIx2)
					document._1.apply(name)._1.align(document._1.apply(name2)._1)
			}
		}
	}
}
