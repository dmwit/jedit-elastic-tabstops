// type Block = (Stop, String)
// type BlockName = String
// type Line = [BlockName]
// type Contents = Map BlockName Block
// type Document = (Contents, [Line])

object Demo {
	def pad(s : String, len : Int) = Stream.continually(" ").take(len - s.length).mkString + s
	def bracket(s : String) = "[" + s + "]"
	def prompt(prompt : String, choices : List[String], default : Int) = {
		val short  = choices forall { _.length == 1 }
		val valid  = (default >= 0 && default < choices.length)
		var choice = -1
		while(choice < 0 || choice >= choices.length) {
			if(short) {
				print(prompt)
				print(" (")
				for(s <- choices take default) print(s)
				for(s <- choices drop default take 1) if(default >= 0) print(bracket(s))
				for(s <- choices drop (default + 1)) print(s)
				print(") ")
				readLine match {
					case "" => choice = default
					case s  => choice = choices indexOf s
				}
			}
			else {
				val padding = choices.length.toString.length + (if(valid) 3 else 1)
				for(i <- 0 to (choices.length - 1)) {
					var s = i.toString + "."
					if(i == default) s = bracket(s)
					else             s = s + (if(valid) " " else "")
					println(pad(s, padding) + " " + choices.apply(i))
				}
				print(prompt + " ")
				readLine match {
					case "" => choice = default
					case s  => try { choice = s toInt } catch { case e : NumberFormatException => choice = -1 }
				}
			}
		}
		choice
	}

	def main(args : Array[String]) {
		var document = (Map(), List())
		prompt("Whaddaya want?", List("a", "b", "c"), -1)
		prompt("Whaddaya want?", List("a", "b", "c"),  0)
		prompt("Whaddaya want?", List("a", "b", "c"),  1)
		prompt("Whaddaya want?", List("a", "b", "c"),  2)
		prompt("Whaddaya want?", List("a", "b", "c"),  3)
		prompt("Whatcha sellin?", List("weed", "hotcakes", "diddly"), -1)
		prompt("Whatcha sellin?", List("weed", "hotcakes", "diddly"),  0)
		prompt("Whatcha sellin?", List("weed", "hotcakes", "diddly"),  1)
		prompt("Whatcha sellin?", List("weed", "hotcakes", "diddly"),  2)
		prompt("Whatcha sellin?", List("weed", "hotcakes", "diddly"),  3)
		prompt("What now?", Stream.continually("quit").take(15).toList, 6) // TODO: formatting is a bit off for this one
	}
}
