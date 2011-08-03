jedit_jar := $(shell locate jedit.jar | tail -n1)
final_jar := ElasticTabstopsPlugin.jar

$(final_jar): *.html *.props *.scala
	CLASSPATH=$(jedit_jar) scalac *.scala
	jar cf $@ $^

.PHONY: install clean

install: $(final_jar)
	mkdir -p ~/.jedit/jars
	cp $^ ~/.jedit/jars

clean:
	-rm *.class $(final_jar)
