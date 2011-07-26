scala_files := $(wildcard *.scala)
class_files := $(scala_files:.scala=.class)
jedit_jar   := $(shell locate jedit.jar)
final_jar   := ElasticTabstopsPlugin.jar

$(final_jar): *.html *.props $(class_files)
	jar cf $@ $^

$(class_files): $(scala_files)
	CLASSPATH=$(jedit_jar) scalac $^

.PHONY: install clean

install: $(final_jar)
	mkdir -p ~/.jedit/jars
	cp $^ ~/.jedit/jars

clean:
	-rm $(class_files) $(final_jar)
