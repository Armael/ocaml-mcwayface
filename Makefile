default:
	jbuilder build src/hello.exe

lib:
	jbuilder build @install

clean:
	rm -rf _build

.PHONY: default clean lib
