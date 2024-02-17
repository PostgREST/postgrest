#!/usr/bin/env python
import sys
from livereload import Server, shell
from subprocess import call

if len(sys.argv) == 1:
    locale = "default"
    build = "./build.sh"
else:
    locale = sys.argv[1]
    build = f"./build.sh {locale}"

call(build, shell=True)

server = Server()
server.watch("**/*.rst", shell(build))
server.watch(f"locales/{locale}/LC_MESSAGES/*.po", shell(build))
server.serve(root=f"_build/html/{locale}")
