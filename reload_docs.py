#!/usr/bin/env python
from livereload import Server, shell
server = Server()
server.watch('*.rst', shell('sphinx-build -b html -a -n . _build'))
server.watch('tutorials/*.rst', shell('sphinx-build -b html -a -n . _build'))
server.serve(root='_build/')
