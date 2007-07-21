DISTNAME = rna2fuun
BINFILES = rna2fuun grna2fuun
SBINFILES =
LIBFILES =
DOCFILES = COPYING LICENSE CHANGELOG VERSION 
OTHERFILES = $(DISTNAME).spec
SRCFILES = $(shell echo *.ml *.mli *.mll *.mly) Makefile Makefile.ocaml
DISTFILES = $(DOCFILES) $(OTHERFILES) $(SRCFILES)
DISTVERSIONFILES =
MODULES = rna parseintern
LIBRARIES =
LIBRARIES-grna2fuun = lablgtk2/lablgtk
DISTVERSION = $(shell cat VERSION)

LIBDIRS = /usr/lib/ocaml/lablgtk2
BUILDSUBDIRS = 
REBUILDSUBDIRS = 

INSTALL_PREFIX = /

include ../Makefile.ocaml
