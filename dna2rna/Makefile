DISTNAME = dna
BINFILES = dna
SBINFILES =
LIBFILES =
DOCFILES = COPYING LICENSE CHANGELOG VERSION 
OTHERFILES = $(DISTNAME).spec
SRCFILES = $(shell echo *.ml *.mli *.mll *.mly) Makefile Makefile.ocaml
DISTFILES = $(DOCFILES) $(OTHERFILES) $(SRCFILES)
DISTVERSIONFILES =
MODULES =  dnabuf rna dna2rna zweiundvierzig disasm
LIBRARIES = nums
DISTVERSION = $(shell cat VERSION)

LIBDIRS = 
BUILDSUBDIRS = 
REBUILDSUBDIRS = 

INSTALL_PREFIX = /

include ../Makefile.ocaml

