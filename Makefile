OPTIONS? = -I /usr/local/lib/ocaml/ocamlmpi unix.cmxa mpi.cmxa str.cmxa -ccopt -L. -cclib -lmpich -cclib -lmpl -cclib -lopa
## see: http://www.ocaml.info/home/ocaml_sources.html#toc16

## put here the names of your source files (in the right order)
SOURCES = \
		dataStructures/color.ml \
		dataStructures/set_patched.ml \
		dataStructures/binomialtree.ml dataStructures/largeArray.ml dataStructures/memoryManagement.ml dataStructures/longString.ml dataStructures/heap.ml dataStructures/safeHeap.ml \
		dataStructures/tools.ml dataStructures/cache.ml main/parameter.ml dataStructures/mapExt.ml dataStructures/valMap.ml dataStructures/dynamicArray.ml dataStructures/mods.ml dataStructures/random_tree.ml \
		Error/debug.ml Error/ExceptionDefn.ml dataStructures/liftSet.ml \
		grammar/ast.ml grammar/kappaParser.mly grammar/kappaLexer.mll \
		pattern/signature.ml pattern/environment.ml \
		siteGraphs/node.ml pattern/mixture.mli  \
		pattern/mixture.ml pattern/precondition.ml pattern/precondition.mli \
		siteGraphs/graph.ml siteGraphs/species.ml pattern/matching.ml pattern/dynamics.ml \
		simulation/state.ml simulation/nonLocal.ml grammar/eval.ml simulation/external.ml  \
		cflow/graph_closure.ml \
		cflow/priority.ml \
		cflow/cflow_handler.ml cflow/profiling2.ml cflow/causal.ml  \
		cflow/kappa_instantiation.ml cflow/po_cut.ml cflow/pseudo_inverse.ml cflow/blackboard_generation.ml cflow/blackboard.ml cflow/propagation_heuristics.ml \
		cflow/generic_branch_and_cut_solver.ml cflow/dag.ml cflow/dag2.ml cflow/compression_main.ml  simulation/plot.ml Error/safe.ml \
		dataStructures/linear.ml \
		spatial/quality.ml spatial/spatial_util.ml spatial/spatial_eval.ml spatial/transport.ml spatial/communication.ml \
		simulation/run.ml \
		main/main.ml 

## the name of the resulting executable
ifeq (Windows,$(findstring Windows,$(OS)))
	RESULT = PISKa.exe
else
	RESULT = PISKa
endif

## generate type information (.annot files)
ANNOTATE = no

## make target (see manual) : byte-code, debug-code, native-code, ...
all: native-code

install:
	cp -f PISKa /usr/bin/PISKa

##if ocamlopt.opt is not in your path, 
##uncomment the line below and set value below according to the location of your ocaml compilers 
##(usually /usr/bin/ under linux and /sw/bin under MAC OS X)

#OCAMLBINPATH = /opt/local/bin/

OCAMLCP = $(OCAMLBINPATH)ocamlc
OCAMLLEX = $(OCAMLBINPATH)ocamllex
OCAMLYACC = $(OCAMLBINPATH)ocamlyacc
OCAMLC = $(OCAMLBINPATH)ocamlc -I unix.cma mpi.cma # 
#`$(OCAMLC) -where`/../mpi
OCAMLOPT = $(OCAMLBINPATH)ocamlopt.opt -I `ocamlfind query mpi`  unix.cmxa mpi.cmxa str.cmxa #-p #-g -ccopt -g -ccopt -pg
OCAMLDEP = $(OCAMLBINPATH)ocamldep unix.cma mpi.cma 
CC = mpicc.openmpi

include OCamlMakefile
