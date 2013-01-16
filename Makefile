OCAMLC=ocamlc
RM= rm -f
CP=cp

all: compilation
	@echo "*** linking minijava"
	$(OCAMLC) -o minijavac $(INCS) $(OBJS)

compilation: 
	@for d in $(DIRS) ; do                  \
		echo "*** compiling in  $$d";                     \
		$(MAKE) -C $$d --no-print-directory;           \
	done

DEPENDENCIES = $(DIRS)

clean: 
	@echo '*** cleaning...'
	@$(RM) *.cm[ixo] *~ *.~* minijavac
	@for d in $(DEPENDENCIES); do                             \
		echo $$d;                                         \
		$(MAKE) -C $$d clean --no-print-directory;        \
	done
	@echo '*** done.'

clean-all:
	@echo '*** cleaning all...'
	@$(RM) *.cm[ixo] *~ *.~* .depend minijavac
	@for d in $(DEPENDENCIES); do                             \
		echo $$d;                                         \
		$(MAKE) -C $$d clean-all --no-print-directory;    \
	done
	@echo '*** done.'

dep: depend

depend:
	@echo '*** computing dependencies...'
	@for d in $(DEPENDENCIES); do                             \
		echo $$d;                                         \
		$(MAKE) -C $$d depend --no-print-directory;       \
	done
	@echo '*** done.'




### ********************************************************************* ###
### ********************************************************************* ###
###                                                                       ###
### Unless you now what you're doing, don't modify the following lines.   ###
###                                                                       ###
### ********************************************************************* ###
### ********************************************************************* ###


# directories to go in (they must be in the good dependency order!!)
DIRS = Utils Parsing Typing Compilation Runtime Main 

# -I directives for linkage
INCS = $(patsubst %,-I %, $(DIRS))

# object files needed for linkage
OBJS = Location.cmo Error.cmo TypeError.cmo Type.cmo AST.cmo Parser.cmo Lexer.cmo \
	Env.cmo Typing.cmo TypeEnv.cmo Eval.cmo Main.cmo

