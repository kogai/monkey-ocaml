NAME := monkey
TEST_NAME := $(NAME)_test
PKGS := core,menhirlib,ppx_deriving,ppx_deriving.show
SRC_FILES := $(shell find ./src -type f -name '*.m*')
SRC_DIRS := "src"

OCB_FLAGS := -use-ocamlfind -use-menhir -Is $(SRC_DIRS) -pkgs $(PKGS)
OCB := ocamlbuild $(OCB_FLAGS)
OPAM_VER := 4.03.0
ARGS := "fixture/bool.mky"

all:$(NAME).native $(NAME).byte

$(NAME).native: $(SRC_FILES)
	eval `opam config env` && \
	$(OCB) $(NAME).native

$(NAME).byte: $(SRC_FILES)
	eval `opam config env` && \
	$(OCB) $(NAME).byte

.PHONY: native
native: $(NAME).native
	@./$(NAME).native $(ARGS)

.PHONY: byte
byte: $(NAME).byte
	@./$(NAME).byte $(ARGS)

.PHONY: docker
docker:
	docker-compose build && \
	docker-compose run $(NAME) make

.PHONY: run
run:
	docker-compose run $(NAME) $(ARGS)

.PHONY: install
install:
	opam init -ya --comp=$(OPAM_VER) && \
	opam switch $(OPAM_VER) && \
	eval `opam config env` && \
	opam update && \
	opam install -y \
		ocamlfind \
		merlin \
		core

.PHONY: setup
setup:
	opam user-setup install

.PHONY: clean
clean:
	$(OCB) -clean
