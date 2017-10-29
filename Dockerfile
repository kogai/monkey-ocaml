FROM ocaml/opam:ubuntu-17.04_ocaml-4.03.0

ADD . /monkey_ocaml

RUN sudo chown opam /monkey_ocaml
USER opam
WORKDIR /monkey_ocaml

RUN make install

CMD make native
