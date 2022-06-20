(lang dune 2.9)
(using mdx 0.1)
(generate_opam_files true)
(name ipld)
(source (github patricoferris/ocaml-ipld))
(authors "patrick@sirref.org")
(maintainers "patrick@sirref.org")
(package
 (name ipld)
 (synopsis "Inter Planetary Linked Data")
 (depends
  cid
  (alcotest :with-test)
  (cbor :with-test)))