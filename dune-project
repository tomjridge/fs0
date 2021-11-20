(lang dune 2.9)
(name fs0)

;; for opam
(authors "Tom Ridge <tom.j.ridge@googlemail.com>")
(maintainers "Tom Ridge <tom.j.ridge@googlemail.com>")
(source (github tomjridge/fs0))
(homepage https://github.com/tomjridge/fs0)
(documentation https://tomjridge.github.io/fs0/ocamldoc/index.html)
(generate_opam_files true)


(package 
 (name fs0)
 (synopsis "A minimal basis for building a file system on a block device")
 (depends ocamlfind dune ppx_jane 
          core ;; for profiling, in bin
))


