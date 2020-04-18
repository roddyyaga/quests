(lang dune 2.4)
(name quests)
(generate_opam_files true)

(package
(name quests)
(synopsis "Python requests but it's OCaml")
(depends
dune
ssl
lwt_ssl
lwt
cohttp
cohttp-lwt-unix))
