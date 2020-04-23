(lang dune 2.0)
(generate_opam_files true)
(name quests)
(version 0.1)
(documentation "https://github.com/roddyyaga/quests")
(license MIT)
(authors "Roddy MacSween <github@roddymacsween.co.uk>")
(maintainers "Roddy MacSween <github@roddymacsween.co.uk>")

(package
(name quests)
(synopsis "HTTP client library like Python requests")
(depends
(dune (>= 2.0))
ssl
lwt_ssl
tls
lwt
cohttp
cohttp-lwt-unix
yojson
ppx_yojson
ppx_deriving
lwt_ppx
ezgzip
(alcotest :with-test)
(alcotest-lwt :with-test)))
