let package_name = "ppx_sexp_message"

let sections =
  [ ("lib",
    [ ("built_lib_ppx_sexp_message", None)
    ],
    [ ("META", None)
    ])
  ; ("bin",
    [ ("built_exec_ppx", Some "../lib/ppx_sexp_message/ppx")
    ],
    [])
  ]
