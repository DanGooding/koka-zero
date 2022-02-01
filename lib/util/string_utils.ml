open Core

let limit_length ~limit s = String.slice s 0 (min limit (String.length s))
