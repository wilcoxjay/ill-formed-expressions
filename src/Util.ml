module Combinators =
struct
  let (|>) x f = f x
  
  let (@@) f x = f x
  
  let (>>) x f = ignore (f x); x
end

let with_ic path f =
  let ic = Pervasives.open_in path in
  let res = f ic in
  Pervasives.close_in ic;
  res

let with_oc path f =
  let oc = Pervasives.open_out path in
  let res = f oc in
  Pervasives.close_out oc;
  res

let substring_at needle haystack i =
  let rec loop j =
    if j >= String.length needle then true
    else if i + j >= String.length haystack then false
    else
      String.get haystack (i + j) = String.get needle j &&
        loop (j + 1)
  in loop 0

let is_substring needle haystack =
  let rec loop i =
    if i >= String.length haystack then false
    else substring_at needle haystack i || loop (i + 1)
  in loop 0

let startswith needle haystack = substring_at needle haystack 0
