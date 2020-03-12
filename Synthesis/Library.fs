module Synthesis

let abelar x = (x > 12 && x < 3097 && x%12=0 )
  //  failwith "Not implemented"

let area b h =
   match (b >= 0.0) && (h >= 0.0) with
   | true -> 0.5*b*h
   | false -> failwith("Can't handle negative values")
    
    // failWith("Can't handle negatives")
   
    // failwith "Not implemented"

let zollo _ =
    failwith "Not implemented"

let min _ _ =
    failwith "Not implemented"

let max _ _ =
    failwith "Not implemented"

let ofTime _ _ _ =
    failwith "Not implemented"

let toTime _ =
    failwith "Not implemented"

let digits _ =
    failwith "Not implemented"

let minmax _ =
    failwith "Not implemented"

let isLeap _ =
    failwith "Not implemented"

let month _ =
    failwith "Not implemented"

let toBinary _ =
    failwith "Not implemented"

let bizFuzz _ =
    failwith "Not implemented"

let monthDay _ _ =
    failwith "Not implemented"

let coord _ =
    failwith "Not implemented"