signature TEMP = 
sig
  eqtype temp
  val reset : unit -> unit
  val newtemp : unit -> temp
  structure TempOrd : ORD_KEY sharing type TempOrd.ord_key = temp
  val makestring: temp -> string
  type label = Symbol.symbol
  val newlabel : unit -> label
  val namedlabel : string -> label
  val newFunctionLabel : unit -> label
  structure Set : ORD_SET sharing type Set.Key.ord_key = temp
  structure Map : ORD_MAP sharing type Map.Key.ord_key = temp
end

