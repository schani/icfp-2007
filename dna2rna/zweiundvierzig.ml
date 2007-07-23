(* 42 *)

let answer : (int * (string * int option * int option * int option)) list = [
  ( 0x000510, ("AAA_geneTablePageNr", None, None, None ));
  ( 0x2ccd88, ("M-class-planet", None, None, None ));
  ( 0x0c4589, ("__array_index", None, None, None ));
  ( 0x0c45a1, ("__array_value", None, None, None ));
  ( 0x0c45e9, ("__bool", None, None, None ));
  ( 0x0c45ea, ("__bool_2", None, None, None ));
  ( 0x0c45b9, ("__funptr", None, None, None ));
  ( 0x0c461b, ("__int1", None, None, None ));
  ( 0x0c4628, ("__int12", None, None, None ));
  ( 0x0c4634, ("__int12_2", None, None, None ));
  ( 0x0c45eb, ("__int24", None, None, None ));
  ( 0x0c4603, ("__int24_2", None, None, None ));
  ( 0x0c4625, ("__int3", None, None, None ));
  ( 0x0c4640, ("__int48", None, None, None ));
  ( 0x0c461c, ("__int9", None, None, None ));
  ( 0x0c4541, ("acc1", None, None, None ));
  ( 0x0c4559, ("acc2", None, None, None ));
  ( 0x0c4571, ("acc3", None, None, None ));
  ( 0x0c7da8, ("angleSkip", None, None, None ));
  ( 0x5a5f7e, ("drawPoly", None, Some 24529, Some 0));
  ( 0x6fce9c, ("activateAdaptionTree", None, None, None ));
  ( 0x6fd99e, ("activateGene", None, None, None ));
  ( 0x252fa1, ("adapter", None, None, None ));
  ( 0x41b532, ("addFunctinosCBF", None, None, None ));
  ( 0x5412ab, ("move", None, Some 48, Some 0));
  ( 0x54b1ba, ("addInts", None, None, None ));
  ( 0x5580c4, ("anticompressant", None, None, None ));
  ( 0x571a21, ("??571a21", None, Some 48, None ));
  ( 0x65f785, ("apple", None, Some 0, Some 0 ));
  ( 0x3c870e, ("appletree", None, None, None ));
  ( 0x711dc6, ("apply1_adaption", None, None, None ));
  ( 0x719633, ("apply2_adaption", None, None, None ));
  ( 0x6f1943, ("drawEllipse", None, Some 97, Some 0));
]

let get_answer key =
  if List.mem_assoc key answer then
    List.assoc key answer
  else
    ((Printf.sprintf "#x%x" key), None, None, None)
  
