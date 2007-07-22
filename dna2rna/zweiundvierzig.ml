(* 42 *)

let answer = [
  ( 0x000510, "AAA_geneTablePageNr" );
  ( 0x2ccd88, "M-class-planet" );
  ( 0x0c4589, "__array_index" );
  ( 0x0c45a1, "__array_value" );
  ( 0x0c45e9, "__bool" );
  ( 0x0c45ea, "__bool_2" );
  ( 0x0c45b9, "__funptr" );
  ( 0x0c461b, "__int1" );
  ( 0x0c4628, "__int12" );
  ( 0x0c4634, "__int12_2" );
  ( 0x0c45eb, "__int24" );
  ( 0x0c4603, "__int24_2" );
  ( 0x0c4625, "__int3" );
  ( 0x0c4640, "__int48" );
  ( 0x0c461c, "__int9" );
  ( 0x0c4541, "acc1" );
  ( 0x0c4559, "acc2" );
  ( 0x0c4571, "acc3" );
  ( 0x6fce9c, "activateAdaptionTree" );
  ( 0x6fd99e, "activateGene" );
  ( 0x252fa1, "adapter" );
  ( 0x41b532, "addFunctinosCBF" );
  ( 0x54b1ba, "addInts" );
  ( 0x5580c4, "anticompressant" );
  ( 0x65f785, "apple" );
  ( 0x3c870e, "appletree" );
  ( 0x711dc6, "apply1_adaption" );
  ( 0x719633, "apply2_adaption" );
]

let get_answer key =
  if List.mem_assoc key answer then
    List.assoc key answer
  else
    Printf.sprintf "0x%06x" key
