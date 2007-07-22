(* Module Rna2fuun
 *
 * Ocaml Rna to Fuun graphical hoscher
 *)

open Printf
open Rna
open Parseintern


type meta_instr = {
  mi_instr : rna_instr;
  mi_count : int;
  mi_rnaline : int;
}

type bm_mode =
    GM_BITMAP | GM_SRC | GM_DST

let history_granularity = 5000

type gui = {
  mainWindow : GWindow.window;
  visual : Gdk.visual;
  mutable bitmapSelectorButtons : GButton.toggle_button array;
  mutable src_toggler : GButton.toggle_button;
  mutable dest_toggler : GButton.toggle_button;
  mutable rating_mask_toggler : GButton.toggle_button;
  mutable rna_state : rna_state;
  mutable rna_state_history : rna_state option array;
  instructions : meta_instr array;
  mutable currentPos : int;
  mutable currentBitmap : int;
  drawing : GDraw.drawable;
  update_status_fun : gui -> unit;
  mutable breakPoints : rna_instr list;
  mutable srcImg : Gdk.image option;
  mutable destImg : Gdk.image option;
  mutable destBitmap : bitmap;
  mutable bmMode : bm_mode;
}

let usage () =
  fprintf stderr "usage not yet written, sorry";
  exit 1

let bitmap_from_gdkPixbuf pixbuf =
  let bytes = GdkPixbuf.get_pixels pixbuf
  and bitmap = createTransparentBitmap ()
  and i = ref 0
  in
    for y = 0 to 599 do
      for x = 0 to 599 do
	let p = ((Gpointer.get_byte bytes ~pos:!i),
		(Gpointer.get_byte bytes ~pos:(!i + 1)),
		(Gpointer.get_byte bytes ~pos:(!i + 2))), 255
	in
	  bitmap.(y).(x) <- p;
	  i := !i + 3
      done
    done;
    bitmap

let gdkImage_from_gdkPixbuf gui pixbuf =
  let bytes = GdkPixbuf.get_pixels pixbuf
  and img = Gdk.Image.create ~kind:`FASTEST ~visual:gui.visual
    ~width:600 ~height:600
  and i = ref 0
  in
    for y = 0 to 599 do
      for x = 0 to 599 do
	Gdk.Image.put_pixel img ~x ~y
	  ~pixel:(Gdk.Truecolor.color_creator gui.visual
		     ~red:(256 * (Gpointer.get_byte bytes ~pos:!i))
		     ~green:(256 * (Gpointer.get_byte bytes ~pos:(!i + 1)))
		     ~blue:(256 * (Gpointer.get_byte bytes ~pos:(!i + 2))));
	i := !i + 3
      done
    done;
    img

let computeRating bitmap optimalBitmap =
  let errors = ref 0
  in
    for y = 0 to 599 do
      for x = 0 to 599 do
	let p1,_ = bitmap.(y).(x)
	and p2,_ = optimalBitmap.(y).(x)
	in
	  if not (p1 = p2) then
	    incr errors
      done
    done;
    !errors

let meta_instrs_from_rna_instrs = function
    [] -> []
  | x :: xs ->
      let rec work lnr result cur = function
	  [] -> cur :: result
	| x :: xs ->
	    if cur.mi_instr = x then
	      work (lnr + 1) result { cur with mi_count = cur.mi_count + 1 } xs
	    else
	      work (lnr + 1) (cur :: result)
		{ mi_rnaline = lnr; mi_instr = x; mi_count = 1 } xs
      in
	List.rev (work 2 [] { mi_rnaline = 1; mi_instr = x; mi_count = 1 } xs)

let displayBitmap gui bitmap darea =
  let image = Gdk.Image.create ~kind:`FASTEST ~visual:gui.visual
    ~width:600 ~height:600
  and useMask = if gui.rating_mask_toggler#active then true else false
  in
    for y = 0 to 599 do
      for x = 0 to 599 do
	let ((r,g,b), a) =
	  if useMask && bitmap.(y).(x) = gui.destBitmap.(y).(x) then
	    red, 255
	  else
	    bitmap.(y).(x)
	in
	  Gdk.Image.put_pixel image ~x:x ~y:y
	    ~pixel:(Gdk.Truecolor.color_creator gui.visual
		       ~red:(r*256) ~green:(g*256) ~blue:(b*256))
      done
    done;
    gui.drawing#put_image image ~x:0 ~y:0

let displayImage gui = function
  | Some img ->
      gui.drawing#put_image img ~x:0 ~y:0
  | None -> 
      fprintf stderr "failed to draw Gdk.Image :(\n";
      flush stderr

let displayPixBuf gui = function
  | Some pixbuf ->
      gui.drawing#put_pixbuf ~x:0 ~y:0 pixbuf
  | None ->
      fprintf stderr "failed to draw Gdk.PixBuf :(\n";
      flush stderr

let adjustImageSelectorButtons gui =
  let bitmap_nr = List.length gui.rna_state.bitmaps
  in
    begin
      match gui.bmMode with
	  GM_BITMAP ->
	    if gui.src_toggler#active then
	      gui.src_toggler#set_active false;
	    if gui.dest_toggler#active then
	      gui.dest_toggler#set_active false
	| GM_SRC ->
	    if not gui.src_toggler#active then
	      gui.src_toggler#set_active true;
	    if gui.dest_toggler#active then
	      gui.dest_toggler#set_active false
	| GM_DST ->
	    if gui.src_toggler#active then
	      gui.src_toggler#set_active false;
	    if not gui.dest_toggler#active then
	      gui.dest_toggler#set_active true
    end;
    if gui.currentBitmap >= bitmap_nr then
      gui.currentBitmap <- bitmap_nr - 1;
    for i = 0 to 9 do
      let b = gui.bitmapSelectorButtons.(i)
      in
	b#misc#set_sensitive (if i < bitmap_nr then true else false);
	if gui.bmMode != GM_BITMAP || (i != gui.currentBitmap && b#active) then
	  b#set_active false
	else
	  if gui.bmMode == GM_BITMAP &&
	    (i == gui.currentBitmap && (not b#active)) then
	    b#set_active true
    done

let update_gui gui () =
(*  displayDreck gui *)
  adjustImageSelectorButtons gui;
  begin
    match gui.bmMode with
      | GM_BITMAP ->
	  displayBitmap gui (List.nth gui.rna_state.bitmaps
				gui.currentBitmap) gui.drawing;
      | GM_SRC ->
	  displayImage gui gui.srcImg
      | GM_DST ->
	  displayImage gui gui.destImg
  end;
  gui.update_status_fun gui
    (*  displayBitmapManually gui (List.hd gui.rna_state.bitmaps) *)

let redraw_gui gui _ =
  update_gui gui ();
  false

let rnaStep ?(break=false) gui i () =
  let rec step_intern distance =
    begin
      if (gui.currentPos mod history_granularity) = 0 then
	let histpos = gui.currentPos / history_granularity
	in
	  match gui.rna_state_history.(histpos) with
	      None ->
		gui.rna_state_history.(histpos) <-
		  Some (duplicate_rna_state gui.rna_state)
	    | _ -> ()
    end;
    match distance with
	0 -> ()
      | i when break && 
	    (List.memq gui.instructions.(gui.currentPos).mi_instr
		gui.breakPoints) ->
	  ()
      | i when gui.currentPos < Array.length gui.instructions ->
	  let inst = gui.instructions.(gui.currentPos)
	  in
	    for j = 1 to inst.mi_count
	    do
	      apply_instr gui.rna_state inst.mi_instr
	    done;
	    gui.currentPos <- gui.currentPos + 1;
	    step_intern (i - 1)
      | _ -> ()
  in
    step_intern i;
    update_gui gui ()

let rnaReset gui =
    gui.currentPos <- 0;
    gui.rna_state <- createRNAState ()

let rnaGoto gui newPos =
  let rec find_history = function
    | 0 -> 0, None
    | i when gui.rna_state_history.(i) = None -> find_history (i - 1)
    | i -> i, gui.rna_state_history.(i)
  in let missingsteps =
    match find_history (newPos / history_granularity) with
	_, None -> rnaReset gui; newPos
      | i, Some rs ->
	  (*	  fprintf stderr "rolling back to history %i\n"
		  (i * history_granularity);
		  flush stderr;
	  *)
	  gui.currentPos <- i * history_granularity;
	  gui.rna_state <- duplicate_rna_state rs;
	  newPos - (i * history_granularity)
  in
       (*       fprintf stderr "goto executes %i steps now\n" missingsteps;
		flush stderr;
       *)
       rnaStep gui missingsteps ()

let breakpointChanged (combo : #GEdit.combo_box) gui () =
  gui.breakPoints <-
    [| []; [RI_Compose; RI_AddBitmap]; [RI_Compose]; [RI_AddBitmap];
       [RI_Clip]; [RI_Fill] |].(combo#active)

let setupGui (rna_instrs : rna_instr list) =
  let meta_instrs = Array.of_list (meta_instrs_from_rna_instrs rna_instrs)
  in let w = GWindow.window ~title:"grna2fuun"
    ~show:true ~width:1000 ~height:750 ()
    (* rest | cmdlist *)
  in let hb1 = GPack.hbox ~border_width:4 ~spacing:4 ~packing:w#add ()
    (* image notebook - small images - status - commands *)
  in let vbMain = GPack.vbox ~border_width:4 ~spacing:4 ~packing:hb1#add ()
    (* instr list container *)
  in let hbImgSelButs = GPack.hbox ~border_width:4 ~spacing:4
    ~packing:vbMain#add ()
  in let areaEventBox = GBin.event_box ~border_width:0
    ~width:600 ~height:600 ~packing:vbMain#add ()
  in let hbStatus = GPack.hbox ~border_width:4 ~spacing:4
    ~packing:vbMain#add ()
  in let hbStatus2 = GPack.hbox ~border_width:4 ~spacing:4
    ~packing:vbMain#add ()
  in let hbCmd = GPack.hbox ~border_width:4 ~spacing:4
    ~packing:vbMain#add ()
  in let instrListBar = GRange.scrollbar `VERTICAL
    ~packing:(hb1#pack ~from:`END) ()
  in let instrList = GList.clist
    ~titles:["Number";"Count";"Command";"RNA-Line"]
    ~shadow_type:`OUT ~vadjustment:instrListBar#adjustment
    ~packing:(hb1#pack ~expand:true) ()
  in let reset = GButton.button ~label:"reset" ~packing:hbCmd#pack ()
  in let redraw = GButton.button ~label:"redraw" ~packing:hbCmd#pack ()
  in let plus1 = GButton.button ~label:"+1" ~packing:hbCmd#pack ()
  in let plus10 = GButton.button ~label:"+10" ~packing:hbCmd#pack ()
  in let plus100 = GButton.button ~label:"+100" ~packing:hbCmd#pack ()
  in let plus1000 = GButton.button ~label:"+1000" ~packing:hbCmd#pack ()
  in let plus10000 = GButton.button ~label:"+10000" ~packing:hbCmd#pack ()
  in let runto = GButton.button ~label:"Run to:" ~packing:hbCmd#pack ()
  in let (breakCombo, (_, breakComboColumn)) =
    GEdit.combo_box_text ~packing:hbCmd#pack 
      ~strings:["End" ; "Compose/AddBitmap" ; "Compose" ; "AddBitmap" ;
		"Clip" ; "Fill";] ()
  in let _ = GMisc.label ~text:"POS:" ~packing:hbStatus#pack ()
  in let posLabel = GMisc.label ~text:"?" ~packing:hbStatus#pack ()
  in let _ = GMisc.label ~text:"MARK:" ~packing:hbStatus#pack ()
  in let markLabel = GMisc.label ~text:"?" ~packing:hbStatus#pack ()
  in let _ = GMisc.label ~text:"DIR:" ~packing:hbStatus#pack ()
  in let dirLabel = GMisc.label ~text:"?" ~packing:hbStatus#pack ()
  in let _ = GMisc.label ~text:"BITMAPS:" ~packing:hbStatus#pack ()
  in let bitmapsLabel = GMisc.label ~text:"?" ~packing:hbStatus#pack ()
  in let statelistlenlabel = GMisc.label ~text:"RNAs:"
    ~packing:hbStatus#pack ()
  in let ratingLabel = GMisc.label ~text:"Rating: ? (?%)" ~packing:hbStatus2#pack ()
  in let mouseCoordLabel = GMisc.label ~text:"Mouse()"
    ~packing:hbStatus#pack ()
  in let area = GMisc.drawing_area ~width:600 ~height:600
    ~packing:areaEventBox#add ()
  in let drawing = area#misc#realize (); new GDraw.drawable (area#misc#window)
  in let update_status gui =
    let rnaState = gui.rna_state
    in let posix, posiy = rnaState.Rna.position
    and markx, marky = rnaState.mark
    and rating = computeRating (List.hd gui.rna_state.bitmaps) gui.destBitmap
    in
      instrList#select gui.currentPos 1;
      instrList#moveto (abs (gui.currentPos - 5)) 1;
      posLabel#set_text (sprintf "(%i,%i)" posix posiy);
      markLabel#set_text (sprintf "(%i,%i)" markx marky);
      dirLabel#set_text (string_of_dir rnaState.dir);
      bitmapsLabel#set_text (string_of_int (List.length rnaState.bitmaps));
      ratingLabel#set_text (sprintf "Rating: %i (%f%%)" rating
			       ((float_of_int (600*600 - rating))
				 /. (600.0 *. 6.0)));
      statelistlenlabel#set_text (sprintf "RNA: %i/%i"
				     gui.currentPos (Array.length meta_instrs))
  in let mouseMoveCB moEv =
    mouseCoordLabel#set_text (sprintf "Mouse(%i,%i)"
				 (int_of_float (GdkEvent.Motion.x moEv))
				 (int_of_float (GdkEvent.Motion.x moEv)));
       false
  in let rna_state_history_size =
    1 + (Array.length meta_instrs) / history_granularity
  in let gui = { mainWindow = w;
		 visual = w#misc#visual;
		 rna_state = createRNAState ();
		 rna_state_history = Array.create rna_state_history_size None;
		 instructions = meta_instrs;
		 currentPos = 0;
		 currentBitmap = 0;
		 drawing = drawing;
		 breakPoints = [];
		 bitmapSelectorButtons = [| |];
		 destBitmap = createTransparentBitmap ();
		 src_toggler = GButton.toggle_button ();
		 dest_toggler = GButton.toggle_button ();
		 rating_mask_toggler = GButton.toggle_button ();
		 update_status_fun = update_status;
		 srcImg = None;
		 destImg = None;
		 bmMode = GM_BITMAP;
  }
  in let select_bitmap nr () =
    if (nr != gui.currentBitmap &&
	gui.bitmapSelectorButtons.(nr)#active) then begin
      gui.bmMode <- GM_BITMAP;
      gui.currentBitmap <- nr;
      adjustImageSelectorButtons gui;
      update_gui gui ()
    end
  in let createBitmapSelectorButtons () =
    let a = Array.create 10 (GButton.toggle_button ())
    in
      for i = 0 to 9 do
	let b = GButton.toggle_button ~label:("BM"^(string_of_int i))
	  ~packing:hbImgSelButs#pack ()
	in
	  ignore (b#connect#toggled ~callback:(select_bitmap i));
	  a.(i) <- b
      done;
      a
  in let reset_gui () =
    rnaReset gui;
    update_gui gui ()
  in let goto_gui newPos =
    if newPos != gui.currentPos then begin
      rnaGoto gui newPos;
      update_gui gui ()
    end
  in let mi_convert i e =
    ignore (instrList#append [string_of_int i;
			      string_of_int e.mi_count;
			      string_of_instr e.mi_instr;
                              string_of_int e.mi_rnaline])
  and rmt_cb () =
    update_gui gui ()
  and srcdest_cb who what () =
    if who#active then
      begin
	if gui.bmMode != what then begin
	  gui.currentBitmap <- -1;
	  gui.bmMode <- what;
	  adjustImageSelectorButtons gui;
	  update_gui gui ()
	end
      end
  in
       begin
	 try
	   gui.srcImg <- Some (gdkImage_from_gdkPixbuf gui
				  (GdkPixbuf.from_file "source.png"))
	 with
	     _ ->
	       fprintf stderr "failed to load source.png\n"; flush stderr
       end;
    begin
      try
	let pixbuf = (GdkPixbuf.from_file "target.png")
	in
	  gui.destImg <- Some (gdkImage_from_gdkPixbuf gui
				  (GdkPixbuf.from_file "target.png"));
	  gui.destBitmap <- bitmap_from_gdkPixbuf pixbuf
      with
	  _ ->
	    fprintf stderr "failed to load target.png\n"; flush stderr
    end;
    breakCombo#set_active 0;
    ignore (breakCombo#connect#changed (breakpointChanged breakCombo gui));
    gui.src_toggler <-
      GButton.toggle_button ~label:"SRC" ~packing:hbImgSelButs#pack ();
    ignore (gui.src_toggler#connect#clicked
	       ~callback:(srcdest_cb gui.src_toggler GM_SRC));
    gui.dest_toggler <-
      GButton.toggle_button ~label:"DST" ~packing:hbImgSelButs#pack ();
    ignore (GMisc.separator `VERTICAL ~packing:hbImgSelButs#pack ());
    gui.bitmapSelectorButtons <- createBitmapSelectorButtons ();
    ignore (gui.dest_toggler#connect#clicked
	       ~callback:(srcdest_cb gui.dest_toggler GM_DST));
    ignore (GMisc.separator `VERTICAL ~packing:hbImgSelButs#pack ());
    gui.rating_mask_toggler <-
      GButton.toggle_button ~label:"Rating" ~packing:hbImgSelButs#pack ();
    ignore (gui.rating_mask_toggler#connect#toggled ~callback:rmt_cb);
    Array.iteri mi_convert meta_instrs;
    ignore (reset#connect#clicked ~callback:reset_gui);
    ignore (redraw#connect#clicked ~callback:(update_gui gui));
    ignore (plus1#connect#clicked ~callback:(rnaStep gui 1));
    ignore (plus10#connect#clicked ~callback:(rnaStep gui 10));
    ignore (plus100#connect#clicked ~callback:(rnaStep gui 100));
    ignore (plus1000#connect#clicked ~callback:(rnaStep gui 1000));
    ignore (plus10000#connect#clicked ~callback:(rnaStep gui 10000));
    ignore (areaEventBox#event#connect#motion_notify ~callback:mouseMoveCB);
    areaEventBox#event#add [`POINTER_MOTION];
    ignore (runto#connect#clicked ~callback:(rnaStep ~break:true gui max_int));
    ignore (area#event#connect#expose ~callback:(redraw_gui gui));
    ignore (w#connect#destroy ~callback:GMain.Main.quit);
    ignore (instrList#connect#select_row
	       ~callback:(fun ~row ~column ~event -> goto_gui row))

let _ =
  begin
    try
      ignore (GMain.init ());
    with
	x ->
	  fprintf stderr "are you sure you have an X server running?\n";
	  raise x
  end;
if Array.length Sys.argv != 2 then
    usage ();
  let statelist =
    try
      Parseintern.read_trace Sys.argv.(1);
    with
	x ->
	  fprintf stderr "parse error: %s\n" (Printexc.to_string x);
	  exit 2
  in
    setupGui statelist;
    (*
      Gdk.Rgb.init ();
      GtkBase.Widget.set_default_visual (Gdk.Rgb.get_visual ());
      GtkBase.Widget.set_default_colormap (Gdk.Rgb.get_cmap ());
    *)
    GMain.Main.main ()
(*  Parseintern.read_trace Sys.argv.(1); *)
