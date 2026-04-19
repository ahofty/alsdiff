open Alsdiff_merge

type resolution_status =
  | Auto
  | Unresolved
  | Resolved of Conflict.resolution

type merge_node = {
  path : string;
  label : string;
  status : resolution_status;
  ours_desc : string option;
  theirs_desc : string option;
  depth : int;
  is_expandable : bool;
  children : merge_node list;
}

module StringSet = Set.Make (String)

type mode = Merge | Help

type t = {
  mode : mode;
  context : Merge.merge_context;
  flat_nodes : merge_node list;
  expanded_paths : StringSet.t;
  cursor_index : int;
  resolutions : (string, Conflict.resolution) Hashtbl.t;
  viewport_width : int;
  viewport_height : int;
  ours_file : string;
  base_file : string;
  theirs_file : string;
}

let create
    ~(context : Merge.merge_context)
    ~(flat_nodes : merge_node list)
    ~(ours_file : string)
    ~(base_file : string)
    ~(theirs_file : string) () : t =
  {
    mode = Merge;
    context;
    flat_nodes;
    expanded_paths = StringSet.empty;
    cursor_index = 0;
    resolutions = Hashtbl.create 16;
    viewport_width = 80;
    viewport_height = 24;
    ours_file;
    base_file;
    theirs_file;
  }
