open Alsdiff_merge

type resolution_status =
  | Auto
  | Unresolved
  | Resolved of Conflict.resolution
  | Mixed_resolved

type version_presence = { ours : bool; base : bool; theirs : bool }

type entity_xml_data = {
  base_xml : Alsdiff_base.Xml.t option;
  ours_xml : Alsdiff_base.Xml.t option;
  theirs_xml : Alsdiff_base.Xml.t option;
}

type merge_node = {
  path : string;
  label : string;
  name : string;
  presence : version_presence;
  status : resolution_status;
  ours_desc : string option;
  theirs_desc : string option;
  conflict_kind : Conflict.conflict_kind option;
  depth : int;
  is_expandable : bool;
  entity_data : entity_xml_data option;
  children : merge_node list;
}

module StringSet = Set.Make (String)

type mode = Merge | SideBySide | Help | Detail | CherryPick

type cherry_pick_state = {
  entity_path : string;
  field_selections : (string * Conflict.resolution) list;
  field_diffs : Xml_compare.field_diff list;
  cursor_field : int;
}

type t = {
  mode : mode;
  context : Merge.merge_context;
  flat_nodes : merge_node list;
  expanded_paths : StringSet.t;
  cursor_index : int;
  resolutions : (string, Conflict.resolution) Hashtbl.t;
  viewport_width : int;
  viewport_height : int;
  detail_node : merge_node option;
  ours_file : string;
  base_file : string;
  theirs_file : string;
  last_action : string;
  cherry_pick : cherry_pick_state option;
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
    detail_node = None;
    resolutions = Hashtbl.create 16;
    viewport_width = 80;
    viewport_height = 24;
    ours_file;
    base_file;
    theirs_file;
    last_action = "";
    cherry_pick = None;
  }
