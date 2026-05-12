open Alsdiff_base
open Alsdiff_base.Diff


module CurveControls = struct
  type t = {
    curve1_x : float;
    curve1_y : float;
    curve2_x : float;
    curve2_y : float;
  } [@@deriving eq, patch, view_spec] [@@patch.generate_diff]
end


type event_value =
  | FloatEvent of float
  | IntEvent of int
  | EnumEvent of int
[@@deriving eq]

module EnvelopeEvent = struct
  type t = {
    id : int; [@id.id] [@patch.skip]
    time : float;
    value : event_value;       [@view.skip]
    curve : CurveControls.t option;  [@view.skip]
  } [@@deriving eq, id, patch, view_spec] [@@patch.generate_diff]

  let create (xml : Xml.t) : t =
    let tag_name = Xml.get_name xml in
    let id = Xml.get_int_attr "Id" xml in
    let time = Xml.get_float_attr "Time" xml in
    let curve =
      match (Xml.get_float_attr_opt "CurveControl1X" xml,
             Xml.get_float_attr_opt "CurveControl1Y" xml,
             Xml.get_float_attr_opt "CurveControl2X" xml,
             Xml.get_float_attr_opt "CurveControl2Y" xml) with
      | (Some c1x, Some c1y, Some c2x, Some c2y) ->
        Some { CurveControls.curve1_x = c1x; curve1_y = c1y;
               curve2_x = c2x; curve2_y = c2y }
      | _ -> None
    in
    let value = match tag_name with
      | "FloatEvent" -> FloatEvent (Xml.get_float_attr "Value" xml)
      | "IntEvent" -> IntEvent (Xml.get_int_attr "Value" xml)
      | "EnumEvent" -> EnumEvent (Xml.get_int_attr "Value" xml)
      | _ -> raise (Xml.Xml_error (xml, "Unknown event type: " ^ tag_name))
    in
    { id; time; value; curve }
end


type t = {
  id : int; [@id.id] [@patch.identity] [@view.skip]
  target : int; [@id.id] [@patch.identity] [@view.skip]
  events : EnvelopeEvent.t list;  [@view.collection "DTEvent"] [@view.label "Events"]
} [@@deriving eq, id, patch, view_spec] [@@patch.generate_diff]

(* Automation contains a list of EnvelopeEvents and is therefore
   a structured type at a higher level of abstraction. *)

let create (xml : Alsdiff_base.Xml.t) : t =
  let id = Xml.get_int_attr "Id" xml in
  let target = Upath.get_int_attr "/EnvelopeTarget/PointeeId" "Value" xml in
  let events =
    xml |> Upath.find_all "/Automation/Events/'(Float|Int|Enum)Event'"
    |> List.map (fun (_, event) -> EnvelopeEvent.create event)
    |> List.sort (fun a b -> Float.compare a.EnvelopeEvent.time b.EnvelopeEvent.time)
  in
  { id; target; events }
