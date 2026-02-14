open View_model
open Config

let display_name (dt : domain_type) : string =
  (domain_type_to_string dt) ^ "s"

(* Build the initial stats list from config - include types not set to Ignore *)
let stats_from_config (cfg : detail_config) : (domain_type * change_breakdown) list =
  (* All domain types we might track - check if they're not Ignored *)
  let all_types = [
    DTLiveset; DTTrack; DTDevice; DTClip; DTNote;
    DTAutomation; DTMixer; DTRouting; DTLocator;
    DTParam; DTEvent; DTSend; DTPreset; DTMacro;
    DTSnapshot; DTLoop; DTSignature; DTSampleRef;
    DTVersion; DTOther;
  ] in
  (* Filter to types that should be tracked (not Ignore for their change type) *)
  List.filter_map (fun dt ->
      (* Check if any change type for this domain type is not Ignore *)
      let trackable =
        (get_effective_detail cfg Added dt <> Ignore) ||
        (get_effective_detail cfg Removed dt <> Ignore) ||
        (get_effective_detail cfg Modified dt <> Ignore) ||
        (get_effective_detail cfg Unchanged dt <> Ignore)
      in
      if trackable then Some (dt, { added = 0; removed = 0; modified = 0 })
      else None
    ) all_types

let increment_stats stats dt ct =
  List.map
    (fun (d, b) -> if d = dt then (d, increment_breakdown b ct) else (d, b))
    stats

let rec collect_view (cfg : detail_config) (stats : (domain_type * change_breakdown) list) (view : view) =
  match view with
  | Field _ -> stats
  | Item item ->
    let stats =
      (* Use get_effective_detail to check if type should be tracked *)
      let level = get_effective_detail cfg item.change item.domain_type in
      if should_render_level level && item.change <> Unchanged then
        increment_stats stats item.domain_type item.change
      else stats
    in
    List.fold_left (collect_view cfg) stats item.children
  | Collection col -> List.fold_left (collect_view cfg) stats col.items

let collect (cfg : detail_config) (views : view list) : (domain_type * change_breakdown) list =
  List.fold_left (collect_view cfg) (stats_from_config cfg) views

let render_line (dt : domain_type) (b : change_breakdown) : string option =
  if total_breakdown b = 0 then None
  else
    let parts =
      List.filter_map Fun.id
        [
          (if b.added > 0 then Some (Printf.sprintf "%d Added" b.added)
           else None);
          (if b.removed > 0 then Some (Printf.sprintf "%d Removed" b.removed)
           else None);
          (if b.modified > 0 then Some (Printf.sprintf "%d Modified" b.modified)
           else None);
        ]
    in
    Some (Printf.sprintf "%s: %s" (display_name dt) (String.concat ", " parts))

let render (cfg : detail_config) (views : view list) : string =
  let stats = collect cfg views in
  let lines =
    List.filter_map (fun (dt, b) -> render_line dt b) stats
  in
  match lines with
  | [] -> "No changes."
  | _ -> String.concat "\n" lines
