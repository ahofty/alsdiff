open View_model
open Config

let reportable_types =
  [ DTTrack; DTDevice; DTClip; DTNote; DTAutomation; DTSend; DTParam; DTLocator ]

let display_name (dt : domain_type) : string =
  match dt with
  | DTTrack -> "Tracks"
  | DTDevice -> "Devices"
  | DTClip -> "Clips"
  | DTNote -> "Notes"
  | DTAutomation -> "Automations"
  | DTSend -> "Sends"
  | DTParam -> "Parameters"
  | DTLocator -> "Locators"
  | _ -> domain_type_to_string dt

let is_reportable (dt : domain_type) : bool = List.mem dt reportable_types

let empty_stats () : (domain_type * change_breakdown) list =
  List.map (fun dt -> (dt, { added = 0; removed = 0; modified = 0 })) reportable_types

let increment_stats stats dt ct =
  List.map
    (fun (d, b) -> if d = dt then (d, increment_breakdown b ct) else (d, b))
    stats

let rec collect_view stats (view : view) =
  match view with
  | Field _ -> stats
  | Item item ->
    let stats =
      if is_reportable item.domain_type && item.change <> Unchanged then
        increment_stats stats item.domain_type item.change
      else stats
    in
    List.fold_left collect_view stats item.children
  | Collection col -> List.fold_left collect_view stats col.items

let collect (views : view list) : (domain_type * change_breakdown) list =
  List.fold_left collect_view (empty_stats ()) views

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

let render (views : view list) : string =
  let stats = collect views in
  let lines =
    List.filter_map (fun (dt, b) -> render_line dt b) stats
  in
  match lines with
  | [] -> "No changes."
  | _ -> String.concat "\n" lines
