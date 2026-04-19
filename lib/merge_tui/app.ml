let init ~(context : Alsdiff_merge.Merge.merge_context)
    ~(ours_file : string)
    ~(base_file : string)
    ~(theirs_file : string) () : Model.t * Msg.t Mosaic.Cmd.t =
  let flat_nodes = Merge_tree.build context in
  (Model.create ~context ~flat_nodes ~ours_file ~base_file ~theirs_file (), Mosaic.Cmd.none)

let update (msg : Msg.t) (model : Model.t) : Model.t * Msg.t Mosaic.Cmd.t =
  Update.update model msg

let view (model : Model.t) : Msg.t Mosaic.t =
  View.view model

let subscriptions (_model : Model.t) : Msg.t Mosaic.Sub.t =
  let key_sub = Mosaic.Sub.on_key (fun key_event ->
      Keymap.handle_key key_event
    )
  in
  let resize_sub = Mosaic.Sub.on_resize (fun ~width ~height ->
      Msg.Resize (width, height))
  in
  Mosaic.Sub.batch [key_sub; resize_sub]

let run ~(context : Alsdiff_merge.Merge.merge_context)
    ~(ours_file : string)
    ~(base_file : string)
    ~(theirs_file : string) () : int =
  let app = {
    Mosaic.init = init ~context ~ours_file ~base_file ~theirs_file;
    update;
    view;
    subscriptions;
  } in
  Mosaic.run app;
  !Update.exit_code_ref
