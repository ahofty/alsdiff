let init ~(views : Alsdiff_output.View_model.view list)
    ?(detail_config = Alsdiff_output.Config.full) () : Model.t * Msg.t Mosaic.Cmd.t =
  (Model.init ~detail_config views, Mosaic.Cmd.none)

let init_browser ~root ?(note_name_style = Alsdiff_output.View_model.Sharp) ?(format_time = None) () : Model.t * Msg.t Mosaic.Cmd.t =
  (Model.init_browser ~root ~note_name_style ?format_time (), Mosaic.Cmd.none)

let update (msg : Msg.t) (model : Model.t) : Model.t * Msg.t Mosaic.Cmd.t =
  let model, cmd = Update.update model msg in
  model, cmd

let view (model : Model.t) : Msg.t Mosaic.t =
  View.view model

let subscriptions (model : Model.t) : Msg.t Mosaic.Sub.t =
  let key_sub = Mosaic.Sub.on_key (fun key_event ->
      Keymap.handle_key
        ~mode:model.mode
        ~search_mode:model.search_mode
        ~export_selector_active:model.export_selector_active
        key_event
    )
  in
  let resize_sub = Mosaic.Sub.on_resize (fun ~width ~height ->
      Msg.Resize (width, height))
  in
  Mosaic.Sub.batch [key_sub; resize_sub]

let run ~(views : Alsdiff_output.View_model.view list)
    ?(detail_config = Alsdiff_output.Config.full) () : unit =
  Update.export_output_ref := None;
  let app = {
    Mosaic.init = init ~views ~detail_config;
    update;
    view;
    subscriptions;
  } in
  Mosaic.run app

let run_browser ~root ?(note_name_style = Alsdiff_output.View_model.Sharp) ?(format_time = None) () : unit =
  Update.export_output_ref := None;
  let app = {
    Mosaic.init = init_browser ~root ~note_name_style ?format_time;
    update;
    view;
    subscriptions;
  } in
  Mosaic.run app
