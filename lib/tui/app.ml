let init ~(views : Alsdiff_output.View_model.view list) : unit -> Model.t * Msg.t Mosaic.Cmd.t =
  fun () -> (Model.init ~detail_config:Alsdiff_output.Config.compact views, Mosaic.Cmd.none)

let update (msg : Msg.t) (model : Model.t) : Model.t * Msg.t Mosaic.Cmd.t =
  let model, cmd = Update.update model msg in
  model, cmd

let view (model : Model.t) : Msg.t Mosaic.t =
  View.view model

let subscriptions (model : Model.t) : Msg.t Mosaic.Sub.t =
  let key_sub = Mosaic.Sub.on_key (fun key_event ->
      Keymap.handle_key ~search_mode:model.search_mode key_event
    )
  in
  let resize_sub = Mosaic.Sub.on_resize (fun ~width ~height ->
      Msg.Resize (width, height))
  in
  Mosaic.Sub.batch [key_sub; resize_sub]

let run ~(views : Alsdiff_output.View_model.view list) : unit =
  let app = {
    Mosaic.init = init ~views;
    update;
    view;
    subscriptions;
  } in
  Mosaic.run app
