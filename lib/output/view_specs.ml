module B : Alsdiff_view_spec_types.View_spec_types.S
  with type domain_type = Change_projector.Builder.domain_type
   and type change_type = Change_projector.Builder.change_type
   and type field_value = Change_projector.Builder.field_value
   and type view = Change_projector.Builder.view
   and type item = Change_projector.Builder.item
   and type collection = Change_projector.Builder.collection
   and type dual_time_formatter = Change_projector.Builder.dual_time_formatter
= struct
  include Change_projector.Builder
  type ('v, 'p) unified_field_spec = ('v, 'p) Change_projector.unified_field_spec
  type ('v, 'p) section_spec = ('v, 'p) Change_projector.section_spec

  let make_spec = Change_projector.make_spec
  let make_spec_const = Change_projector.make_spec_const
  let make_int = Change_projector.make_int
  let make_float = Change_projector.make_float
  let make_string = Change_projector.make_string
  let make_bool = Change_projector.make_bool
  let make_time_field = Change_projector.make_time_field
  let build_value_field_views = Change_projector.build_value_field_views
  let build_patch_field_views = Change_projector.build_patch_field_views
  let map_specs = Change_projector.map_specs
  let build_item_from_specs = Change_projector.build_item_from_specs

  module Spec = struct
    let inline_fields = Change_projector.Spec.inline_fields
    let child = Change_projector.Spec.child
    let child_optional = Change_projector.Spec.child_optional
    let collection = Change_projector.Spec.collection
  end
end

(* TimeSignature *)
module TimeSignatureVS = Alsdiff_live.Clip.TimeSignature.ViewSpec(B)

(* Loop *)
module LoopVS = Alsdiff_live.Clip.Loop.ViewSpec(B)

(* CurveControls *)
module CurveControlsVS = Alsdiff_live.Automation.CurveControls.ViewSpec(B)

(* MidiNote *)
module MidiNoteVS = Alsdiff_live.Clip.MidiNote.ViewSpec(B)

(* SampleRef *)
module SampleRefVS = Alsdiff_live.Clip.SampleRef.ViewSpec(B)

(* Fade *)
module FadeVS = Alsdiff_live.Clip.Fade.ViewSpec(B)

(* Version *)
module VersionVS = Alsdiff_live.Liveset.Version.ViewSpec(B)

(* Locator *)
module LocatorVS = Alsdiff_live.Liveset.Locator.ViewSpec(B)

(* PresetRef *)
module PresetRefVS = Alsdiff_live.Device.PresetRef.ViewSpec(B)

(* PatchRef *)
module PatchRefVS = Alsdiff_live.Device.PatchRef.ViewSpec(B)

(* Send *)
module SendVS = Alsdiff_live.Track.Send.ViewSpec(B)

(* GenericParam *)
module GenericParamVS = Alsdiff_live.Device.GenericParam.ViewSpec(B)

(* DeviceParam *)
module DeviceParamVS = Alsdiff_live.Device.DeviceParam.ViewSpec(B)

(* Max4LiveParam *)
module Max4LiveParamVS = Alsdiff_live.Device.Max4LiveParam.ViewSpec(B)

(* Macro *)
module MacroVS = Alsdiff_live.Device.Macro.ViewSpec(B)

(* MixerDevice *)
module MixerDeviceVS = Alsdiff_live.Device.MixerDevice.ViewSpec(B)

(* Snapshot *)
module SnapshotVS = Alsdiff_live.Device.Snapshot.ViewSpec(B)

(* PluginParam *)
module PluginParamVS = Alsdiff_live.Device.PluginParam.ViewSpec(B)

(* PluginDesc *)
module PluginDescVS = Alsdiff_live.Device.PluginDesc.ViewSpec(B)

(* Routing *)
module RoutingVS = Alsdiff_live.Track.Routing.ViewSpec(B)

(* RoutingSet *)
module RoutingSetVS = Alsdiff_live.Track.RoutingSet.ViewSpec(B)

(* Mixer *)
module MixerVS = Alsdiff_live.Track.Mixer.ViewSpec(B)

(* MainMixer *)
module MainMixerVS = Alsdiff_live.Track.MainMixer.ViewSpec(B)

(* EnvelopeEvent *)
module EnvelopeEventVS = Alsdiff_live.Automation.EnvelopeEvent.ViewSpec(B)

(* Automation *)
module AutomationVS = Alsdiff_live.Automation.ViewSpec(B)
