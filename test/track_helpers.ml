open Alsdiff_base.Xml
open Alsdiff_live

let dummy_xml = read_string "<dummy/>"

let make_generic_param name value =
  {
    Device.GenericParam.name = name;
    value = value;
    automation = 0;
    modulation = 0;
    mapping = None;
    xml = dummy_xml;
  }


let make_mixer volume pan =
  {
    Track.Mixer.volume = make_generic_param "Volume" (Device.Float volume);
    pan = make_generic_param "Pan" (Device.Float pan);
    mute = make_generic_param "On" (Device.Bool false);
    solo = make_generic_param "SoloSink" (Device.Bool false);
    sends = [];
    xml = dummy_xml;
  }


let make_empty_routing_set () =
  let make_routing route_type =
    {
      Track.Routing.route_type;
      target = "";
      upper_string = "";
      lower_string = "";
      xml = dummy_xml;
    }
  in
  {
    Track.RoutingSet.audio_in = make_routing Track.Routing.AudioIn;
    audio_out = make_routing Track.Routing.AudioOut;
    midi_in = make_routing Track.Routing.MidiIn;
    midi_out = make_routing Track.Routing.MidiOut;
    xml = dummy_xml;
  }


let make_main_mixer () =
  let base = make_mixer 1.0 0.0 in
  {
    Track.MainMixer.base;
    tempo = make_generic_param "Tempo" (Device.Float 120.0);
    time_signature = make_generic_param "TimeSignature" (Device.Int 4);
    crossfade = make_generic_param "CrossFade" (Device.Float 1.0);
    global_groove = make_generic_param "GlobalGroove" (Device.Float 0.0);
    xml = dummy_xml;
  }
