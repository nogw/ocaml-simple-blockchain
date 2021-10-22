type uuid' = string

type message = QueryAll | ResponseBlockchain of string

type uuids = uuid' list

type peer = {id: uuid'; socket: Dream.websocket; time: float}

type store = {mutable peers: peer list; mutable chain: Chain.Blockchain.block}

val message_of_yojson : Ppx_yojson_conv_lib.Yojson.Safe.t -> message

val yojson_of_message : message -> Ppx_yojson_conv_lib.Yojson.Safe.t

val uuid'_of_yojson : Ppx_yojson_conv_lib.Yojson.Safe.t -> uuid'

val yojson_of_uuid' : uuid' -> Ppx_yojson_conv_lib.Yojson.Safe.t

val uuids_of_yojson : Ppx_yojson_conv_lib.Yojson.Safe.t -> uuids

val yojson_of_uuids : uuids -> Ppx_yojson_conv_lib.Yojson.Safe.t

val chain_response : Chain.Blockchain.block -> message

val write : peer -> message -> unit Dream.promise

val broadcast : peer list -> message -> unit

val blockchain_response : store -> Chain.Blockchain.block -> Chain.Blockchain.block

val init_message_handle : store -> peer -> unit Lwt.t

val init_p2p_server : store -> peer -> unit Lwt.t

val init_ws_server : store -> Dream.response Dream.promise

val init_server : store -> int -> unit