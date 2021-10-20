open Blockchain

type message = 
  | QueryAll
  | ResponseBlockchain of string
  [@@deriving yojson]

type peer = {
  socket: Dream.websocket;
  time: float
}

type store = {
  mutable peers: peer list;
  mutable chain: block
} 

let chain_response m =
  let d = m 
  |> yojson_of_block 
  |> Yojson.Safe.to_string
  in ResponseBlockchain d

let write peer message =
  message 
  |> yojson_of_message
  |> Yojson.Safe.to_string
  |> Dream.send peer.socket

let broadcast peers message =
  List.iter (fun peer -> write peer message |> ignore) peers

let blockchain_response store received = 
  if index_of store.chain > index_of received then
    if hash_of store.chain = hash_of received then 
      begin
        received |> chain_response |> broadcast store.peers;
        received
      end
    else 
      replace_chain received store.chain
  else 
    store.chain

let init_message_handle store peer =
  match%lwt Dream.receive peer.socket with
  | Some message -> 
      begin
        match message |> Yojson.Safe.from_string |> message_of_yojson with
        | QueryAll -> 
          store.chain 
          |> chain_response 
          |> write peer
        | ResponseBlockchain chain -> 
          store.chain <- chain 
          |> Yojson.Safe.from_string 
          |> block_of_yojson 
          |> blockchain_response store; 
          Lwt.return_unit
      end
  | None -> 
      store.peers <- List.filter ( fun { socket: _; time } -> 
        ignore socket;
        time <> peer.time   
      ) store.peers;
      Dream.close_websocket peer.socket

let init_p2p_server store peer = 
  store.peers <- peer :: store.peers;
  store.chain |> chain_response |> write peer |> ignore;
  init_message_handle store peer

let init_ws_server store  =
  Dream.websocket ( 
    fun wbs -> init_p2p_server store { socket = wbs; time = Unix.time () } 
  )

let init_server store port =
  Dream.run ~port
  @@ Dream.logger
  @@ Dream.router [
    Dream.get "/blocks" (
      fun _ -> 
      store.chain
      |> yojson_of_block
      |> Yojson.Safe.to_string
      |> Dream.json
    );

    Dream.post "/mineBlock" (
      fun req -> 
      let%lwt body = Dream.body req in
      store.chain <- add_next_block body store.chain;
      store.chain
      |> yojson_of_block
      |> Yojson.Safe.to_string
      |> Dream.json
    );

    Dream.get "/peers" (
      fun _ -> 
      store.chain
      |> yojson_of_block
      |> Yojson.Safe.to_string
      |> Dream.json   
    );

    Dream.get "/addPeer" (
      fun _ -> init_ws_server store
    )
  ]
  @@ Dream.not_found