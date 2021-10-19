open Blockchain

type peer = {
  wb: Dream.websocket;
  ct: float
}

type store = {
  mutable peers: peer list;
  mutable chain: block
}

let init_server store port =
  Dream.run ~port
  @@ Dream.logger
  @@ Dream.router [
    Dream.get "/" (
      fun _ -> 
      Dream.json "hello world"
    );

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
    )
  ]
  @@ Dream.not_found