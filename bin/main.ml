include Chain

let () =
     Blockchain.initial_block "test"
  |> Blockchain.add_next_block "test 2"
  |> Blockchain.add_next_block "test 3"
  |> Blockchain.add_next_block "test 4"
  |> (fun initial_chain -> 
      Network.init_server { peers = []; chain = initial_chain } 4000)