type timestamp = [%import: Unix.tm] [@@deriving yojson]

type block = 
  | GenesisBlock of 
  {
    index: int;
    data: string;
    timestamp: timestamp;
    hash: string;  
  }
  | Block of 
  { 
    index: int;
    data: string;
    timestamp: timestamp;
    previous_hash: block;
    hash: string;
  }
  [@@deriving yojson]

let block index data timestamp previous_hash hash = 
  Block 
  {
    index = index;
    data = data;
    timestamp = timestamp;
    previous_hash = previous_hash;
    hash = hash;
  }

let index_of index =
  match index with
  | GenesisBlock b -> b.index
  | Block b -> b.index

let data_of data =
  match data with
  | GenesisBlock b -> b.data
  | Block b -> b.data

let timestamp_of timestamp =
  match timestamp with
  | GenesisBlock b -> b.timestamp
  | Block b -> b.timestamp

let hash_of hash =
  match hash with
  | GenesisBlock b -> b.hash
  | Block b -> b.hash

let previous_hash_of previous_hash =
  match previous_hash with
  | GenesisBlock _ -> failwith "Error: tried to obtain previous hash of genesis block"
  | Block b -> hash_of b.previous_hash


(* create an new hash *)
let create_hash index data timestamp previous_hash = 
  [ 
    string_of_int index;
    previous_hash; 
    Unix.mktime timestamp |> fst |> string_of_float; 
    data;
  ]
  |> String.concat ""
  |> Sha256.string
  |> Sha256.to_hex

(* createBlock 1 "e6c1afb4w" "Fri, 15 Oct 2021 16:52:18 GMT" "e6c1afb4" "e6c1afb4";; *)
