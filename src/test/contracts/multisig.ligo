// storage type
type threshold_t is nat
type authorized_keys_t is list(key)
type storage_t is record
  // counter : nat ;
  threshold : threshold_t ;
  auth : authorized_keys_t ;
end

// entry points parameter types
type check_message_pt is record
  message : (unit -> list(operation)) ;
  signatures : list(signature) ;
end

type contract_return_t is (list(operation) * storage_t)

type entry_point_t is
| CheckMessage of check_message_pt

function check_message (const param : check_message_pt;
                        const s : storage_t) : contract_return_t is
  var valid : nat := 0n ;
  var message : (unit -> list(operation)) := param.message ;
  var ops : list(operation) := message(unit) ;
  var packed_msg : bytes := bytes_pack(message) ;
  var ret : list(operation) := (nil : list(operation));
  //TODO: replace with for_collect_loops
    function sig_it (const valid : nat; const cur_sig : signature) : nat is
        function auth_it (const acc : nat ; const cur_k : key) : nat is block {
          if ( crypto_check(cur_k,cur_sig,packed_msg) ) then
            acc := acc + 1n ;
          else
            skip;
        } with acc;
     block { skip } with (valid + list_fold(s.auth,0n,auth_it))

  begin
    if ( size(param.signatures) < s.threshold ) then
      failwith ("Requires more signatures")
    else
      valid := list_fold(param.signatures,0n,sig_it);
      if valid >= s.threshold then
        ret := ops ;
      else 
        failwith ("Not enough signatures passed the check")

end with (ret, s)

function main(const param : entry_point_t; const s : storage_t) : contract_return_t is 
block {skip} with 
  case param of
  | CheckMessage (p) -> check_message(p,s)
end