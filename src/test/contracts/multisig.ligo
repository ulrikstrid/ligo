// storage type
type counter_t is nat
type threshold_t is nat
type authorized_keys_t is list(key)

type storage_t is record
  counter : counter_t ;
  threshold : threshold_t ;
  auth : authorized_keys_t ;
end

// entry points parameter types
type check_message_pt is record
  counter : counter_t ;
  message : (unit -> list(operation)) ;
  signatures : list(signature) ;
end

type contract_return_t is (list(operation) * storage_t)

type entry_point_t is
| CheckMessage of check_message_pt

function check_message (const param : check_message_pt;
                        const s : storage_t) : contract_return_t is block {
  var message : (unit -> list(operation)) := param.message ;
  //TODO: replace with for_collect_loops
  function sig_it (const valid : nat; const cur_sig : signature) : nat is block {
    var packed_msg : bytes := bytes_pack(message) ;
    function auth_it (const acc : nat ; const cur_k : key) : nat is block {
      if crypto_check(cur_k,cur_sig,packed_msg) then acc := acc + 1n
      else skip
    } with acc ;

    if list_fold(s.auth,0n,auth_it) = 0n then failwith ("Invalid signature")
    else skip
  } with (valid + 1n) ;

  if param.counter =/= s.counter then
    failwith ("Counters does not match")
  else
    if list_fold(param.signatures,0n,sig_it) < s.threshold then
      failwith ("Not enough signatures passed the check")
    else skip ;
  s.counter := s.counter + 1n ;

} with (message(unit), s)

function main(const param : entry_point_t; const s : storage_t) : contract_return_t is 
block {skip} with 
  case param of
  | CheckMessage (p) -> check_message(p,s)
end