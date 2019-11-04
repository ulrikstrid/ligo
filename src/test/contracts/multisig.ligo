// storage type
type threshold_t is nat
type authorized_keys_t is list(key)
type storage_t is record
  // counter : nat ;
  threshold : threshold_t ;
  auth : authorized_keys_t ;
end

// entry points parameter types
type change_threshold_pt is record
  threshold : threshold_t ;
end
type new_key_pt is record
  new_key : key ;
end
type check_message_pt is record
  message : (unit -> list(operation)) ;
  signatures : list(signature) ;
end

type contract_return_t is (list(operation) * storage_t)

type entry_point_t is
| ChangeThreshold of change_threshold_pt
| NewKey of new_key_pt
| CheckMessage of check_message_pt

function new_key (const param : new_key_pt;
                  const s : storage_t) : contract_return_t is 
  var exists : bool := False ;
  var new_key_bytes : bytes := bytes_pack(param.new_key);

  block {

  for k : key in list s.auth
  begin
    var key_bytes : bytes := bytes_pack(k);
    if (key_bytes = new_key_bytes) then
      exists := True;
    else
      skip;
  end ;

  if exists then
    failwith ("This key is already authorized")
  else
    s.auth := cons(param.new_key , s.auth) ;

  } with ((nil: list(operation)),s)

function change_threshold (const param : change_threshold_pt;
                           const s : storage_t) : contract_return_t is
  begin
  s.threshold := param.threshold
end with ((nil: list(operation)),s)

function check_message (const param : check_message_pt;
                        const s : storage_t) : contract_return_t is
  var valid : nat := 0n ;
  var message : (unit -> list(operation)) := param.message ;
  var ops : list(operation) := message(unit) ;
  var packed_msg : bytes := bytes_pack(ops) ;
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
        failwith ("Requires more signatures")

end with (ret, s)

function main(const param : entry_point_t; const s : storage_t) : contract_return_t is 
block {skip} with 
  case param of
  | NewKey (p) -> new_key(p,s)
  | ChangeThreshold (p) -> change_threshold(p,s)
  | CheckMessage (p) -> check_message(p,s)
end