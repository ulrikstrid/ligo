(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019-2020 Nomadic Labs, <contact@nomadic-labs.com>          *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

include Internal_event.Legacy_logging.Make (struct
  let name = "p2p"
end)

type config = {
  listening_port : P2p_addr.port option;
  listening_addr : P2p_addr.t option;
  discovery_port : P2p_addr.port option;
  discovery_addr : Ipaddr.V4.t option;
  trusted_points : (P2p_point.Id.t * P2p_peer.Id.t option) list;
  peers_file : string;
  private_mode : bool;
  identity : P2p_identity.t;
  proof_of_work_target : Crypto_box.pow_target;
  trust_discovered_peers : bool;
  reconnection_config : P2p_point_state.Info.reconnection_config;
}

type limits = {
  connection_timeout : Time.System.Span.t;
  authentication_timeout : Time.System.Span.t;
  greylist_timeout : Time.System.Span.t;
  maintenance_idle_time : Time.System.Span.t;
  min_connections : int;
  expected_connections : int;
  max_connections : int;
  backlog : int;
  max_incoming_connections : int;
  max_download_speed : int option;
  max_upload_speed : int option;
  read_buffer_size : int;
  read_queue_size : int option;
  write_queue_size : int option;
  incoming_app_message_queue_size : int option;
  incoming_message_queue_size : int option;
  outgoing_message_queue_size : int option;
  max_known_peer_ids : (int * int) option;
  max_known_points : (int * int) option;
  peer_greylist_size : int;
  ip_greylist_size_in_kilobytes : int;
  ip_greylist_cleanup_delay : Time.System.Span.t;
  swap_linger : Time.System.Span.t;
  binary_chunks_size : int option;
}

let create_scheduler limits =
  let max_upload_speed = Option.map (( * ) 1024) limits.max_upload_speed in
  let max_download_speed = Option.map (( * ) 1024) limits.max_upload_speed in
  P2p_io_scheduler.create
    ~read_buffer_size:limits.read_buffer_size
    ?max_upload_speed
    ?max_download_speed
    ?read_queue_size:limits.read_queue_size
    ?write_queue_size:limits.write_queue_size
    ()

let create_connection_pool config limits meta_cfg log triggers =
  let pool_cfg =
    {
      P2p_pool.identity = config.identity;
      trusted_points = config.trusted_points;
      peers_file = config.peers_file;
      private_mode = config.private_mode;
      max_known_points = limits.max_known_points;
      max_known_peer_ids = limits.max_known_peer_ids;
      peer_greylist_size = limits.peer_greylist_size;
      ip_greylist_size_in_kilobytes = limits.ip_greylist_size_in_kilobytes;
      ip_greylist_cleanup_delay = limits.ip_greylist_cleanup_delay;
    }
  in
  P2p_pool.create pool_cfg meta_cfg ~log triggers

let create_connect_handler config limits pool msg_cfg conn_meta_cfg io_sched
    triggers log answerer =
  let connect_handler_cfg =
    {
      P2p_connect_handler.identity = config.identity;
      proof_of_work_target = config.proof_of_work_target;
      listening_port = config.listening_port;
      private_mode = config.private_mode;
      reconnection_config = config.reconnection_config;
      min_connections = limits.min_connections;
      max_connections = limits.max_connections;
      max_incoming_connections = limits.max_incoming_connections;
      connection_timeout = limits.connection_timeout;
      authentication_timeout = limits.authentication_timeout;
      incoming_app_message_queue_size = limits.incoming_app_message_queue_size;
      incoming_message_queue_size = limits.incoming_message_queue_size;
      outgoing_message_queue_size = limits.outgoing_message_queue_size;
      binary_chunks_size = limits.binary_chunks_size;
    }
  in
  P2p_connect_handler.create
    connect_handler_cfg
    pool
    msg_cfg
    conn_meta_cfg
    io_sched
    triggers
    ~log
    ~answerer

let may_create_discovery_worker _limits config pool =
  match
    (config.listening_port, config.discovery_port, config.discovery_addr)
  with
  | (Some listening_port, Some discovery_port, Some discovery_addr) ->
      Some
        (P2p_discovery.create
           pool
           config.identity.peer_id
           ~listening_port
           ~discovery_port
           ~discovery_addr
           ~trust_discovered_peers:config.trust_discovered_peers)
  | (_, _, _) ->
      None

let create_maintenance_worker limits pool connect_handler config triggers log =
  let maintenance_config =
    {
      P2p_maintenance.maintenance_idle_time = limits.maintenance_idle_time;
      private_mode = config.private_mode;
      min_connections = limits.min_connections;
      max_connections = limits.max_connections;
      expected_connections = limits.expected_connections;
    }
  in
  let discovery = may_create_discovery_worker limits config pool in
  P2p_maintenance.create
    ?discovery
    maintenance_config
    pool
    connect_handler
    triggers
    ~log

let may_create_welcome_worker config limits connect_handler =
  match config.listening_port with
  | None ->
      Lwt.return_none
  | Some port ->
      P2p_welcome.create
        ~backlog:limits.backlog
        connect_handler
        ?addr:config.listening_addr
        port
      >>= fun w -> Lwt.return_some w

type ('msg, 'peer_meta, 'conn_meta) connection =
  ('msg, 'peer_meta, 'conn_meta) P2p_conn.t

module Real = struct
  type ('msg, 'peer_meta, 'conn_meta) net = {
    config : config;
    limits : limits;
    io_sched : P2p_io_scheduler.t;
    pool : ('msg, 'peer_meta, 'conn_meta) P2p_pool.t;
    connect_handler : ('msg, 'peer_meta, 'conn_meta) P2p_connect_handler.t;
    maintenance : ('msg, 'peer_meta, 'conn_meta) P2p_maintenance.t;
    welcome : P2p_welcome.t option;
    watcher : P2p_connection.P2p_event.t Lwt_watcher.input;
    triggers : P2p_trigger.t;
  }

  let create ~config ~limits meta_cfg msg_cfg conn_meta_cfg =
    let io_sched = create_scheduler limits in
    let watcher = Lwt_watcher.create_input () in
    let log event = Lwt_watcher.notify watcher event in
    let triggers = P2p_trigger.create () in
    create_connection_pool config limits meta_cfg log triggers
    >>= fun pool ->
    (* There is a mutual recursion between an answerer and connect_handler,
       for the default answerer. Because of the swap request mechanism, the
       default answerer needs to initiate new connections using the
       [P2p_connect_handler.connect] callback. *)
    let rec answerer =
      lazy
        ( if config.private_mode then P2p_protocol.create_private ()
        else
          let connect =
            P2p_connect_handler.connect (Lazy.force connect_handler)
          in
          let proto_conf =
            {
              P2p_protocol.swap_linger = limits.swap_linger;
              pool;
              log;
              connect;
              latest_accepted_swap = Ptime.epoch;
              latest_successful_swap = Ptime.epoch;
            }
          in
          P2p_protocol.create_default proto_conf )
    and connect_handler =
      lazy
        (create_connect_handler
           config
           limits
           pool
           msg_cfg
           conn_meta_cfg
           io_sched
           triggers
           log
           answerer)
    in
    let connect_handler = Lazy.force connect_handler in
    let maintenance =
      create_maintenance_worker limits pool connect_handler config triggers log
    in
    may_create_welcome_worker config limits connect_handler
    >>= fun welcome ->
    return
      {
        config;
        limits;
        io_sched;
        pool;
        connect_handler;
        maintenance;
        welcome;
        watcher;
        triggers;
      }

  let peer_id {config; _} = config.identity.peer_id

  let maintain {maintenance; _} () = P2p_maintenance.maintain maintenance

  let activate t () =
    log_info "activate" ;
    (match t.welcome with None -> () | Some w -> P2p_welcome.activate w) ;
    P2p_maintenance.activate t.maintenance ;
    ()

  let roll _net () = Lwt.return_unit (* TODO implement *)

  (* returns when all workers have shut down in the opposite
     creation order. *)
  let shutdown net () =
    lwt_log_notice "Shutting down the p2p's welcome worker..."
    >>= fun () ->
    Option.iter_s P2p_welcome.shutdown net.welcome
    >>= fun () ->
    lwt_log_notice "Shutting down the p2p's network maintenance worker..."
    >>= fun () ->
    P2p_maintenance.shutdown net.maintenance
    >>= fun () ->
    lwt_log_notice "Shutting down the p2p connection pool..."
    >>= fun () ->
    P2p_pool.destroy net.pool
    >>= fun () ->
    lwt_log_notice "Shutting down the p2p connection handler..."
    >>= fun () ->
    P2p_connect_handler.destroy net.connect_handler
    >>= fun () ->
    lwt_log_notice "Shutting down the p2p scheduler..."
    >>= fun () -> P2p_io_scheduler.shutdown ~timeout:3.0 net.io_sched

  let connections {pool; _} () =
    P2p_pool.Connection.fold pool ~init:[] ~f:(fun _peer_id c acc -> c :: acc)

  let find_connection {pool; _} peer_id =
    P2p_pool.Connection.find_by_peer_id pool peer_id

  let disconnect ?wait conn = P2p_conn.disconnect ?wait conn

  let connection_info _net conn = P2p_conn.info conn

  let connection_local_metadata _net conn = P2p_conn.local_metadata conn

  let connection_remote_metadata _net conn = P2p_conn.remote_metadata conn

  let connection_stat _net conn = P2p_conn.stat conn

  let global_stat {connect_handler; _} () =
    P2p_connect_handler.stat connect_handler

  let set_peer_metadata {pool; _} conn meta =
    P2p_pool.Peers.set_peer_metadata pool conn meta

  let get_peer_metadata {pool; _} conn =
    P2p_pool.Peers.get_peer_metadata pool conn

  let recv _net conn =
    P2p_conn.read conn
    >>=? fun msg ->
    lwt_debug
      "message read from %a"
      P2p_peer.Id.pp
      (P2p_conn.info conn).peer_id
    >>= fun () -> return msg

  let rec recv_any net () =
    let pipes =
      P2p_pool.Connection.fold net.pool ~init:[] ~f:(fun _peer_id conn acc ->
          ( P2p_conn.is_readable conn
          >>= function
          | Ok () ->
              Lwt.return_some conn
          | Error _ ->
              Lwt_utils.never_ending () )
          :: acc)
    in
    Lwt.pick
      ( ( P2p_trigger.wait_new_connection net.triggers
        >>= fun () -> Lwt.return_none )
      :: pipes )
    >>= function
    | None ->
        recv_any net ()
    | Some conn -> (
        P2p_conn.read conn
        >>= function
        | Ok msg ->
            lwt_debug
              "message read from %a"
              P2p_peer.Id.pp
              (P2p_conn.info conn).peer_id
            >>= fun () -> Lwt.return (conn, msg)
        | Error _ ->
            lwt_debug
              "error reading message from %a"
              P2p_peer.Id.pp
              (P2p_conn.info conn).peer_id
            >>= fun () -> Lwt_unix.yield () >>= fun () -> recv_any net () )

  let send _net conn m =
    P2p_conn.write conn m
    >>= function
    | Ok () ->
        lwt_debug
          "message sent to %a"
          P2p_peer.Id.pp
          (P2p_conn.info conn).peer_id
        >>= fun () -> return_unit
    | Error err ->
        lwt_debug
          "error sending message from %a: %a"
          P2p_peer.Id.pp
          (P2p_conn.info conn).peer_id
          pp_print_error
          err
        >>= fun () -> Lwt.return_error err

  let try_send _net conn v =
    match P2p_conn.write_now conn v with
    | Ok v ->
        debug
          "message trysent to %a"
          P2p_peer.Id.pp
          (P2p_conn.info conn).peer_id ;
        v
    | Error err ->
        debug
          "error trysending message to %a@ %a"
          P2p_peer.Id.pp
          (P2p_conn.info conn).peer_id
          pp_print_error
          err ;
        false

  let broadcast {pool; _} msg =
    P2p_peer.Table.iter
      (fun _peer_id peer_info ->
        match P2p_peer_state.get peer_info with
        | Running {data = conn; _} ->
            (* Silently discards Error P2p_errors.Connection_closed in case
                the pipe is closed. Shouldn't happen because
                - no race conditions (no Lwt)
                - the peer state is Running. *)
            ignore (P2p_conn.write_now conn msg : bool tzresult)
        | _ ->
            ())
      (P2p_pool.connected_peer_ids pool) ;
    debug "message broadcasted"

  let fold_connections {pool; _} ~init ~f =
    P2p_pool.Connection.fold pool ~init ~f

  let iter_connections {pool; _} f =
    P2p_pool.Connection.fold pool ~init:() ~f:(fun gid conn () -> f gid conn)

  let on_new_connection {connect_handler; _} f =
    P2p_connect_handler.on_new_connection connect_handler f
end

module Fake = struct
  let id = P2p_identity.generate_with_pow_target_0 ()

  let empty_stat =
    {
      P2p_stat.total_sent = 0L;
      total_recv = 0L;
      current_inflow = 0;
      current_outflow = 0;
    }

  let connection_info announced_version faked_metadata =
    {
      P2p_connection.Info.incoming = false;
      peer_id = id.peer_id;
      id_point = (Ipaddr.V6.unspecified, None);
      remote_socket_port = 0;
      announced_version;
      local_metadata = faked_metadata;
      remote_metadata = faked_metadata;
      private_node = false;
    }
end

type ('msg, 'peer_meta, 'conn_meta) t = {
  announced_version : Network_version.t;
  peer_id : P2p_peer.Id.t;
  maintain : unit -> unit Lwt.t;
  roll : unit -> unit Lwt.t;
  shutdown : unit -> unit Lwt.t;
  connections : unit -> ('msg, 'peer_meta, 'conn_meta) connection list;
  find_connection :
    P2p_peer.Id.t -> ('msg, 'peer_meta, 'conn_meta) connection option;
  disconnect :
    ?wait:bool -> ('msg, 'peer_meta, 'conn_meta) connection -> unit Lwt.t;
  connection_info :
    ('msg, 'peer_meta, 'conn_meta) connection ->
    'conn_meta P2p_connection.Info.t;
  connection_local_metadata :
    ('msg, 'peer_meta, 'conn_meta) connection -> 'conn_meta;
  connection_remote_metadata :
    ('msg, 'peer_meta, 'conn_meta) connection -> 'conn_meta;
  connection_stat : ('msg, 'peer_meta, 'conn_meta) connection -> P2p_stat.t;
  global_stat : unit -> P2p_stat.t;
  get_peer_metadata : P2p_peer.Id.t -> 'peer_meta;
  set_peer_metadata : P2p_peer.Id.t -> 'peer_meta -> unit;
  recv : ('msg, 'peer_meta, 'conn_meta) connection -> 'msg tzresult Lwt.t;
  recv_any : unit -> (('msg, 'peer_meta, 'conn_meta) connection * 'msg) Lwt.t;
  send :
    ('msg, 'peer_meta, 'conn_meta) connection -> 'msg -> unit tzresult Lwt.t;
  try_send : ('msg, 'peer_meta, 'conn_meta) connection -> 'msg -> bool;
  broadcast : 'msg -> unit;
  pool : ('msg, 'peer_meta, 'conn_meta) P2p_pool.t option;
  connect_handler :
    ('msg, 'peer_meta, 'conn_meta) P2p_connect_handler.t option;
  fold_connections :
    'a. init:'a ->
    f:(P2p_peer.Id.t -> ('msg, 'peer_meta, 'conn_meta) connection -> 'a -> 'a) ->
    'a;
  iter_connections :
    (P2p_peer.Id.t -> ('msg, 'peer_meta, 'conn_meta) connection -> unit) ->
    unit;
  on_new_connection :
    (P2p_peer.Id.t -> ('msg, 'peer_meta, 'conn_meta) connection -> unit) ->
    unit;
  activate : unit -> unit;
  watcher : P2p_connection.P2p_event.t Lwt_watcher.input;
}

type ('msg, 'peer_meta, 'conn_meta) net = ('msg, 'peer_meta, 'conn_meta) t

let announced_version net = net.announced_version

let pool net = net.pool

let connect_handler net = net.connect_handler

let check_limits =
  let fail_1 v orig =
    if not (Ptime.Span.compare v Ptime.Span.zero <= 0) then return_unit
    else
      Error_monad.failwith
        "value of option %S cannot be negative or null@."
        orig
  in
  let fail_2 v orig =
    if not (v < 0) then return_unit
    else Error_monad.failwith "value of option %S cannot be negative@." orig
  in
  fun c ->
    fail_1 c.authentication_timeout "authentication-timeout"
    >>=? fun () ->
    fail_2 c.min_connections "min-connections"
    >>=? fun () ->
    fail_2 c.expected_connections "expected-connections"
    >>=? fun () ->
    fail_2 c.max_connections "max-connections"
    >>=? fun () ->
    fail_2 c.max_incoming_connections "max-incoming-connections"
    >>=? fun () ->
    fail_2 c.read_buffer_size "read-buffer-size"
    >>=? fun () ->
    fail_1 c.swap_linger "swap-linger"
    >>=? fun () ->
    ( match c.binary_chunks_size with
    | None ->
        return_unit
    | Some size ->
        P2p_socket.check_binary_chunks_size size )
    >>=? fun () -> return_unit

let create ~config ~limits peer_cfg conn_cfg msg_cfg =
  check_limits limits
  >>=? fun () ->
  Real.create ~config ~limits peer_cfg msg_cfg conn_cfg
  >>=? fun net ->
  return
    {
      announced_version =
        Network_version.announced
          ~chain_name:msg_cfg.chain_name
          ~distributed_db_versions:msg_cfg.distributed_db_versions
          ~p2p_versions:P2p_version.supported;
      peer_id = Real.peer_id net;
      maintain = Real.maintain net;
      roll = Real.roll net;
      shutdown = Real.shutdown net;
      connections = Real.connections net;
      find_connection = Real.find_connection net;
      disconnect = Real.disconnect;
      connection_info = Real.connection_info net;
      connection_local_metadata = Real.connection_local_metadata net;
      connection_remote_metadata = Real.connection_remote_metadata net;
      connection_stat = Real.connection_stat net;
      global_stat = Real.global_stat net;
      get_peer_metadata = Real.get_peer_metadata net;
      set_peer_metadata = Real.set_peer_metadata net;
      recv = Real.recv net;
      recv_any = Real.recv_any net;
      send = Real.send net;
      try_send = Real.try_send net;
      broadcast = Real.broadcast net;
      pool = Some net.pool;
      connect_handler = Some net.connect_handler;
      fold_connections = (fun ~init ~f -> Real.fold_connections net ~init ~f);
      iter_connections = Real.iter_connections net;
      on_new_connection = Real.on_new_connection net;
      activate = Real.activate net;
      watcher = net.Real.watcher;
    }

let activate t =
  log_info "activate P2P layer !" ;
  t.activate ()

let faked_network (msg_cfg : 'msg P2p_params.message_config) peer_cfg
    faked_metadata =
  let announced_version =
    Network_version.announced
      ~chain_name:msg_cfg.chain_name
      ~distributed_db_versions:msg_cfg.distributed_db_versions
      ~p2p_versions:P2p_version.supported
  in
  {
    announced_version;
    peer_id = Fake.id.peer_id;
    maintain = Lwt.return;
    roll = Lwt.return;
    shutdown = Lwt.return;
    connections = (fun () -> []);
    find_connection = (fun _ -> None);
    disconnect = (fun ?wait:_ _ -> Lwt.return_unit);
    connection_info =
      (fun _ -> Fake.connection_info announced_version faked_metadata);
    connection_local_metadata = (fun _ -> faked_metadata);
    connection_remote_metadata = (fun _ -> faked_metadata);
    connection_stat = (fun _ -> Fake.empty_stat);
    global_stat = (fun () -> Fake.empty_stat);
    get_peer_metadata = (fun _ -> peer_cfg.P2p_params.peer_meta_initial ());
    set_peer_metadata = (fun _ _ -> ());
    recv = (fun _ -> Lwt_utils.never_ending ());
    recv_any = (fun () -> Lwt_utils.never_ending ());
    send = (fun _ _ -> fail P2p_errors.Connection_closed);
    try_send = (fun _ _ -> false);
    fold_connections = (fun ~init ~f:_ -> init);
    iter_connections = (fun _f -> ());
    on_new_connection = (fun _f -> ());
    broadcast = ignore;
    pool = None;
    connect_handler = None;
    activate = (fun _ -> ());
    watcher = Lwt_watcher.create_input ();
  }

let peer_id net = net.peer_id

let maintain net = net.maintain ()

let roll net = net.roll ()

let shutdown net = net.shutdown ()

let connections net = net.connections ()

let disconnect net = net.disconnect

let find_connection net = net.find_connection

let connection_info net = net.connection_info

let connection_local_metadata net = net.connection_local_metadata

let connection_remote_metadata net = net.connection_remote_metadata

let connection_stat net = net.connection_stat

let global_stat net = net.global_stat ()

let get_peer_metadata net = net.get_peer_metadata

let set_peer_metadata net = net.set_peer_metadata

let recv net = net.recv

let recv_any net = net.recv_any ()

let send net = net.send

let try_send net = net.try_send

let broadcast net = net.broadcast

let fold_connections net = net.fold_connections

let iter_connections net = net.iter_connections

let on_new_connection net = net.on_new_connection

let greylist_addr net addr =
  Option.iter (fun pool -> P2p_pool.greylist_addr pool addr) net.pool

let greylist_peer net peer_id =
  Option.iter (fun pool -> P2p_pool.greylist_peer pool peer_id) net.pool

let watcher net = Lwt_watcher.create_stream net.watcher