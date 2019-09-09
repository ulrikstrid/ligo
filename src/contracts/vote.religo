type storage = {
  title: string,
  candidates: map(string, int),
  voters: set(address),
  beginning_time: timestamp,
  finish_time: timestamp,
};

type init_action = {
  title: string,
  beginning_time: timestamp,
  finish_time: timestamp,
};

type action =
  | Vote(string)
  | Init(init_action);

let init = (init_params: init_action, _: storage) => {
  let candidates = Map([("Yes", 0), ("No", 0)]);
  (
    []: list(operation),
    {
      title: init_params.title,
      candidates,
      voters: (Set([]): set(address)),
      beginning_time: init_params.beginning_time,
      finish_time: init_params.finish_time,
    },
  );
};

let vote = (parameter: string, storage: storage) => {
  let now = Current.time;
  /* let _ = assert (now >= storage.beginning_time && storage.finish_time > now) in */
  let addr = Current.source;
  /* let _ = assert (not Set.mem addr storage.voters) in */
  let x = Map.find(parameter, storage.candidates);
  (
    []: list(operation),
    {
      title: storage.title,
      candidates: Map.update(parameter, Some(x + 1), storage.candidates),
      voters: Set.add(addr, storage.voters),
      beginning_time: storage.beginning_time,
      finish_time: storage.finish_time,
    },
  );
};

let x = [b, ...z];

let z = switch (z) {
  | Z => o
};

let%entry main = (action: action, storage: storage) =>
  switch (action) {
  | Vote(p) => vote(p, storage)
  | Init(ps) => init(ps, storage)
  };
