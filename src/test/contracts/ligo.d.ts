/* For testing purposes only */
type Type<p> = {kind: p}

declare type int = number
declare type nat = number
declare type tez = number
declare type mutez = number
declare type bool = boolean
declare type unit = void
declare const unit: unit;
declare type operation = Type<"operation">

declare type list<a> = Array<a>;
declare type map<a, b> = {
  kind: 'map',
  key: a,
  value: b
}
declare type key_hash = Type<"key_hash">
declare type address = Type<"address">
declare type contract<parameter> = { parameter: parameter }
declare type Some<a> = {
    kind: "Some",
    value: a
}
declare type None = {
    kind: "None"
}
declare type timestamp = Type<"timestamp">
declare type chain_id = Type<"chain_id">
declare type option<a> = 
| Some<a>
| None
declare function ediv (a:number, b:number): option<[number, number]>;
declare function assert_some<t>(a: option<t>):void;
declare function assert(a: bool):void;


declare type big_map<a, b> = {
  kind: 'big_map',
  key: a,
  value: b
}

declare namespace Big_map {
  export const empty: big_map<any, any>;
  export function literal<key, value>(l:list<[key, value]>): big_map<key, value>;
  export function find_opt<key, value>(k:key, b: big_map<key, value>): option<value>;
  export function mem<key, value>(k: key, b: big_map<key, value>): bool;
  export function update<key, value>(k: key, o: option<value>, b: big_map<key, value>): big_map<key, value>;
  export function add<key, value>(k: key, v: value, b: big_map<key, value>): big_map<key, value>;
  export function remove<key, value>(k: key, b: big_map<key, value>): big_map<key, value>;
}

declare namespace Bitwise {
  export function and(a: nat | int, b: nat): nat;
  export function or(a: nat, b: nat): nat;
  export function xor(a: nat, b: nat): nat;
  export function shift_left(a: nat, b: nat): nat;
  export function shift_right(a: nat, b: nat): nat;
}

declare type bytes = Type<"bytes">;

declare namespace Bytes {
  export function concat(a: bytes, b: bytes): bytes;
  export function sub(start: nat, length: nat, bytes: bytes): bytes;
  export function pack(a: any): bytes;
  export function unpack(bytes: bytes): option<any>;
  export function length(bytes: bytes): nat;
}

declare type key = Type<"key">;
declare type signature = Type<"signature">;

declare namespace Crypto {
  export function blake2b(b: bytes): bytes;
  export function sha256(b: bytes): bytes;
  export function sha512(b: bytes): bytes;
  export function hash_key(k: key): key_hash;
  export function check(k: key, s: signature, b: bytes): bool;
}

declare namespace List {
  export function length<i>(l: list<i>): nat;
  export function size<i>(l: list<i>): nat;
  export function head_opt<a>(l: list<a>):option<a>;
  export function tail_opt<a>(l: list<a>):option<list<a>>;
  export function iter<I>(f: (item:I) => unit, l: list<I>): unit;
  export function map<A, B>(mapper: (item: A) => B, l: list<A>): list<B>;
  export function fold<A, I>(a:(a: [acc: A, b: I]) => A, l: list<I>, start:A): A;
}

declare namespace Map {
  export const empty: map<any, any>;
  export function iter<K,V>(i: (k: K, v:V) => unit, m: map<K, V>): unit;
  export function map<K,V, K2, V2>(i: (k: K, v: V) => V2, m: map<K, V>): map<K2, V2>;
  export function fold<K, V, A>(a:(acc: A, kv: [K, V]) => A, m:map<K, V>, acc: A): A;
  export function size<K,V>(m: map<K,V>): nat;
  export function literal<key, value>(l:list<[key, value]>): map<key, value>;
  export function find_opt<key, value>(k:key, b: map<key, value>): option<value>;
  export function mem<key, value>(k: key, b: map<key, value>): bool;
  export function update<key, value>(k: key, o: option<value>, b: map<key, value>): map<key, value>;
  export function add<key, value>(k: key, v: value, b: map<key, value>): map<key, value>;
  export function remove<key, value>(k: key, b: map<key, value>): map<key, value>;

  
}

declare type set<A> = Type<"set">

declare namespace Set {
  export const empty: set<any>;
  export function literal<V>(l: list<V>):set<V>;
  export function mem<V>(v: V, s: set<V>): bool;
  export function cardinal<V>(s: set<V>): nat;
  export function add<V>(v: V, s: set<V>):set<V>;
  export function remove<V>(v: V, s: set<V>): set<V>;
  export function iter<V>(i:((v:V) => unit), s: set<V>): unit;
  export function fold<A, V>(a: ((a: A, v: V) => A), s: set<V>, acc: A): A;
}

declare namespace String {
  export function length(s: string): nat;
  export function sub(offset: nat, length: nat, s: string): string;
  export function concat(a: string, b: string): string;
}


declare namespace Tezos {
    export const balance: tez;
    export const now: timestamp;
    export const amount : number;
    export const sender : address;
    export function address<parameter>(contract: contract<parameter>): address;
    export const self_address: address;
    export function self<A>(annot: string): option<A>;
    export function implicit_account<parameter>(k: key_hash): contract<parameter>;
    export const source: address;
    export function failwith(a: any): any;
    export const chain_id: chain_id;
    export function transaction<P>(parameter:P, mutez: mutez, contract: contract<P>): operation;
    export function set_delegate(delegate: option<key_hash>): operation;
    export function get_contract_opt<P>(address: address): option<contract<P>>;
    export function get_entrypoint_opt<P>(entrypoint: string, address: address): option<contract<P>>;
}

declare function match<T extends {[k in keyof T]:(value: Parameters<T[k]>[0]) => R}, R>(matchee: {kind: keyof T, value?: Parameters<T[keyof T]>[0]}, cases: T): R;

declare function Some<V>(v: V): {
  kind: 'Some',
  value: V
}

declare function is_nat(i: int): option<nat>
declare function failwith(a: any): any;
declare function abs(i:int):nat;