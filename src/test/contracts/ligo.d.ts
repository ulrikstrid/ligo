/* For testing purposes only */
type Type<p> = {kind: p}

declare type int = number
declare type nat = number
declare type tez = number
declare type mutez = number
declare type bool = boolean
declare type unit = Type<"unit">
declare type operation = Type<"operation">
declare type list<a> = Array<a>
declare type map<a> = Type<"map">
declare type key_hash = Type<"key_hash">
declare type address = Type<"address">
declare type contract<parameter> = { parameter: parameter }
declare type Some<a> = a
declare type None = "None"
declare type option<a> = 
| Some<a>
| None
declare function ediv (a:number, b:number): option<[number, number]>;
declare function assert_some<t>(a: option<t>)
declare function assert(a: bool)
declare namespace Tezos {
    export const amount : number;
    export function implicit_account<parameter>(key_hash): contract<parameter>;
    export function address<parameter>(contract: contract<parameter>): address;
}
