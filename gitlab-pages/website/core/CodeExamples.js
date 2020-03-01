import React from 'react';
import Highlight, { defaultProps } from "prism-react-renderer";
import github from "prism-react-renderer/themes/github";


const pre = '```';

const PASCALIGO_EXAMPLE = `
type storage is int

type parameter is
  Increment of int
| Decrement of int
| Reset

type return is list (operation) * storage

// Two entrypoints

function add (const store : storage; const delta : int) : storage is store + delta
function sub (const store : storage; const delta : int) : storage is store - delta

(* Main access point that dispatches to the entrypoints according to
   the smart contract parameter. *)

function main (const action : parameter; const store : storage) : return is
 ((nil : list (operation)),    // No operations
  case action of
    Increment (n) -> add (store, n)
  | Decrement (n) -> sub (store, n)
  | Reset         -> 0
  end)
`;

const CAMELIGO_EXAMPLE = `
type storage = int

type parameter =
  Increment of int
| Decrement of int
| Reset

type return = operation list * storage

// Two entrypoints

let add (store, delta : storage * int) : storage = store + delta
let sub (store, delta : storage * int) : storage = store - delta

(* Main access point that dispatches to the entrypoints according to
   the smart contract parameter. *)

let main (action, store : parameter * storage) : return =
 ([] : operation list),    // No operations
 (match action with
   Increment (n) -> add (store, n)
 | Decrement (n) -> sub (store, n)
 | Reset         -> 0)
`;


const REASONLIGO_EXAMPLE = `
type storage = int;

type parameter =
  Increment (int)
| Decrement (int)
| Reset;

type return = (list (operation), storage);

(* Two entrypoints *)

let add = ((store, delta) : (storage, int)) : storage => store + delta;
let sub = ((store, delta) : (storage, int)) : storage => store - delta;

(* Main access point that dispatches to the entrypoints according to
   the smart contract parameter. *)

let main = ((action, store) : (parameter, storage)) : return => {
 (([] : list (operation)),    // No operations
 (switch (action) {
  | Increment (n) => add ((store, n))
  | Decrement (n) => sub ((store, n))
  | Reset         => 0}))
};
`;


function CodeExamples (props) {
  return (
    <div className="tabs">
      <div className="nav-tabs">
        <div
          className="nav-link active"
          data-group="examples"
          data-tab="pascaligo"
        >
          PascaLIGO
        </div>
        <div className="nav-link" data-group="examples" data-tab="cameligo">
          CameLIGO
        </div>
        <div className="nav-link" data-group="examples" data-tab="reasonligo">
          ReasonLIGO
        </div>
      </div>
      <div className="tab-content">
        <div id="pascaligo" className="tab-pane active" data-group="examples">
          <Highlight {...defaultProps} language="pascaligo" code={PASCALIGO_EXAMPLE} theme={github}>
            {({ className, style, tokens, getLineProps, getTokenProps }) => (
              <pre className={className} style={style}>
                {tokens.map((line, i) => (
                  <div {...getLineProps({ line, key: i })}>
                    {line.map((token, key) => (
                      <span {...getTokenProps({ token, key })} />
                    ))}
                  </div>
                ))}
              </pre>
            )}
          </Highlight>
        </div>
        <div id="cameligo" className="tab-pane" data-group="examples">
          <Highlight {...defaultProps} language="cameligo" code={CAMELIGO_EXAMPLE} theme={github}>
            {({ className, style, tokens, getLineProps, getTokenProps }) => (
              <pre className={className} style={style}>
                {tokens.map((line, i) => (
                  <div {...getLineProps({ line, key: i })}>
                    {line.map((token, key) => (
                      <span {...getTokenProps({ token, key })} />
                    ))}
                  </div>
                ))}
              </pre>
            )}
          </Highlight>
        </div>
        <div id="reasonligo" className="tab-pane" data-group="examples">
        <Highlight {...defaultProps} language="reasonligo" code={REASONLIGO_EXAMPLE} theme={github}>
            {({ className, style, tokens, getLineProps, getTokenProps }) => (
              <pre className={className} style={style}>
                {tokens.map((line, i) => (
                  <div {...getLineProps({ line, key: i })}>
                    {line.map((token, key) => (
                      <span {...getTokenProps({ token, key })} />
                    ))}
                  </div>
                ))}
              </pre>
            )}
          </Highlight>
        </div>
      </div>
    </div>
  );
};

export default CodeExamples