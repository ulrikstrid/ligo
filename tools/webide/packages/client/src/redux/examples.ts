import { ExampleState } from './example';

export enum ActionType {
  ChangeSelected = 'examples-change-selected',
  ClearSelected = 'examples-clear-selected',
  SetDefaultList = 'set-default-examples',
}

export interface ExampleItem {
  id: string;
  name: string;
}

export interface ExamplesState {
  selected: ExampleState | null;
  list?: ExampleItem[];
}

export class SetDefaultList {
  public readonly type = ActionType.SetDefaultList;
  constructor(public payload: ExamplesState['list']) { }
}

export class ChangeSelectedAction {
  public readonly type = ActionType.ChangeSelected;
  constructor(public payload: ExamplesState['selected']) { }
}

export class ClearSelectedAction {
  public readonly type = ActionType.ClearSelected;
}

type Action = ChangeSelectedAction | ClearSelectedAction | SetDefaultList;

const DEFAULT_STATE = {
  selected: null,
  list: []
}

// const fn = async (): Promise<ExampleItem[]> => {
//   const st: [] = await getExampleList()
//   console.log(st)
//   DEFAULT_STATE.list = st
//   return st
// };


// const DEFAULT_STATE = async (): Promise<ExamplesState> => {
//   const data  = await getExampleList()
//   console.log('d',data)
//   return {
//     selected: null,
//     list: await getExampleList().then((a) => {console.log(a);
//     return a})
//   }
// }


// if (process.env.NODE_ENV === 'development') {

//   // The name value configured in this list will only be for the development environment.
//   // For other environments, the name value will be taken directly from your contract's yaml configuration.
//   DEFAULT_STATE.list = [
//     { id: 'FEb62HL7onjg1424eUsGSg', name: 'Increment (PascaLIGO)' },
//     { id: 'MzkMQ1oiVHJqbcfUuVFKTw', name: 'Increment (CameLIGO)' },
//     { id: 'JPhSOehj_2MFwRIlml0ymQ', name: 'Increment (ReasonLIGO)' }
//   ];
// }
    //{ id: 'yP-THvmURsaqHxpwCravWg', name: 'ID (PascaLIGO)' },
    //{ id: 'ehDv-Xaf70mQoiPhQDTAUQ', name: 'ID (CameLIGO)' },
    //{ id: 'CpnK7TFuUjJiQTT8KiiGyQ', name: 'ID (ReasonLIGO)' }

const Examples = (state = DEFAULT_STATE, action: Action): ExamplesState => {
  switch (action.type) {
    case ActionType.ChangeSelected:
      return {
        ...state,
        selected: action.payload
      };
    case ActionType.ClearSelected:
      return {
        ...state,
        selected: null
      };
    case ActionType.SetDefaultList:
      return { ...state}
    default:
      return {
        ...state
      }
  }
};

export default Examples