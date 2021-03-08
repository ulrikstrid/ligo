import {
  ActionType as ExamplesActionType,
  ChangeSelectedAction as ChangeSelectedExampleAction,
} from './examples';
import { Language } from './types';
import {} from './actions/editor';

export enum ActionType {
  ChangeLanguage = 'editor-change-language',
  ChangeCode = 'editor-change-code',
  ChangeDirty = 'editor-change-dirty',
  ChangeTitle = 'editor-change-title',
  ChangeCursorPosition = 'editor-change-cursor-position',
}

export interface CursorPosition {
  lineNumber: Number;
  column: Number;
}

export interface EditorState {
  language: Language;
  code: string;
  title: string;
  dirty: boolean;
  cursorPosition: CursorPosition | null;
}

export class ChangeLanguageAction {
  public readonly type = ActionType.ChangeLanguage;
  constructor(public payload: EditorState['language']) {}
}

export class ChangeCodeAction {
  public readonly type = ActionType.ChangeCode;
  constructor(public payload: EditorState['code']) {}
}

export class ChangeDirtyAction {
  public readonly type = ActionType.ChangeDirty;
  constructor(public payload: EditorState['dirty']) {}
}

export class ChangeTitleAction {
  public readonly type = ActionType.ChangeTitle;
  constructor(public payload: EditorState['title']) {}
}

export class ChangeCursorPositionAction {
  public readonly type = ActionType.ChangeCursorPosition;
  constructor(public payload: EditorState['cursorPosition']) {}
}

type Action =
  | ChangeCodeAction
  | ChangeLanguageAction
  | ChangeDirtyAction
  | ChangeTitleAction
  | ChangeSelectedExampleAction
  | ChangeCursorPositionAction;

const DEFAULT_STATE: EditorState = {
  language: Language.CameLigo,
  code: '',
  title: '',
  dirty: false,
  cursorPosition: null,
};

const editor = (state, action: Action): EditorState => {
  if (!state) {
    state = DEFAULT_STATE;
  }
  switch (action.type) {
    case ExamplesActionType.ChangeSelected:
      return {
        ...state,
        ...(action.payload && {
          ...action.payload.editor,
          title: action.payload.name,
        }),
      };
    case ActionType.ChangeLanguage:
      return {
        ...state,
        language: action.payload,
      };
    case ActionType.ChangeCode:
      return {
        ...state,
        code: action.payload,
      };
    case ActionType.ChangeDirty:
      return {
        ...state,
        dirty: action.payload,
      };
    case ActionType.ChangeTitle:
      return {
        ...state,
        title: action.payload,
      };
    case ActionType.ChangeCursorPosition:
      return {
        ...state,
        cursorPosition: action.payload,
      };
    default:
      return { ...state };
  }
};

export default editor;
