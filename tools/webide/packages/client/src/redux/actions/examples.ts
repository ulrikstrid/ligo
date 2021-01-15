import { Dispatch, ActionCreator } from 'redux';
import {ThunkAction} from 'redux-thunk';
import { getExampleList } from '../../services/api';
import { AppState } from '../app';
import { CancellableAction } from './cancellable';
import { SetDefaultList } from '../examples'

export class ExamplesAction extends CancellableAction {
  getAction() {
    return async (dispatch: Dispatch, getState: () => AppState) => {
     const List = new Promise(resolve => {
      getExampleList()
        .then(list => {
          resolve(list);
          return dispatch({...new SetDefaultList(list) })
        });
    })
    return List
  }
}

}
