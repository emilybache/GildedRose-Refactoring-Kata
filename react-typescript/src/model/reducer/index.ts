import { TState, TAction, EActionTypes  } from '../../types';
import getItemOnNextDay from './getItemOnNextDay';

function reducer(state: TState, action: TAction): TState {
    if(action.type === EActionTypes.NEXT_DAY) {
        return { ...state, items: state.items.map(getItemOnNextDay) };
    }
    return state;
}

export default reducer;