type TItem = {
    name: string,
    sellIn: number,
    quality: number,
    isConjured: boolean
}

type TState = {
    items: TItem[]
};

type TAction = {
    type: EActionTypes,
    payload?: any
}

type TContext = {
    state: TState,
    dispatch: (action: TAction) => void
}

export enum EActionTypes {
    NEXT_DAY = 'NEXT_DAY',
}

export type {
    TItem,
    TState,
    TAction,
    TContext
};
