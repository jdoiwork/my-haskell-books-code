// Made by `cabal run gen-ts` from the Haskell types. Do not edit.
// Haskell の型から cabal run gen-ts で作った。手で直さない
export type Quest = IQuest;

export interface IQuest {
  questId: number;
  title: string;
  reward: number;
  done: boolean;
}

export type NewQuest = INewQuest;

export interface INewQuest {
  title: string;
  reward: number;
}

export type ApiError = IApiError;

export interface IApiError {
  message: string;
}
