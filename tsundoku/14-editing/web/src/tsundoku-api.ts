// Made by `cabal run -v0 gen-ts` from Tsundoku.Api.Json.
// Tsundoku.Api.Json から `cabal run -v0 gen-ts` で作ったもの

export type WorkSummary = IWorkSummary;

export interface IWorkSummary {
  id: number;
  title: string;
  creator: string;
  kind: string;
  reading: string;
  rating: number | null;
  progress: ProgressNumbers | null;
}

export type ProgressNumbers = IProgressNumbers;

export interface IProgressNumbers {
  done: number;
  total: number;
  unit: string;
  percent: number;
}

export type CardDetail = ICardDetail;

export interface ICardDetail {
  work: WorkSummary;
  next: string[];
  minutes: number | null;
  toc: TocItem[];
  notes: NoteItem[];
}

export type TocItem = ITocItem;

export interface ITocItem {
  id: number;
  heading: string;
  size: number;
  done: boolean;
  rating: number | null;
  children: TocItem[];
}

export type NoteItem = INoteItem;

export interface INoteItem {
  entry: number | null;
  body: string;
  writtenAt: string;
  spoiler: boolean;
}

export type ReadingNow = IReadingNow;

export interface IReadingNow {
  reading: string;
  next: string[];
}

export type HitItem = IHitItem;

export interface IHitItem {
  work: number;
  title: string;
  place: string;
  entry: number | null;
  spoiler: boolean;
  trail: string[];
  snippet: Snip[];
}

export type Snip = ISnip;

export interface ISnip {
  text: string;
  match: boolean;
}

export type Problem = IProblem;

export interface IProblem {
  mistake: string;
  messages: string[];
}

export type NewWork = INewWork;

export interface INewWork {
  title: string;
  creator: string;
  kind: string;
  minutes: number | null;
  toc: Outline[];
}

export type Outline = IOutline;

export interface IOutline {
  heading: string;
  size: number;
  children: Outline[];
}

export type NewNote = INewNote;

export interface INewNote {
  entry: number | null;
  body: string;
  spoiler: boolean;
}

export type Happened = IHappened;

export interface IHappened {
  event: string;
}

export type ReadOn = IReadOn;

export interface IReadOn {
  day: string;
  entry: number;
}

export type Points = IPoints;

export interface IPoints {
  points: number;
}

export type FormField = IFormField;

export interface IFormField {
  label: string;
  kind: string;
  choices: string[];
  current: string;
}

export type EditedWork = IEditedWork;

export interface IEditedWork {
  title: string;
  creator: string;
  toc: EditedItem[];
}

export type EditedItem = IEditedItem;

export interface IEditedItem {
  id: number | null;
  heading: string;
  size: number;
  children: EditedItem[];
}
