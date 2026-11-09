// The shelf's API in tRPC, for comparing with servant. Called with
// createCaller, so no network is used.
// 本棚の API を tRPC で。servant とくらべるため。createCaller で呼ぶので、
// ネットワークは使わない
import { TRPCError } from "@trpc/server";

function check(ok: boolean, what: string): void {
  if (!ok) throw new Error(`check failed: ${what}`);
}

// ANCHOR: router
// TypeScript (tRPC: the router's type is the promise to the client)
// TypeScript（tRPC：ルーターの型が、そのままクライアントとの約束）
import { initTRPC, type inferRouterInputs } from "@trpc/server";
import { z } from "zod";

const t = initTRPC.create();
const ratings = new Map<number, number>([[1, 90]]);

export const shelfRouter = t.router({
  rate: t.procedure
    .input(z.object({
      id: z.number(),
      points: z.number().int().min(0).max(100),
    }))
    .mutation(({ input }) => {
      ratings.set(input.id, input.points);
    }),
  happen: t.procedure
    .input(z.object({ event: z.enum(["Start", "Finish", "Stop"]) }))
    .mutation(({ input }) => (input.event === "Start" ? "Reading" : "?")),
});

// The client imports only this type: no code is generated.
// クライアントは、この型だけを import する。コードの生成はない
export type ShelfRouter = typeof shelfRouter;
// ANCHOR_END: router

// ANCHOR: inputs
// TypeScript (the inputs, read from the router's type)
// TypeScript（入力の型を、ルーターの型から読む）
type Inputs = inferRouterInputs<ShelfRouter>;
const given: Inputs["rate"] = { id: 1, points: 70 };
// ANCHOR_END: inputs

// @ts-expect-error: "stars" is not an input of rate.
const stars: Inputs["rate"] = { id: 1, stars: 4.5 };
void stars;

const caller = t.createCallerFactory(shelfRouter)({});
await caller.rate(given);
check(ratings.get(1) === 70, "rated 70");

async function refused(run: () => Promise<unknown>): Promise<string> {
  try {
    await run();
    return "accepted";
  } catch (e) {
    return e instanceof TRPCError ? e.code : "other";
  }
}

const tooMuch = await refused(() => caller.rate({ id: 1, points: 101 }));
check(tooMuch === "BAD_REQUEST", "101");
check(ratings.get(1) === 70, "still 70");
// A word it does not know never reaches the procedure.
// 知らない言葉は、手続きまで届かない
const jump = await refused(() =>
  caller.happen({ event: "Jump" } as unknown as Inputs["happen"]));
check(jump === "BAD_REQUEST", "Jump");
check((await caller.happen({ event: "Start" })) === "Reading", "Start");
