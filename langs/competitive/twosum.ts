// The TypeScript code in the competitive programming chapter.
// 競技プログラミングの章の TypeScript のコード
function check(ok: boolean, what: string): void {
  if (!ok) throw new Error(`failed: ${what}`);
}

// ANCHOR: twosum
// TypeScript (LeetCode 1, Two Sum, with a Map)
// TypeScript（LeetCode の1問目 Two Sum を、Map で）
function twoSum(nums: number[], target: number): number[] | null {
  const seen = new Map<number, number>();
  for (let i = 0; i < nums.length; i++) {
    const j = seen.get(target - nums[i]);
    if (j !== undefined) return [j, i];
    seen.set(nums[i], i);
  }
  return null;
}
// ANCHOR_END: twosum

check(JSON.stringify(twoSum([2, 7, 11, 15], 9)) === "[0,1]", "first");
check(JSON.stringify(twoSum([3, 2, 4], 6)) === "[1,2]", "second");
check(twoSum([1, 2], 10) === null, "none");
