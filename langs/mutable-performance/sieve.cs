// The C# code in the "rewrite to go faster" chapter.
// 書き換えて速くするの章の C# のコード
using System.Diagnostics;
using System.Linq;

// ANCHOR: array
// C# (an array of bools, rewritten in place)
// C#（bool の配列を、その場で書き換える）
static bool[] Sieve(int n)
{
    var isPrime = new bool[n + 1];
    Array.Fill(isPrime, true);
    isPrime[0] = false;
    isPrime[1] = false;
    for (int p = 2; p * p <= n; p++)
    {
        if (isPrime[p])
        {
            for (int m = p * p; m <= n; m += p)
            {
                isPrime[m] = false;
            }
        }
    }
    return isPrime;
}
// ANCHOR_END: array

var primes = Enumerable.Range(0, 31).Where(i => Sieve(30)[i]).ToArray();
Trace.Assert(string.Join(",", primes) == "2,3,5,7,11,13,17,19,23,29");
Trace.Assert(Sieve(1000000).Count(ok => ok) == 78498);
Console.WriteLine("ok");
