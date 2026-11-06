// The sieve race: how long does a sieve up to n take? (the best of 3)
// ふるいの競走：n までのふるいに、どれだけかかる？（3回のうちの最速）
//   dotnet run -c Release race.cs 10000000
using System.Diagnostics;

static int Sieve(int n)
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
    int count = 0;
    foreach (var ok in isPrime) if (ok) count++;
    return count;
}

int n = int.Parse(args[0]);
double best = double.PositiveInfinity;
int primes = 0;
for (int i = 0; i < 3; i++)
{
    var watch = Stopwatch.StartNew();
    primes = Sieve(n);
    best = Math.Min(best, watch.Elapsed.TotalMilliseconds);
}
Console.WriteLine($"{primes} {best:F1}");
