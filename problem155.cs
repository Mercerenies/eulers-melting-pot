
using System;
using System.Collections.Generic;

public class problem155 {

  public class Fraction {
    public long numerator { get; }
    public long denominator { get; }

    public Fraction(long numerator, long denominator) {
      long d = Gcd(numerator, denominator);
      this.numerator = numerator / d;
      this.denominator = denominator / d;
    }

    public override bool Equals(object that) =>
      (that is Fraction) &&
      Equals(that as Fraction);

    public bool Equals(Fraction that) =>
      this.numerator == that.numerator &&
      this.denominator == that.denominator;

    public override int GetHashCode() =>
      HashCode.Combine(numerator, denominator);

    public Fraction Reciprocal() =>
      new Fraction(denominator, numerator);

    public static Fraction operator +(Fraction a, Fraction b) =>
      new Fraction(a.numerator * b.denominator + b.numerator * a.denominator, a.denominator * b.denominator);

    public Fraction ParallelAdd(Fraction that) =>
      (this.Reciprocal() + that.Reciprocal()).Reciprocal();

  }

  public static long Gcd(long a, long b) {
    while (b != 0) {
      long tmp = a;
      a = b;
      b = tmp % a;
    }
    return a;
  }

  private static HashSet<Fraction> BaseCase() {
    var result = new HashSet<Fraction>();
    result.Add(new Fraction(1, 1));
    return result;
  }

  public static long UpTo(int n) {
    var distinct = new List<HashSet<Fraction>>();
    distinct.Add(BaseCase());
    for (var x = 2; x <= n; x++) {
      var newDistinct = new List<Fraction>();
      for (var n1 = 1; n1 <= x / 2; n1++) {
        foreach (var c1 in distinct[n1 - 1]) {
          var n2 = x - n1;
          foreach (var c2 in distinct[n2 - 1]) {
            newDistinct.Add(c1 + c2);
            newDistinct.Add(c1.ParallelAdd(c2));
          }
        }
      }
      distinct.Add(new HashSet<Fraction>(newDistinct));
    }

    var finalSet = new HashSet<Fraction>();
    foreach (var constituent in distinct) {
      finalSet.UnionWith(constituent);
    }
    return finalSet.Count;
  }

  public static void Main(string[] args) {
    Console.WriteLine(UpTo(18));
  }

}
