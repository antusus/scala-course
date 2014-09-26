object rationals {
    val x = new Rational(1, 3)
    val y = new Rational(5, 7)
    val z = new Rational(3, 2)
    x.sub(y).sub(z)
    y.add(y)
    z.less(x)
    z.max(x)
    x.less(y)
    x.max(y)
    new Rational(2)
}

class Rational(x: Int, y: Int) {
    require(y != 0, "y must be nonzero")
    private val g = gcd(x, y)

    def nominator = x / g

    def denominator = y / g

    def this(x: Int) = this(x, 1)

    def add(other: Rational) = {
        new Rational(nominator * other.denominator + denominator * other.nominator,
            denominator * other.denominator)
    }

    def sub(other: Rational) = add(other.neg)

    def neg: Rational = new Rational(-nominator, denominator)

    def less(that: Rational) = nominator * that.denominator < that.nominator * denominator

    def max(that: Rational) = if (this.less(that)) that else this

    override def toString() = nominator + "/" + denominator

    private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
}