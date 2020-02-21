package io.mhmt.hackerrank


object WarmingUp extends App {

  def countingValleys(n: Int, s: String): Int = {
    var valleys = 0
    var height = 0
    for (i <- s.indices) {
      if (s(i) == 'D') {
        height = height - 1
      } else {
        height = height + 1
        if (height == 0) {
          valleys = valleys + 1
        }
      }
    }
    valleys
  }

  def jumpingOnClouds(c: Array[Int]): Int = {
    def go(clouds: Array[Int], jumps: Int): Int = {
      if (clouds.length == 1 || clouds.isEmpty) {
        jumps
      } else {
        if (clouds.length > 2 && clouds(2) == 0) {
          go(clouds.drop(2), jumps + 1)
        } else {
          go(clouds.drop(1), jumps + 1)
        }
      }

    }
    go(c, 0)
  }

  def repeatedString(s: String, n: Long): Long = {
    if (n <= s.length) s.take(n.toInt).filter(p => p == 'a').length
    else {
     val as = s.filter(_ == 'a').length
      as * (n / s.length) + s.take((n % s.length).toInt).filter(_ == 'a').length
    }
  }

  println(repeatedString("abcac", 10))


}

