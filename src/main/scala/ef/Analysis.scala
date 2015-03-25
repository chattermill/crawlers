package ef

import codes.reactive.scalatime.LocalDateTime
import codes.reactive.scalatime.format.DateTimeFormatter

object Analysis {
  val formatter = DateTimeFormatter.Iso.OffsetDateTime

  // From http://stackoverflow.com/questions/10160280/how-to-implement-generic-average-function-in-scala
  def average[T](ts: Iterable[T])(implicit num: Numeric[T]): Option[Double] = {
    if (ts.size == 0) None
    else Some(num.toDouble(ts.sum) / ts.size)
  }

  def isPositive(review: Model.Review): Boolean = review.rating > 3
  def isNeutral (review: Model.Review): Boolean = review.rating == 3
  def isNegative(review: Model.Review): Boolean = review.rating < 3

  def main(args: Array[String]) {
    val categories = Database.loadCategories()
    val shops      = Database.loadShops()

    println(s"Considering ${shops.size} shops (with ${Database.Threshold}+ reviews)")

    categories.foreach { category =>
      println(s"Category: ${category.category.caption}")

      val categoryReviews = category.subCategories.map { subCategory =>
        val matchingShops = shops.filter { shop =>
          shop.reviews.categories.contains(category.category) ||
          shop.reviews.categories.contains(subCategory)
        }

        val reviewsCount = matchingShops.map(_.reviews.reviews.size).sum

        val reviewsPerMonth = average(matchingShops.map { shop =>
          val perMonth = shop.reviews.reviews.groupBy { review =>
            val dt = LocalDateTime.parse(review.dateTime, formatter)
            (dt.getYear, dt.getMonth)
          }

          shop.reviews.reviews.size / perMonth.size
        }).getOrElse(0)

        val positiveCount = matchingShops.map(_.reviews.reviews.count(isPositive)).sum
        val neutralCount  = matchingShops.map(_.reviews.reviews.count(isNeutral)).sum
        val negativeCount = matchingShops.map(_.reviews.reviews.count(isNegative)).sum
        val avgRating     = average(matchingShops.flatMap(_.reviews.reviews).map(_.rating)).getOrElse(0)

        println(s"  Sub-category: ${subCategory.caption}")
        println(s"    Reviews:")
        println(s"      # total             : $reviewsCount")
        println(s"      # positive [4, 5]   : $positiveCount")
        println(s"      # neutral  [3]      : $neutralCount")
        println(s"      # negative [1, 2]   : $negativeCount")
        println(s"      Avg. rating         : $avgRating")
        println(s"      # per shop and month: $reviewsPerMonth")

        reviewsCount
      }.sum

      println(s"  # total reviews: $categoryReviews")
    }
  }
}
