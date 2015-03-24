package ef

import java.io.File

import codes.reactive.scalatime.LocalDateTime
import codes.reactive.scalatime.format.DateTimeFormatter

object Analysis {
  import Model._

  val formatter = DateTimeFormatter.Iso.OffsetDateTime

  // From http://stackoverflow.com/questions/10160280/how-to-implement-generic-average-function-in-scala
  def average[T](ts: Iterable[T])(implicit num: Numeric[T]): Option[Double] = {
    if (ts.size == 0) None
    else Some(num.toDouble(ts.sum) / ts.size)
  }

  def load(path: String): String =
    scala.io.Source.fromFile(new File(path)).mkString

  def shopFiles: Seq[String] = new File("data/")
    .listFiles()
    .filter(_.getName.endsWith(".json"))
    .filter(_.getName != "_categories.json")
    .map(_.getPath)

  def main(args: Array[String]) {
    import upickle._

    val categories = read[Seq[CategoryTree]](load(s"data/_categories.json"))
    val shops      = shopFiles.map(file => read[ShopReviews](load(file)))

    categories.foreach { category =>
      println(s"Category: ${category.category.caption}")

      val categoryReviews = category.subCategories.map { subCategory =>
        val matchingShops = shops.filter { shop =>
          shop.reviews.categories.contains(category.category) ||
          shop.reviews.categories.contains(subCategory)
        }

        val reviewsCount = matchingShops.map(_.reviews.count).sum

        val reviewsPerMonth = average(matchingShops.map { shop =>
          val perMonth = shop.reviews.reviews.groupBy { review =>
            val dt = LocalDateTime.parse(review.dateTime, formatter)
            (dt.getYear, dt.getMonth)
          }

          shop.reviews.reviews.size / perMonth.size
        }).getOrElse(0)

        println(s"  Sub-category: ${subCategory.caption}")
        println(s"    Reviews: $reviewsCount")
        println(s"    Average # per shop in a month: $reviewsPerMonth")

        reviewsCount
      }.sum

      println(s"  Reviews: $categoryReviews")
    }
  }
}
