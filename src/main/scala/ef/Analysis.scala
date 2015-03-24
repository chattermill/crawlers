package ef

import java.io.File

object Analysis {
  import Model._

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
        val reviewsCount = shops.filter { shop =>
          shop.reviews.categories.contains(category.category) ||
          shop.reviews.categories.contains(subCategory)
        }.map(_.reviews.count)
         .sum

        println(s"  Sub-category: ${subCategory.caption}")
        println(s"    Reviews: $reviewsCount")

        reviewsCount
      }.sum

      println(s"  Reviews: $categoryReviews")
    }
  }
}
