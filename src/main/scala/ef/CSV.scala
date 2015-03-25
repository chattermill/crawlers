package ef

import java.io.File

import com.github.tototoshi.csv._

object CSV {
  def main(args: Array[String]) {
    val categories = Database.loadCategories()
    val shops      = Database.loadShops()

    categories
      .filter(_.category.caption == "Clothes & Fashion")  // TODO Extend to other categories
      .foreach
    { category =>
      category.subCategories.foreach { subCategory =>
        val matchingShops = shops.filter { shop =>
          shop.reviews.categories.contains(category.category) ||
            shop.reviews.categories.contains(subCategory)
        }

        val f = new File("csv/" + subCategory.caption + ".csv")
        val writer = CSVWriter.open(f)

        writer.writeRow(Seq(
          "shop"
        , "rating"
        , "dateTime"
        , "userId"
        ))

        matchingShops.foreach { shop =>
          shop.reviews.reviews.foreach { review =>
            writer.writeRow(Seq(
              shop.shop.url.split('/').last
            , review.rating
            , review.dateTime
            , review.userId
            ))
          }
        }

        writer.close()
      }
    }
  }
}
