package ef

import java.io.File

object Database {
  import Model._

  val Threshold = 500  // Minimum number of reviews per shop

  def load(path: String): String =
    scala.io.Source.fromFile(new File(path)).mkString

  def shopFiles: Seq[String] = new File("data/")
    .listFiles()
    .filter(_.getName.endsWith(".json"))
    .filter(_.getName != "_categories.json")
    .map(_.getPath)

  def loadCategories(): Seq[CategoryTree] = {
    import upickle._
    read[Seq[CategoryTree]](load(s"data/_categories.json"))
  }

  def loadShops(): Seq[ShopReviews] = {
    import upickle._
    shopFiles.map(file => read[ShopReviews](load(file)))
      .filter(_.reviews.reviews.size >= Threshold)
  }
}
