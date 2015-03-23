package ef

import java.io.{FileWriter, File}
import java.net.URL
import org.jsoup.nodes.Element

import scala.collection.JavaConverters._
import org.jsoup.Jsoup

object Crawler {
  val urlCategories = "https://uk.trustpilot.com/categories"

  case class Category(caption: String, url: String)
  case class Shop(caption: String, url: String)
  case class Review(title: String, body: String, rating: Int)

  case class CategoryTree(
    category: Category
  , subCategories: Seq[Category]
  )

  case class ShopReviews(
    category: Category
  , shop: Shop
  , reviews: Seq[Review]
  )

  def loadCategories(): Seq[Category] = {
    val document = Jsoup.parse(new URL(urlCategories), 5000)
    val categories = document.select("div.menuleft").select("a").asScala
    categories.map(n => Category(n.text(), n.absUrl("href"))).toSeq
  }

  def loadSubCategories(cat: Category): Seq[Category] = {
    val document = Jsoup.parse(new URL(cat.url), 5000)
    val subCategories = document.select("li.selected").select("ul").select("a").asScala
    subCategories.map(n => Category(n.select("span").text(), n.absUrl("href"))).toSeq
  }

  def loadCategory(cat: Category, page: Int = 1): Seq[Shop] = {
    println(s"Loading page $page of $cat")

    def shopName(text: String) = text.split('.').tail.mkString(".").trim

    val document = Jsoup.parse(new URL(s"${cat.url}?page=$page"), 5000)

    val results = document
      .select("div.ranking")
      .select("h3")
      .select("a")
      .asScala

    val current  = results.map(result => Shop(shopName(result.text()), result.absUrl("href")))
    val nextPage = document.select("div.AjaxPagerLinkWrapper").first() != null

    if (nextPage) current ++ loadCategory(cat, page + 1)
    else current
  }

  def rating(elem: Element): Int = {
    if (elem.hasClass("count-1")) 1
    else if (elem.hasClass("count-2")) 2
    else if (elem.hasClass("count-3")) 3
    else if (elem.hasClass("count-4")) 4
    else if (elem.hasClass("count-5")) 5
    else 0
  }

  def loadReviews(shop: Shop, page: Int = 1): Seq[Review] = {
    println(s"Loading reviews for ${shop.caption}, page $page")
    val document = Jsoup.parse(new URL(s"${shop.url}?page=$page"), 5000)

    val reviews = document.select("div.review").select("div.review-info").asScala
    val current = reviews.flatMap { review =>
      val r = review.select("div.star-rating").first()
      if (r == null) None // It will be null if the review was reported
      else Some(Review(
        review.select("h3.review-title").text()
      , review.select("div.review-body").text() // TODO Should not strip paragraphs
      , rating(r)
      ))
    }.toSeq
    val nextPage = document.select("div.AjaxPagerLinkWrapper").first() != null

    if (nextPage) current ++ loadReviews(shop, page + 1)
    else current
  }

  def save(path: String, s: String): Unit = {
    val file = new FileWriter(path)
    file.write(s)
    file.close()
  }

  def shopId(url: String) = url.split('/').last

  def main(args: Array[String]) {
    import upickle._

    val categoryTree = loadCategories().map { category =>
      CategoryTree(category, loadSubCategories(category))
    }

    save(s"data/_categories.json", write(categoryTree))

    categoryTree.flatMap(_.subCategories).foreach { category =>
      val shops = loadCategory(category)
      shops.foreach { shop =>
        val id = shopId(shop.url)
        val path = s"data/$id.json"
        if (new File(path).exists()) println(s"$id exists")
        else save(path, write(ShopReviews(category, shop, loadReviews(shop))))
      }
    }
  }
}