package ef

import java.io.{FileWriter, File}
import java.net.URL
import org.jsoup.nodes.{TextNode, Document, Element}

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import org.jsoup.Jsoup

import scala.util.{Failure, Success, Try}

object Crawler {
  import Model._

  val urlCategories = "https://uk.trustpilot.com/categories"

  @tailrec
  def request(url: String): Document = {
    Try(Jsoup.parse(new URL(url), 5000)) match {
      case Success(document) => document
      case Failure(fail) =>
        println(s"Requesting $url failed: $fail")
        Thread.sleep(1000)
        request(url)
    }
  }

  // See also http://stackoverflow.com/questions/20075991/how-to-keep-line-breaks-when-using-jsoup-parse
  def getText(elem: Element): String = {
    elem.childNodes().asScala.map {
      case tn: TextNode => tn.text()
      case e: Element =>
        val nl = if (e.tag().getName.equalsIgnoreCase("br")) "\n" else ""
        nl + getText(e)
    }.mkString
  }

  def loadCategories(): Seq[Category] = {
    val document = request(urlCategories)
    val categories = document.select("div.menuleft a").asScala
    categories.tail.map(n => Category(n.text(), n.absUrl("href"))).toSeq
  }

  def loadSubCategories(cat: Category): Seq[Category] = {
    val document = request(cat.url)
    val subCategories = document.select("li.selected ul a").asScala
    subCategories.map(n => Category(n.select("span").text(), n.absUrl("href"))).toSeq
  }

  def loadCategory(cat: Category, page: Int = 1): Seq[Shop] = {
    println(s"Loading page $page of category ${cat.caption}")

    def shopName(text: String) = text.split('.').tail.mkString(".").trim

    val document = request(s"${cat.url}?page=$page")

    val results = document
      .select("div.ranking h3 a")
      .asScala

    val current  = results.map(result => Shop(shopName(result.text()), result.absUrl("href")))
      .filter(_.caption != "K-way") // TODO Workaround to prevent charset issues
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

  def loadReviews(shop: Shop): Reviews = {
    def loadReply(review: Element): Option[Reply] = {
      val reply = review.select("company-reply")
      if (reply.first() == null) None
      else Some(Reply(
        dateTime = reply.select("time.ndate").attr("datetime")
      , comment  = getText(reply.select("div.comment").first())
      ))
    }

    def loadPage(shop: Shop, page: Int): (Document, Seq[Review]) = {
      println(s"Loading reviews for ${shop.caption}, page $page")
      val document = request(s"${shop.url}?page=$page")

      val reviews = document.select("div.review").asScala
      val current = reviews.flatMap { review =>
        val r = review.select("div.star-rating").first()
        if (r == null) None // It will be null if the review was reported
        else Some(Review(
          title    = review.select("h3.review-title").text()
        , body     = getText(review.select("div.review-body").first())
        , rating   = rating(r)
        , dateTime = review.select("time.ndate").attr("datetime")
        , user     = review.select("div.user-review-name span").text()
        , userId   = review.select("div.user-review-name a").attr("href").split('/').last.toInt
        , reply    = loadReply(review)
        ))
      }.toSeq

      (document, current)
    }

    def parseCategories(document: Document): Seq[Category] =
      document
        .select("div.company-info-wrapper a")
        .asScala
        .filter(_.attr("href").startsWith("/categories"))
        .filter(!_.hasAttr("hidden"))  // Sometimes the top-level category is included, but hidden
        .map(element => Model.Category(element.text(), element.absUrl("href")))

    val pages = Stream.from(1)
      .map(page => loadPage(shop, page))
      .zipWithIndex
      .takeWhile
    { case (_, 0)      => true // Some shops consist of only one page
      case ((d, _), _) => d.select("div.AjaxPagerLinkWrapper").first() != null
    }

    val ((document, _), _) = pages.head

    Reviews(
      average    = document.select("span.average").text().toDouble
    , count      = document.select("span.ratingCount").text().toInt
    , reviews    = pages.flatMap { case ((_, r), _) => r }
    , categories = parseCategories(document)
    )
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
        else save(path, write(ShopReviews(shop, loadReviews(shop))))
      }
    }
  }
}
