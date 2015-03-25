package ef

object Model {
  case class Category(
    caption: String
  , url    : String
  )

  case class Shop(
    caption: String
  , url    : String
  )

  case class Review(
    title   : String
  , body    : String
  , rating  : Int
  , dateTime: String
  , user    : String  // Author name
  , userId  : Int
  , reply   : Option[Reply]
  )

  case class Reply(
    dateTime: String
  , comment : String
  )

  case class Reviews(
    average   : Double  // TODO Remove
  , count     : Int     // TODO Remove
  , reviews   : Seq[Review]
  , categories: Seq[Category]
  )

  case class CategoryTree(
    category     : Category
  , subCategories: Seq[Category]
  )

  case class ShopReviews(
    shop   : Shop
  , reviews: Reviews
  )
}
