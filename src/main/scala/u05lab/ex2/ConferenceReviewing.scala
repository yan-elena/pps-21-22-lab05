package u05lab.ex2

enum Question:
  case RELEVANCE, SIGNIFICANCE, CONFIDENCE, FINAL

trait ConferenceReviewing:
  def loadReview(article: Int, scores: Map[Question, Int]): Unit
  def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin:Int): Unit
  def orderedScores(article: Int, question: Question): List[Int]
  def averageFinalScore(article: Int): Double
  def acceptedArticles: Set[Int]
  def sortedAcceptedArticles: List[(Int, Double)]
  def averageWeightedFinalScoreMap: Map[Int, Double]

class ConferenceReviewingImpl extends ConferenceReviewing:
  private var reviews = List[(Int, Map[Question, Int])]()

  override def loadReview(article: Int, scores: Map[Question, Int]): Unit =
    if scores.size < Question.values.length then throw new IllegalArgumentException
    reviews = (article, scores) :: reviews

  override def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit =
    import Question.*
    reviews = (article, Map(RELEVANCE -> relevance, SIGNIFICANCE -> significance, CONFIDENCE -> confidence, FINAL -> fin)) :: reviews

  override def orderedScores(article: Int, question: Question): List[Int] =
    reviews.filter(_._1 == article).map(_._2(question)).sorted

  override def averageFinalScore(article: Int): Double =
    val articleReviews = reviews.filter(_._1 == article)
    articleReviews.map(_._2(Question.FINAL)).sum.toDouble / articleReviews.length

  override def acceptedArticles: Set[Int] =
    reviews.map(_._1).distinct.filter(accepted).toSet

  override def sortedAcceptedArticles: List[(Int, Double)] =
    acceptedArticles.map(a => (a, averageFinalScore(a))).toList.sortBy(_._2)

  override def averageWeightedFinalScoreMap: Map[Int, Double] =
    reviews.map(_._1).distinct.map(e => e -> averageWeightedFinalScore(e)).toMap

  private def accepted(article: Int): Boolean =
    averageFinalScore(article) > 5.0 && reviews.filter(_._1 == article).map(_._2.get(Question.RELEVANCE)).exists(_.get >= 8)

  private def averageWeightedFinalScore(article: Int): Double =
    val articleReviews = reviews.filter(_._1 == article)
    articleReviews.map(p => p._2(Question.FINAL) * p._2(Question.CONFIDENCE) / 10.0).sum / articleReviews.length.toDouble
