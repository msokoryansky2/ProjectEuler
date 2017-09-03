package mike.sokoryansky.MathUtils

/**
  * StreamSearcher is a way to iteratively search a stream of (presumably expensive to generate) data in chunks ,
  * without having to pull the same piece of data twice or re-compute something for a piece of data more than once.
  * This is useful when there is no guarantee that the desired data will be found some number of elements into the
  * stream. At that point, if data is not found, more of the stream needs to be read and searched through. If some
  * computation took place while searching the first chunk of the stream it's nice to be able to pass it into the second
  * chunk search and so on, so there is no need to re-compute what was already computed on earlier chunks.
  *
  * @param data       Stream of source data of As
  * @param searcher   Predicate that analyzes a chunk/Seq of As and either outputs a C if it finds what it's looking for
  *                   or internal state B that will be fed into the next searcher iteration
  * @param stopper    Predicate that takes A and decides whether more A's should be examined or whether to make this one
  *                   its last
  * @tparam A         Class of data being analyzed (e.g. Long)
  * @tparam B         Class representing state of searcher that's maintained between iterations
  * @tparam C         Class of searcher's output
  */
class StreamSearcher[A, B, C](data: Stream[A],
                              searcher: SearchInput[A, B] => SearchOutput[B, C],
                              stopper: A => Boolean,
                              chunkSize: Int) {
  def search: Option[C] = {

    def pull(data: Stream[A]): (List[A], Stream[A]) = {
      def pullAcc(data: Stream[A], count: Int, acc: List[A]): (List[A], Stream[A]) = {
        if (data.isEmpty || stopper(data.head)) (acc, Stream.Empty)
        else if (count >= chunkSize) (acc, data)
        else pullAcc(data.tail, count + 1, data.head :: acc)
      }
      pullAcc(data, 0, List())
    }

    def searchNext(data: Stream[A], state: Option[B]): Option[C] = {
      val (elements, remainingData) = pull(data)
      if (elements.isEmpty) None
      else {
        val output = searcher(SearchInput(elements, state))
        if (output.success) output.result else searchNext(remainingData, output.state)
      }
    }
    searchNext(data, None)
  }
}

object StreamSearcher {
  def apply[A, B, C](data: Stream[A],
                      searcher: SearchInput[A, B] => SearchOutput[B, C],
                      stopper: A => Boolean,
                      chunkSize: Int): StreamSearcher[A, B, C] =
    new StreamSearcher[A, B, C](data, searcher, stopper, chunkSize)
}

case class SearchInput[A, B](elements: List[A], state: Option[B])
case class SearchOutput[B, C](state: Option[B], result: Option[C]) {
  def success: Boolean = result.nonEmpty
}