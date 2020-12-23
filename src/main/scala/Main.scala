
import scala.io.Source

case class Rule(name: String, ranges: Seq[(Long, Long)]) {
  def valid(candidate: Long): Boolean = {
    ranges.exists(range => range._1 <= candidate && range._2 >= candidate)
  }
}

object Day16 extends App {
  val ruleRegex = """(.+): (\d+)-(\d+) or (\d+)-(\d+)""".r.unanchored
  val inputFile = Source.fromResource("input.txt").getLines.toList

  val myTicketIndex: Int = inputFile.indexOf("your ticket:") + 1
  val myTicket: Seq[Long] = inputFile(myTicketIndex).split(",").map(_.toLong).toSeq
  val nearbyTickets: Seq[Seq[Long]] = inputFile.slice(inputFile.indexOf("nearby tickets:") + 1, inputFile.size).map(_.split(",").map(_.toLong))

  val rules: Set[Rule] = inputFile.slice(0, myTicketIndex - 2).map(_ match {
      case ruleRegex(name, lower1, upper1, lower2, upper2) => {
        Rule(name, Seq((lower1.toLong, upper1.toLong), (lower2.toLong, upper2.toLong)))
      }
    }
  ).toSet
  
  val nearbyTicketsWithErrorRate = nearbyTickets.map(
    ticket => {
      // Filter for field values that are invalid (i.e. does not match ANY of the rules)
      val invalid = ticket.filter(fieldValue => !rules.exists(rule => rule.valid(fieldValue)))
      val errorRate = invalid.sum // Get the error rate of this ticket
      (ticket, errorRate, invalid.size > 0)
    }
  )

  // Total error rate of all nearby tickets
  val totalErrorRate = nearbyTicketsWithErrorRate.map { case (_, errorRate, _) => errorRate }.sum
  println(s"ticket error rate: $totalErrorRate")

  // Get only nearby tickets that are error-free / valid
  val validNearbyTickets = nearbyTicketsWithErrorRate.filterNot { case (_, _, invalid) => invalid }.map { case (ticket, _, _) => ticket} 

  // Combine my ticket and nearby tickets
  val allTickets = myTicket +: validNearbyTickets

  // Get all potential field candidates for each field position
  val posToPotentialFields: Seq[(Set[Rule], Int)] = (0 until rules.size).map( pos => {
    // Get all field values at field/column pos i
    val column: Seq[Long] = allTickets.map(ticket => ticket(pos))
    val potentialFields = rules.filter(rule => column.forall(c => rule.valid(c)))
    (potentialFields, pos)
  })

  // Recursively choose the best field for each position
  def chooseFields(posToFields: Seq[(Set[Rule], Int)]): Seq[(Rule, Int)] = {
    if (posToFields.size > 0) {
      // Start with the field position with the lowest number of potential choices
      val posToFieldsSortedBySize = posToFields.sortBy(_._1.size)
      val first: (Set[Rule], Int) = posToFieldsSortedBySize.head
      val chosenRule: Rule = first._1.head

      (chosenRule, first._2) +: chooseFields(posToFieldsSortedBySize.tail.map { case (choices, index) => (choices - chosenRule, index)})
    } else {
      Seq.empty
    }
  }

  val answer = chooseFields(posToPotentialFields).filter { 
    case (Rule(name, _), _) => name.startsWith("departure")
  }.map { case (_, pos) => myTicket(pos) }.reduce(_ * _)

  println(s"'depature*' fields multiply to $answer")
}