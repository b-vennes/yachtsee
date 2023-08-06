package org.vennes.yahtzee.types

import cats.syntax.all.*
import org.vennes.yahtzee.*

case class Card(
    ones: Option[Int],
    twos: Option[Int],
    threes: Option[Int],
    fours: Option[Int],
    fives: Option[Int],
    sixes: Option[Int],
    threeOak: Option[Int],
    fourOak: Option[Int],
    fullHouse: Option[Int],
    smallStraight: Option[Int],
    largeStraight: Option[Int],
    yahtzee: Option[Int],
    chance: Option[Int],
    bonuses: Int
):

  import Card.*

  def withOnes(dice: DiceSet): Option[Card] =
    whenNone(ones, copy(ones = scoreOnes(dice).some))

  def withTwos(dice: DiceSet): Option[Card] =
    whenNone(twos, copy(twos = scoreTwos(dice).some))

  def withThrees(dice: DiceSet): Option[Card] =
    whenNone(threes, copy(threes = scoreThrees(dice).some))

  def withFours(dice: DiceSet): Option[Card] =
    whenNone(fours, copy(fours = scoreFours(dice).some))

  def withFives(dice: DiceSet): Option[Card] =
    whenNone(fives, copy(fives = scoreFives(dice).some))

  def withSixes(dice: DiceSet): Option[Card] =
    whenNone(sixes, copy(sixes = scoreSixes(dice).some))

  def withThreeOak(dice: DiceSet): Option[Card] =
    whenNone(threeOak, copy(threeOak = scoreThreeOak(dice).some))

  def withFourOak(dice: DiceSet): Option[Card] =
    whenNone(fourOak, copy(fourOak = scoreFourOak(dice).some))

  def withFullHouse(dice: DiceSet): Option[Card] =
    whenNone(fullHouse, copy(fullHouse = scoreFullHouse(dice).some))

  def withSmallStraight(dice: DiceSet): Option[Card] =
    whenNone(smallStraight, copy(smallStraight = scoreSmallStraight(dice).some))

  def withLargeStraight(dice: DiceSet): Option[Card] =
    whenNone(largeStraight, copy(largeStraight = scoreLargeStraight(dice).some))

  def withYahtzee(dice: DiceSet): Option[Card] =
    yahtzee.fold(copy(yahtzee = scoreYahtzee(dice).some).some)(_ =>
      if isYahtzee(dice) then copy(bonuses = bonuses + 1).some else None
    )

  def withChance(dice: DiceSet): Option[Card] =
    whenNone(chance, copy(chance = scoreChance(dice).some))

  def withOpt(opt: Opt, dice: DiceSet): Option[Card] =
    scoreCardByOpt(this, opt, dice)

  def toList: List[Option[Int]] =
    List(
      ones,
      twos,
      threes,
      fours,
      fives,
      sixes,
      threeOak,
      fourOak,
      fullHouse,
      smallStraight,
      largeStraight,
      yahtzee
    )

  def active: Boolean =
    toList.exists(_.isEmpty)

  def complete: Boolean = !active

  def topScore: Int =
    ones.getOrElse(0) +
      twos.getOrElse(0) +
      threes.getOrElse(0) +
      fours.getOrElse(0) +
      fives.getOrElse(0) +
      sixes.getOrElse(0)

  def topBonus: Option[Int] = if topScore >= 63 then 35.some else None

  def bottomScore: Int =
    threeOak.getOrElse(0) +
      fourOak.getOrElse(0) +
      fullHouse.getOrElse(0) +
      smallStraight.getOrElse(0) +
      largeStraight.getOrElse(0) +
      chance.getOrElse(0) +
      yahtzee.getOrElse(0) +
      bonuses

  def score: Int =
    topScore + topBonus.getOrElse(0) + bottomScore

object Card:

  enum Opt:
    case Ones
    case Twos
    case Threes
    case Fours
    case Fives
    case Sixes
    case ThreeOfAKind
    case FourOfAKind
    case FullHouse
    case SmallStraight
    case LargeStraight
    case Yahtzee
    case Chance

  object Opt:
    def from(s: String): Option[Opt] =
      s.toLowerCase().replace(" ", "") match
        case "ones"          => Opt.Ones.some
        case "twos"          => Opt.Twos.some
        case "threes"        => Opt.Threes.some
        case "fours"         => Opt.Fours.some
        case "fives"         => Opt.Fives.some
        case "sixes"         => Opt.Sixes.some
        case "threeofakind"  => Opt.ThreeOfAKind.some
        case "fourofakind"   => Opt.FourOfAKind.some
        case "fullhouse"     => Opt.FullHouse.some
        case "smallstraight" => Opt.SmallStraight.some
        case "largestraight" => Opt.LargeStraight.some
        case "yahtzee"       => Opt.Yahtzee.some
        case "ance"          => Opt.Chance.some
        case _               => None

  def whenNone[A, B](option: Option[A], value: => B): Option[B] =
    option match
      case None => value.some
      case _    => None

  def scoreOnes(dice: DiceSet): Int =
    countMatching(dice, Dice.Side.One)

  def scoreTwos(dice: DiceSet): Int =
    2 * countMatching(dice, Dice.Side.Two)

  def scoreThrees(dice: DiceSet): Int =
    3 * countMatching(dice, Dice.Side.Three)

  def scoreFours(dice: DiceSet): Int =
    4 * countMatching(dice, Dice.Side.Four)

  def scoreFives(dice: DiceSet): Int =
    5 * countMatching(dice, Dice.Side.Five)

  def scoreSixes(dice: DiceSet): Int =
    6 * countMatching(dice, Dice.Side.Six)

  def countMatching(dice: DiceSet, side: Dice.Side): Int =
    dice.toList.count(_ == side)

  def scoreThreeOak(dice: DiceSet): Int =
    if isOak(3, dice) then dice.sum
    else 0

  def scoreFourOak(dice: DiceSet): Int =
    if isOak(4, dice) then dice.sum
    else 0

  def isOak(required: Int, dice: DiceSet): Boolean =
    Dice.sides.exists(search => dice.toList.count(d => search == d) >= required)

  def scoreFullHouse(dice: DiceSet): Int =
    val intDice = dice.toIntList
    val distinct = intDice.distinct
    val first = intDice.head
    val firstDiceCount = intDice.count(_ == first)

    if distinct.length == 2 && (firstDiceCount == 2 || firstDiceCount == 3) then
      25
    else 0

  def isSubset(value: List[Int], of: List[Int]): Boolean =
    value.forall(of.contains)

  def scoreSmallStraight(dice: DiceSet): Int =
    if List(
        List(1, 2, 3, 4),
        List(2, 3, 4, 5),
        List(3, 4, 5, 6)
      ).exists(l => isSubset(l, dice.toIntList))
    then 30
    else 0

  def scoreLargeStraight(dice: DiceSet): Int =
    if List(
        List(1, 2, 3, 4, 5),
        List(2, 3, 4, 5, 6)
      ).exists(l => isSubset(l, dice.toIntList))
    then 40
    else 0

  def isYahtzee(dice: DiceSet): Boolean =
    val diceList = dice.toList
    val first = diceList.head
    diceList.forall(_ == first)

  def scoreYahtzee(dice: DiceSet): Int =
    if isYahtzee(dice) then 50
    else 0

  def scoreChance(dice: DiceSet): Int =
    dice.toIntList.sum

  def scoreOpt(opt: Opt, dice: DiceSet): Int =
    (opt match {
      case Opt.Ones          => scoreOnes
      case Opt.Twos          => scoreTwos
      case Opt.Threes        => scoreThrees
      case Opt.Fours         => scoreFours
      case Opt.Fives         => scoreFives
      case Opt.Sixes         => scoreSixes
      case Opt.ThreeOfAKind  => scoreThreeOak
      case Opt.FourOfAKind   => scoreFourOak
      case Opt.FullHouse     => scoreFullHouse
      case Opt.SmallStraight => scoreSmallStraight
      case Opt.LargeStraight => scoreLargeStraight
      case Opt.Yahtzee       => scoreYahtzee
      case Opt.Chance        => scoreChance
    })(dice)

  def scoreCardByOpt(card: Card, opt: Opt, dice: DiceSet): Option[Card] =
    opt match {
      case Opt.Ones          => card.withOnes(dice)
      case Opt.Twos          => card.withTwos(dice)
      case Opt.Threes        => card.withThrees(dice)
      case Opt.Fours         => card.withFours(dice)
      case Opt.Fives         => card.withFives(dice)
      case Opt.Sixes         => card.withSixes(dice)
      case Opt.ThreeOfAKind  => card.withThreeOak(dice)
      case Opt.FourOfAKind   => card.withFourOak(dice)
      case Opt.FullHouse     => card.withFullHouse(dice)
      case Opt.SmallStraight => card.withSmallStraight(dice)
      case Opt.LargeStraight => card.withLargeStraight(dice)
      case Opt.Yahtzee       => card.withYahtzee(dice)
      case Opt.Chance        => card.withChance(dice)
    }

  def initial(): Card =
    Card(
      None,
      None,
      None,
      None,
      None,
      None,
      None,
      None,
      None,
      None,
      None,
      None,
      None,
      0
    )
