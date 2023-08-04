package org.vennes.yahtzee.types

import org.vennes.yahtzee.types.*

enum GameState:
  case TurnStart(card: Card)
  case FirstRoll(card: Card, roll: DiceSet)
  case RoundOne(card: Card, roll: DiceSet)
  case SecondRoll(card: Card, roll: DiceSet, rolling: List[DiceSet.Index])
  case RoundTwo(card: Card, roll: DiceSet)
  case ThirdRoll(card: Card, roll: DiceSet, rolling: List[DiceSet.Index])
  case Selection(card: Card, dice: DiceSet, previous: Option[GameState])
  case GameEnd(card: Card)
