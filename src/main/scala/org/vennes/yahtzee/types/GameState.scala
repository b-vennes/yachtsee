package org.vennes.yahtzee.types

import org.vennes.yahtzee.types.*


enum GameState:
  case TurnStart(card: Card)
  case RoundOne(card: Card, roll: DiceSet, keep: List[DiceSet.Index])
  case RoundTwo(card: Card, roll: DiceSet, keep: List[DiceSet.Index])
  case Selection(card: Card, dice: DiceSet, choose: Option[Card.Opt], previous: Option[GameState])
  case GameEnd(card: Card)
