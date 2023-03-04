package org.vennes.yahtzee.types

import org.vennes.yahtzee.types.*

enum GameState:
  case TurnStart(card: Card)
  case RoundOne(card: Card, roll: DiceSet)
  case RoundTwo(card: Card, roll: DiceSet)
  case Selection(card: Card, dice: DiceSet, previous: Option[GameState])
  case GameEnd(card: Card)
