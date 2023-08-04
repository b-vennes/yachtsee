# Yahtzee

A solo game of Yahtzee that runs within your terminal.

Decisions in the game are driven by text commands like "roll" and "ch" (choose).

Rolling dice is performed by saying which dice you'd like to roll:

```
First Roll:
  -------    -------    -------    -------    -------
 | o   o |  | o   o |  | o     |  | o   o |  | o o o |
 |   o   |  |       |  |   o   |  |       |  |       |
 | o   o |  | o   o |  |     o |  | o   o |  | o o o |
  -------    -------    -------    -------    ------- 
     A          B          C          D          E
```

To re-roll A, C, and D to try and get more fours, one would say `roll a c e`, or more simply, `roll ace`.

Making a selection of where to store a roll can be done with the `ch` phrase.
For example, to choose to save a roll in the full house space, one would say `ch full house` or `ch fullhouse`.

Enjoy!

## Run application

```shell
sbt run
```

## Run tests

There are currently no tests (gasp!), but there might eventually be some.
Use the following SBT task to run all the tests.

```shell
sbt test
```
