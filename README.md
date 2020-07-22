# icfpc2020
  
Team: Rotten Lambdas

Members: Daniel Kauke, Christoph Breitkopf, Jan Dreske.

We live in Hannover and Vienna, but met in Hannover for the contest.

## Galaxy evaluator

We hacked an old SK reducer one of us had written in Java years ago
(<https://github.com/bokesan/skred>)
to read and evaluate the alien messages (text form only).

We also implemented a Python program to display the resulting images.

Being a bit frustrated by the "puzzle" approach we later tried to
implement a fully automatic way to extract the images from the galaxy message
without user interaction (`auto_solve.sh`). This gets to the spriral
galaxy image and the first send instruction in a few seconds, but
after that point too many new images appear and it becomes too slow.

## Game bot

Written in Haskell. We just implemented the protocol so that
our bot won't timeout or crash. We did not get around to implement
a strategy.
