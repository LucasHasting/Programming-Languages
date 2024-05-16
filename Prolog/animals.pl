mammal(bear).
mammal(tiger).
mammal(whale).

bird(ostrich).
bird(peacock).
bird(eagle).

fish(salmon).
fish(goldfish).
fish(guppy).

reptile(turtle).
reptile(crocodile).
reptile(snake).

amphibian(frog).
amphibian(toad).
amphibian(newt).

with3PairsOfLegs(ant).
with3PairsOfLegs(cockroch).
with3PairsOfLegs(ladybug).

withMoreThan3PairsOfLegs(scorpion).
withMoreThan3PairsOfLegs(spider).
withMoreThan3PairsOfLegs(millipede).

wormLike(earthworm).
wormLike(leech).

notWormLike(flukeWorm).
notWormLike(tapeworm).

warmBlooded(X) :- mammal(X).
warmBlooded(X) :- bird(X).

coldBlooded(X) :- fish(X).
coldBlooded(X) :- reptile(X).
coldBlooded(X) :- amphibian(X).

withJointedLegs(X) :- with3PairsOfLegs(X).
withJointedLegs(X) :- withMoreThan3PairsOfLegs(X).

withoutLegs(X) :- wormLike(X).
withoutLegs(X) :- notWormLike(X).

vertibrate(X) :- warmBlooded(X).
vertibrate(X) :- coldBlooded(X).

invertibrate(X) :- withJointedLegs(X).
invertibrate(X) :- withoutLegs(X).

animal(X) :- vertibrate(X).
animal(X) :- invertibrate(X).









