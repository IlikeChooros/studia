TODO:
- [x] Move:
  - [x] holds a position from and a position to
  - [x] has field for the creature that is moving
  - [x] has a field for the action (move or eat)
- [x] Global Manager:
  - [x] holds Random class instance
  - [x] keeps track of all creatures (wolves and rabbits)
- [x] Creature:
  - [x] cycle method -> returns the next move
  - [x] getColor -> returns the color of the creature
  - [x] getPosition -> returns the position of the creature
  - [x] findClosest -> returns the closest creature of a given type
  - [x] MovePolicy object, that's attached to the creature
    - [x] refactor Creature:
      - [x] remove genMove, genMoveType, MoveType etc. and move it to the MovePolicy
- [x] Wolf:
  - [x] Features:
    - [x] speed (by deafult, 1)
    - [x] range (for eating)
    - [x] MovePolicy (default: move towards the closest rabbit)
      - [ ] After killing a rabbit, may rest for a few cycles (default = 5)
      - [ ] May die of hunger (default = 20 cycles without eating)
  - [x] .cycle method:
    - [x] find nearest rabbit (for optimization, use a priority queue)
    - [x] If rabbit is within range, eat it
    - [x] Else, move towards the rabbit (or randomly)
- [x] Rabbit:
  - [x] Features:
    - [x] speed (by deafult, 1)
    - [x] range (for escaping)
    - [x] MovePolicy (default: always move away from the closest wolf)
  - [x] .cycle method:
    - [x] esape from the nearest wolf (if within range)