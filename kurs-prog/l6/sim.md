## Simulation of wolf population dynamics

Simulates wolf hunting on a 2D plane on rabbits

Made in Java, with JavaFX for the GUI, user on startup may
parameterize the simulation by setting:
- the initial number of wolves (default = 1)
- ... rabbits (default = 20)
- cycle parameter (so that the cycle duration is a random number
  between 0.5 * cycle duration and 1.5 * cycle duration)
- size of the plane (n x m)
- speed, range and number of the wolves (default = 1, max, 1)
- speed, range and number of the rabbits (default = 1, max, 20)


FEATURES:
- Wolves:
  - move policies:
    - move towards the closest rabbit
    - if within range, eat the rabbit
    - may rest after eating (default = 5 cycles)
    - may die of hunger (default = 10 cycles without eating)
  - if the user clicks on the wolf, it will be suspended, until the user clicks on it again
- Rabbits:
  - move policies:
    - **A** ALWAYS move away from the closest wolf
    - **B**:
      - run away from the closest wolf:
        - if the escape square is empty, move there
        - else stay in place
      - if on the edge of the board, move randomly
  - if the user clicks on the rabbit, it will be suspended, until the user clicks on it again