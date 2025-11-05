### Title of the text: What is the Monte Carlo Tree Search?

Link: https://builtin.com/machine-learning/monte-carlo-tree-search
 

### Exercise 1 What questions do you think the text may answer?

```
......................................................................................
......................................................................................
......................................................................................
```

### Glossary

| Word              | Definition | Polish meaning |
| ----------------- | ---------- | -------------- |
| Node              | A point in the search tree representing a particular game state or glossary decision. | Wierzchołek drzewa gry, reprezentujący stan gry (np. pozycję) |
| Roll‑out          | A quick simulation from a node to the end of the game or scenario to estimate its value. | Symulacja rozgrywki aż do otrzymania końca gry (wygranej, przegranej lub remisu z perspektywy rozpoczynającego wierzchołka) |
| Exploration       | Trying out less‑visited moves/states to discover new possibilities in the search tree. | Eksploracja mniej odkrytych pozycji |
| Exploitation      | Focusing on moves/states known to perform well based on previous simulations. | Rozwijanie i pogłębianie najbardziej optymalnej ścieżki |
| Search Tree       | A branching structure of nodes representing possible actions and subsequent states. | Struktura drzewa reprezentująca możliwe ruchy (łączenia) i ich stany (wierzchołki) |
| Playout           | Another term for a roll‑out; playing out a sequence of moves randomly or heuristically to completion. | Synonim dla Roll-out |
| Domain-agnostic | A characteristic of an algorithm that allows it to be applied across many different problem domains without domain‑specific adjustments. | Typ alogrytmu, który może być zastosowany do rozwiązania wielu problemów, bez wprowadzania dodakowych zmian wynikających z dziedziny problemu | 
| Heuristic         | A simple strategy or rule of thumb used to guide or speed up search rather than try everything. | Zestaw zasad/strategii do szybkiego kierowania się ku optymalnemu rozwiązaniu |
| Branching Factor  | The average number of possible moves from each state; high branching factor means many choices. | Średnia ilość legalnych ruchów w danej grze |
| Anytime Algorithm | An algorithm that can be stopped at any time and still provide a valid (though possibly sub‑optimal) result. | Algorytm, który może być zatrzymany w dowolnym czasie i wciąż zwróci poprawne rozwiązanie |

### Exercise 2 Matching
Match each word to the definitions below (Branching Factor, Search Tree, Heuristic, Playout, Exploration):

A. The rate at which new possibilities are discovered versus using known good ones.

B. A quick function or rule used to guide decision‑making rather than complete enumeration.

C. The network of possible game states and actions branching from a root position.

D. A complete play‑out from a given position to a terminal result used to estimate value.

E. The number of options available at each decision point in a game or tree.

### Exercise 3 Vocabulary

Complete the sentences with the correct word from the glossary:

1. In MCTS you start at the root and repeatedly traverse the tree by selecting child ____ until you reach a leaf node.

1. After choosing a leaf, you perform a random ____ (or roll‑out) to see how good that move might turn out.

1. MCTS is classified as an ____ algorithm because you can stop it any time and still pick your best known move.

1. Because games like Go have a massive number of possible moves per state, the ____ can be extremely large and exhaust brute‑force search methods.

1. The algorithm is designed to be ___, meaning it can apply to board games, resource‑allocation problems or other domains without substantial adaptation.

### Exercise 4 Comprehension Questions

1. What are the main steps described in the article that MCTS follows in its search process?

2. Why is MCTS particularly suited for games or decision problems with a high branching factor?

3. What are the disadvantages of MCTS?

4. What is the benefit of being able to stop the algorithm early?

5. Explain the domain-agnostic nature of the MCTS algorithm.
