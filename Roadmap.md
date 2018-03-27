
# Roadmap #

## Questions (que je dois me poser) ##

- α-conversion necessaire ?
- free\_variable et bound\_variable nécessaire ?

## Feuille de route ##

Pour le moment mon objectif est de faire tout ça sur des type non-récursifs:

- Définir un type anonyme *alpha* (α)
- ```itype``` sera le type inféré. Il contiendra un *α*
- Définir proprement un unificateur
- Implémenter la composition des unificateurs
- Implémenter l'unification d'un système E (qui devra être bien défini)
- Intégrer le tout dans l'algorithme W


## Extensions possibles (si feuille de route OK et si j'ai le temps) ##

- Gérer les types récursifs
