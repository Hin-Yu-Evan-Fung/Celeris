# Celeris Chess Engine
- A fast chess engine made for the purpose of learning rust

## CCRL Blitz Ratings (as of 30th August 2025)
- Version 2.0 - (3480)[https://computerchess.org.uk/ccrl/404/cgi/engine_details.cgi?print=Details&each_game=1&eng=Celeris%202.0%2064-bit#Celeris_2_0_64-bit] Elo
- Version 1.0 - (3129)[https://computerchess.org.uk/ccrl/404/cgi/engine_details.cgi?match_length=30&each_game=1&print=Details&each_game=1&eng=Celeris%200.0.1%2064-bit#Celeris_0_0_1_64-bit] Elo

## Move generation
- Magic Bitboards ([wiki](https://www.chessprogramming.org/Magic_Bitboards))
    - PEXT intrinsics (Optional) ([wiki](https://www.chessprogramming.org/BMI2#PEXTBitboards))
    - Fancy Magic Bitboards ([wiki](https://www.chessprogramming.org/Magic_Bitboards))
- Fully Legal and Fast Move Generator
    - Keep tracks of pins and checks etc
    - Fast move generation (Up to 600 Mnps on a R9 7900)

## Move ordering
- Killer Move Heuristics
 - History Table ([wiki](https://www.chessprogramming.org/History_Heuristic))
- MVV-LVA ([wiki](https://www.chessprogramming.org/MVV-LVA))
-  Staged Move Generation ([wiki](https://www.chessprogramming.org/Move_Generation))
- Static Exchange Evaluation ([wiki](https://www.chessprogramming.org/Static_Exchange_Evaluation))
- Capture History ([wiki](https://www.chessprogramming.org/History_Heuristic))
- Continuation History ([wiki](https://www.chessprogramming.org/History_Heuristic))

Search
- Iterative Deepening ([wiki](https://www.chessprogramming.org/Iterative_Deepening))
- Classic Alpha Beta Search ([wiki](https://www.chessprogramming.org/Alpha-Beta))
     - Negamax
     - Quiescence Search ([wiki](https://www.chessprogramming.org/Quiescence_Search))
- Transposition Table ([wiki](https://www.chessprogramming.org/Transposition_Table))
       - Lockless Hashing
- Null Move Pruning ([wiki](https://www.chessprogramming.org/Null_Move_Pruning))
- Late Move Reductions ([wiki](https://www.chessprogramming.org/Late_Move_Reductions))
- Principal Variation Search ([wiki](https://www.chessprogramming.org/Principal_Variation_Search))
- Late Move Pruning 
- Futility Pruning ([wiki](https://www.chessprogramming.org/Futility_Pruning))
- Singular Extension Search ([wiki](https://www.chessprogramming.org/Singular_Extensions))
- Internal Iterative Deepening/Reductions ([wiki](https://www.chessprogramming.org/Internal_Iterative_Deepening))
- SEE Pruning
- Mate Distance Pruning ([wiki](https://www.chessprogramming.org/Mate_Distance_Pruning))

Evaluation
-  NNUE evaluation

Demo link on github main page


 ### Note that the binaries below for linux and windows are only for devices that support bmi2 and avx2 instructions.
