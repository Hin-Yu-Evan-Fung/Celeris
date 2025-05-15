# Chess Library

# Move generation
- Fully legal move generator
- Fancy Magic Bitboards
- Reaching speeds of up to 600Mnps on a R9 7900 and 320Mnps on a i7-10750H
<details>
  <summary>Perft results (For R9 7900) </summary>
  
  ```
  status: PASSED, time:  267ms, Mnps: 445.9, Fen: rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1
  status: PASSED, time:  297ms, Mnps: 652.2, Fen: r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1
  status: PASSED, time:  482ms, Mnps: 370.6, Fen: 8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1
  status: PASSED, time: 1191ms, Mnps: 592.8, Fen: r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1
  status: PASSED, time:    3ms, Mnps: 354.5, Fen: 1k6/1b6/8/8/7R/8/8/4K2R b K - 0 1
  status: PASSED, time:    5ms, Mnps: 227.0, Fen: 3k4/3p4/8/K1P4r/8/8/8/8 b - - 0 1
  status: PASSED, time:    4ms, Mnps: 253.8, Fen: 8/8/4k3/8/2p5/8/B2P2K1/8 w - - 0 1
  status: PASSED, time:    6ms, Mnps: 240.1, Fen: 8/8/1k6/2b5/2pP4/8/5K2/8 b - d3 0 1
  status: PASSED, time:    3ms, Mnps: 220.4, Fen: 5k2/8/8/8/8/8/8/4K2R w K - 0 1
  status: PASSED, time:    3ms, Mnps: 267.9, Fen: 3k4/8/8/8/8/8/8/R3K3 w Q - 0 1
  status: PASSED, time:    2ms, Mnps: 637.1, Fen: r3k2r/1b4bq/8/8/8/8/7B/R3K2R w KQkq - 0 1
  status: PASSED, time:    3ms, Mnps: 573.5, Fen: r3k2r/8/3Q4/8/8/5q2/8/R3K2R b KQkq - 0 1
  status: PASSED, time:    9ms, Mnps: 424.6, Fen: 2K2r2/4P3/8/8/8/8/8/3k4 w - - 0 1
  status: PASSED, time:    1ms, Mnps: 1004.7, Fen: 8/8/1P2K3/8/2n5/1q6/8/5k2 b - - 0 1
  status: PASSED, time:    1ms, Mnps: 217.3, Fen: 4k3/1P6/8/8/8/8/K7/8 w - - 0 1
  status: PASSED, time:    0ms, Mnps: inf, Fen: 8/P1k5/K7/8/8/8/8/8 w - - 0 1
  status: PASSED, time:    0ms, Mnps: inf, Fen: K1k5/8/P7/8/8/8/8/8 w - - 0 1
  status: PASSED, time:    1ms, Mnps: 567.6, Fen: 8/k1P5/8/1K6/8/8/8/8 w - - 0 1
  status: PASSED, time:    0ms, Mnps: inf, Fen: 8/8/2k5/5q2/5n2/8/5K2/8 b - - 0 1
  status: PASSED, time:    3ms, Mnps: 254.9, Fen: 4k3/8/8/8/8/8/8/4K2R w K - 0 1 
  status: PASSED, time:    4ms, Mnps: 211.7, Fen: 4k3/8/8/8/8/8/8/R3K3 w Q - 0 1 
  status: PASSED, time:    1ms, Mnps: 899.4, Fen: 4k2r/8/8/8/8/8/8/4K3 w k - 0 1 
  status: PASSED, time:    1ms, Mnps: 1001.5, Fen: r3k3/8/8/8/8/8/8/4K3 w q - 0 1 
  status: PASSED, time:    1ms, Mnps: 674.6, Fen: 8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1
  status: PASSED, time:   31ms, Mnps: 510.8, Fen: r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1
  status: PASSED, time:  157ms, Mnps: 572.9, Fen: rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8  
  status: PASSED, time:  257ms, Mnps: 638.4, Fen: r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10 
  status: PASSED, time:    9ms, Mnps: 424.6, Fen: 2K2r2/4P3/8/8/8/8/8/3k4 w - - 0 1
  status: PASSED, time:    0ms, Mnps: inf, Fen: K1k5/8/P7/8/8/8/8/8 w - - 0 1
  status: PASSED, time:    6ms, Mnps: 240.1, Fen: 8/8/1k6/2b5/2pP4/8/5K2/8 b - d3 0 1
  status: PASSED, time:    5ms, Mnps: 227.0, Fen: 3k4/3p4/8/K1P4r/8/8/8/8 b - - 0 1
  status: PASSED, time:  281ms, Mnps: 442.6, Fen: rbbknnqr/pppppppp/8/8/8/8/PPPPPPPP/RBBKNNQR w KQkq - 0 1
  status: PASSED, time:  329ms, Mnps: 446.4, Fen: bnrkrnqb/pppppppp/8/8/8/8/PPPPPPPP/BNRKRNQB w KQkq - 0 1
  status: PASSED, time:  222ms, Mnps: 441.2, Fen: nrbbqknr/pppppppp/8/8/8/8/PPPPPPPP/NRBBQKNR w KQkq - 0 1
  status: PASSED, time:  331ms, Mnps: 441.1, Fen: bnrbnkrq/pppppppp/8/8/8/8/PPPPPPPP/BNRBNKRQ w KQkq - 0 1
  status: PASSED, time:  284ms, Mnps: 445.4, Fen: rbknqnbr/pppppppp/8/8/8/8/PPPPPPPP/RBKNQNBR w KQkq - 0 1
  status: PASSED, time:  279ms, Mnps: 435.9, Fen: qbrnnkbr/pppppppp/8/8/8/8/PPPPPPPP/QBRNNKBR w KQkq - 0 1
  ```
</details>