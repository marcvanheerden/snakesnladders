#include <RcppArmadilloExtensions/sample.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp ;

// [[Rcpp::export]]
NumericVector move(NumericVector dieOutcomes,
                   NumericVector board,
                   NumericVector maxPosition,
                   NumericVector startPosition
) { // Simulate a single move in a game
    NumericVector roll = RcppArmadillo::sample(dieOutcomes, 1, TRUE);
    // Rcout << "Start: " << startPosition << "\n";
    // Rcout << "Roll: " << roll << "\n";
    NumericVector midPosition = all(startPosition + roll < maxPosition).is_true() ? startPosition + roll : maxPosition;
    // Rcout << "Mid: " << midPosition << "\n";
    NumericVector endPosition = board[midPosition]; // calculate change in position due to a snake or a ladder
    // Rcout << "End: " << endPosition << "\n";
    return endPosition;
}

// [[Rcpp::export]]
NumericVector singlePlayer(NumericVector dieOutcomes,
                           NumericVector board,
                           NumericVector maxPosition,
                           int maxMoves
) { // Simulate a game for a single player up to a maximum number of moves
    NumericVector position = NumericVector::create(0);
    NumericVector moves = NumericVector::create(0);
    NumericVector one = NumericVector::create(1);
    
    for (int i = 1; i <= maxMoves; i++) {
        position = move(dieOutcomes, board, maxPosition, position);
        // Rcout << "Position: " << position << "\n";
        moves += one;
        if (all(position >= maxPosition).is_true()) {
            break;
        }
    }
    return moves;
}

// [[Rcpp::export]]
NumericVector batchSinglePlayer(NumericVector dieOutcomes,
                                NumericVector board,
                                NumericVector maxPosition,
                                int maxMoves,
                                int games
) { // Simulate a number of games for a single player and return the number of turns per game
    NumericVector turns(games);

    for (int i = 0; i < games; i++ ) {
        turns[i] = singlePlayer(dieOutcomes, board, maxPosition, maxMoves)[0]; 
    }
    
    return turns;
}

// [[Rcpp::export]]
NumericVector multiPlayer(NumericVector dieOutcomes,
                                NumericVector board,
                                NumericVector maxPosition,
                                int maxMoves,
                                int players,
                                bool allFinish
) { // Simulate turns taken for a multiplayer game
    NumericVector turns = NumericVector::create(0);
    
    NumericVector allTurns = batchSinglePlayer(dieOutcomes, board, maxPosition, maxMoves, players);
    
    if (allFinish) {
        turns[0] = sum(allTurns);
    } else {
        turns[0] = min(allTurns)*players; // not exactly, some approximation here with upward bias
    }
    
    return turns;
}


// [[Rcpp::export]]
NumericVector batchMultiPlayer(NumericVector dieOutcomes,
                                NumericVector board,
                                NumericVector maxPosition,
                                int maxMoves,
                                int players,
                                bool allFinish,
                                int games
) { // Simulate a number of multiplayer games and return the number of turns per game
    NumericVector turns(games);
    
    for (int i = 0; i < games; i++ ) {
        turns[i] = multiPlayer(dieOutcomes, board, maxPosition, maxMoves, players, allFinish)[0]; 
    }
    
    return turns;
}