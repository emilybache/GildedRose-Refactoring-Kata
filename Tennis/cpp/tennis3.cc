#include <string>

const std::string tennis_score(int p1, int p2) {
    std::string s;
    std::string p1N = "player1";
    std::string p2N = "player2";
    if (p1 < 4 && p2 < 4 && !(p1 == 3 && p2 == 3)) {
        std::string p[4] = {"Love", "Fifteen", "Thirty", "Forty"}; 
        s = p[p1];
        return (p1 == p2) ? s + "-All" : s + "-" + p[p2];
    } else {
        if (p1 == p2)
            return "Deuce";
        s = p1 > p2 ? p1N : p2N;
        return ((p1-p2)*(p1-p2) == 1) ? "Advantage " + s : "Win for " + s;
    }
}