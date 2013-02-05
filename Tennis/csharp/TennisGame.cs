using System;

namespace Tennis
{
  public interface TennisGame
  {
    void WonPoint (string playerName);
    string GetScore ();

  }
}

