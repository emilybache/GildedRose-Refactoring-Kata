package factored1;

public class TennisGame {

	public static final String[] POINTS = new String[]{"Love", "Fifteen", "Thirty", "Forty"}; 
	
	private int player2Score;
	private int player1Score;
	
	private String player1Name;
	private String player2Name;

	public TennisGame(String player1Name, String player2Name) {
		this.player1Name = player1Name;
		this.player2Name = player2Name;
	}

	public String getScore() {
		if (someoneHasWon())
			return "Win for " + winningPlayerName();

		if (isEndgame()) {
			if (pointsAreEven())
				return "Deuce";
			else
				return "Advantage " + winningPlayerName();
		} else {
			if (pointsAreEven())
				return POINTS[player1Score] + "-All";
			else  {
				return POINTS[player1Score] + "-" + POINTS[player2Score];
			} 
		}
	}

	public boolean someoneHasWon() {
		return isEndgame() && (player1Score-player2Score >= 2 || player2Score-player1Score >= 2);
	}

	private boolean pointsAreEven() {
		return player1Score == player2Score;
	}

	private boolean isEndgame() {
		return player1Score > 3 || player2Score > 3;
	}
	
	public String winningPlayerName() {
		if (player1Score > player2Score)
			return player1Name;
		else
			return player2Name;
	}

	public void wonPoint(String playerName) {
		if (playerName == player1Name)
			this.player1Score += 1;
		else
			this.player2Score += 1;
		
	}

}
