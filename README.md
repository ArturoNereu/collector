collector
=========
This is an ERLANG implementation of a modified version of the card game Collector found here: http://www.pagat.com/invented/collector.html

Many instances of the game can run on the same machine concurrently.

The Collector Rules (from professor Ariel Ortiz, http://webcem01.cem.itesm.mx:8005/apps/s201011/tc2006/activity_collector/)

This game is for 2 to 4 players.

The object of the game is to collect a full set of 13 cards (King to Ace) — the suit of the cards doesn't matter. Each player starts off with 5 cards dealt from a shuffled 52-card deck. The remaining cards are placed face down as a drawing stock.

The game is played as follows:

Each player selects simultaneously one card from their hand and places it face down so that nobody else sees it.
All the placed cards are turned face up at the same time.
Each player has the choice of drawing two (unknown) cards from the stock, or taking one of the face up cards played by the other players.
The played cards that were not chosen are added to the bottom of the deck.
This process is repeated until someone has a complete set of 13 cards of different ranks. The first player who correctly claims to have a complete collection wins.

Players takes place in an equitative way. So, assuming A, B, C, D, are players, in the first round the choosing order should be A, B, C, D. In the second round the order should be B, C, D, A, and so on.

Example: Bill and Chris start off by picking up 5 cards each. Bill has [1, 3, 5, 4, King], so he decides to dispose of the King. Chris has [2, 4, 6, 6, 1]; since he already has one 6 he decides to discard the other. Neither one of them picks the other's card to add to their hand, so they put the King and the 6 at the bottom of the deck and they both draw two cards. Bill now has [1, 3, 5, 4, 6, 7], and he decides to discard his 3. Chris has [2, 4, 6, 1, 5, 3] and doesn't want to get rid of any of his cards, but he has to. He plays the 3 and takes Bill's 3. Bill puts Chris's three at the bottom of the deck and draws two cards, but Chris doesn't since he took Bill's card. They keep playing till someone has [Ace, 2, 3, 4, 5, 6, 7, 8, 9, 10, Jack, Queen, King].

The client program allows the player to see at every moment all his cards, as well as the number of cards held by the other players.