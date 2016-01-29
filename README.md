# Tools for Go/Igo/Weiqi/Baduk using Elm

Try the [interactive example](http://www.gregrosenblatt.com/elm-goban/)!  Currently, the example implements alternating play, navigating through variations, and the basic capturing rules.  It allows self-capture and does not detect Ko/Pae.

## TODO

- variation trees
    - simple whole-variation ko recognition
        - check for identical positions in a particular variation
            - Zobrist hashing?

- visualization
    - board and stones (possibly only a portion of a full board)
        - coordinate axis labels
        - location/stone labels, shadings, highlights
        - move/capture/arbitrary transitions, possibly animations
    - metadata
        - commentary
        - labels
        - position of unusual illegal moves (ko)
        - position of the last move played, etc.
        - whose turn is it?
    - variation tree
        - horizontal layout of position icons with edges to prev/next position
        - icons summarize information about a position
           - e.g. color that played, presence of edits/annotations, etc.
    - commentary

- UI
    - mark/select/label stone/group
    - edit position
    - play move
    - pass
    - select position in variation tree
    - advance to move
        - select coord and traverse game tree until that coord is played
    - cutting/grafting of variation tree

- misc
    - clean up module interfaces
    - docs
    - exposed-modules
