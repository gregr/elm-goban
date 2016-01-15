# Tools for Go/Igo/Weiqi/Baduk using Elm

## TODO

- board state
    - Group
        - set of member coordinates
        - possibly also track liberties and neighboring opponent coords
        - uniquely identify via uppermost-leftmost member for fast comparison?
    - Board = Coord => (Color, Group)
    - neighboring coords
    - neighboring group identification
    - stone insertion
        - liberty reduction
        - group merging
        - capture resolution

- variation trees
    - current position in tree tracked via zipper
    - Position
        - current board state
            - arbitrary edits may be made in each position
                - no required relationship with predecessor state
        - board/stone/group annotations
            - commentary
            - labels
            - position of unusual illegal moves (ko)
            - position of the last move played, etc.
            - whose turn is it?
    - simple whole-variation ko recognition
        - check for identical positions in a particular variation
            - Zobrist hashing?
    - insertion/deletion for cutting/grafting

- visualization
    - board and stones (possibly only a portion of a full board)
        - coordinate axis labels
        - location/stone labels, shadings, highlights
        - move/capture/arbitrary transitions, possibly animations
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

- sgf processing
