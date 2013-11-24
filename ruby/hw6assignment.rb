# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
  All_My_Pieces = All_Pieces + [[[[0, 0], [-1, 0], [1, 0], [2, 0], [-2,0]], # long(er) (only needs two)
                                [[0, 0], [0, -1], [0, 1], [0, 2],[0,-2]]],
                                rotations([[0, 0], [1, 0], [0, -1], [-1, -1], [-1,0]]), # Weird Z
                                rotations([[0, 0], [1, 0], [0, -1]]) # tiny L
                               ]

  # class method to choose the next piece
  def self.next_piece (board)
    MyPiece.new(All_My_Pieces.sample, board)
  end

  def self.cheat_piece (board)
    MyPiece.new([[0,0]], board)
  end
end

class MyBoard < Board
  # your enhancements here

  # rotates the current piece 180 degrees
  def rotate_180
    if !game_over? and @game.is_running?
      @current_block.move(0, 0, 1)
      @current_block.move(0, 0, 1)
    end
    draw
  end

  def cheat
    if score >= 100 and !@cheat
      @cheat = true
      @score -= 100
    end
  end

  def next_piece
    if @cheat
      @current_block = MyPiece.cheat_piece(self)
      @cheat = false
    else
      @current_block = MyPiece.next_piece(self)
    end
    @current_pos = nil
  end
end

class MyTetris < Tetris
  # your enhancements here
  def key_bindings
    super
    @root.bind('u' , proc {@board.rotate_180}) 
    @root.bind('c' , proc {@board.cheat}) 
  end

  # creates a canvas and the board that interacts with it
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

end


