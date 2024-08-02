attribute \generator "Amaranth"
attribute \src "/Users/yehowshuaimmanuel/git/Haskellator/fsm.py:19"
attribute \top 1
module \top

  attribute \src "/Users/yehowshuaimmanuel/git/Haskellator/fsm.py:21"
  attribute \init 5'00000
  wire width 5 \ctr

  attribute \src "/Library/Developer/CommandLineTools/Library/Frameworks/Python3.framework/Versions/3.9/lib/python3.9/contextlib.py:124"
  attribute \init 3'000
  wire width 3 \fsm_state

  attribute \src "/Users/yehowshuaimmanuel/git/Haskellator/fsm.py:29"
  attribute \init 3'000
  wire width 3 \bit

  attribute \src "/Users/yehowshuaimmanuel/git/Haskellator/fsm.py:22"
  wire width 1 \stb

  attribute \src "/Users/yehowshuaimmanuel/git/Haskellator/fsm.py:8"
  wire width 1 input 0  \i

  attribute \src "/Users/yehowshuaimmanuel/git/Haskellator/fsm.py:11"
  wire width 1 input 1  \ack

  attribute \src "/Users/yehowshuaimmanuel/git/Haskellator/.env/lib/python3.9/site-packages/amaranth/hdl/_ir.py:215"
  wire width 1 input 2  \clk

  attribute \src "/Users/yehowshuaimmanuel/git/Haskellator/.env/lib/python3.9/site-packages/amaranth/hdl/_ir.py:215"
  wire width 1 input 3  \rst

  attribute \src "/Users/yehowshuaimmanuel/git/Haskellator/fsm.py:9"
  attribute \init 8'00000000
  wire width 8 output 4  \data

  attribute \src "/Users/yehowshuaimmanuel/git/Haskellator/fsm.py:10"
  wire width 1 output 5  \rdy

  attribute \src "/Users/yehowshuaimmanuel/git/Haskellator/fsm.py:12"
  wire width 1 output 6  \err

  wire width 1 $1

  wire width 6 $2

  wire width 1 $3

  wire width 4 $4

  wire width 1 $5

  wire width 1 $6

  wire width 1 $7

  wire width 1 $8

  wire width 1 $9

  wire width 1 $10

  wire width 5 $11

  wire width 3 $12

  wire width 3 $13

  wire width 8 $14

  attribute \src "/Users/yehowshuaimmanuel/git/Haskellator/fsm.py:23"
  cell $eq $15
    parameter \A_SIGNED 0
    parameter \B_SIGNED 0
    parameter \A_WIDTH 5
    parameter \B_WIDTH 0
    parameter \Y_WIDTH 1
    connect \A \ctr [4:0]
    connect \B {  }
    connect \Y $1
  end

  attribute \src "/Users/yehowshuaimmanuel/git/Haskellator/fsm.py:27"
  cell $sub $16
    parameter \A_SIGNED 0
    parameter \B_SIGNED 0
    parameter \A_WIDTH 5
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 6
    connect \A \ctr [4:0]
    connect \B 1'1
    connect \Y $2
  end

  attribute \src "/Users/yehowshuaimmanuel/git/Haskellator/fsm.py:32"
  cell $not $17
    parameter \A_SIGNED 0
    parameter \A_WIDTH 1
    parameter \Y_WIDTH 1
    connect \A \i [0]
    connect \Y $3
  end

  attribute \src "/Users/yehowshuaimmanuel/git/Haskellator/fsm.py:41"
  cell $sub $18
    parameter \A_SIGNED 0
    parameter \B_SIGNED 0
    parameter \A_WIDTH 3
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 4
    connect \A \bit [2:0]
    connect \B 1'1
    connect \Y $4
  end

  attribute \src "/Users/yehowshuaimmanuel/git/Haskellator/fsm.py:44"
  cell $eq $19
    parameter \A_SIGNED 0
    parameter \B_SIGNED 0
    parameter \A_WIDTH 3
    parameter \B_WIDTH 0
    parameter \Y_WIDTH 1
    connect \A \bit [2:0]
    connect \B {  }
    connect \Y $5
  end

  attribute \src "/Users/yehowshuaimmanuel/git/Haskellator/fsm.py:23"
  cell $eq $20
    parameter \A_SIGNED 0
    parameter \B_SIGNED 0
    parameter \A_WIDTH 5
    parameter \B_WIDTH 0
    parameter \Y_WIDTH 1
    connect \A \ctr [4:0]
    connect \B {  }
    connect \Y $6
  end

  attribute \src "/Users/yehowshuaimmanuel/git/Haskellator/.env/lib/python3.9/site-packages/amaranth/hdl/_dsl.py:490"
  cell $eq $21
    parameter \A_SIGNED 0
    parameter \B_SIGNED 0
    parameter \A_WIDTH 3
    parameter \B_WIDTH 0
    parameter \Y_WIDTH 1
    connect \A \fsm_state [2:0]
    connect \B {  }
    connect \Y $7
  end

  attribute \src "/Users/yehowshuaimmanuel/git/Haskellator/.env/lib/python3.9/site-packages/amaranth/hdl/_dsl.py:490"
  cell $eq $22
    parameter \A_SIGNED 0
    parameter \B_SIGNED 0
    parameter \A_WIDTH 3
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 1
    connect \A \fsm_state [2:0]
    connect \B 1'1
    connect \Y $8
  end

  attribute \src "/Users/yehowshuaimmanuel/git/Haskellator/.env/lib/python3.9/site-packages/amaranth/hdl/_dsl.py:490"
  cell $eq $23
    parameter \A_SIGNED 0
    parameter \B_SIGNED 0
    parameter \A_WIDTH 3
    parameter \B_WIDTH 2
    parameter \Y_WIDTH 1
    connect \A \fsm_state [2:0]
    connect \B 2'10
    connect \Y $9
  end

  attribute \src "/Users/yehowshuaimmanuel/git/Haskellator/.env/lib/python3.9/site-packages/amaranth/hdl/_dsl.py:490"
  cell $eq $24
    parameter \A_SIGNED 0
    parameter \B_SIGNED 0
    parameter \A_WIDTH 3
    parameter \B_WIDTH 2
    parameter \Y_WIDTH 1
    connect \A \fsm_state [2:0]
    connect \B 2'11
    connect \Y $10
  end

  attribute \src "/Users/yehowshuaimmanuel/git/Haskellator/.env/lib/python3.9/site-packages/amaranth/hdl/_dsl.py:490"
  cell $eq $25
    parameter \A_SIGNED 0
    parameter \B_SIGNED 0
    parameter \A_WIDTH 3
    parameter \B_WIDTH 3
    parameter \Y_WIDTH 1
    connect \A \fsm_state [2:0]
    connect \B 3'100
    connect \Y \err
  end

  attribute \src "/Users/yehowshuaimmanuel/git/Haskellator/fsm.py:21"
  process $26
    assign $11 [4:0] \ctr [4:0]
    switch $1 [0]
      case 1'1
        assign $11 [4:0] 5'10011
      case
        assign $11 [4:0] $2 [4:0]
    end
    switch \fsm_state [2:0]
      case 3'000
        switch $3 [0]
          case 1'1
            assign $11 [4:0] 5'01010
        end
      case 3'001
      case 3'010
      case 3'011
      case 3'100
    end
    switch \rst [0]
      case 1'1
        assign $11 [4:0] 5'00000
    end
  end

  attribute \src "/Users/yehowshuaimmanuel/git/Haskellator/fsm.py:21"
  cell $dff $27
    parameter \WIDTH 5
    parameter \CLK_POLARITY 1
    connect \D $11 [4:0]
    connect \CLK \clk [0]
    connect \Q \ctr
  end

  attribute \src "/Library/Developer/CommandLineTools/Library/Frameworks/Python3.framework/Versions/3.9/lib/python3.9/contextlib.py:124"
  process $28
    assign $12 [2:0] \fsm_state [2:0]
    switch \fsm_state [2:0]
      case 3'000
        switch $3 [0]
          case 1'1
            assign $12 [2:0] 3'001
        end
      case 3'001
        switch \stb [0]
          case 1'1
            switch $5 [0]
              case 1'1
                assign $12 [2:0] 3'010
            end
        end
      case 3'010
        switch \stb [0]
          case 1'1
            switch \i [0]
              case 1'1
                assign $12 [2:0] 3'011
              case
                assign $12 [2:0] 3'100
            end
        end
      case 3'011
        switch \ack [0]
          case 1'1
            assign $12 [2:0] 3'000
        end
      case 3'100
    end
    switch \rst [0]
      case 1'1
        assign $12 [2:0] 3'000
    end
  end

  attribute \src "/Library/Developer/CommandLineTools/Library/Frameworks/Python3.framework/Versions/3.9/lib/python3.9/contextlib.py:124"
  cell $dff $29
    parameter \WIDTH 3
    parameter \CLK_POLARITY 1
    connect \D $12 [2:0]
    connect \CLK \clk [0]
    connect \Q \fsm_state
  end

  attribute \src "/Users/yehowshuaimmanuel/git/Haskellator/fsm.py:29"
  process $30
    assign $13 [2:0] \bit [2:0]
    switch \fsm_state [2:0]
      case 3'000
        switch $3 [0]
          case 1'1
            assign $13 [2:0] 3'111
        end
      case 3'001
        switch \stb [0]
          case 1'1
            assign $13 [2:0] $4 [2:0]
        end
      case 3'010
      case 3'011
      case 3'100
    end
    switch \rst [0]
      case 1'1
        assign $13 [2:0] 3'000
    end
  end

  attribute \src "/Users/yehowshuaimmanuel/git/Haskellator/fsm.py:29"
  cell $dff $31
    parameter \WIDTH 3
    parameter \CLK_POLARITY 1
    connect \D $13 [2:0]
    connect \CLK \clk [0]
    connect \Q \bit
  end

  attribute \src "/Users/yehowshuaimmanuel/git/Haskellator/fsm.py:9"
  process $32
    assign $14 [7:0] \data [7:0]
    switch \fsm_state [2:0]
      case 3'000
      case 3'001
        switch \stb [0]
          case 1'1
            assign $14 [7:0] { \data [6:0] \i [0] }
        end
      case 3'010
      case 3'011
      case 3'100
    end
    switch \rst [0]
      case 1'1
        assign $14 [7:0] 8'00000000
    end
  end

  attribute \src "/Users/yehowshuaimmanuel/git/Haskellator/fsm.py:9"
  cell $dff $33
    parameter \WIDTH 8
    parameter \CLK_POLARITY 1
    connect \D $14 [7:0]
    connect \CLK \clk [0]
    connect \Q \data
  end

  attribute \src "/Users/yehowshuaimmanuel/git/Haskellator/fsm.py:22"
  process $34
    assign \stb [0] 1'0
    switch $6 [0]
      case 1'1
        assign \stb [0] 1'1
      case
    end
  end

  attribute \src "/Users/yehowshuaimmanuel/git/Haskellator/fsm.py:10"
  process $35
    assign \rdy [0] 1'0
    switch \fsm_state [2:0]
      case 3'000
      case 3'001
      case 3'010
      case 3'011
        assign \rdy [0] 1'1
      case 3'100
    end
  end

end


