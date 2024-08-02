attribute \generator "Amaranth"
attribute \src "/Users/yehowshuaimmanuel/git/Haskellator1/fsm.py:19"
attribute \top 1
module \top



  attribute \src "/Users/yehowshuaimmanuel/git/Haskellator1/fsm.py:21"
  process $26
    assign $11 [4:0] \ctr [4:0]
    switch $1 [0]
      case 1'1
        assign $11 [4:0] 5'10011
      case
        assign $11 [4:0] $2 [4:0]
    end
  end


end
