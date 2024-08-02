# Comment #1
autoidx 23
# Comment #2
attribute \src "wrapper.v:1.1-3.10"
# Comment #3
attribute \top 1
# Comment #4
module \uut
  # Another comment
  wire $auto$rtlil.cc:2399:Not$14
  # Another comment
  wire $auto$rtlil.cc:2399:Not$19
  # Another comment
  wire $auto$rtlil.cc:2403:ReduceOr$9
  # Another comment
  wire $auto$rtlil.cc:2404:ReduceXor$11
  # Another comment
  wire $auto$rtlil.cc:2429:And$15
  # Another comment
  wire $auto$rtlil.cc:2430:Or$18
  # Another comment
  wire width 3 $auto$xprop.cc:204:encoded$1
  # Another comment
  wire width 3 $auto$xprop.cc:204:encoded$4
  # Another comment
  wire width 3 $auto$xprop.cc:205:encoded$2
  # Another comment
  wire width 3 $auto$xprop.cc:205:encoded$5
  # Another comment
  wire width 3 $auto$xprop.cc:206:encoded$3
  # Another comment
  wire width 3 $auto$xprop.cc:206:encoded$6
  # Another comment
  attribute \src "wrapper.v:1.28-1.29"
  # Another comment
  wire width 3 input 1 \A
  # Another comment
  attribute \src "wrapper.v:1.44-1.45"
  # Another comment
  wire width 3 output 2 \Y
  # Another comment
  attribute \hdlname "uut A"
  # Another comment
  attribute \src "wrapper.v:2.5-2.32|uut.v:1.24-1.25"
  # Another comment
  wire width 3 \uut.A
  # Another comment
  attribute \hdlname "uut Y"
  # Another comment
  attribute \src "wrapper.v:2.5-2.32|uut.v:1.42-1.43"
  # Another comment
  wire width 3 \uut.Y
  # Another comment
  cell $bweqx $auto$xprop.cc:1080:encode_remaining$20
  # Another comment
    parameter \WIDTH 3
  # Another comment
    connect \A \A
  # Another comment
    connect \B 3'000
  # Another comment
    connect \Y $auto$xprop.cc:204:encoded$1
  # Another comment
  end
  # Another comment
  cell $bweqx $auto$xprop.cc:1081:encode_remaining$21
  # Another comment
    parameter \WIDTH 3
  # Another comment
    connect \A \A
  # Another comment
    connect \B 3'111
  # Another comment
    connect \Y $auto$xprop.cc:205:encoded$2
  # Another comment
  end
  # Another comment
  cell $bweqx $auto$xprop.cc:1082:encode_remaining$22
  # Another comment
    parameter \WIDTH 3
  # Another comment
    connect \A \A
  # Another comment
    connect \B 3'x
  # Another comment
    connect \Y $auto$xprop.cc:206:encoded$3
  # Another comment
  end
  # Another comment
  attribute \xprop_decoder 1
  # Another comment
  cell $bwmux $auto$xprop.cc:255:encoded$7
  # Another comment
    parameter \WIDTH 1
  # Another comment
    connect \A $auto$xprop.cc:205:encoded$5 [0]
  # Another comment
    connect \B 1'x
  # Another comment
    connect \S $auto$xprop.cc:206:encoded$6 [0]
  # Another comment
    connect \Y \uut.Y [0]
  # Another comment
  end
  # Another comment
  cell $not $auto$xprop.cc:57:auto_0$16
  # Another comment
    parameter \A_SIGNED 0
  # Another comment
    parameter \A_WIDTH 1
  # Another comment
    parameter \Y_WIDTH 1
  # Another comment
    connect \A $auto$rtlil.cc:2430:Or$18
  # Another comment
    connect \Y $auto$rtlil.cc:2399:Not$19
  # Another comment
  end
  # Another comment
  cell $or $auto$xprop.cc:57:auto_0$17
  # Another comment
    parameter \A_SIGNED 0
  # Another comment
    parameter \A_WIDTH 1
  # Another comment
    parameter \B_SIGNED 0
  # Another comment
    parameter \B_WIDTH 1
  # Another comment
    parameter \Y_WIDTH 1
  # Another comment
    connect \A $auto$xprop.cc:205:encoded$5 [0]
  # Another comment
    connect \B $auto$xprop.cc:206:encoded$6 [0]
  # Another comment
    connect \Y $auto$rtlil.cc:2430:Or$18
  # And here's a comment
  end
  # And here's a comment
  cell $reduce_or $auto$xprop.cc:585:process_cell$8
  # And here's a comment
    parameter \A_SIGNED 0
  # Another comment
    parameter \A_WIDTH 3
  # Another comment
    parameter \Y_WIDTH 1
  # Another comment
    connect \A $auto$xprop.cc:206:encoded$3
  # Another comment
    connect \Y $auto$rtlil.cc:2403:ReduceOr$9
  # Another comment
  end
  # Another comment
  cell $reduce_xor $auto$xprop.cc:586:process_cell$10
  # Another comment
    parameter \A_SIGNED 0
  # Another comment
    parameter \A_WIDTH 3
  # Another comment
    parameter \Y_WIDTH 1
  # Another comment
    connect \A $auto$xprop.cc:205:encoded$2
  # Another comment
    connect \Y $auto$rtlil.cc:2404:ReduceXor$11
  # Another comment
  end
  # Another comment
  cell $and $auto$xprop.cc:65:connect_1_under_x$12
  # Another comment
    parameter \A_SIGNED 0
  # Another comment
    parameter \A_WIDTH 1
  # Another comment
    parameter \B_SIGNED 0
  # Another comment
    parameter \B_WIDTH 1
  # Another comment
    parameter \Y_WIDTH 1
  # Another comment
    connect \A $auto$rtlil.cc:2404:ReduceXor$11
  # Another comment
    connect \B $auto$rtlil.cc:2399:Not$14
  # Another comment
    connect \Y $auto$rtlil.cc:2429:And$15
  # Another comment
  end
  # Another comment
  cell $not $auto$xprop.cc:65:connect_1_under_x$13
  # Another comment
    parameter \A_SIGNED 0
  # Another comment
    parameter \A_WIDTH 1
  # Another comment
    parameter \Y_WIDTH 1
  # Another comment
    connect \A $auto$xprop.cc:206:encoded$6 [0]
  # Another comment
    connect \Y $auto$rtlil.cc:2399:Not$14
  # Another comment
  end
  # Another comment
  connect \uut.A \A
  # Another comment
  connect \Y \uut.Y
  # Another comment
  connect \uut.Y [2:1] $auto$xprop.cc:205:encoded$5 [2:1]
  # Another comment
  connect $auto$xprop.cc:204:encoded$4 [2:1] 2'11
  # Another comment
  connect $auto$xprop.cc:205:encoded$5 [2:1] 2'00
  # Another comment
  connect $auto$xprop.cc:206:encoded$6 [2:1] 2'00
  # Another comment
  connect $auto$xprop.cc:206:encoded$6 [0] $auto$rtlil.cc:2403:ReduceOr$9
  # Yet another comment
  connect $auto$xprop.cc:205:encoded$5 [0] $auto$rtlil.cc:2429:And$15
  # Yet another comment
  connect $auto$xprop.cc:204:encoded$4 [0] $auto$rtlil.cc:2399:Not$19
# Yet another comment
end
# Final comment # 1
# Final comment # 2
# Final comment # 3
