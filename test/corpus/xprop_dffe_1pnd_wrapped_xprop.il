# Generated by Yosys 0.31+16 (git sha1 c7670b36d, clang 14.0.3 -fPIC -Os)
autoidx 58
attribute \src "wrapper.v:1.1-3.10"
attribute \top 1
module \uut
  wire $auto$rtlil.cc:2399:Not$15
  wire $auto$rtlil.cc:2399:Not$20
  wire $auto$rtlil.cc:2399:Not$43
  wire $auto$rtlil.cc:2399:Not$54
  wire $auto$rtlil.cc:2402:ReduceAnd$52
  wire $auto$rtlil.cc:2429:And$16
  wire $auto$rtlil.cc:2429:And$33
  wire $auto$rtlil.cc:2429:And$39
  wire $auto$rtlil.cc:2430:Or$19
  wire $auto$rtlil.cc:2430:Or$30
  wire $auto$rtlil.cc:2430:Or$32
  wire $auto$rtlil.cc:2430:Or$36
  wire $auto$rtlil.cc:2430:Or$38
  wire $auto$rtlil.cc:2430:Or$42
  wire width 32 $auto$rtlil.cc:2432:Xnor$48
  wire width 32 $auto$rtlil.cc:2432:Xnor$50
  wire $auto$rtlil.cc:2496:Mux$3
  wire $auto$xprop.cc:204:encoded$21
  wire $auto$xprop.cc:204:encoded$24
  wire $auto$xprop.cc:204:encoded$4
  wire $auto$xprop.cc:204:encoded$44
  wire $auto$xprop.cc:204:encoded$7
  wire $auto$xprop.cc:205:encoded$22
  wire $auto$xprop.cc:205:encoded$25
  wire $auto$xprop.cc:205:encoded$45
  wire $auto$xprop.cc:205:encoded$5
  wire $auto$xprop.cc:205:encoded$8
  wire $auto$xprop.cc:206:encoded$23
  wire $auto$xprop.cc:206:encoded$26
  wire $auto$xprop.cc:206:encoded$46
  wire $auto$xprop.cc:206:encoded$6
  attribute \init 1'1
  wire $auto$xprop.cc:206:encoded$9
  attribute \init 1'0
  wire $auto$xprop.cc:882:process_cell$11
  attribute \src "wrapper.v:2.5-2.59|uut.v:2.67-2.76"
  wire $flatten\uut.$nex$uut.v:2$1_Y
  attribute \src "wrapper.v:1.28-1.29"
  wire width 3 input 1 \A
  attribute \src "wrapper.v:1.44-1.45"
  wire output 2 \Y
  attribute \hdlname "uut CLK"
  attribute \src "wrapper.v:2.5-2.59|uut.v:1.24-1.27"
  wire \uut.CLK
  attribute \hdlname "uut D"
  attribute \src "wrapper.v:2.5-2.59|uut.v:1.57-1.58"
  wire \uut.D
  attribute \hdlname "uut EN"
  attribute \src "wrapper.v:2.5-2.59|uut.v:1.41-1.43"
  wire \uut.EN
  attribute \hdlname "uut Q"
  attribute \src "wrapper.v:2.5-2.59|uut.v:1.73-1.74"
  wire \uut.Q
  cell $bweqx $auto$xprop.cc:1080:encode_remaining$55
    parameter \WIDTH 3
    connect \A \A
    connect \B 3'000
    connect \Y { $auto$xprop.cc:204:encoded$21 $auto$xprop.cc:204:encoded$24 $auto$xprop.cc:204:encoded$44 }
  end
  cell $bweqx $auto$xprop.cc:1081:encode_remaining$56
    parameter \WIDTH 3
    connect \A \A
    connect \B 3'111
    connect \Y { $auto$xprop.cc:205:encoded$22 $auto$xprop.cc:205:encoded$25 $auto$xprop.cc:205:encoded$45 }
  end
  cell $bweqx $auto$xprop.cc:1082:encode_remaining$57
    parameter \WIDTH 3
    connect \A \A
    connect \B 3'x
    connect \Y { $auto$xprop.cc:206:encoded$23 $auto$xprop.cc:206:encoded$26 $auto$xprop.cc:206:encoded$46 }
  end
  attribute \xprop_decoder 1
  cell $bwmux $auto$xprop.cc:255:encoded$10
    parameter \WIDTH 1
    connect \A $auto$xprop.cc:205:encoded$8
    connect \B 1'x
    connect \S $auto$xprop.cc:206:encoded$9
    connect \Y \uut.Q
  end
  attribute \xprop_decoder 1
  cell $bwmux $auto$xprop.cc:255:encoded$27
    parameter \WIDTH 1
    connect \A $auto$xprop.cc:205:encoded$5
    connect \B 1'x
    connect \S $auto$xprop.cc:206:encoded$6
    connect \Y $auto$rtlil.cc:2496:Mux$3
  end
  cell $not $auto$xprop.cc:57:auto_0$17
    parameter \A_SIGNED 0
    parameter \A_WIDTH 1
    parameter \Y_WIDTH 1
    connect \A $auto$rtlil.cc:2430:Or$19
    connect \Y $auto$rtlil.cc:2399:Not$20
  end
  cell $or $auto$xprop.cc:57:auto_0$18
    parameter \A_SIGNED 0
    parameter \A_WIDTH 1
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 1
    connect \A $auto$xprop.cc:205:encoded$8
    connect \B $auto$xprop.cc:206:encoded$9
    connect \Y $auto$rtlil.cc:2430:Or$19
  end
  cell $not $auto$xprop.cc:59:auto_x$40
    parameter \A_SIGNED 0
    parameter \A_WIDTH 1
    parameter \Y_WIDTH 1
    connect \A $auto$rtlil.cc:2430:Or$42
    connect \Y $auto$rtlil.cc:2399:Not$43
  end
  cell $or $auto$xprop.cc:59:auto_x$41
    parameter \A_SIGNED 0
    parameter \A_WIDTH 1
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 1
    connect \A $auto$xprop.cc:204:encoded$4
    connect \B $auto$xprop.cc:205:encoded$5
    connect \Y $auto$rtlil.cc:2430:Or$42
  end
  cell $and $auto$xprop.cc:65:connect_1_under_x$13
    parameter \A_SIGNED 0
    parameter \A_WIDTH 1
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 1
    connect \A $auto$xprop.cc:882:process_cell$11
    connect \B $auto$rtlil.cc:2399:Not$15
    connect \Y $auto$rtlil.cc:2429:And$16
  end
  cell $not $auto$xprop.cc:65:connect_1_under_x$14
    parameter \A_SIGNED 0
    parameter \A_WIDTH 1
    parameter \Y_WIDTH 1
    connect \A $auto$xprop.cc:206:encoded$9
    connect \Y $auto$rtlil.cc:2399:Not$15
  end
  cell $xnor $auto$xprop.cc:679:process_cell$47
    parameter \A_SIGNED 0
    parameter \A_WIDTH 32
    parameter \B_SIGNED 0
    parameter \B_WIDTH 32
    parameter \Y_WIDTH 32
    connect \A { 31'1111111111111111111111111111111 $auto$xprop.cc:204:encoded$44 }
    connect \B 32'11111111111111111111111111111111
    connect \Y $auto$rtlil.cc:2432:Xnor$48
  end
  cell $xnor $auto$xprop.cc:680:process_cell$49
    parameter \A_SIGNED 0
    parameter \A_WIDTH 32
    parameter \B_SIGNED 0
    parameter \B_WIDTH 32
    parameter \Y_WIDTH 32
    connect \A { 31'0000000000000000000000000000000 $auto$xprop.cc:205:encoded$45 }
    connect \B 0
    connect \Y $auto$rtlil.cc:2432:Xnor$50
  end
  cell $reduce_and $auto$xprop.cc:682:process_cell$51
    parameter \A_SIGNED 0
    parameter \A_WIDTH 64
    parameter \Y_WIDTH 1
    connect \A { $auto$rtlil.cc:2432:Xnor$48 $auto$rtlil.cc:2432:Xnor$50 }
    connect \Y $auto$rtlil.cc:2402:ReduceAnd$52
  end
  cell $not $auto$xprop.cc:684:process_cell$53
    parameter \A_SIGNED 0
    parameter \A_WIDTH 1
    parameter \Y_WIDTH 1
    connect \A $auto$rtlil.cc:2402:ReduceAnd$52
    connect \Y $auto$rtlil.cc:2399:Not$54
  end
  cell $and $auto$xprop.cc:722:process_cell$28
    parameter \A_SIGNED 0
    parameter \A_WIDTH 1
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 1
    connect \A $auto$rtlil.cc:2430:Or$30
    connect \B $auto$rtlil.cc:2430:Or$32
    connect \Y $auto$rtlil.cc:2429:And$33
  end
  cell $or $auto$xprop.cc:723:process_cell$29
    parameter \A_SIGNED 0
    parameter \A_WIDTH 1
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 1
    connect \A $auto$xprop.cc:205:encoded$22
    connect \B $auto$xprop.cc:205:encoded$25
    connect \Y $auto$rtlil.cc:2430:Or$30
  end
  cell $or $auto$xprop.cc:724:process_cell$31
    parameter \A_SIGNED 0
    parameter \A_WIDTH 1
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 1
    connect \A $auto$xprop.cc:205:encoded$8
    connect \B $auto$xprop.cc:204:encoded$24
    connect \Y $auto$rtlil.cc:2430:Or$32
  end
  cell $and $auto$xprop.cc:725:process_cell$34
    parameter \A_SIGNED 0
    parameter \A_WIDTH 1
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 1
    connect \A $auto$rtlil.cc:2430:Or$36
    connect \B $auto$rtlil.cc:2430:Or$38
    connect \Y $auto$rtlil.cc:2429:And$39
  end
  cell $or $auto$xprop.cc:726:process_cell$35
    parameter \A_SIGNED 0
    parameter \A_WIDTH 1
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 1
    connect \A $auto$xprop.cc:204:encoded$21
    connect \B $auto$xprop.cc:205:encoded$25
    connect \Y $auto$rtlil.cc:2430:Or$36
  end
  cell $or $auto$xprop.cc:727:process_cell$37
    parameter \A_SIGNED 0
    parameter \A_WIDTH 1
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 1
    connect \A $auto$xprop.cc:204:encoded$7
    connect \B $auto$xprop.cc:204:encoded$24
    connect \Y $auto$rtlil.cc:2430:Or$38
  end
  attribute \hdlname "uut cell"
  attribute \module_not_derived 1
  attribute \src "wrapper.v:2.5-2.59|uut.v:2.56-2.101"
  cell $dff $auto$xprop.cc:889:process_cell$12
    parameter \CLK_POLARITY 1
    parameter \WIDTH 1
    connect \CLK $flatten\uut.$nex$uut.v:2$1_Y
    connect \D $auto$xprop.cc:206:encoded$6
    connect \Q $auto$xprop.cc:206:encoded$9
  end
  attribute \hdlname "uut cell"
  attribute \module_not_derived 1
  attribute \src "wrapper.v:2.5-2.59|uut.v:2.56-2.101"
  cell $dff \uut.cell
    parameter \CLK_POLARITY 1
    parameter \WIDTH 1
    connect \CLK $flatten\uut.$nex$uut.v:2$1_Y
    connect \D $auto$xprop.cc:205:encoded$5
    connect \Q $auto$xprop.cc:882:process_cell$11
  end
  connect \uut.CLK \A [0]
  connect \uut.D \A [2]
  connect \uut.EN \A [1]
  connect \Y \uut.Q
  connect $auto$xprop.cc:205:encoded$8 $auto$rtlil.cc:2429:And$16
  connect $auto$xprop.cc:204:encoded$7 $auto$rtlil.cc:2399:Not$20
  connect $auto$xprop.cc:205:encoded$5 $auto$rtlil.cc:2429:And$33
  connect $auto$xprop.cc:204:encoded$4 $auto$rtlil.cc:2429:And$39
  connect $auto$xprop.cc:206:encoded$6 $auto$rtlil.cc:2399:Not$43
  connect $flatten\uut.$nex$uut.v:2$1_Y $auto$rtlil.cc:2399:Not$54
end
