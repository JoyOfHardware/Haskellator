# Generated by Yosys 0.31+16 (git sha1 c7670b36d, clang 14.0.3 -fPIC -Os)
autoidx 53
attribute \src "wrapper.v:1.1-3.10"
attribute \top 1
module \uut
  wire $auto$rtlil.cc:2403:ReduceOr$19
  wire width 4 $auto$rtlil.cc:2429:And$18
  wire width 4 $auto$rtlil.cc:2444:Sub$17
  wire $auto$rtlil.cc:2496:Mux$21
  wire $auto$rtlil.cc:2496:Mux$23
  wire $auto$rtlil.cc:2496:Mux$25
  wire $auto$rtlil.cc:2496:Mux$27
  wire $auto$rtlil.cc:2496:Mux$29
  wire $auto$rtlil.cc:2496:Mux$31
  wire $auto$rtlil.cc:2496:Mux$33
  wire $auto$rtlil.cc:2496:Mux$35
  wire $auto$rtlil.cc:2496:Mux$37
  wire $auto$rtlil.cc:2496:Mux$39
  wire $auto$rtlil.cc:2496:Mux$41
  wire $auto$rtlil.cc:2496:Mux$43
  wire $auto$rtlil.cc:2496:Mux$45
  wire $auto$rtlil.cc:2496:Mux$47
  wire $auto$rtlil.cc:2496:Mux$49
  wire $auto$xprop.cc:204:encoded$1
  wire $auto$xprop.cc:204:encoded$10
  wire width 4 $auto$xprop.cc:204:encoded$4
  wire width 4 $auto$xprop.cc:204:encoded$7
  wire $auto$xprop.cc:205:encoded$11
  wire $auto$xprop.cc:205:encoded$2
  wire width 4 $auto$xprop.cc:205:encoded$5
  wire width 4 $auto$xprop.cc:205:encoded$8
  wire $auto$xprop.cc:206:encoded$12
  wire $auto$xprop.cc:206:encoded$3
  wire width 4 $auto$xprop.cc:206:encoded$6
  wire width 4 $auto$xprop.cc:206:encoded$9
  attribute \src "wrapper.v:1.28-1.29"
  wire width 9 input 1 \A
  attribute \src "wrapper.v:1.44-1.45"
  wire output 2 \Y
  attribute \hdlname "uut A"
  attribute \src "wrapper.v:2.5-2.56|uut.v:1.24-1.25"
  wire \uut.A
  attribute \hdlname "uut B"
  attribute \src "wrapper.v:2.5-2.56|uut.v:1.39-1.40"
  wire width 4 \uut.B
  attribute \hdlname "uut S"
  attribute \src "wrapper.v:2.5-2.56|uut.v:1.54-1.55"
  wire width 4 \uut.S
  attribute \hdlname "uut Y"
  attribute \src "wrapper.v:2.5-2.56|uut.v:1.70-1.71"
  wire \uut.Y
  cell $bweqx $auto$xprop.cc:1080:encode_remaining$50
    parameter \WIDTH 9
    connect \A \A
    connect \B 9'000000000
    connect \Y { $auto$xprop.cc:204:encoded$7 $auto$xprop.cc:204:encoded$4 $auto$xprop.cc:204:encoded$1 }
  end
  cell $bweqx $auto$xprop.cc:1081:encode_remaining$51
    parameter \WIDTH 9
    connect \A \A
    connect \B 9'111111111
    connect \Y { $auto$xprop.cc:205:encoded$8 $auto$xprop.cc:205:encoded$5 $auto$xprop.cc:205:encoded$2 }
  end
  cell $bweqx $auto$xprop.cc:1082:encode_remaining$52
    parameter \WIDTH 9
    connect \A \A
    connect \B 9'x
    connect \Y { $auto$xprop.cc:206:encoded$9 $auto$xprop.cc:206:encoded$6 $auto$xprop.cc:206:encoded$3 }
  end
  attribute \xprop_decoder 1
  cell $bwmux $auto$xprop.cc:255:encoded$13
    parameter \WIDTH 1
    connect \A $auto$xprop.cc:205:encoded$11
    connect \B 1'x
    connect \S $auto$xprop.cc:206:encoded$12
    connect \Y \uut.Y
  end
  cell $reduce_or $auto$xprop.cc:746:process_cell$14
    parameter \A_SIGNED 0
    parameter \A_WIDTH 8
    parameter \Y_WIDTH 1
    connect \A { $auto$xprop.cc:206:encoded$9 $auto$rtlil.cc:2429:And$18 }
    connect \Y $auto$rtlil.cc:2403:ReduceOr$19
  end
  cell $and $auto$xprop.cc:748:process_cell$15
    parameter \A_SIGNED 0
    parameter \A_WIDTH 4
    parameter \B_SIGNED 0
    parameter \B_WIDTH 4
    parameter \Y_WIDTH 4
    connect \A $auto$xprop.cc:205:encoded$8
    connect \B $auto$rtlil.cc:2444:Sub$17
    connect \Y $auto$rtlil.cc:2429:And$18
  end
  cell $sub $auto$xprop.cc:748:process_cell$16
    parameter \A_SIGNED 0
    parameter \A_WIDTH 4
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 4
    connect \A $auto$xprop.cc:205:encoded$8
    connect \B 1'1
    connect \Y $auto$rtlil.cc:2444:Sub$17
  end
  cell $mux $auto$xprop.cc:755:process_cell$20
    parameter \WIDTH 1
    connect \A $auto$xprop.cc:204:encoded$1
    connect \B $auto$xprop.cc:204:encoded$4 [0]
    connect \S $auto$xprop.cc:205:encoded$8 [0]
    connect \Y $auto$rtlil.cc:2496:Mux$21
  end
  cell $mux $auto$xprop.cc:755:process_cell$26
    parameter \WIDTH 1
    connect \A $auto$rtlil.cc:2496:Mux$21
    connect \B $auto$xprop.cc:204:encoded$4 [1]
    connect \S $auto$xprop.cc:205:encoded$8 [1]
    connect \Y $auto$rtlil.cc:2496:Mux$27
  end
  cell $mux $auto$xprop.cc:755:process_cell$32
    parameter \WIDTH 1
    connect \A $auto$rtlil.cc:2496:Mux$27
    connect \B $auto$xprop.cc:204:encoded$4 [2]
    connect \S $auto$xprop.cc:205:encoded$8 [2]
    connect \Y $auto$rtlil.cc:2496:Mux$33
  end
  cell $mux $auto$xprop.cc:755:process_cell$38
    parameter \WIDTH 1
    connect \A $auto$rtlil.cc:2496:Mux$33
    connect \B $auto$xprop.cc:204:encoded$4 [3]
    connect \S $auto$xprop.cc:205:encoded$8 [3]
    connect \Y $auto$rtlil.cc:2496:Mux$39
  end
  cell $mux $auto$xprop.cc:756:process_cell$22
    parameter \WIDTH 1
    connect \A $auto$xprop.cc:205:encoded$2
    connect \B $auto$xprop.cc:205:encoded$5 [0]
    connect \S $auto$xprop.cc:205:encoded$8 [0]
    connect \Y $auto$rtlil.cc:2496:Mux$23
  end
  cell $mux $auto$xprop.cc:756:process_cell$28
    parameter \WIDTH 1
    connect \A $auto$rtlil.cc:2496:Mux$23
    connect \B $auto$xprop.cc:205:encoded$5 [1]
    connect \S $auto$xprop.cc:205:encoded$8 [1]
    connect \Y $auto$rtlil.cc:2496:Mux$29
  end
  cell $mux $auto$xprop.cc:756:process_cell$34
    parameter \WIDTH 1
    connect \A $auto$rtlil.cc:2496:Mux$29
    connect \B $auto$xprop.cc:205:encoded$5 [2]
    connect \S $auto$xprop.cc:205:encoded$8 [2]
    connect \Y $auto$rtlil.cc:2496:Mux$35
  end
  cell $mux $auto$xprop.cc:756:process_cell$40
    parameter \WIDTH 1
    connect \A $auto$rtlil.cc:2496:Mux$35
    connect \B $auto$xprop.cc:205:encoded$5 [3]
    connect \S $auto$xprop.cc:205:encoded$8 [3]
    connect \Y $auto$rtlil.cc:2496:Mux$41
  end
  cell $mux $auto$xprop.cc:757:process_cell$24
    parameter \WIDTH 1
    connect \A $auto$xprop.cc:206:encoded$3
    connect \B $auto$xprop.cc:206:encoded$6 [0]
    connect \S $auto$xprop.cc:205:encoded$8 [0]
    connect \Y $auto$rtlil.cc:2496:Mux$25
  end
  cell $mux $auto$xprop.cc:757:process_cell$30
    parameter \WIDTH 1
    connect \A $auto$rtlil.cc:2496:Mux$25
    connect \B $auto$xprop.cc:206:encoded$6 [1]
    connect \S $auto$xprop.cc:205:encoded$8 [1]
    connect \Y $auto$rtlil.cc:2496:Mux$31
  end
  cell $mux $auto$xprop.cc:757:process_cell$36
    parameter \WIDTH 1
    connect \A $auto$rtlil.cc:2496:Mux$31
    connect \B $auto$xprop.cc:206:encoded$6 [2]
    connect \S $auto$xprop.cc:205:encoded$8 [2]
    connect \Y $auto$rtlil.cc:2496:Mux$37
  end
  cell $mux $auto$xprop.cc:757:process_cell$42
    parameter \WIDTH 1
    connect \A $auto$rtlil.cc:2496:Mux$37
    connect \B $auto$xprop.cc:206:encoded$6 [3]
    connect \S $auto$xprop.cc:205:encoded$8 [3]
    connect \Y $auto$rtlil.cc:2496:Mux$43
  end
  cell $mux $auto$xprop.cc:760:process_cell$44
    parameter \WIDTH 1
    connect \A $auto$rtlil.cc:2496:Mux$39
    connect \B 1'0
    connect \S $auto$rtlil.cc:2403:ReduceOr$19
    connect \Y $auto$rtlil.cc:2496:Mux$45
  end
  cell $mux $auto$xprop.cc:761:process_cell$46
    parameter \WIDTH 1
    connect \A $auto$rtlil.cc:2496:Mux$41
    connect \B 1'0
    connect \S $auto$rtlil.cc:2403:ReduceOr$19
    connect \Y $auto$rtlil.cc:2496:Mux$47
  end
  cell $mux $auto$xprop.cc:762:process_cell$48
    parameter \WIDTH 1
    connect \A $auto$rtlil.cc:2496:Mux$43
    connect \B 1'1
    connect \S $auto$rtlil.cc:2403:ReduceOr$19
    connect \Y $auto$rtlil.cc:2496:Mux$49
  end
  connect \uut.A \A [0]
  connect \uut.B \A [4:1]
  connect \uut.S \A [8:5]
  connect \Y \uut.Y
  connect $auto$xprop.cc:204:encoded$10 $auto$rtlil.cc:2496:Mux$45
  connect $auto$xprop.cc:205:encoded$11 $auto$rtlil.cc:2496:Mux$47
  connect $auto$xprop.cc:206:encoded$12 $auto$rtlil.cc:2496:Mux$49
end
