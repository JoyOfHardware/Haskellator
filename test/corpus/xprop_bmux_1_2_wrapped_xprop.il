# Generated by Yosys 0.31+16 (git sha1 c7670b36d, clang 14.0.3 -fPIC -Os)
autoidx 87
attribute \src "wrapper.v:1.1-3.10"
attribute \top 1
module \uut
  wire width 2 $auto$bmuxmap.cc:84:execute$1
  wire $auto$bmuxmap.cc:84:execute$4
  wire $auto$rtlil.cc:2399:Not$34
  wire $auto$rtlil.cc:2399:Not$60
  wire $auto$rtlil.cc:2399:Not$83
  wire $auto$rtlil.cc:2429:And$24
  wire $auto$rtlil.cc:2429:And$30
  wire $auto$rtlil.cc:2429:And$50
  wire $auto$rtlil.cc:2429:And$56
  wire $auto$rtlil.cc:2429:And$73
  wire $auto$rtlil.cc:2429:And$79
  wire $auto$rtlil.cc:2430:Or$21
  wire $auto$rtlil.cc:2430:Or$23
  wire $auto$rtlil.cc:2430:Or$27
  wire $auto$rtlil.cc:2430:Or$29
  wire $auto$rtlil.cc:2430:Or$33
  wire $auto$rtlil.cc:2430:Or$47
  wire $auto$rtlil.cc:2430:Or$49
  wire $auto$rtlil.cc:2430:Or$53
  wire $auto$rtlil.cc:2430:Or$55
  wire $auto$rtlil.cc:2430:Or$59
  wire $auto$rtlil.cc:2430:Or$70
  wire $auto$rtlil.cc:2430:Or$72
  wire $auto$rtlil.cc:2430:Or$76
  wire $auto$rtlil.cc:2430:Or$78
  wire $auto$rtlil.cc:2430:Or$82
  wire $auto$xprop.cc:204:encoded$12
  wire $auto$xprop.cc:204:encoded$15
  wire $auto$xprop.cc:204:encoded$35
  wire $auto$xprop.cc:204:encoded$38
  wire $auto$xprop.cc:204:encoded$41
  wire $auto$xprop.cc:204:encoded$6
  wire $auto$xprop.cc:204:encoded$61
  wire $auto$xprop.cc:204:encoded$64
  wire $auto$xprop.cc:204:encoded$9
  wire $auto$xprop.cc:205:encoded$10
  wire $auto$xprop.cc:205:encoded$13
  wire $auto$xprop.cc:205:encoded$16
  wire $auto$xprop.cc:205:encoded$36
  wire $auto$xprop.cc:205:encoded$39
  wire $auto$xprop.cc:205:encoded$42
  wire $auto$xprop.cc:205:encoded$62
  wire $auto$xprop.cc:205:encoded$65
  wire $auto$xprop.cc:205:encoded$7
  wire $auto$xprop.cc:206:encoded$11
  wire $auto$xprop.cc:206:encoded$14
  wire $auto$xprop.cc:206:encoded$17
  wire $auto$xprop.cc:206:encoded$37
  wire $auto$xprop.cc:206:encoded$40
  wire $auto$xprop.cc:206:encoded$43
  wire $auto$xprop.cc:206:encoded$63
  wire $auto$xprop.cc:206:encoded$66
  wire $auto$xprop.cc:206:encoded$8
  attribute \src "wrapper.v:1.28-1.29"
  wire width 6 input 1 \A
  attribute \src "wrapper.v:1.44-1.45"
  wire output 2 \Y
  attribute \hdlname "uut A"
  attribute \src "wrapper.v:2.5-2.44|uut.v:1.24-1.25"
  wire width 4 \uut.A
  attribute \hdlname "uut S"
  attribute \src "wrapper.v:2.5-2.44|uut.v:1.39-1.40"
  wire width 2 \uut.S
  attribute \hdlname "uut Y"
  attribute \src "wrapper.v:2.5-2.44|uut.v:1.55-1.56"
  wire \uut.Y
  cell $bweqx $auto$xprop.cc:1080:encode_remaining$84
    parameter \WIDTH 6
    connect \A \A
    connect \B 6'000000
    connect \Y { $auto$xprop.cc:204:encoded$61 $auto$xprop.cc:204:encoded$12 $auto$xprop.cc:204:encoded$9 $auto$xprop.cc:204:encoded$6 $auto$xprop.cc:204:encoded$38 $auto$xprop.cc:204:encoded$35 }
  end
  cell $bweqx $auto$xprop.cc:1081:encode_remaining$85
    parameter \WIDTH 6
    connect \A \A
    connect \B 6'111111
    connect \Y { $auto$xprop.cc:205:encoded$62 $auto$xprop.cc:205:encoded$13 $auto$xprop.cc:205:encoded$10 $auto$xprop.cc:205:encoded$7 $auto$xprop.cc:205:encoded$39 $auto$xprop.cc:205:encoded$36 }
  end
  cell $bweqx $auto$xprop.cc:1082:encode_remaining$86
    parameter \WIDTH 6
    connect \A \A
    connect \B 6'x
    connect \Y { $auto$xprop.cc:206:encoded$63 $auto$xprop.cc:206:encoded$14 $auto$xprop.cc:206:encoded$11 $auto$xprop.cc:206:encoded$8 $auto$xprop.cc:206:encoded$40 $auto$xprop.cc:206:encoded$37 }
  end
  attribute \xprop_decoder 1
  cell $bwmux $auto$xprop.cc:255:encoded$18
    parameter \WIDTH 1
    connect \A $auto$xprop.cc:205:encoded$16
    connect \B 1'x
    connect \S $auto$xprop.cc:206:encoded$17
    connect \Y $auto$bmuxmap.cc:84:execute$1 [1]
  end
  attribute \xprop_decoder 1
  cell $bwmux $auto$xprop.cc:255:encoded$44
    parameter \WIDTH 1
    connect \A $auto$xprop.cc:205:encoded$42
    connect \B 1'x
    connect \S $auto$xprop.cc:206:encoded$43
    connect \Y $auto$bmuxmap.cc:84:execute$1 [0]
  end
  attribute \xprop_decoder 1
  cell $bwmux $auto$xprop.cc:255:encoded$67
    parameter \WIDTH 1
    connect \A $auto$xprop.cc:205:encoded$65
    connect \B 1'x
    connect \S $auto$xprop.cc:206:encoded$66
    connect \Y $auto$bmuxmap.cc:84:execute$4
  end
  cell $not $auto$xprop.cc:59:auto_x$31
    parameter \A_SIGNED 0
    parameter \A_WIDTH 1
    parameter \Y_WIDTH 1
    connect \A $auto$rtlil.cc:2430:Or$33
    connect \Y $auto$rtlil.cc:2399:Not$34
  end
  cell $or $auto$xprop.cc:59:auto_x$32
    parameter \A_SIGNED 0
    parameter \A_WIDTH 1
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 1
    connect \A $auto$xprop.cc:204:encoded$15
    connect \B $auto$xprop.cc:205:encoded$16
    connect \Y $auto$rtlil.cc:2430:Or$33
  end
  cell $not $auto$xprop.cc:59:auto_x$57
    parameter \A_SIGNED 0
    parameter \A_WIDTH 1
    parameter \Y_WIDTH 1
    connect \A $auto$rtlil.cc:2430:Or$59
    connect \Y $auto$rtlil.cc:2399:Not$60
  end
  cell $or $auto$xprop.cc:59:auto_x$58
    parameter \A_SIGNED 0
    parameter \A_WIDTH 1
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 1
    connect \A $auto$xprop.cc:204:encoded$41
    connect \B $auto$xprop.cc:205:encoded$42
    connect \Y $auto$rtlil.cc:2430:Or$59
  end
  cell $not $auto$xprop.cc:59:auto_x$80
    parameter \A_SIGNED 0
    parameter \A_WIDTH 1
    parameter \Y_WIDTH 1
    connect \A $auto$rtlil.cc:2430:Or$82
    connect \Y $auto$rtlil.cc:2399:Not$83
  end
  cell $or $auto$xprop.cc:59:auto_x$81
    parameter \A_SIGNED 0
    parameter \A_WIDTH 1
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 1
    connect \A $auto$xprop.cc:204:encoded$64
    connect \B $auto$xprop.cc:205:encoded$65
    connect \Y $auto$rtlil.cc:2430:Or$82
  end
  cell $and $auto$xprop.cc:722:process_cell$19
    parameter \A_SIGNED 0
    parameter \A_WIDTH 1
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 1
    connect \A $auto$rtlil.cc:2430:Or$21
    connect \B $auto$rtlil.cc:2430:Or$23
    connect \Y $auto$rtlil.cc:2429:And$24
  end
  cell $and $auto$xprop.cc:722:process_cell$45
    parameter \A_SIGNED 0
    parameter \A_WIDTH 1
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 1
    connect \A $auto$rtlil.cc:2430:Or$47
    connect \B $auto$rtlil.cc:2430:Or$49
    connect \Y $auto$rtlil.cc:2429:And$50
  end
  cell $and $auto$xprop.cc:722:process_cell$68
    parameter \A_SIGNED 0
    parameter \A_WIDTH 1
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 1
    connect \A $auto$rtlil.cc:2430:Or$70
    connect \B $auto$rtlil.cc:2430:Or$72
    connect \Y $auto$rtlil.cc:2429:And$73
  end
  cell $or $auto$xprop.cc:723:process_cell$20
    parameter \A_SIGNED 0
    parameter \A_WIDTH 1
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 1
    connect \A $auto$xprop.cc:205:encoded$7
    connect \B $auto$xprop.cc:205:encoded$13
    connect \Y $auto$rtlil.cc:2430:Or$21
  end
  cell $or $auto$xprop.cc:723:process_cell$46
    parameter \A_SIGNED 0
    parameter \A_WIDTH 1
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 1
    connect \A $auto$xprop.cc:205:encoded$36
    connect \B $auto$xprop.cc:205:encoded$13
    connect \Y $auto$rtlil.cc:2430:Or$47
  end
  cell $or $auto$xprop.cc:723:process_cell$69
    parameter \A_SIGNED 0
    parameter \A_WIDTH 1
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 1
    connect \A $auto$xprop.cc:205:encoded$42
    connect \B $auto$xprop.cc:205:encoded$62
    connect \Y $auto$rtlil.cc:2430:Or$70
  end
  cell $or $auto$xprop.cc:724:process_cell$22
    parameter \A_SIGNED 0
    parameter \A_WIDTH 1
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 1
    connect \A $auto$xprop.cc:205:encoded$10
    connect \B $auto$xprop.cc:204:encoded$12
    connect \Y $auto$rtlil.cc:2430:Or$23
  end
  cell $or $auto$xprop.cc:724:process_cell$48
    parameter \A_SIGNED 0
    parameter \A_WIDTH 1
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 1
    connect \A $auto$xprop.cc:205:encoded$39
    connect \B $auto$xprop.cc:204:encoded$12
    connect \Y $auto$rtlil.cc:2430:Or$49
  end
  cell $or $auto$xprop.cc:724:process_cell$71
    parameter \A_SIGNED 0
    parameter \A_WIDTH 1
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 1
    connect \A $auto$xprop.cc:205:encoded$16
    connect \B $auto$xprop.cc:204:encoded$61
    connect \Y $auto$rtlil.cc:2430:Or$72
  end
  cell $and $auto$xprop.cc:725:process_cell$25
    parameter \A_SIGNED 0
    parameter \A_WIDTH 1
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 1
    connect \A $auto$rtlil.cc:2430:Or$27
    connect \B $auto$rtlil.cc:2430:Or$29
    connect \Y $auto$rtlil.cc:2429:And$30
  end
  cell $and $auto$xprop.cc:725:process_cell$51
    parameter \A_SIGNED 0
    parameter \A_WIDTH 1
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 1
    connect \A $auto$rtlil.cc:2430:Or$53
    connect \B $auto$rtlil.cc:2430:Or$55
    connect \Y $auto$rtlil.cc:2429:And$56
  end
  cell $and $auto$xprop.cc:725:process_cell$74
    parameter \A_SIGNED 0
    parameter \A_WIDTH 1
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 1
    connect \A $auto$rtlil.cc:2430:Or$76
    connect \B $auto$rtlil.cc:2430:Or$78
    connect \Y $auto$rtlil.cc:2429:And$79
  end
  cell $or $auto$xprop.cc:726:process_cell$26
    parameter \A_SIGNED 0
    parameter \A_WIDTH 1
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 1
    connect \A $auto$xprop.cc:204:encoded$6
    connect \B $auto$xprop.cc:205:encoded$13
    connect \Y $auto$rtlil.cc:2430:Or$27
  end
  cell $or $auto$xprop.cc:726:process_cell$52
    parameter \A_SIGNED 0
    parameter \A_WIDTH 1
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 1
    connect \A $auto$xprop.cc:204:encoded$35
    connect \B $auto$xprop.cc:205:encoded$13
    connect \Y $auto$rtlil.cc:2430:Or$53
  end
  cell $or $auto$xprop.cc:726:process_cell$75
    parameter \A_SIGNED 0
    parameter \A_WIDTH 1
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 1
    connect \A $auto$xprop.cc:204:encoded$41
    connect \B $auto$xprop.cc:205:encoded$62
    connect \Y $auto$rtlil.cc:2430:Or$76
  end
  cell $or $auto$xprop.cc:727:process_cell$28
    parameter \A_SIGNED 0
    parameter \A_WIDTH 1
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 1
    connect \A $auto$xprop.cc:204:encoded$9
    connect \B $auto$xprop.cc:204:encoded$12
    connect \Y $auto$rtlil.cc:2430:Or$29
  end
  cell $or $auto$xprop.cc:727:process_cell$54
    parameter \A_SIGNED 0
    parameter \A_WIDTH 1
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 1
    connect \A $auto$xprop.cc:204:encoded$38
    connect \B $auto$xprop.cc:204:encoded$12
    connect \Y $auto$rtlil.cc:2430:Or$55
  end
  cell $or $auto$xprop.cc:727:process_cell$77
    parameter \A_SIGNED 0
    parameter \A_WIDTH 1
    parameter \B_SIGNED 0
    parameter \B_WIDTH 1
    parameter \Y_WIDTH 1
    connect \A $auto$xprop.cc:204:encoded$15
    connect \B $auto$xprop.cc:204:encoded$61
    connect \Y $auto$rtlil.cc:2430:Or$78
  end
  connect \uut.A \A [3:0]
  connect \uut.S \A [5:4]
  connect \Y \uut.Y
  connect \uut.Y $auto$bmuxmap.cc:84:execute$4
  connect $auto$xprop.cc:205:encoded$16 $auto$rtlil.cc:2429:And$24
  connect $auto$xprop.cc:204:encoded$15 $auto$rtlil.cc:2429:And$30
  connect $auto$xprop.cc:206:encoded$17 $auto$rtlil.cc:2399:Not$34
  connect $auto$xprop.cc:205:encoded$42 $auto$rtlil.cc:2429:And$50
  connect $auto$xprop.cc:204:encoded$41 $auto$rtlil.cc:2429:And$56
  connect $auto$xprop.cc:206:encoded$43 $auto$rtlil.cc:2399:Not$60
  connect $auto$xprop.cc:205:encoded$65 $auto$rtlil.cc:2429:And$73
  connect $auto$xprop.cc:204:encoded$64 $auto$rtlil.cc:2429:And$79
  connect $auto$xprop.cc:206:encoded$66 $auto$rtlil.cc:2399:Not$83
end
