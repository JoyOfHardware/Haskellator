# Generated by Yosys 0.31+16 (git sha1 c7670b36d, clang 14.0.3 -fPIC -Os)
autoidx 2
attribute \top 1
attribute \src "uut.v:1.1-3.10"
module \uut
  attribute \src "uut.v:2.49-2.58"
  wire $nex$uut.v:2$1_Y
  attribute \src "uut.v:1.24-1.27"
  wire input 1 \CLK
  attribute \src "uut.v:1.41-1.42"
  wire input 2 \D
  attribute \src "uut.v:1.57-1.58"
  wire output 3 \Q
  attribute \src "uut.v:2.49-2.58"
  cell $nex $nex$uut.v:2$1
    parameter \A_SIGNED 0
    parameter \A_WIDTH 1
    parameter \B_SIGNED 0
    parameter \B_WIDTH 32
    parameter \Y_WIDTH 1
    connect \A \CLK
    connect \B 0
    connect \Y $nex$uut.v:2$1_Y
  end
  attribute \module_not_derived 1
  attribute \src "uut.v:2.38-2.74"
  cell $dff \cell
    parameter signed \CLK_POLARITY 0
    parameter signed \WIDTH 1
    connect \CLK $nex$uut.v:2$1_Y
    connect \D \D
    connect \Q \Q
  end
end
